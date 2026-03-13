toolForestRelocateLP <- function(lu, natTarget, vegC) {
  luCountry <- dimSums(lu, dim = c("x", "y"))
  stopifnot(sameDims(luCountry, natTarget),
            setequal(getItems(lu, 1), getItems(vegC, 1)),
            identical(getYears(vegC), getYears(natTarget)),
            ndata(vegC) == 1)
  luCountry <- luCountry[getItems(natTarget, 1), , getItems(natTarget, 3)]
  lu <- lu[, , getItems(natTarget, 3)]
  vegC <- vegC[getItems(lu, 1), , ]

  # harmonize total area
  scalingFactor <- dimSums(luCountry, 3) / dimSums(natTarget, 3)
  scalingFactor[is.na(scalingFactor)] <- 1
  stopifnot(0 <= scalingFactor, scalingFactor < 2)
  natTarget <- natTarget * scalingFactor

  out <- list()
  for (i in seq_len(nregions(natTarget))) {
    country <- getItems(natTarget, 1)[i]
    message(i, "/", nregions(natTarget), " ", country)
    out[country] <- toolForestRelocateCountry(lu[country, , ], natTarget[country, , ])
  }
  message("done")
}

toolForestRelocateCountry <- function(lu, natTarget) {
  stopifnot(length(getItems(lu, "iso")) == 1,
            dim(natTarget)[1] == 1,
            getItems(lu, 2) == getItems(natTarget, 2),
            getItems(lu, 3) == getItems(natTarget, 3))

  cells <- getItems(lu, 1)
  years <- getItems(lu, 2)
  landtypes <- getItems(lu, 3)

  luTotal <- dimSums(lu, 3)

  # lp variables
  v <- rbind(expand.grid(cells, years, landtypes, stringsAsFactors = FALSE),
             expand.grid("slackPositive", years, landtypes, stringsAsFactors = FALSE),
             expand.grid("slackNegative", years, landtypes, stringsAsFactors = FALSE))
  colnames(v) <- c("cell", "year", "landtype")

  # get lp variable id number
  vid <- function(cell, year, landtype) {
    if (missing(cell) || is.null(cell)) {
      return(which(v$year == year & v$landtype == landtype))
    } else if (missing(landtype) || is.null(landtype)) {
      return(which(v$cell == cell & v$year == year))
    } else {
      return(which(v$cell == cell & v$year == year & v$landtype == landtype))
    }
  }

  nConstraints1 <- nyears(lu) * ndata(lu)
  nConstraints2 <- nyears(lu) * ncells(lu)
  nConstraints3 <- (nyears(lu) - 1) * ncells(lu)
  nConstraints <- nConstraints1 + nConstraints2 + nConstraints3

  # dense constraint matrix: constraint number, column/variable id number, value
  constraints <- array(dim = c(0, 3))
  rightHandSide <- rep(NA, nConstraints)
  constraintsDirection <- rep(NA, nConstraints)

  # index of the next constraint to add; independent of row number of `constraints`, because that's a dense matrix
  iConstraint <- 1

  for (y in seq_len(nyears(natTarget))) {
    message(Sys.time(), " - ", years[y])

    # 1. sum_over_cells(v[, y, landtype]) + v["slackPositive", y, landtype] - v["slackNegative", y, landtype]
    #    == natTarget[, y, landtype]
    # country level: total of each landtype should match natTarget
    for (landtype in landtypes) {
      newConstraint <- array(dim = c(ncells(lu) + 2, 3))
      newConstraint[, 1] <- iConstraint
      newConstraint[, 2] <- vid(, years[y], landtype)
      newConstraint[, 3] <- ifelse(v$cell[newConstraint[, 2]] == "slackNegative", -1, 1)
      constraints <- rbind(constraints, newConstraint)

      constraintsDirection[iConstraint] <- "=="
      rightHandSide[iConstraint] <- natTarget[, y, landtype]
      iConstraint <- iConstraint + 1
    }
    message(Sys.time(), " - 1. added")
    # 2. sum(v[cell, y, ]) == luTotal[, y, ]
    # cell level: total nature (primf+secdf+forestry+other) must match lu
    for (i in seq_len(ncells(lu))) {
      constraints <- rbind(constraints,
                           cbind(iConstraint + i - 1,
                                 vid(cells[i], years[y], ),
                                 1))
    }
    nConstraintsAdded <- ncells(lu)

    constraintsDirection[iConstraint:(iConstraint + nConstraintsAdded - 1)] <- "=="
    rightHandSide[iConstraint:(iConstraint + nConstraintsAdded - 1)] <- luTotal[, y, ]
    iConstraint <- iConstraint + nConstraintsAdded
    message(Sys.time(), " - 2. added")

    # 3. v[cell, y, primf] - v[cell, y - 1, primf] <= 0
    # cell level: primf cannot be larger than in previous timestep
    if (y > 1) {
      for (i in seq_len(ncells(lu))) {
        # TODO fix this constraint - primforest is still growing
        constraints <- rbind(constraints,
                             cbind(iConstraint + i - 1,
                                   c(vid(cells[i], years[y], "primforest"),
                                     vid(cells[i], years[y - 1], "primforest")),
                                   c(1, -1)))
      }
      nConstraintsAdded <- ncells(lu)

      constraintsDirection[iConstraint:(iConstraint + nConstraintsAdded - 1)] <- "<="
      rightHandSide[iConstraint:(iConstraint + nConstraintsAdded - 1)] <- 0
      iConstraint <- iConstraint + nConstraintsAdded
      message(Sys.time(), " - 3. added")
    }
  }

  message(Sys.time(), " - starting solve...")
  solution <- lpSolve::lp(direction = "min",
                          objective.in = ifelse(v$cell == "slack", 1, 0),
                          dense.const = constraints,
                          const.dir = constraintsDirection,
                          const.rhs = rightHandSide)
  message(Sys.time(), " - solved")

  solution <- data.frame(x.y.iso = v$cell,
                         year = v$year,
                         landuse = v$landtype,
                         value = solution$solution)
  solution <- solution[!startsWith(solution$x.y.iso, "slack"), ]
  solution <- as.magpie(solution, spatial = "x.y.iso")
  getItems(solution, 1, raw = TRUE) <- gsub("_", ".", getItems(solution, 1))

  message(Sys.time(), " - done")
  return(solution)
}
