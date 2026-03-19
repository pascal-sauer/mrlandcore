toolForestRelocateLP <- function(lu, natTarget, vegC) {
  luCountry <- dimSums(lu, dim = c("x", "y"))
  stopifnot(sameDims(luCountry, natTarget),
            setequal(getItems(lu, 1), getItems(vegC, 1)),
            identical(getYears(vegC), getYears(natTarget)),
            ndata(vegC) == 1)
  luCountry <- luCountry[getItems(natTarget, 1), , getItems(natTarget, 3)]
  lu <- lu[, , getItems(natTarget, 3)]
  vegC <- vegC[getItems(lu, 1), , ]

  out <- list()
  for (i in seq_len(nregions(natTarget))) {
    country <- getItems(natTarget, 1)[i]
    message(i, "/", nregions(natTarget), " ", country)
    out[country] <- toolForestRelocateCountry(lu[country, , ], natTarget[country, , ])
  }
  message("done")
}

toolForestRelocateCountry <- function(lu, natTarget, recursion = TRUE, tolerance = 1e-8) {
  stopifnot(identical(getItems(lu, 1), c("firstHalf", "secondHalf")) || length(getItems(lu, "iso")) == 1,
            dim(natTarget)[1] == 1,
            getItems(lu, 2) == getItems(natTarget, 2),
            getItems(lu, 3) == getItems(natTarget, 3),
            toolMaxExpansion(natTarget[, , "primforest"]) < tolerance)

  cells <- getItems(lu, 1)
  years <- getItems(lu, 2)
  landtypes <- getItems(lu, 3)

  luTotal <- dimSums(lu, 3)

  # lp variables
  v <- lu
  v[] <- seq_along(v)
  nVariables <- length(v)

  slack1 <- new.magpie(c("positive", "negative"), years, landtypes)
  slack1[] <- nVariables + seq_along(slack1)
  nVariables <- nVariables + length(slack1)

  # objective
  objective <- rep(0, nVariables)
  objective[slack1] <- 1

  # constraints
  nConstraints1 <- nyears(lu) * ndata(lu)
  nConstraints2 <- nyears(lu) * ncells(lu)
  nConstraints3 <- (nyears(lu) - 1) * ncells(lu)
  nConstraints <- nConstraints1 + nConstraints2 + nConstraints3

  if (recursion && nVariables + nConstraints > 1e4) {
    firstHalf <- cells[seq_len(length(cells) / 2)]
    secondHalf <- setdiff(cells, firstHalf)
    stopifnot(setequal(c(firstHalf, secondHalf), cells))

    luCoarse <- mbind(setItems(dimSums(lu[firstHalf, , ], 1), 1, "firstHalf"),
                      setItems(dimSums(lu[secondHalf, , ], 1), 1, "secondHalf"))
    stopifnot(all.equal(dimSums(luCoarse, 1), dimSums(lu, 1)))
    intermediateTarget <- toolForestRelocateCountry(luCoarse, natTarget)

    out <- mbind(toolForestRelocateCountry(lu[firstHalf, , ], natTarget = intermediateTarget["firstHalf", , ]),
                 toolForestRelocateCountry(lu[secondHalf, , ], natTarget = intermediateTarget["secondHalf", , ]))
  } else {
    # dense constraint matrix: constraint number, column/variable id number, value
    constraints <- array(dim = c(0, 3))
    rightHandSide <- rep(NA, nConstraints)
    constraintsDirection <- rep(NA, nConstraints)

    # index of the next constraint to add; independent of row number of `constraints`, because that's a dense matrix
    iConstraint <- 1

    for (y in seq_along(years)) {

      # 1. sum_over_cells(v[, y, landtype]) + v["slackPositive", y, landtype] - v["slackNegative", y, landtype]
      #    == natTarget[, y, landtype]
      # country level: total of each landtype should match natTarget
      for (landtype in landtypes) {
        newConstraint <- array(dim = c(ncells(lu) + 2, 3))
        newConstraint[, 1] <- iConstraint
        newConstraint[, 2] <- c(v[, y, landtype], slack1[, y, landtype])
        newConstraint[, 3] <- ifelse(newConstraint[, 2] %in% slack1["negative", y, landtype], -1, 1)
        constraints <- rbind(constraints, newConstraint)

        constraintsDirection[iConstraint] <- "=="
        rightHandSide[iConstraint] <- natTarget[, y, landtype]
        iConstraint <- iConstraint + 1
      }

      # 2. sum(v[cell, y, ]) == luTotal[, y, ]
      # cell level: total nature (primf+secdf+forestry+other) must match lu
      constraintIds <- rep(iConstraint:(iConstraint + length(cells) - 1),
                           length(landtypes))
      variableIds <- as.vector(v[, y, ])
      stopifnot(length(constraintIds) == length(variableIds))
      constraints <- rbind(constraints, cbind(constraintIds, variableIds, 1))

      nConstraintsAdded <- ncells(lu)

      constraintsDirection[iConstraint:(iConstraint + nConstraintsAdded - 1)] <- "=="
      rightHandSide[iConstraint:(iConstraint + nConstraintsAdded - 1)] <- luTotal[, y, ]
      iConstraint <- iConstraint + nConstraintsAdded

      # 3. v[cell, y, primf] - v[cell, y - 1, primf] <= 0
      # cell level: primf cannot be larger than in previous timestep
      if (y > 1) {
        constraintIds <- rep(iConstraint:(iConstraint + length(cells) - 1), 2)
        variableIds <- c(v[, y, "primforest"], v[, y - 1, "primforest"])
        values <- rep(c(1, -1), each = length(cells))
        stopifnot(length(constraintIds) == length(variableIds), length(variableIds) == length(values))
        constraints <- rbind(constraints, cbind(constraintIds, variableIds, values))
        nConstraintsAdded <- length(cells)

        constraintsDirection[iConstraint:(iConstraint + nConstraintsAdded - 1)] <- "<="
        rightHandSide[iConstraint:(iConstraint + nConstraintsAdded - 1)] <- 0
        iConstraint <- iConstraint + nConstraintsAdded
      }
    }

    stopifnot(nConstraints == iConstraint - 1)

    solution <- lpSolve::lp(direction = "min",
                            objective.in = objective,
                            dense.const = constraints,
                            const.dir = constraintsDirection,
                            const.rhs = rightHandSide)

    out <- v
    out[] <- solution$solution[v]
  }

  stopifnot(abs(dimSums(out, 1) - natTarget) < tolerance) # target is reached
  stopifnot(abs(dimSums(out, 3) - dimSums(lu, 3)) < tolerance) # land area per grid cell is unchanged
  stopifnot(toolMaxExpansion(out[, , "primforest"]) < tolerance) # no primforest expansion

  return(out)
}
