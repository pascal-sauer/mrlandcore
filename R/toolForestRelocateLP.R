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
  stopifnot(length(getItems(lu, "iso")) == 1)
  luTotal <- dimSums(lu, 3)

  # lp variables
  v <- rbind(expand.grid(getItems(lu, 1), getItems(lu, 2), getItems(lu, 3), stringsAsFactors = FALSE),
             expand.grid("slackPositive", getItems(lu, 2), getItems(lu, 3), stringsAsFactors = FALSE),
             expand.grid("slackNegative", getItems(lu, 2), getItems(lu, 3), stringsAsFactors = FALSE))
  colnames(v) <- c("cell", "year", "landtype")

  # get lp variable id (row) number
  vid <- function(cell, year, landtype) {
    if (missing(cell) || is.null(cell)) {
      return(which(v$year == year & v$landtype == landtype))
    } else if (missing(landtype) || is.null(landtype)) {
      return(which(v$cell == cell & v$year == year))
    } else {
      return(which(v$cell == cell & v$year == year & v$landtype == landtype))
    }
  }
  constraints <- matrix(nrow = 0, ncol = nrow(v))
  rightHandSide <- c()
  constraintsDirection <- c()

  for (y in seq_len(nyears(natTarget))) {
    year <- getYears(natTarget)[y]
    message(Sys.time(), " - ", year)

    # 1. country level: total of each landtype should match natTarget
    for (landtype in getItems(lu, 3)) {
      equation <- matrix(0, nrow = 1, ncol = nrow(v))
      equation[, vid(, year, landtype)] <- 1
      equation[, vid("slackNegative", year, landtype)] <- -1
      constraints <- rbind(constraints, equation)
      constraintsDirection <- c(constraintsDirection, "==")
      rightHandSide <- c(rightHandSide, natTarget[, y, landtype])
    }
    message(Sys.time(), " - 1. added")

    # 2. cell level: total nature (primf+secdf+forestry+other) must match lu
    equations <- matrix(0, nrow = ncells(lu), ncol = nrow(v))
    for (i in seq_len(ncells(lu))) {
      equations[i, vid(getItems(lu, 1)[i], year, )] <- 1
    }
    constraints <- rbind(constraints, equations)
    constraintsDirection <- c(constraintsDirection, rep("==", nrow(equations)))
    rightHandSide <- c(rightHandSide, luTotal[, y, ])
    message(Sys.time(), " - 2. added")

    # 3. primf cannot be larger than in previous timestep
    if (y > 1) {
      equations <- matrix(0, nrow = ncells(lu), ncol = nrow(v))
      i <- 1
      for (cell in getItems(lu, 1)) {
        equations[i, vid(cell, year, "primforest")] <- 1
        i <- i + 1
      }
      constraints <- rbind(constraints, equations)
      constraintsDirection <- c(constraintsDirection, rep("<=", nrow(equations)))
      rightHandSide <- c(rightHandSide, lu[, y - 1, "primforest"])
    }
  }
  message(Sys.time(), " - 3. added")

  solution <- lpSolve::lp(direction = "min",
                          objective.in = ifelse(v$cell == "slack", 1, 0),
                          const.mat = constraints,
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
