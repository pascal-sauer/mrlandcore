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
  luTotal <- dimSums(lu, 3)
  for (y in seq_len(nyears(natTarget))) {
    message(getYears(lu)[y])
    variables <- c(paste0(getItems(lu, 1), ".", rep(getItems(lu, 3), ncells(lu))),
                   paste0("slackPositive.", getItems(lu, 3)),
                   paste0("slackNegative.", getItems(lu, 3)))

    constraints <- matrix(nrow = 0, ncol = length(variables))
    colnames(constraints) <- variables
    rightHandSide <- c()
    constraintsDirection <- c()

    # 1. country level: total of each landtype should match natTarget
    for (landtype in getItems(lu, 3)) {
      equation <- matrix(0, nrow = 1, ncol = length(variables))
      colnames(equation) <- variables
      equation[, endsWith(variables, landtype)] <- 1
      equation[, paste0("slackNegative.", landtype)] <- -1
      constraints <- rbind(constraints, equation)
      constraintsDirection <- c(constraintsDirection, "==")
      rightHandSide <- c(rightHandSide, natTarget[, y, landtype])
    }

    # 2. cell level: total nature (primf+secdf+forestry+other) must match lu
    equations <- matrix(0, nrow = ncells(lu), ncol = length(variables))
    colnames(equations) <- variables
    for (i in seq_len(ncells(lu))) {
      equations[i, startsWith(variables, getItems(lu, 1)[i])] <- 1
    }
    constraints <- rbind(constraints, equations)
    constraintsDirection <- c(constraintsDirection, rep("==", nrow(equations)))
    rightHandSide <- c(rightHandSide, luTotal[, y, ])

    if (y > 1) {
      equations <- matrix(0, nrow = ncells(lu), ncol = length(variables))
      colnames(equations) <- variables
      i <- 1
      for (cell in getItems(lu, 1)) {
        equations[i, paste0(cell, ".primforest")] <- 1
        i <- i + 1
      }
      constraints <- rbind(constraints, equations)
      constraintsDirection <- c(constraintsDirection, rep("<=", nrow(equations)))
      rightHandSide <- c(rightHandSide, lu[, y - 1, "primforest"])
    }

    solution <- lpSolve::lp(direction = "min",
                            objective.in = ifelse(startsWith(variables, "slack"), 1, 0),
                            const.mat = constraints,
                            const.dir = constraintsDirection,
                            const.rhs = rightHandSide)

    solution <- data.frame(x.y.iso = sub("\\.[^.]+$", "", variables),
                           landuse = sub("^.+\\.", "", variables),
                           value = solution$solution)
    solution <- solution[!startsWith(solution$x.y.iso, "slack"), ]
    solution <- as.magpie(solution, spatial = "x.y.iso")
    getItems(solution, 1, raw = TRUE) <- gsub("_", ".", getItems(solution, 1))
    lu[, y, ] <- solution
  }
  return(lu)
}
