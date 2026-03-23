toolForestRelocateLP <- function(x, xTarget, vegC) {
  stopifnot(setequal(getItems(x, 1), getItems(vegC, 1)),
            identical(getYears(vegC), getYears(xTarget)),
            getItems(x, 3) == getItems(xTarget, 3),
            ndata(vegC) == 1)
  vegC <- vegC[getItems(x, 1), , ]

  out <- list()
  for (i in seq_len(nregions(xTarget))) {
    country <- getItems(xTarget, 1)[i]
    message(Sys.time(), "\t", i, "/", nregions(xTarget), " ", country)
    out[[country]] <- toolForestRelocateCountry(x[country, , ], xTarget[country, , ])
  }
  message("done")
}

toolForestRelocateCountry <- function(x, xTarget, recursion = TRUE, tolerance = 1e-8) {
  stopifnot(identical(getItems(x, 1), c("firstHalf", "secondHalf")) || length(getItems(x, "iso")) == 1,
            dim(xTarget)[1] == 1,
            getItems(x, 2) == getItems(xTarget, 2),
            getItems(x, 3) == getItems(xTarget, 3),
            toolMaxExpansion(xTarget[, , "primforest"]) < tolerance)

  # area constant over time
  stopifnot(max(abs(dimSums(x[, 1, ], 3) - dimSums(x[, -1, ], 3))) < tolerance,
            max(abs(dimSums(xTarget[, 1, ], 3) - dimSums(xTarget[, -1, ], 3))) < tolerance)

  cells <- getItems(x, 1)
  years <- getItems(x, 2)
  landtypes <- getItems(x, 3)

  xTotal <- dimSums(x, 3)

  # lp variables
  v <- x
  v[] <- seq_along(v) # value of v[cell, year, landtype] is the variable id of that cell+year+landtype combination
  nVariables <- length(v)

  slack1 <- new.magpie(c("positive", "negative"), years, landtypes)
  slack1[] <- nVariables + seq_along(slack1)
  nVariables <- nVariables + length(slack1)

  slack2 <- add_dimension(x, 3.2, "slack", c("positive", "negative"))
  slack2[] <- nVariables + seq_along(slack2)
  nVariables <- nVariables + length(slack2)

  # objective
  objective <- rep(0, nVariables)
  objective[slack1] <- 100 * length(slack2) / length(slack1)
  objective[slack2] <- 1

  # constraints
  nConstraints1 <- nyears(x) * ndata(x)
  nConstraints2 <- nyears(x) * ncells(x)
  nConstraints3 <- (nyears(x) - 1) * ncells(x)
  nConstraints4 <- length(x)
  nConstraints <- nConstraints1 + nConstraints2 + nConstraints3 + nConstraints4

  if (recursion && nVariables + nConstraints > 1e4) {
    firstHalf <- cells[seq_len(length(cells) / 2)]
    secondHalf <- setdiff(cells, firstHalf)
    stopifnot(setequal(c(firstHalf, secondHalf), cells))

    xCoarse <- mbind(setItems(dimSums(x[firstHalf, , ], 1), 1, "firstHalf"),
                     setItems(dimSums(x[secondHalf, , ], 1), 1, "secondHalf"))
    stopifnot(all.equal(dimSums(xCoarse, 1), dimSums(x, 1)))
    intermediateTarget <- toolForestRelocateCountry(xCoarse, xTarget)

    out <- mbind(toolForestRelocateCountry(x[firstHalf, , ], xTarget = intermediateTarget["firstHalf", , ]),
                 toolForestRelocateCountry(x[secondHalf, , ], xTarget = intermediateTarget["secondHalf", , ]))
  } else {
    # dense constraint matrix: constraint number, column/variable id number, value
    constraints <- array(dim = c(0, 3))
    rightHandSide <- rep(NA, nConstraints)
    constraintsDirection <- rep(NA, nConstraints)

    # index of the next constraint to add; independent of row number of `constraints`, because that's a dense matrix
    iConstraint <- 1

    for (y in seq_along(years)) {

      # 1. sum_over_cells(v[, y, landtype]) + v["slackPositive", y, landtype] - v["slackNegative", y, landtype]
      #    == xTarget[, y, landtype]
      # country level: total of each landtype should match xTarget
      for (landtype in landtypes) {
        newConstraint <- array(dim = c(ncells(x) + 2, 3))
        newConstraint[, 1] <- iConstraint
        newConstraint[, 2] <- c(v[, y, landtype], slack1[, y, landtype])
        newConstraint[, 3] <- ifelse(newConstraint[, 2] %in% slack1["negative", y, landtype], -1, 1)
        constraints <- rbind(constraints, newConstraint)

        constraintsDirection[iConstraint] <- "=="
        rightHandSide[iConstraint] <- xTarget[, y, landtype]
        iConstraint <- iConstraint + 1
      }

      # 2. sum(v[cell, y, ]) == xTotal[, y, ]
      # cell level: total nature (primf+secdf+forestry+other) must match x
      constraintIds <- rep(iConstraint:(iConstraint + length(cells) - 1),
                           length(landtypes))
      variableIds <- as.vector(v[, y, ])
      stopifnot(length(constraintIds) == length(variableIds))
      constraints <- rbind(constraints, cbind(constraintIds, variableIds, 1))

      nConstraintsAdded <- ncells(x)

      constraintsDirection[iConstraint:(iConstraint + nConstraintsAdded - 1)] <- "=="
      rightHandSide[iConstraint:(iConstraint + nConstraintsAdded - 1)] <- xTotal[, y, ]
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

      # 4. v[cell, y, landtype] + slack2[cell, y, landtype] == x[cell, y, landtype]
      # cell level: keep x spatial information as much as possible
      nConstraintsAdded <- length(cells) * length(landtypes)
      constraintIds <- rep(iConstraint:(iConstraint + nConstraintsAdded - 1), 3)
      variableIds <- c(v[, y, ], slack2[, y, "positive"], slack2[, y, "negative"])
      stopifnot(length(constraintIds) == length(variableIds))
      values <- ifelse(variableIds %in% slack2[, y, "negative"], -1, 1)
      constraints <- rbind(constraints, cbind(constraintIds, variableIds, values))

      constraintsDirection[iConstraint:(iConstraint + nConstraintsAdded - 1)] <- "=="
      rightHandSide[iConstraint:(iConstraint + nConstraintsAdded - 1)] <- x[, y, ]
      iConstraint <- iConstraint + nConstraintsAdded
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

  maxdiff <- max(abs(dimSums(out, 1) - xTarget))
  if (maxdiff > tolerance) {
    warning("xTarget was not reached, maxdiff: ", maxdiff)
  }

  stopifnot(abs(dimSums(out, 3) - dimSums(x, 3)) < tolerance) # land area per grid cell is unchanged
  stopifnot(toolMaxExpansion(out[, , "primforest"]) < tolerance) # no primforest expansion

  return(out)
}
