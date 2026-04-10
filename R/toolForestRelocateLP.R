toolForestRelocate2 <- function(x, xTarget, vegC) {
  stopifnot(setequal(getItems(x, 1), getItems(vegC, 1)),
            identical(getYears(vegC), getYears(xTarget)),
            getItems(x, 3) == getItems(xTarget, 3),
            ndata(vegC) == 1)
  vegC <- vegC[getItems(x, 1), , ]

  out <- list()
  for (i in seq_len(nregions(xTarget))) {
    country <- getItems(xTarget, 1)[i]
    message(Sys.time(), "\t", i, "/", nregions(xTarget), " ", country)
    out[[country]] <- toolForestRelocateCountryNLP(x[country, , ], xTarget[country, , ])
  }
  message("done")
}

toolForestRelocateCountryLP <- function(x, xTarget, recursion = TRUE, tolerance = 1e-8) {
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
  objective[slack1] <- 100 / length(slack1)
  objective[slack2] <- 1 / length(slack2)

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
    intermediateTarget <- toolForestRelocateCountryLP(xCoarse, xTarget)

    out <- mbind(toolForestRelocateCountryLP(x[firstHalf, , ], xTarget = intermediateTarget["firstHalf", , ]),
                 toolForestRelocateCountryLP(x[secondHalf, , ], xTarget = intermediateTarget["secondHalf", , ]))
  } else {
    if (!getItems(xTarget, 1) %in% c("firstHalf", "secondHalf")
        && !identical(getItems(x, 1), c("firstHalf", "secondHalf"))) {
      warning("no recursion necessary for ", getItems(xTarget, 1))
    }
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

toolForestRelocateCountryNLP <- function(x, xTarget, recursionThreshold = 600, tolerance = 1e-8) {
  # TODO decide which message calls to keep
  message(Sys.time(),
          " toolForestRelocateCountryNLP ", getItems(xTarget, 1),
          " ncells=", ncells(x),
          " length=", length(x))
  stopifnot(all(startsWith(getItems(x, 1), "part")) || length(getItems(x, "iso")) == 1,
            dim(xTarget)[1] == 1,
            getItems(x, 2) == getItems(xTarget, 2),
            getItems(x, 3) == getItems(xTarget, 3),
            toolMaxExpansion(xTarget[, , "primforest"]) < tolerance)

  stopifnot(`x area is not constant over time` = max(abs(dimSums(x[, 1, ], 3) - dimSums(x[, -1, ], 3))) < tolerance,
            `xTarget area is not constant over time` = max(abs(dimSums(xTarget[, 1, ], 3)
                                                               - dimSums(xTarget[, -1, ], 3))) < tolerance)

  if (ncells(x) > recursionThreshold) {
    cells <- getItems(x, 1)
    nParts <- ceiling(length(cells) / recursionThreshold)
    parts <- split(cells, cut(seq_along(cells), nParts))
    # parts <- split(cells, cut(sample(seq_along(cells), length(cells)), nParts)) # shuffle before splitting
    stopifnot(setequal(Reduce(union, parts), cells))

    xCoarse <- do.call(mbind, lapply(seq_len(nParts), function(i) {
      return(setItems(dimSums(x[parts[[i]], , ], 1), 1, paste0("part", i)))
    }))
    stopifnot(all.equal(dimSums(xCoarse, 1), dimSums(x, 1)))

    intermediateTarget <- toolForestRelocateCountryNLP(xCoarse, xTarget,
                                                       recursionThreshold = recursionThreshold,
                                                       tolerance = tolerance / nParts)

    # TODO parallelize?
    out <- do.call(mbind, lapply(seq_len(nParts), function(i) {
      return(toolForestRelocateCountryNLP(x[parts[[i]], , ], xTarget = intermediateTarget[paste0("part", i), , ],
                                          recursionThreshold = recursionThreshold,
                                          tolerance = tolerance / nParts))
    }))
  } else {
    # objective
    objective <- function(xx) {
      return(sum((xx - x)^2))
    }

    objectiveGradient <- function(xx) {
      return(2 * (xx - x))
    }

    # constraints
    equalZero1 <- function(xx) {
      # 1. sum_over_cells(v[, y, landtype]) == xTarget[, y, landtype]
      # country level: total of each landtype must match xTarget
      return(dimSums(xx, 1) - xTarget)
    }

    xTotal <- dimSums(x, 3)
    equalZero2 <- function(xx){
      # 2. sum(v[cell, y, ]) == xTotal[, y, ]
      # cell level: total nature (primf+secdf+forestry+other) must match x
      return(dimSums(xx, 3) - xTotal)
    }

    equalZero <- function(xx) {
      result <- c(equalZero1(xx), equalZero2(xx))
      # message("equalZero max(abs(result)) ", max(abs(result)))
      return(result)
    }

    xId <- x
    xId[] <- seq_along(x)
    nyrs <- nyears(x)
    ncell <- ncells(x)

    equalZero1Gradient <- matrix(data = 0,
                                 nrow = length(equalZero1(x)),
                                 ncol = length(x))
    for (i in seq_len(nrow(equalZero1Gradient))) {
      yearId <- ((i - 1) %% nyrs) + 1
      landtypeId <- ceiling(i / nyrs)
      equalZero1Gradient[i, xId[, yearId, landtypeId]] <- 1
    }

    equalZero2Gradient <- matrix(data = 0,
                                 nrow = length(equalZero2(x)),
                                 ncol = length(x))
    for (i in seq_len(nrow(equalZero2Gradient))) {
      cellId <- ((i - 1) %% ncell) + 1
      yearId <- ceiling(i / ncell)
      equalZero2Gradient[i, xId[cellId, yearId, ]] <- 1
    }

    equalZeroGradient <- rbind(equalZero1Gradient, equalZero2Gradient)

    yearsExceptFirst <- getYears(x[, -1, ])
    lessThanZero <- function(xx) {
      # 3. v[cell, y, primf] - v[cell, y - 1, primf] <= 0
      # cell level: primf cannot be larger than in previous timestep
      result <- xx[, -1, "primforest"] - setYears(xx[, -nyrs, "primforest"], yearsExceptFirst)
      # message("lessThanZero max(result) ", max(result))
      return(result)
    }

    # gradient/derivative of v[cell, y, primf] - v[cell, y - 1, primf] <= 0
    # is independent of input, so can calculate statically
    # is the same for all cells & only landtype == primf, so simplified: v[y] - v[y - 1] <= 0
    # 1 if differentiationVariable (matrix column) == y
    # -1 if differentiationVariable (matrix column) == y - 1
    # 0 otherwise
    lessThanZeroGradient <- matrix(data = 0,
                                   nrow = ncell * (nyrs - 1),
                                   ncol = length(x))

    for (i in seq_len(nyrs - 1)) {
      for (j in seq_len(ncell)) {
        lessThanZeroGradient[j + ncell * (i - 1), xId[j, i + 1, "primforest"]] <- 1
        lessThanZeroGradient[j + ncell * (i - 1), xId[j, i, "primforest"]] <- -1
      }
    }

    magpieWrapper <- function(f) {
      wrappedF <- function(xx) {
        xxIn <- x
        xxIn[] <- xx
        return(as.vector(f(xxIn)))
      }
      return(wrappedF)
    }

    x0 <- xTarget / dimSums(xTarget, 3) * dimSums(x, 3)

    solution <- nloptr::nloptr(x0 = as.vector(x0),
                               eval_f = magpieWrapper(objective),
                               eval_grad_f = magpieWrapper(objectiveGradient),
                               lb = rep(0, length(x)),
                               eval_g_eq = magpieWrapper(equalZero),
                               eval_jac_g_eq = function(xx) equalZeroGradient,
                               eval_g_ineq = magpieWrapper(lessThanZero),
                               eval_jac_g_ineq = function(xx) lessThanZeroGradient,
                               opts = list(algorithm = "NLOPT_LD_SLSQP",
                                           ftol_abs = tolerance, # stop when objective value change < tolerance
                                           tol_constraints_ineq = rep(tolerance, length(lessThanZero(x))),
                                           tol_constraints_eq = rep(tolerance, length(equalZero(x))),
                                           print_level = 0))
    message("nloptr done ", Sys.time())

    if (!grepl("NLOPT_[XF]TOL_REACHED: Optimization stopped because [xf]tol_rel or [xf]tol_abs .above. was reached",
               solution$message)) {
      warning(solution$message)
    }
    out <- x
    out[] <- solution$solution
  }

  stopifnot(abs(dimSums(out, 1) - xTarget) < tolerance) # aggregated out == xTarget
  stopifnot(abs(dimSums(out, 3) - dimSums(x, 3)) < tolerance) # land area per grid cell is unchanged
  stopifnot(toolMaxExpansion(out[, , "primforest"]) < tolerance) # no primforest expansion

  return(out)
}
