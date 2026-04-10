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
