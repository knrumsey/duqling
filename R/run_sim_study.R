#' @title Reproducible Simulation Study for Test Functions
#'
#' @description
#' Run a configurable simulation study over one or more test functions
#' from \pkg{duqling}, training one or more emulators and evaluating their
#' point accuracy, uncertainty calibration, and probabilistic scores.
#'
#' @param fit_func either a single function or a list of functions. if
#'   \code{pred_func} is \emph{not} specified, each \code{fit_func} must have
#'   signature \code{function(X_train, y_train, X_test)} and return signature
#'   identical to \code{pred_func} (read below).
#' @param pred_func optional function or list of functions with signature
#'   \code{function(fitted_object, X_test)} returning one of the result shapes
#'   described above. if omitted, \code{fit_func} must directly produce
#'   predictions for \code{X_test} as either:
#'   (1) an \eqn{M \times n} matrix of predictive samples (recommended),
#'   (2) a length-\eqn{n} numeric vector of point predictions, or
#'   (3) a named list with fields:
#'      - samples: An M x n matrix of samples from the predictive distribution.
#'                 If NULL, you must provide a `preds` field.
#'      - preds: An n-vector of predictions. If NULL, you must provide a
#'               `samples` field.
#'      - intervals: A 2 x n x K array (or a matrix, if K = 1) where
#'                   K = length(conf_level). If NULL, then quantiles of the
#'                   samples field will be used.
#' @param fnames character vector of function names from \code{duqling}.
#'   See \code{\link[duqling]{quack()}} for available names. if \code{NULL},
#'   defaults to all 1D duqling functions (\code{quack(input_dim = 1)$fname}).
#' @param conf_level numeric vector: confidence levels in (0,1) to
#'   compute coverages and mean interval scores.
#' @param score logical: compute CRPS?
#' @param n_train integer or integer vector: training set size(s) to evaluate.
#' @param n_test integer: test set size for each scenario.
#' @param nsr numeric (>= 0): the noise-to-signal ratio (inverse of the
#'   more-standard signal-to-noise ratio).
#' @param design_type character: sampling heuristic for train/test inputs.
#'   one of \code{"LHS"}, \code{"grid"}, or \code{"random"}.
#' @param replications a single integer \code{r} (runs replications 1:r),
#'   a vector of integers (runs only those specific replications), or a
#'   single negative integer \code{-k} (runs only replication k).
#' @param seed integer: seed for deterministic case-seeding.
#'   for reproducibility, we discourage the use of this argument.
#' @param method_names optional character vector of method names. length must
#'   equal \code{length(fit_func)}. Defaults to \code{"method1"},
#'   \code{"method2"}, ...
#' @param mc_cores integer: number of cores for parallelization over
#'   replications.
#' @param fallback_on_error optional logical: use deterministic null predictive
#'   model (\code{N(mean(y_train), sd(y_train))}) if error occurs in fitting
#'   or predicting? Default is \code{TRUE}.
#' @param verbose logical: report progress?
#' @return A \code{data.frame} where each row corresponds to the results of a
#'   single simulation scenario, replication index, and method.
#'   The columns include:
#'   \itemize{
#'     \item \code{method}: Name of the fitted method, if provided.
#'       Unique names (\code{method<indx>}) names are assigned otherwise.
#'     \item \code{fname}: The test function name from
#'       \code{duqling::quack()$fname}.
#'     \item \code{input_dim}: Input dimension of the test function.
#'     \item \code{n}: Number of training points.
#'     \item \code{nsr}: Noise-to-signal ratio used in data generation.
#'     \item \code{design_type}: Type of experimental design used for data
#'       generation ("LHS", "grid", or "random").
#'     \item \code{rep}: Replication index.
#'     \item \code{t_fit}: Elapsed time (seconds) for model fitting (if
#'       applicable).
#'     \item \code{t_pred}: Elapsed time (seconds) for model prediction (if
#'       applicable).
#'     \item \code{t_tot}: Total elapsed time for fitting and prediction.
#'     \item \code{failure_type}: Indicates if the method failed at "fit",
#'       "pred", or "none" (if no failure).
#'     \item \code{RMSE}: Root mean squared error on the test set.
#'     \item \code{FVU}: Fraction of variance unexplained (RMSE\(^2\) divided
#'       by total variance).
#'     \item \code{COVER<level>}: Coverage rates for each specified confidence
#'       level (e.g., \code{COVER0.8}, \code{COVER0.9}, etc.).
#'     \item \code{MIS<level>}: Mean interval scores for each specified
#'       confidence level.
#'     \item \code{CRPS}: Mean continuous ranked probability score (CRPS) on
#'       the test set.
#'     \item \code{CRPS_min}, \code{CRPS_Q1}, \code{CRPS_med}, \code{CRPS_Q3},
#'       \code{CRPS_max}: Summary statistics (minimum, first quartile, median,
#'       third quartile, maximum) for the CRPS distribution over the test set.
#'   }
#' Additional columns may be included for other metrics or settings depending
#' on options provided.
#'
#' @details
#' Code to conduct a reproducible simulation study to compare emulators. By
#' reporting the parameters to the study, other authors can compare their
#' results directly.
#' Only \code{fit_func} needs to be specified, but only the total time will be
#' reported. The simplest (and recommended) approach is that the
#' \code{fit_func} (or \code{pred_func}) should return a matrix of posterior
#' samples, with one column per test point (e.g., per row in \code{X_test}).
#' Any number of rows (predictive samples) is allowed. In this case, the mean
#' of the samples is used as a prediction and the R \code{quantile} function is
#' used to construct confidence intervals. This default behavior can be changed
#' by instead allowing \code{fit_func} (or \code{pred_func}) to return a named
#' list with fields i) \code{samples} (required), ii) \code{preds} (optional; a
#' vector of predictions), and iii) intervals (optional; a 2 by n by k array of
#' interval bounds where n is the number of test points and k is
#' \code{length(conf_level)}).
#'
#' @references
#' Gneiting, T., & Raftery, A. E. (2007). Strictly Proper Scoring Rules,
#' Prediction, and Estimation. \emph{JASA}, 102(477), 359–378.
#' Surjanovic, S., & Bingham, D. (2013). Virtual Library of Simulation
#' Experiments: Test Functions and Datasets. Simon Fraser University.
#'
#' @export
#' @examples
#'
#' # METHOD 1: Linear Regression
#' lm_fit <- function(X, y) {
#'   Xdf <- as.data.frame(X)
#'   colnames(Xdf) <- paste0("X", seq_len(ncol(X)))
#'   lm(y ~ ., data = Xdf)
#' }
#' lm_pred <- function(obj, Xt) {
#'   Xt_df <- as.data.frame(Xt)
#'   colnames(Xt_df) <- paste0("X", seq_len(ncol(Xt_df)))
#'   mean_pred <- predict(obj, Xt_df)
#'   sigma_est <- sd(residuals(obj))
#'   nt <- nrow(Xt_df)
#'   preds <- t(replicate(1000, mean_pred + rnorm(nt, 0, sigma_est)))
#' }
#'
#' # METHOD 2: Projection Pursuit
#' ppr_fit <- function(X, y) {
#'   ppr(X, y, nterms = 3)
#' }
#' ppr_pred <- function(obj, Xt) {
#'   nt <- nrow(Xt)
#'   mean_pred <- predict(obj, data.frame(Xt))
#'   sigma_est <- sd(residuals(obj))
#'   preds <- t(replicate(1000, mean_pred + rnorm(nt, 0, sigma_est)))
#' }
#'
#' my_fit <- list(lm_fit, ppr_fit)
#' my_pred <- list(lm_pred, ppr_pred)
#' run_sim_study(my_fit, my_pred,
#'   fnames = c("dms_additive", "borehole"),
#'   n_train = 50,
#'   replications = 2,
#'   method_names = c("lm", "ppr") # Optional
#' )
#'
run_sim_study <- function(fit_func, pred_func = NULL,
                          fnames = NULL,
                          conf_level = c(0.8, 0.9, 0.95, 0.99),
                          score = TRUE,
                          n_train = 100,
                          n_test = 1000,
                          nsr = 0,
                          design_type = "LHS",
                          replications = 1,
                          seed = 42,
                          method_names = NULL,
                          mc_cores = 1,
                          fallback_on_error = TRUE,
                          verbose = TRUE) {
  if (length(replications) == 1) {
    rep_vec <- if (replications < 0) -replications else seq_len(replications)
  } else {
    if (length(replications) == 0) {
      warning("No replications given. Defaulting to 1 replication.")
      rep_vec <- 1L
    } else {
      rep_vec <- replications
    }
  }

  # default values for 'fnames' and 'method_names'
  fnames <- fnames %||% duqling::quack(input_dim = 1)$fname
  method_names <- method_names %||% names(fit_func)
  method_names <- method_names %||% paste0("method", seq_along(fit_func))

  # parallelize if there are more than 1 available CPU cores.
  # note: Windows OS doesn't support forking.
  apply_fun <- if (mc_cores > 1 && .Platform$OS.type != "windows") {
    function(X, FUN, ...) parallel::mclapply(X, FUN, ..., mc.cores = mc_cores)
  } else {
    lapply
  }

  # prebuild all combinations of training set sizes, noise-to-signal ratios,
  # and sampling methods to feed as inputs to model training.
  combos <- expand.grid(
    n = n_train,
    nsr = nsr,
    dsgn = design_type,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  out_all <- vector("list", length(fnames) * nrow(combos))
  idx <- 0L

  for (f_idx in seq_along(fnames)) {
    fn <- fnames[f_idx]
    input_dim <- duqling::quack(fn)$input_dim
    if (verbose) {
      cat("Starting function ", f_idx, "/",
        length(fnames), ": ", fn, "\n",
        sep = ""
      )
    }
    for (i in seq_len(nrow(combos))) {
      n <- combos$n[i]
      nsr_i <- combos$nsr[i]
      dsgn <- combos$dsgn[i]
      if (verbose) {
        cat(
          "\t Running all combinations and replications for",
          "n =", n, " nsr =", nsr_i, " dsgn =", dsgn, "\n"
        )
      }

      results <- apply_fun(
        rep_vec,
        run_one_sim_case,
        seed = seed, fn = fn, fnum = NA, p = input_dim, n = n,
        conf_level = conf_level, score = score,
        nsr = nsr_i, dsgn = dsgn, n_test = n_test,
        method_names = method_names, fit_func = fit_func,
        pred_func = pred_func, fallback = fallback_on_error,
        verbose = verbose
      )

      idx <- idx + 1L
      out_all[[idx]] <- do.call(rbind, results)
    }
  }

  do.call(rbind, out_all)
}


#' @title k-fold Cartesian product.
#' @note orders items lexicographically left-to-right (previous implementation,
#'       ordered right-to-left, but i don't think this change should impact
#'       downstream behavior).
#' @param xs numeric vector.
#' @param k integer (> 0): length of each permuted object.
#' @return (length(xs)^k x k) numeric matrix: all permutations of length `k`
#'         using objects from `xs`.
expand_grid <- function(xs, k) {
  args <- rep(list(xs), k)
  do.call(
    function(...) {
      as.matrix(expand.grid(...,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      ))
    },
    args
  )
}


#' @title Almost Fair Continuous Ranked Probability Score
#' @references https://arxiv.org/html/2412.15832v1
#' @details computation uses the identity of the sum of pairwise differences,
#'          $\sum_{i=1}^m\sum_{j=1}^m \left( x_i - x_j \right)
#'          = \sum_{i=1}^m (2i - m - 1)x_i$,
#'          to reduce time complexity from O($m^2$) to O($m \log m$).
#' @param y numeric scalar (truth).
#' @param x numeric vector of predictive samples.
#' @param eps optional "fairness" parameter in [0, 1]
#'            where 1 = fair and 0 = biased.
#' @return numeric scalar CRPS, or MAE if the number of samples is 1.
crpsf <- function(y, x, eps = 0) {
  m <- length(x)
  if (m < 2) {
    return(mean(abs(x - y)))
  } # avoid div by 0
  x_sorted <- sort(x)
  mae <- mean(abs(x - y))
  spread <- sum((2 * (1:m) - m - 1) * x_sorted) / (m * (m - 1))
  correction_term <- (1 - eps / m)
  mae - correction_term * spread
}


#' @title Rabin Fingerprint
#' @param str UTF-8 encodable string.
#' @return integer hash in [0, 100030001).
str2num <- function(str) {
  # note: we can't use integers for the base and modulus bc it'll produce
  # integer overflow: 31 * 100030001 ~ 3e+9 > ~ 2e+9 = max int.
  # so, we instantiate the base, modulus, and accumulator as doubles
  # (which can represent numbers up to ~1.8e+308).
  b <- 31
  m <- 100030001

  # slightly more robust than iconv (which could only encode ASCII)
  bytes <- utf8ToInt((enc2utf8(str)))
  hash <- 0
  for (x in bytes) hash <- (hash * b + x) %% m # Horner's method
  hash
}


#' @title Deterministic Scenario Seed
#' @description deterministically maps scenario settings into a reproducible
#'              integer seed via a rolling hash.
#' @param seed integer: base seed.
#' @param n_train integer (> 0): training size.
#' @param design_type character: sampling heuristic
#'                    ("LHS", "grid", or "random").
#' @param nsr numeric (>= 0): noise-to-signal ratio.
#' @param fname string: function name.
#' @param rep integer (>= 1): replication index.
#' @return integer in [0, 100030001).
get_case_seed <- function(seed, n_train, design_type, nsr, fname, rep) {
  fname_hash <- str2num(fname)

  # encode nsr
  if (nsr < 1) {
    nsr_low <- if (nsr == 0) 99999 else round(-log(nsr) * 1e4)
    nsr_high <- 0
  } else {
    nsr_low <- 0
    nsr_high <- round(nsr * 1e4)
  }

  # encode sampling heuristic
  design_code <- switch(design_type,
    LHS = 1,
    grid = 2,
    random = 3
  )

  # aggregate all encodings into one vector
  encodings <- c(
    seed,
    n_train %% 1e15,
    rep %% 1e15,
    fname_hash,
    nsr_low,
    nsr_high,
    design_code
  )

  # base, modulus, and hash are doubles. refer to `str2num`
  # internal documentation for more details.
  b <- 101
  m <- 100030001
  hash <- 0
  # to match original behavior, the exponent of b should range from -1:5.
  # i'm assuming the intended behavior of the original code was to have the
  # exponent range from 0:6, which avoids messy floating point operations.
  for (i in seq_along(encodings)) hash <- (hash + b^(i - 1) * encodings[i]) %% m
  hash
}


#' @title Generate Train/Test Samples
#' @param dsgn character: sampling heuristic ("LHS", "random" or "grid").
#' @param n_samples integer: sample size.
#' @param dim integer: the dimension of each sample.
#' @param test_mode logical: sample for test set?.
#' @return (n_samples x dim) numeric matrix.
sample_values <- function(dsgn, n_samples, dim, test_mode) {
  if (dsgn == "LHS") { # latin hypercube sampling
    # assuming we still only want to use maximin for training data
    X <- if ((n_samples <= 1200) && !test_mode) {
      lhs::maximinLHS(n_samples, dim)
    } else {
      lhs::randomLHS(n_samples, dim)
    }
  } else if (dsgn == "random") { # uniform sampling
    X <- matrix(stats::runif(n_samples * dim), ncol = dim)
  } else { # grid sampling
    ni <- ceiling(n_samples^(1 / dim))
    xx <- seq(0, 1, length.out = ni)
    X <- expand_grid(xx, dim)
  }
  X
}


#' @title Simulate Gaussian Noise and Obtain Total Variance
#' @param y_train (length n_train) numeric vector: noise-free training outputs.
#' @param nsr numeric: noise-to-signal ratio.
#' @param n_train numeric: training size.
#' @return list with:
#'   \itemize{
#'     \item \code{noise} (length n_train) numeric vector: Gaussian noise.
#'     \item \code{total_var} numeric scalar: variance of the response
#'           (signal + noise).
#'   }
get_noise_and_var <- function(y_train, nsr, n_train) {
  training_variance <- stats::var(y_train)
  if (training_variance == 0) {
    # cannot make sense of SNR when there is no signal
    noise_lvl <- 1e-6 * sqrt(nsr)
    total_var <- 1e-12 * (1 + nsr)
  } else {
    noise_lvl <- sqrt(training_variance * nsr)
    total_var <- training_variance * (1 + nsr)
  }
  list(noise = stats::rnorm(n_train, 0, noise_lvl), total_var = total_var)
}


#' @title Null Predictive Model
#' @description Deterministic Student-t posterior centered on mean(y_train).
#' @param y_train (length n_train) numeric vector: training outputs.
#' @param n_test integer: test size.
#' @param n_train integer: train size.
#' @param seed integer: seed for reproducibility.
#' @return (1000 x n_test) numeric matrix with Student-t samples.
null_model <- function(y_train, n_test, n_train, seed) {
  set.seed(seed)
  mu <- mean(y_train)
  sc <- sd(y_train) * sqrt(1 + 1 / n_train)
  matrix(mu + stats::rt(1000 * n_test, df = n_train - 1L) * sc,
    nrow = 1000, ncol = n_test
  )
}


#' @title Execute and Time an Expression
#' @param expr functional object: expression to evaluate.
#' @return list with:
#'   \itemize{
#'     \item \code{value} function output.
#'     \item \code{elapsed} numeric: elapsed time (seconds).
#'   }
time_try <- function(expr) {
  t0 <- proc.time()[3]
  val <- try(eval.parent(substitute(expr)), silent = TRUE)
  list(value = val, elapsed = proc.time()[3] - t0)
}


#' @title Enforce Stop vs Fallback on Error
#' @param fallback logical: enable null-model fallback?
#' @param x try-error: returned function error.
#' @param fn_name character: name of operation that failed.
ensure_fallback <- function(fallback, x, fn_name) {
  if (!fallback) {
    stop(paste0("Failure in ", fn_name, ":"), attr(x, "condition")$message)
  }
}


#' @title Fit and Predict with Timing and Optional Fallback
#' @param X_train (n_train x dimensions) numeric matrix: training set inputs.
#' @param y_train (length n_train) numeric vector: training set outputs.
#' @param X_test (n_test x dimensions) numeric matrix: testing set inputs.
#' @param n_train integer: training set size.
#' @param n_test integer: testing set size.
#' @param fit_func see \code{\link{run_sim_study}}.
#' @param pred_func see \code{\link{run_sim_study}}.
#' @param fallback enable null-model fallback (bool).
#' @param seed integer: case seed.
#' @return list with:
#'   \itemize{
#'     \item \code{preds} (M x n_test) numeric matrix: model predictions
#'           (rows = samples, cols = test points), or (length n_test) numeric
#'            vector if the method returns point predictions.
#'     \item \code{t_fit} numeric: fit time (seconds, or NA).
#'     \item \code{t_pred} numeric: predict time (seconds, or NA).
#'     \item \code{t_tot} numeric: total elapsed time (seconds).
#'     \item \code{failure_type} string: the type of failure that occurred
#'                               (one of "none", "fit", or "pred").
#'   }
fit_and_predict <- function(X_train, y_train, X_test, n_train, n_test,
                            fit_func, pred_func, fallback, seed) {
  run_with_fallback <- function(func, args, process_name) {
    result <- time_try(do.call(func, args))
    if (is_err(result$value)) {
      ensure_fallback(fallback, result$value, process_name)
      list(value = NA, elapsed = result$elapsed, failed = TRUE)
    } else {
      list(value = result$value, elapsed = result$elapsed, failed = FALSE)
    }
  }

  if (is.null(pred_func)) { # train and predict
    # train and predict
    tr <- run_with_fallback(
      fit_func,
      list(X_train, y_train, X_test),
      "fit_func"
    )

    preds <- if (tr$failed) {
      null_model(y_train, n_test, n_train, seed)
    } else {
      tr$value
    }

    list(
      preds = preds,
      t_fit  = NA_real_,
      t_pred = NA_real_,
      t_tot  = tr$elapsed,
      failure_type = if (tr$failed) "fit" else "none"
    )
  } else { # train, then predict
    # train
    tr_fit <- run_with_fallback(
      fit_func,
      list(X_train, y_train),
      "fit"
    )

    # predict
    tr_pred <- run_with_fallback(
      pred_func,
      list(tr_fit$value, X_test),
      "pred_func"
    )

    list(
      preds = tr_pred$value %||% null_model(y_train, n_test, n_train, seed),
      t_fit  = tr_fit$elapsed,
      t_pred = tr_pred$elapsed,
      t_tot  = tr_fit$elapsed + tr_pred$elapsed,
      failure_type = ifelse((tr_fit$failed), "fit",
        if (tr_pred$failed) "pred" else "none"
      )
    )
  }
}


`%||%` <- function(x, y) if (is.null(x)) y else x


is_err <- function(x) inherits(x, "try-error")


#' @title Interval bounds helper
#' @description
#' Return lower/upper bounds for a central (equal-tailed) interval. If
#' `intervals` is provided (2 x n or 2 x n x K), return the appropriate slice;
#' otherwise compute bounds from `samples` at `level` using empirical quantiles.
#'
#' @param samples (M x n_test) numeric matrix: predictive samples. Used only
#'   when `intervals` is NULL.
#' @param alpha optional numeric in (0, 1).
#' @param intervals optional (2 x n_test) numeric matrix or
#'   (2 x n_test x K) array: precomputed bounds.
#' @param k optional integer: only applicable if selecting which level to
#'   return when `intervals` is 3D.
#' @return (2 x n_test) numeric matrix: row 1 = lower, row 2 = upper.
interval_bounds <- function(samples = NULL, alpha = NULL,
                            intervals = NULL, k = NULL) {
  if (!is.null(intervals)) {
    if (is.matrix(intervals)) {
      return(intervals)
    }
    if (length(dim(intervals)) == 3L) {
      return(matrix(intervals[, , k], nrow = 2L))
    }
    stop("'intervals' must be 2 x n or 2 x n x K.")
  }
  apply(samples, 2L, stats::quantile, probs = c(alpha / 2, 1 - alpha / 2))
}

#' @title Compute RMSE, FVU, Coverage/MIS, and CRPS Summaries
#' @param samples (M x n_test) numeric matrix: predictive samples
#'                (rows = samples, cols = test points).
#' @param y_test (length n_test) numeric vector: true test responses.
#' @param total_var numeric: variance of the response.
#' @param conf_levels numeric iterable: confidence levels in (0, 1).
#' @param score logical: compute CRPS?
#' @param verbose logical: report progress?
#' @return Named list of scalar metrics:
#'   \itemize{
#'     \item \code{RMSE}: test set RMSE.
#'     \item \code{FVU}: fraction of variance unexplained.
#'     \item \code{COVER<level>}: empirical coverage of the central
#'                                \eqn{100 \times \text{level}\%} equal-tailed
#'                                interval.
#'     \item \code{MIS<level>}: mean interval score for the same central
#'                              intervals.
#'     \item (Only if \code{score = TRUE})
#'       \itemize{
#'         \item \code{CRPS}: mean CRPS over test points.
#'         \item \code{CRPS_min}: min CRPS across test points.
#'         \item \code{CRPS_Q1}: first quartile of CRPS.
#'         \item \code{CRPS_med}: median CRPS.
#'         \item \code{CRPS_Q3}: third quartile of CRPS.
#'         \item \code{CRPS_max}: max CRPS.
#'       }
#'   }
calculate_metrics <- function(samples, y_test, total_var,
                              conf_levels, score, verbose,
                              intervals = NULL) {
  # colMeans of `samples` (M x n_test) produces vector `y_pred` (length n_test)
  y_pred <- colMeans(samples)
  rmse <- sqrt(mean((y_test - y_pred)^2))

  metrics <- list(
    RMSE = rmse,
    FVU  = rmse^2 / total_var
  )

  # record coverage and interval scores
  for (k in seq_along(conf_levels)) {
    cl <- conf_levels[k]
    alpha <- 1 - cl
    bounds <- interval_bounds(samples, alpha, intervals, k)

    # coverage
    coverage <- mean(y_test >= bounds[1, ] & y_test <= bounds[2, ])
    metrics[[paste0("COVER", cl)]] <- coverage

    # negatively oriented mean interval score
    # paper: https://sites.stat.washington.edu/raftery/Research/PDF/Gneiting2007jasa.pdf
    width <- bounds[2, ] - bounds[1, ]
    lower_penalty <- 2 / alpha * pmax(0, bounds[1, ] - y_test)
    upper_penalty <- 2 / alpha * pmax(0, y_test - bounds[2, ])
    metrics[[paste0("MIS", cl)]] <- mean(width + lower_penalty + upper_penalty)
  }

  if (score) {
    if (verbose) cat("Computing CRPS...")

    crps_vals <- sapply(seq_along(y_test), function(i) {
      crpsf(y_test[i], samples[, i])
    })

    metrics$CRPS     <- mean(crps_vals)
    metrics$CRPS_min <- min(crps_vals)
    metrics$CRPS_Q1  <- quantile(crps_vals, 0.25)
    metrics$CRPS_med <- median(crps_vals)
    metrics$CRPS_Q3  <- quantile(crps_vals, 0.75)
    metrics$CRPS_max <- max(crps_vals)

    if (verbose) cat("\nDone.\n")
  }

  metrics
}


#' @title Generate Train/Test Data for One Scenario
#' @param fn character: test function in the duqling namespace.
#' @param n_train integer: training size.
#' @param n_test integer: testing size.
#' @param p integer: sample dimension.
#' @param dsgn character: sampling heuristic for train/test inputs.
#'   one of "LHS", "grid", or "random".
#' @param nsr numeric: the noise-to-signal ratio in (0,1) (inverse of the
#'   more-standard signal-to-noise ratio).
#' @return simulation data:
#'   \itemize{
#'     \item \code{X_train} (n_train x input_dim) numeric matrix:
#'                          training inputs.
#'     \item \code{y_train} (length n_train) numeric vector:
#'                          noisy training outputs.
#'     \item \code{X_test} (n_test x input_dim) numeric matrix: test inputs.
#'     \item \code{y_test} (length n_test) numeric vector: test outputs
#'                         (noise-free).
#'     \item \code{n_train} numeric: number of training samples.
#'     \item \code{n_test} numeric: number of testing samples.
#'     \item \code{total_var} numeric: total variance; see
#'                            \code{\link{get_noise_and_var()}}
#'   }
generate_simulation_data <- function(fn, n_train, n_test, p, dsgn, nsr) {
  f <- get(fn, loadNamespace("duqling"))

  # obtain train/test inputs using some sampling heuristic `dsgn`
  X_train <- sample_values(dsgn, n_train, p, test_mode = FALSE)
  X_test <- sample_values(dsgn, n_test, p, test_mode = TRUE)
  # a little janky (we have to reset in case grid
  # sampling requires us to change these values)
  n_train <- nrow(X_train)
  n_test <- nrow(X_test)

  # obtain train/test outputs (add Gaussian noise to training set)
  y_train_clean <- apply(X_train, 1, f, scale01 = TRUE)
  y_test <- apply(X_test, 1, f, scale01 = TRUE)
  # add noise to training
  noise_info <- get_noise_and_var(y_train_clean, nsr, n_train)
  y_train <- y_train_clean + noise_info$noise

  list(
    X_train = X_train,
    y_train = y_train,
    X_test = X_test,
    y_test = y_test,
    n_train = n_train,
    n_test = n_test,
    total_var = noise_info$total_var
  )
}


#' @title Evaluate One Method Under One Scenario
#' @param method_idx integer: index of the method to run.
#' @param method_names character vector: names for each method.
#' @param fit_funcs function or list of functions:
#'   see \code{\link{run_sim_study}}.
#' @param pred_funcs function, list, or \code{NULL}:
#'   see \code{\link{run_sim_study}}.
#' @param data list from \code{generate_simulation_data()}.
#' @param scenario_info named list: scenario metadata
#'   (fname, input_dim, n, nsr, design_type, rep).
#' @param conf_level numeric vector: confidence levels in (0, 1).
#' @param score logical: compute CRPS?
#' @param fallback logical: enable null-model fallback?
#' @param seed integer: case seed.
#' @param verbose logical: report progress?
#' @return vector with:
#'   \itemize{
#'     \item \code{scenario_info} named list (see input argument docstring).
#'     \item \code{t_fit} numeric: time to fit (seconds).
#'     \item \code{t_pred} numeric: time to predict (seconds).
#'     \item \code{t_tot} numeric: total elapsed time (seconds).
#'     \item \code{failure_type} string: the type of failure that occurred
#'                               (one of "none", "fit", or "pred").
#'     \item \code{metrics} output from \code{calculate_metrics} call.
#'   }
evaluate_method <- function(method_idx, method_names,
                            fit_funcs, pred_funcs,
                            data, scenario_info,
                            conf_level, score,
                            fallback, seed, verbose) {
  # if `fit_funcs` or `pred_funcs` are lists of methods, index the current
  #  method. otherwise, assume they are functional objects.
  fit_func <- if (is.list(fit_funcs)) fit_funcs[[method_idx]] else fit_funcs
  pred_func <- if (is.list(pred_funcs)) pred_funcs[[method_idx]] else pred_funcs

  result <- fit_and_predict(
    data$X_train, data$y_train,
    data$X_test,
    data$n_train, data$n_test,
    fit_func, pred_func,
    fallback, seed
  )
  preds <- result$preds

  intervals <- if (is.list(preds)) result$preds$intervals else NULL

  if (is.list(preds)) {
    if (!is.null(preds$samples)) {
      preds <- preds$samples
    } else if (!is.null(preds$preds)) {
      preds <- matrix(preds$preds, nrow = 1L)
    } else {
      stop("List output must include `samples` or `preds`.")
    }
  } else if (!is.numeric(preds)) {
    stop("fit/pred functions must return a vector, matrix, or a list.
          See help file for details. ")
  }

  # coerce model predictions into a (1 x n_test) matrix
  if (!is.matrix(preds)) preds <- matrix(preds, nrow = 1)

  # transpose `preds` if its shape is n_test x M instead of M x n_test
  n_test <- length(data$y_test)
  if (nrow(preds) == n_test && ncol(preds) != n_test) {
    preds <- t(preds)
  }

  metrics <- calculate_metrics(
    preds, data$y_test, data$total_var, conf_level,
    score, verbose, intervals
  )

  c(scenario_info,
    method = method_names[method_idx] %||% paste0("method", method_idx),
    t_fit = result$t_fit,
    t_pred = result$t_pred,
    t_tot = result$t_tot,
    failure_type = result$failure_type,
    metrics
  )
}


#' @title Run One Simulation Scenario
#' @param rr integer: replication index.
#' @param seed integer: base seed.
#' @param fn character: function name in \pkg{duqling}.
#' @param fnum unused: kept for backward compatibility.
#' @param p integer: input dimension.
#' @param n_train integer: training size.
#' @param nsr numeric: noise-to-signal ratio.
#' @param dsgn character: sampling heuristic ("LHS", "grid", or "random").
#' @param n_test integer: test size.
#' @param conf_level numeric vector: confidence levels in (0, 1).
#' @param score logical: compute CRPS?
#' @param method_names character vector: method labels.
#' @param fit_func function or list: see \code{\link{run_sim_study}}.
#' @param pred_func function, list, or \code{NULL}:
#'                  see \code{\link{run_sim_study}}.
#' @param fallback logical: enable null-model fallback?
#' @param verbose logical: report progress?
#' @return \code{data.frame} with one row per method under the scenario.
run_one_sim_case <- function(rr, seed, fn, fnum, p, n_train, nsr, dsgn, n_test,
                             conf_level, score, method_names, fit_func,
                             pred_func, fallback, verbose) {
  seed_t <- get_case_seed(seed, n_train, dsgn, nsr, fn, rr)
  set.seed(seed_t)

  data <- generate_simulation_data(fn, n_train, n_test, p, dsgn, nsr)

  scenario_info <- list(
    fname = fn, input_dim = p,
    n = n_train,
    nsr = nsr,
    design_type = dsgn,
    rep = rr
  )

  results <- lapply(
    seq_along(fit_func),
    evaluate_method,
    fit_funcs = fit_func,
    pred_funcs = pred_func,
    method_names = method_names,
    data = data,
    scenario_info = scenario_info,
    conf_level = conf_level,
    score = score,
    fallback = fallback,
    seed = seed_t,
    verbose = verbose
  )

  do.call(rbind.data.frame, results)
}
