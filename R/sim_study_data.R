#' Reproducible Simulation Study (Real Data)
#'
#' Reproducible code for emulator simulation studies using real datasets.
#'
#' @param fit_func If \code{pred_func} is specified, \code{fit_func} should take
#'   two arguments called \code{X_train} and \code{y_train}, and should return
#'   an object which will be passed to \code{pred_func}. If \code{pred_func} is
#'   \code{NULL}, then \code{fit_func} should take a third argument called
#'   \code{X_test}, and should return predictive samples (see Details).
#' @param pred_func A function taking two arguments: (i) the object returned by
#'   \code{fit_func} and (ii) a matrix \code{X_test}. The function should return
#'   predictive samples or a named list as described in the Details section.
#' @param dnames A vector of dataset names from the \code{duqling} package. See
#'   \code{data_quack()} for details.
#' @param dsets A list of custom datasets. Each list component should itself be
#'   a list with elements \code{X} (a matrix of inputs) and \code{y} (a response
#'   vector).
#' @param folds Number of folds in cross validation. Can be a scalar or a vector
#'   with \code{length(folds) == length(dnames) + length(dsets)}. If a value of
#'   \code{folds} is negative, then a bootstrap cross-validation procedure is
#'   run \code{-folds} times.
#' @param seed Seed for random number generators. For reproducibility, use of
#'   the default seed is recommended.
#' @param conf_level A vector of confidence levels used for empirical coverage,
#'   interval length, and mean interval score calculations.
#' @param score Logical. Should CRPS be computed?
#' @param x_scale01 Logical. Should the inputs be internally scaled to the unit
#'   interval? Default is \code{TRUE}.
#' @param method_names A vector of method names, length equal to
#'   \code{length(fit_func)}. If \code{NULL}, indexed names
#'   \code{method<i>} will be used.
#' @param custom_data_names An optional vector of dataset names corresponding to
#'   the argument \code{dsets}.
#' @param mc_cores How many cores to use for parallelization over folds.
#' @param fallback_on_error When \code{TRUE} (default), a null model
#'   (\code{N(mean(y_train), sd(y_train))}) is used for robustness if a failure
#'   is detected in either \code{fit_func} or \code{pred_func}.
#' @param print_error Logical (default \code{FALSE}). Should error messages
#'   from \code{fit_func} or \code{pred_func} be printed?
#' @param verbose Should progress be reported?
#'
#' @return A data frame with one row per dataset, fold, and method combination.
#'   Columns include:
#'   \itemize{
#'     \item \code{dname}: Dataset name.
#'     \item \code{method}: Method name.
#'     \item \code{n}: Number of training points used in the fold.
#'     \item \code{input_dim}: Input dimension.
#'     \item \code{fold}: Fold index.
#'     \item \code{fold_size}: Number of held-out points in the fold.
#'     \item \code{t_fit}: Elapsed time (seconds) for model fitting (if applicable).
#'     \item \code{t_pred}: Elapsed time (seconds) for model prediction (if applicable).
#'     \item \code{t_tot}: Total elapsed time for fitting and prediction.
#'     \item \code{failure_type}: Indicates if the method failed at \code{"fit"},
#'       \code{"pred"}, or \code{"none"}.
#'     \item \code{RMSE}: Root mean squared error on the test set.
#'     \item \code{MAE}: Mean absolute error on the test set.
#'     \item \code{Bias}: Mean signed prediction error on the test set
#'       (predicted minus true response).
#'     \item \code{FVU}: Fraction of variance unexplained (RMSE\eqn{^2} divided
#'       by the variance of the full dataset response).
#'     \item \code{COVER<level>}: Empirical coverage for each specified confidence level.
#'     \item \code{LENGTH<level>}: Mean predictive interval length for each specified confidence level.
#'     \item \code{MIS<level>}: Mean interval score for each specified confidence level.
#'     \item \code{IAE_alpha}: Average absolute deviation between empirical coverage
#'       and the nominal confidence levels.
#'     \item \code{CRPS}: Mean continuous ranked probability score (CRPS) on the test set.
#'     \item \code{CRPS_min}, \code{CRPS_Q1}, \code{CRPS_med}, \code{CRPS_Q3},
#'       \code{CRPS_max}: Summary statistics of the CRPS distribution over the test set.
#'   }
#'
#' @details Code to conduct a reproducible simulation study for emulator
#'   comparison using real datasets. By reporting the study settings, other
#'   authors can compare results directly.
#'
#'   Only \code{fit_func} is strictly required, but in that case only total
#'   runtime will be reported. The simplest (and recommended) interface is for
#'   \code{fit_func} (or \code{pred_func}) to return a numeric matrix of
#'   predictive samples, with one column per test point and one row per
#'   predictive draw. Any number of predictive samples is allowed. In this
#'   default case, point predictions are taken to be the column means, and
#'   prediction intervals are constructed using the R \code{quantile} function.
#'
#'   Alternatively, \code{fit_func} (or \code{pred_func}) may return a named
#'   list. The field \code{samples} is typically required and should again be an
#'   \code{M x n} matrix with one column per test point. If \code{samples} is
#'   omitted, then both \code{preds} and \code{sd} must be supplied, and
#'   \code{samples} will be approximated internally using Normal draws centered
#'   at \code{preds} with standard deviation \code{sd}. The optional field
#'   \code{preds} should be a numeric vector of point predictions of length
#'   equal to the number of test points. The optional field \code{intervals}
#'   should be a \code{2 x n x k} array of interval bounds, where \code{n} is
#'   the number of test points and \code{k = length(conf_level)}. When supplied,
#'   \code{preds} and \code{intervals} are used directly for point- and
#'   interval-based metrics; otherwise they are constructed from \code{samples}.
#'
#'   Positive values of \code{folds} correspond to ordinary cross-validation.
#'   Negative values of \code{folds} request bootstrap validation, in which a
#'   bootstrap sample is used for training and the out-of-bag observations are
#'   used for testing.
#'
#'   For reproducibility, random-number generation is controlled at the dataset
#'   and fold levels. This is especially helpful for stochastic fitting methods
#'   and when folds are processed in parallel.
#'
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation
#' experiments: test functions and datasets." Simon Fraser University,
#' Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' @export
#'
#' @examples
#' \donttest{
#' my_fit <- function(X, y) {
#'   lm(y ~ ., data = as.data.frame(X))
#' }
#'
#' my_pred <- function(mod, Xt) {
#'   mu <- predict(mod, as.data.frame(Xt))
#'   s  <- sd(residuals(mod))
#'   t(replicate(1000, mu + rnorm(length(mu), 0, s)))
#' }
#'
#' run_sim_study_data(
#'   fit_func = my_fit,
#'   pred_func = my_pred,
#'   dnames = "pbx9501_gold",
#'   folds = 5
#' )
#' }
run_sim_study_data <- function(fit_func, pred_func = NULL,
                                   dnames = get_sim_data_tiny(),
                                   dsets = NULL,
                                   folds = 10,
                                   seed = 42,
                                   conf_level = c(0.8, 0.9, 0.95, 0.99),
                                   x_scale01 = TRUE,
                                   method_names = NULL,
                                   custom_data_names = NULL,
                                   score = TRUE,
                                   mc_cores = 1,
                                   fallback_on_error = TRUE,
                                   print_error = FALSE,
                                   verbose = TRUE) {

  mc_cores <- validate_mc_cores(mc_cores)

  if (is.null(method_names)) {
    method_names <- names(fit_func)
  }

  ds_info <- resolve_dataset_inputs(dnames, dsets, custom_data_names)
  dnames_all <- ds_info$dnames_all
  is_custom <- ds_info$is_custom
  dsets <- ds_info$dsets

  if (length(folds) == 1) {
    folds <- rep(folds, length(dnames_all))
  }

  all_results <- list()
  res_idx <- 1L
  custom_cnt <- 1L

  for (dd in seq_along(dnames_all)) {
    loaded <- load_one_dataset(
      dd = dd,
      dnames_all = dnames_all,
      is_custom = is_custom,
      dsets = dsets,
      x_scale01 = x_scale01,
      custom_cnt = custom_cnt
    )

    X <- loaded$X
    y <- loaded$y
    dn <- loaded$dname
    cdn <- loaded$custom_data_name
    custom_cnt <- loaded$custom_cnt

    total_var <- stats::var(y)

    seed_t <- seed + str2num(dn)

    if (verbose) {
      cat("Starting dataset ", dd, "/", length(dnames_all), ": ", dn, "\n", sep = "")
    }

    cv_info <- make_cv_groups(n = nrow(X), folds = folds[dd], seed_t = seed_t)
    K <- cv_info$K

    if (mc_cores == 1) {
      results <- lapply(seq_len(K), function(k) {
        run_one_sim_case_data(
          k = k,
          seed = seed,
          XX = X,
          yy = y,
          groups = cv_info$groups,
          cv_type = cv_info$cv_type,
          dn = dn,
          score = score,
          conf_level = conf_level,
          method_names = method_names,
          custom_data_name = cdn,
          fit_func = fit_func,
          pred_func = pred_func,
          fallback = fallback_on_error,
          verbose = verbose,
          print_error = print_error,
          total_var = total_var
        )
      })
    } else {
      results <- parallel::mclapply(
        seq_len(K),
        function(k) {
          run_one_sim_case_data(
            k = k,
            seed = seed,
            XX = X,
            yy = y,
            groups = cv_info$groups,
            cv_type = cv_info$cv_type,
            dn = dn,
            score = score,
            conf_level = conf_level,
            method_names = method_names,
            custom_data_name = cdn,
            fit_func = fit_func,
            pred_func = pred_func,
            fallback = fallback_on_error,
            verbose = verbose,
            print_error = print_error,
            total_var = total_var
          )
        },
        mc.cores = mc_cores
      )
    }

    all_results[[res_idx]] <- do.call(rbind, results)
    res_idx <- res_idx + 1L
  }

  do.call(rbind, all_results)
}

#' @keywords internal
run_one_sim_case_data <- function(k, seed, XX, yy, groups, cv_type,
                                      dn, score, conf_level, method_names,
                                      custom_data_name, fit_func, pred_func,
                                      fallback, verbose, print_error,
                                      total_var) {

  if (cv_type == "cv") {
    indx <- which(groups == k)
    mk <- length(indx)

    X_test  <- XX[indx, , drop = FALSE]
    y_test  <- yy[indx]
    X_train <- XX[-indx, , drop = FALSE]
    y_train <- yy[-indx]

  } else {
    train_indx <- groups[[k]]
    test_indx  <- setdiff(seq_len(nrow(XX)), unique(train_indx))
    mk <- length(test_indx)

    X_train <- XX[train_indx, , drop = FALSE]
    y_train <- yy[train_indx]
    X_test  <- XX[test_indx, , drop = FALSE]
    y_test  <- yy[test_indx]
  }

  n_test <- nrow(X_test)
  n <- nrow(X_train)
  p <- ncol(X_test)

  # Set fold specific seed
  fold_seed <- get_case_seed(seed, n, "data", 0, dn, k)

  cdn <- if (is.na(custom_data_name)) dn else custom_data_name

  method_rows <- vector("list", length(fit_func))

  for (ii in seq_along(fit_func)) {
    set.seed(fold_seed)

    fit_pred <- fit_and_predict_one_method(
      ii = ii,
      method_names = method_names,
      fit_func = fit_func,
      pred_func = pred_func,
      X_train = X_train,
      y_train = y_train,
      X_test = X_test,
      seed_t = fold_seed,
      fallback = fallback,
      print_error = print_error,
      verbose = verbose
    )

    pred_obj <- try(
      normalize_prediction_object(
        preds = fit_pred$preds,
        n_test = n_test,
        conf_level = conf_level
      ),
      silent = !print_error
    )

    if (inherits(pred_obj, "try-error")) {
      if (fit_pred$failure_type == "none") fit_pred$failure_type <- "pred"

      if (fallback) {
        set.seed(seed_t)
        fit_pred$preds <- null_fallback_preds(y_train, X_test)
        pred_obj <- normalize_prediction_object(
          preds = fit_pred$preds,
          n_test = n_test,
          conf_level = conf_level
        )
      } else {
        stop(paste("Invalid prediction object:", attr(pred_obj, "condition")$message))
      }
    }

    metrics <- compute_metrics_from_prediction(
      pred_obj = pred_obj,
      y_test_latent = y_test,
      y_test_obs = y_test,
      total_var = total_var,
      conf_level = conf_level,
      score = score,
      verbose = verbose,
      use_latent_point = TRUE,
      use_latent_distr = TRUE
    )

    row <- data.frame(
      method = fit_pred$method,
      dname = cdn,
      input_dim = p,
      n = n,
      fold = k,
      fold_size = mk,
      stringsAsFactors = FALSE
    )

    if (is.na(fit_pred$t_fit)) {
      row$t_tot <- fit_pred$t_tot
    } else {
      row$t_fit <- fit_pred$t_fit
      row$t_pred <- fit_pred$t_pred
      row$t_tot <- fit_pred$t_tot
    }

    row$failure_type <- fit_pred$failure_type

    for (nm in names(metrics)) {
      row[[nm]] <- metrics[[nm]]
    }

    method_rows[[ii]] <- row
  }

  do.call(rbind, method_rows)
}
