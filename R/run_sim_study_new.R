#' Reproducible Simulation Study (Test Functions)
#'
#' Reproducible code for simulation studies.
#'
#' @param fit_func If \code{pred_func} is specified, the \code{fit_func} should take two arguments called \code{X_train} and \code{y_train}, and should return an object which will be passed to \code{pred_func}. If \code{pred_func} is NOT specified, then \code{fit_func} should take a third argument called \code{X_test}, and should return predictive samples (see pred_func documentation).
#' @param pred_func A function taking two arguments: (i) the object returned by \code{fit_func} and a matrix \code{X_test}. The function should return an matrix of samples from the predictive distribution, with one column per test point. For additional flexibility, see the details below.
#' @param fnames A vector of function names from the \code{duqling} package. See \code{quack()} for details.
#' @param conf_level A vector of confidence levels. For computing coverages and interval scores.
#' @param n_train the sample size (or vector of sample sizes) for each training set
#' @param n_test the sample size for each testing set
#' @param NSR the noise to signal ratio (inverse of the more-standard signal to noise ratio).
#' @param design_type How should the training and testing designs be generated? Options are \code{"LHS"}, \code{"grid"}, \code{"random"}, or \code{"custom"}. See details for information on custom designs.
#' @param replications The replications to run. Can be a single integer \code{r} (runs replications 1 to r), a vector of integers (runs only those specific replications), or a single negative integer \code{-k} (runs only replication k).
#' @param seed Seed for random number generators. For reproducibility, we discourage the use of this argument.
#' @param method_names A vector of method names, length equal to \code{length(fit_func)}. If NULL, the indexed names \code{my_method<i>} will be used.
#' @param score Logical. Should CRPS be computed?
#' @param use_latent_point Logical. Should point-prediction metrics
#'   (\code{RMSE}, \code{MAE}, \code{Bias}, and \code{FVU}) be evaluated against
#'   the latent test response? Default is \code{TRUE}.
#' @param use_latent_distr Logical. Should distributional and interval-based
#'   metrics (\code{CRPS}, \code{COVER<level>}, \code{LENGTH<level>},
#'   \code{MIS<level>}, and \code{IAE_alpha}) be evaluated against the latent
#'   test response? Default is \code{FALSE}.
#' @param mc_cores How many cores to use for parallelization over replications.
#' @param fallback_on_error When \code{TRUE} (default), we use a null model (\code{N(mean(y_train), sd(y_train))}) for robustness if a failure is detected in either \code{fit_func} or \code{pred_func}.
#' @param print_error Logical (default FALSE). Should error messages (from \code{fit_func} or \code{pred_func})be printed?
#' @param verbose should progress be reported?
#' @param ... Additional arguments for advance (custom) usage. See vignette for details.
#' @return A data frame where each row corresponds to the results of a single simulation scenario, replication index, and method. The columns include:
#'   \itemize{
#'     \item \code{method}: Name of the fitted method, if provided. Unique names (\code{method<i>}) are assigned otherwise.
#'     \item \code{fname}: The test function name from \code{duqling::quack()$fname}. See details for custom functions.
#'     \item \code{input_dim}: Input dimension of the test function.
#'     \item \code{n_train}: Number of training points.
#'     \item \code{NSR}: Noise-to-signal ratio used in data generation.
#'     \item \code{design_type}: Type of experimental design used for data generation ("LHS", "grid", "random", or "custom").
#'     \item \code{replication}: Replication index.
#'     \item \code{t_fit}: Elapsed time (seconds) for model fitting (if applicable).
#'     \item \code{t_pred}: Elapsed time (seconds) for model prediction (if applicable).
#'     \item \code{t_tot}: Total elapsed time for fitting and prediction.
#'     \item \code{failure_type}: Indicates if the method failed at "fit", "pred", or "none".
#'     \item \code{RMSE}: Root mean squared error on the test set.
#'     \item \code{MAE}: Mean absolute error on the test set.
#'     \item \code{Bias}: Mean signed prediction error on the test set (predicted minus true response).
#'     \item \code{FVU}: Fraction of variance unexplained (RMSE\(^2\) divided by total variance).
#'     \item \code{COVER<level>}: Empirical coverage rates for each specified confidence level.
#'     \item \code{LENGTH<level>}: Mean predictive interval length for each confidence level.
#'     \item \code{MIS<level>}: Mean interval score for each confidence level.
#'     \item \code{IAE_alpha}: Average absolute deviation between empirical coverage and nominal levels.
#'     \item \code{CRPS}: Mean continuous ranked probability score (CRPS) on the test set.
#'     \item \code{CRPS_min}, \code{CRPS_Q1}, \code{CRPS_med}, \code{CRPS_Q3}, \code{CRPS_max}: Summary statistics of the CRPS distribution over the test set.
#'   }
#' Additional columns may be included for other metrics or settings depending on options provided.
#'
#' @details Code to conduct a reproducible simulation study for emulator comparison.
#' By reporting the study settings, other authors can compare results directly.
#'
#' Only \code{fit_func} is strictly required, but in that case only total runtime
#' will be reported. The simplest (and recommended) interface is for
#' \code{fit_func} (or \code{pred_func}) to return a numeric matrix of posterior
#' samples, with one column per test point (e.g., per row of \code{X_test}) and
#' one row per predictive draw. Any number of predictive samples is allowed. In
#' this default case, point predictions are taken to be the column means, and
#' prediction intervals are constructed using the R \code{quantile} function.
#'
#' Alternatively, \code{fit_func} (or \code{pred_func}) may return a named list.
#' The field \code{samples} is normally required and should again be a matrix with
#' one column per test point. If \code{samples} is omitted, then both
#' \code{preds} and \code{sd} must be supplied, and \code{samples} will be
#' approximated internally using Normal draws centered at \code{preds} with
#' standard deviation \code{sd}. The optional field \code{preds} should be a
#' numeric vector of point predictions of length equal to the number of test
#' points. The optional field \code{intervals} should be a \code{2 x n x k} array
#' of interval bounds, where \code{n} is the number of test points and
#' \code{k = length(conf_level)}. When supplied, \code{preds} and
#' \code{intervals} are used directly for point- and interval-based metrics;
#' otherwise they are constructed from \code{samples}.
#'
#'
#' Point-prediction metrics (\code{RMSE}, \code{MAE}, \code{Bias}, and
#' \code{FVU}) may be evaluated either against the latent test response or
#' against a noise-contaminated test response, depending on
#' \code{use_latent_point}. Likewise, interval-based and distributional metrics
#' may be evaluated on either scale depending on \code{use_latent_distr}. When
#' \code{NSR = 0}, these choices are equivalent.
#'
#'
#' @section Custom functions and designs:
#'
#' Advanced users can extend \code{run_sim_study()} with their own test
#' functions or design types without modifying the package code.
#'
#' \strong{Custom functions}
#'
#' To supply a custom test function, create a list with fields:
#'
#' \itemize{
#'   \item \code{func}: a function with signature \code{f(x, scale01)} that
#'   returns a scalar. Here \code{x} is a numeric vector of length \code{p},
#'   and \code{scale01} is a logical flag indicating whether inputs are expected
#'   to be in \eqn{[0,1]^p}.
#'
#'   \item \code{input_dim}: an integer specifying the input dimension \code{p}.
#' }
#'
#' Pass this list as an extra argument to \code{run_sim_study()} with an
#' arbitrary name. For example:
#'
#' \preformatted{
#' f <- function(x, scale01=TRUE) x[1] - x[2]^2
#' my_list <- list(func = f, input_dim = 5)
#' run_sim_study(fit_func, pred_func,
#'               fnames = "custom_foobar",
#'               foobar = my_list)
#' }
#'
#' Inside \code{fnames}, use the string \code{"custom_<name>"} to refer to
#' your custom function (here, \code{"custom_foobar"}).
#'
#' \strong{Custom designs}
#'
#' The \code{design_type} argument may also be set to \code{"custom"}. In this
#' case, you must supply an additional argument \code{design_func} (through
#' \code{...}), which should be a function of \code{n} and \code{p} returning
#' an \eqn{n \times p} design matrix.
#'
#' For example:
#'
#' \preformatted{
#' correlated_design <- function(n, p) {
#'   d <- lhs::randomLHS(n, p)
#'   d[,1] <- (d[,2] + d[,3]) / 2
#'   d
#' }
#'
#' run_sim_study(fit_func, pred_func,
#'               fnames = "ishigami",
#'               design_type = "custom",
#'               design_func = correlated_design)
#' }
#'
#' @references Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#' @export
#' @examples
#' \donttest{
#'
#' # METHOD 1: Linear Regression
#' lm_fit <- function(X, y){
#'   Xdf <- as.data.frame(X)
#'   colnames(Xdf) <- paste0("X", seq_len(ncol(X)))
#'   lm(y ~ ., data = Xdf)
#' }
#' lm_pred <- function(obj, Xt){
#'   Xt_df <- as.data.frame(Xt)
#'   colnames(Xt_df) <- paste0("X", seq_len(ncol(Xt_df)))
#'   mean_pred <- predict(obj, Xt_df)
#'   sigma_est <- sd(residuals(obj))
#'   nt <- nrow(Xt_df)
#'   preds <- replicate(1000, mean_pred + rnorm(nt, 0, sigma_est))
#' }
#'
#' # METHOD 2: Projection Pursuit
#' ppr_fit <- function(X, y){
#'   ppr(X, y, nterms=3)
#' }
#' ppr_pred <- function(obj, Xt){
#'   nt <- nrow(Xt)
#'   mean_pred <- predict(obj, data.frame(Xt))
#'   sigma_est <- sd(residuals(obj))
#'   preds <- replicate(1000, mean_pred + rnorm(nt, 0, sigma_est))
#' }
#'
#' my_fit  <- list(lm_fit, ppr_fit)
#' my_pred <- list(lm_pred, ppr_pred)
#'
#' run_sim_study(my_fit, my_pred,
#'               fnames=c("dms_additive", "borehole"),
#'               n_train=50,
#'               replications=2,
#'               method_names=c("lm", "ppr"))
#' }
run_sim_study <- function(fit_func, pred_func = NULL,
                              fnames = NULL,
                              conf_level = c(0.8, 0.9, 0.95, 0.99),
                              n_train = 100,
                              n_test = 1000,
                              NSR = 0,
                              design_type = "LHS",
                              replications = 1,
                              seed = 42,
                              method_names = NULL,
                              score = TRUE,
                              use_latent_point = TRUE,
                              use_latent_distr = FALSE,
                              mc_cores = 1,
                              fallback_on_error = TRUE,
                              print_error = FALSE,
                              verbose = TRUE,
                              ...) {

  dots <- list(...)

  mc_cores <- validate_mc_cores(mc_cores)

  rep_vec <- resolve_replications(replications)

  if (seed != 42 && verbose) {
    warning(
      "Changing the seed will affect reproducibility. ",
      "The default seed (42) is recommended for consistent duqling results."
    )
  }

  if (is.null(fnames)) {
    fnames <- quack(input_dim = 1, verbose = FALSE)$fname
  }

  if (is.null(method_names)) {
    method_names <- names(fit_func)
  }

  all_results <- list()
  res_idx <- 1L

  for (ff in seq_along(fnames)) {
    fn <- fnames[ff]
    fn_info <- get_function_info(fn, dots)
    p <- fn_info$input_dim

    if (verbose) {
      cat("Starting function ", ff, "/", length(fnames), ": ", fn, "\n", sep = "")
    }

    for (ii in seq_along(n_train)) {
      n <- n_train[ii]
      if (verbose) {
        cat("\t Running all combinations and replications for n =", n, "\n")
      }

      for (jj in seq_along(NSR)) {
        for (kk in seq_along(design_type)) {

          one_case_args <- list(
            rr_vec = rep_vec,
            seed = seed,
            fn = fn,
            p = p,
            n = n,
            nsr = NSR[jj],
            dsgn = design_type[kk],
            n_test = n_test,
            conf_level = conf_level,
            score = score,
            method_names = method_names,
            fit_func = fit_func,
            pred_func = pred_func,
            fallback = fallback_on_error,
            print_error = print_error,
            verbose = verbose,
            dots = dots,
            use_latent_point = use_latent_point,
            use_latent_distr = use_latent_distr
          )

          if (mc_cores == 1) {
            results <- lapply(rep_vec, function(rr) {
              run_one_sim_case(
                rr = rr,
                seed = seed,
                fn = fn,
                p = p,
                n = n,
                nsr = NSR[jj],
                dsgn = design_type[kk],
                n_test = n_test,
                conf_level = conf_level,
                score = score,
                method_names = method_names,
                fit_func = fit_func,
                pred_func = pred_func,
                fallback = fallback_on_error,
                print_error = print_error,
                verbose = verbose,
                dots = dots,
                use_latent_point = use_latent_point,
                use_latent_distr = use_latent_distr
              )
            })
          } else {
            results <- parallel::mclapply(
              rep_vec,
              function(rr) {
                run_one_sim_case(
                  rr = rr,
                  seed = seed,
                  fn = fn,
                  p = p,
                  n = n,
                  nsr = NSR[jj],
                  dsgn = design_type[kk],
                  n_test = n_test,
                  conf_level = conf_level,
                  score = score,
                  method_names = method_names,
                  fit_func = fit_func,
                  pred_func = pred_func,
                  fallback = fallback_on_error,
                  print_error = print_error,
                  verbose = verbose,
                  dots = dots,
                  use_latent_point = use_latent_point,
                  use_latent_distr = use_latent_distr
                )
              },
              mc.cores = mc_cores
            )
          }

          all_results[[res_idx]] <- do.call(rbind, results)
          res_idx <- res_idx + 1L
        }
      }
    }
  }

  do.call(rbind, all_results)
}

# ONE CASE
run_one_sim_case <- function(rr, seed, fn, p, n, nsr, dsgn, n_test,
                                 conf_level, score, method_names,
                                 fit_func, pred_func, fallback,
                                 print_error, verbose, dots,
                                 use_latent_point, use_latent_distr) {

  seed_t <- get_case_seed(seed, n, dsgn, nsr, fn, rr)

  need_obs_test <- (!use_latent_point) || (!use_latent_distr)
  sim_data <- generate_sim_data(
    seed_t = seed_t,
    fn = fn,
    p = p,
    n = n,
    nsr = nsr,
    dsgn = dsgn,
    n_test = n_test,
    dots = dots,
    need_obs_test = need_obs_test
  )

  n_methods <- length(fit_func)
  method_rows <- vector("list", n_methods)

  # Save rng state
  if(n_methods > 1){
    rng_state_methods <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      get(".Random.seed", envir = .GlobalEnv)
    } else {
      NULL
    }
  }

  for (ii in seq_along(fit_func)) {
    #restore rng state
    if(n_methods > 1){
      if (is.null(rng_state_methods)) {
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      } else {
        assign(".Random.seed", rng_state_methods, envir = .GlobalEnv)
      }
    }

    # Fit and predict
    fit_pred <- fit_and_predict_one_method(
      ii = ii,
      method_names = method_names,
      fit_func = fit_func,
      pred_func = pred_func,
      X_train = sim_data$X_train,
      y_train = sim_data$y_train,
      X_test = sim_data$X_test,
      seed_t = seed_t,
      fallback = fallback,
      print_error = print_error,
      verbose = verbose
    )

    pred_obj <- normalize_prediction_object(
      preds = fit_pred$preds,
      n_test = sim_data$n_test,
      conf_level = conf_level
    )

    metrics <- compute_metrics_from_prediction(
      pred_obj = pred_obj,
      y_test_latent = sim_data$y_test,
      y_test_obs = sim_data$y_test_obs,
      total_var = sim_data$total_var,
      conf_level = conf_level,
      score = score,
      verbose = verbose,
      use_latent_point = use_latent_point,
      use_latent_distr = use_latent_distr
    )

    row <- data.frame(
      method = fit_pred$method,
      fname = fn,
      input_dim = p,
      n_train = sim_data$n_train,
      NSR = nsr,
      design_type = dsgn,
      replication = rr,
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
