## HELPER FUNCTIONS
validate_mc_cores <- function(mc_cores) {
  if (!is.null(mc_cores) && mc_cores > 1) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      stop(
        "mc_cores > 1 requires the 'parallel' package.\n",
        "Install it with install.packages('parallel').",
        call. = FALSE
      )
    }

    if (.Platform$OS.type == "windows") {
      warning(
        "mc_cores > 1 is not supported on Windows with mclapply(). ",
        "Falling back to mc_cores = 1.",
        call. = FALSE
      )
      mc_cores <- 1
    }
  }
  mc_cores
}

resolve_replications <- function(replications) {
  if (length(replications) == 1) {
    if (replications < 0) {
      return(-replications)
    } else {
      return(seq_len(replications))
    }
  }

  if (length(replications) == 0) {
    warning("replications has length 0. Running just 1 replication.")
    return(1)
  }

  replications
}

get_function_info <- function(fn, dots) {
  if (is_custom(fn)) {
    fn_sub <- substr(fn, 8, nchar(fn))
    spec <- dots[[fn_sub]]
    if (is.null(spec) || is.null(spec$func) || is.null(spec$input_dim)) {
      stop("Custom function specification must contain fields 'func' and 'input_dim'.")
    }
    return(list(
      func = spec$func,
      input_dim = spec$input_dim
    ))
  }

  list(
    func = get(fn, loadNamespace("duqling")),
    input_dim = quack(fn, verbose = FALSE)$input_dim
  )
}

generate_design_matrix <- function(n, p, dsgn, dots, is_test = FALSE) {
  if (dsgn == "LHS") {
    if (!is_test && n <= 1200) {
      return(lhs::maximinLHS(n, p))
    } else {
      return(lhs::randomLHS(n, p))
    }
  }

  if (dsgn == "random") {
    return(matrix(stats::runif(n * p), ncol = p))
  }

  if (dsgn == "custom") {
    if (is_test) {
      return(matrix(stats::runif(n * p), ncol = p))
    }
    return(dots$design_func(n, p))
  }

  if (dsgn == "grid") {
    ni <- ceiling(n^(1 / p))
    xx <- seq(0, 1, length.out = ni)
    return(expand_grid(xx, p))
  }

  stop("Unknown design_type: ", dsgn)
}

compute_noise_info <- function(y_true, nsr) {
  v_train <- stats::var(y_true)

  if (v_train == 0) {
    noise_lvl <- 1e-6 * sqrt(nsr)
    total_var <- 1e-12 * (1 + nsr)
  } else {
    noise_lvl <- sqrt(v_train * nsr)
    total_var <- v_train * (1 + nsr)
  }

  list(
    noise_lvl = noise_lvl,
    total_var = total_var
  )
}

generate_sim_data <- function(seed_t, fn, p, n, nsr, dsgn, n_test, dots, need_obs_test) {
  set.seed(seed_t)

  fn_info <- get_function_info(fn, dots)
  f <- fn_info$func

  X_train <- generate_design_matrix(n, p, dsgn, dots, is_test = FALSE)
  n_train_actual <- nrow(X_train)

  y_train_mean <- apply(X_train, 1, f, scale01 = TRUE)
  noise_info <- compute_noise_info(y_train_mean, nsr)
  y_train <- y_train_mean + stats::rnorm(n_train_actual, 0, noise_info$noise_lvl)

  X_test <- generate_design_matrix(
    n_test, p,
    if (dsgn == "grid") "grid" else if (dsgn == "custom") "random" else dsgn,
    dots, is_test = TRUE
  )
  n_test_actual <- nrow(X_test)
  y_test <- apply(X_test, 1, f, scale01 = TRUE)

  # NOTE: rng trick is for backwards compatability
  if (!need_obs_test || nsr == 0) {
    y_test_obs <- y_test
  } else {
    rng_state <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      get(".Random.seed", envir = .GlobalEnv)
    } else {
      NULL
    }

    set.seed((seed_t + 9173L) %% 100030001L)
    y_test_obs <- y_test + stats::rnorm(n_test_actual, 0, noise_info$noise_lvl)

    if (is.null(rng_state)) {
      rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", rng_state, envir = .GlobalEnv)
    }
  }

  list(
    X_train = X_train,
    y_train = y_train,
    X_test = X_test,
    y_test = y_test,
    y_test_obs = y_test_obs,
    n_train = n_train_actual,
    n_test = n_test_actual,
    total_var = noise_info$total_var,
    seed_t = seed_t
  )
}

get_method_component <- function(x, i) {
  if (is.function(x)) return(x)
  x[[i]]
}

null_fallback_preds <- function(y_train, X_test, n_draws = 1000) {
  preds_numeric <- mean(y_train) +
    stats::rt(n_draws * nrow(X_test), df = length(y_train) - 1) *
    stats::sd(y_train) * sqrt(1 + 1 / length(y_train))

  matrix(preds_numeric, nrow = n_draws, ncol = nrow(X_test))
}

validate_numeric_preds <- function(preds, n_test) {
  if (is.null(preds)) {
    stop("pred_func returned NULL.")
  }

  if (is.data.frame(preds)) {
    preds <- as.matrix(preds)
  }

  if (is.vector(preds) && !is.list(preds)) {
    preds <- matrix(preds, nrow = 1)
  }

  if (!is.matrix(preds)) {
    stop("pred_func did not return a numeric matrix-like object.")
  }

  if (!is.numeric(preds)) {
    stop("pred_func returned a matrix, but it is not numeric.")
  }

  if (ncol(preds) != n_test) {
    stop(sprintf(
      "pred_func returned %d columns, but expected %d test points.",
      ncol(preds), n_test
    ))
  }

  if (nrow(preds) < 1) {
    stop("pred_func returned a matrix with zero rows.")
  }

  if (any(!is.finite(preds))) {
    stop("pred_func returned predictions containing NA/NaN/Inf.")
  }

  preds
}

validate_numeric_preds <- function(preds, n_test) {
  if (is.null(preds)) {
    stop("pred_func returned NULL.")
  }

  if (is.data.frame(preds)) {
    preds <- as.matrix(preds)
  }

  if (is.vector(preds) && !is.list(preds)) {
    preds <- matrix(preds, nrow = 1)
  }

  if (!is.matrix(preds)) {
    stop("pred_func did not return a numeric matrix-like object.")
  }

  if (!is.numeric(preds)) {
    stop("pred_func returned a matrix, but it is not numeric.")
  }

  if (ncol(preds) != n_test) {
    stop(sprintf(
      "pred_func returned %d columns, but expected %d test points.",
      ncol(preds), n_test
    ))
  }

  if (nrow(preds) < 1) {
    stop("pred_func returned a matrix with zero rows.")
  }

  if (any(!is.finite(preds))) {
    stop("pred_func returned predictions containing NA/NaN/Inf.")
  }

  preds
}

fit_and_predict_one_method <- function(ii, method_names, fit_func, pred_func,
                                       X_train, y_train, X_test,
                                       seed_t, fallback, print_error, verbose) {

  my_method <- ifelse(is.null(method_names[ii]), paste0("method", ii), method_names[ii])
  failure_type <- "none"
  error_message <- ""

  fit_func_curr <- get_method_component(fit_func, ii)

  if (is.null(pred_func)) {
    pred_func_curr <- NULL
  } else {
    pred_func_curr <- get_method_component(pred_func, ii)
  }

  if (is.null(pred_func_curr)) {
    tictoc::tic()
    preds <- try(fit_func_curr(X_train, y_train, X_test), silent = !print_error)

    if (inherits(preds, "try-error")) {
      failure_type <- "fit"
      if (fallback) {
        set.seed(seed_t)
        error_message <- as.character(preds)
        preds <- null_fallback_preds(y_train, X_test)
      } else {
        stop(paste("Failure in fit_func:", attr(preds, "condition")$message))
      }
    }

    t_tot <- tictoc::toc(quiet = !verbose)

    return(list(
      method = my_method,
      preds = preds,
      failure_type = failure_type,
      error_message = error_message,
      t_fit = NA_real_,
      t_pred = NA_real_,
      t_tot = t_tot$toc - t_tot$tic
    ))
  }

  tictoc::tic()
  fitted_object <- try(fit_func_curr(X_train, y_train), silent = !print_error)
  if (inherits(fitted_object, "try-error")) {
    if (fallback) {
      error_message <- as.character(fitted_object)
      fitted_object <- NA
      failure_type <- "fit"
    } else {
      stop(paste("Failure in fit_func:", attr(fitted_object, "condition")$message))
    }
  }
  t_fit <- tictoc::toc(quiet = !verbose)

  tictoc::tic()
  preds <- try(pred_func_curr(fitted_object, X_test), silent = !print_error)
  if (inherits(preds, "try-error")) {
    if (failure_type == "none") failure_type <- "pred"
    if (fallback) {
      set.seed(seed_t)
      if (error_message == "") {
        error_message <- as.character(preds)
      } else {
        error_message <- paste("In fit:", error_message,
                               "\n\nIn pred:", as.character(preds), sep = " | ")
      }
      preds <- null_fallback_preds(y_train, X_test)
    } else {
      stop(paste("Failure in pred_func:", attr(preds, "condition")$message))
    }
  }
  t_pred <- tictoc::toc(quiet = !verbose)

  list(
    method = my_method,
    preds = preds,
    failure_type = failure_type,
    error_message = error_message,
    t_fit = t_fit$toc - t_fit$tic,
    t_pred = t_pred$toc - t_pred$tic,
    t_tot = (t_fit$toc - t_fit$tic) + (t_pred$toc - t_pred$tic)
  )
}


normalize_prediction_object <- function(preds, n_test, conf_level) {
  if (!is.numeric(preds) && !is.list(preds)) {
    stop("fit/pred functions must return a vector, matrix, or a list. See help file for details.")
  }

  if (is.numeric(preds)) {
    preds <- validate_numeric_preds(preds, n_test)
    point_preds <- colMeans(preds)

    n_conf <- length(conf_level)
    intervals <- NULL
    if (n_conf > 0) {
      intervals <- array(NA_real_, dim = c(2, n_test, n_conf))
      for (iii in seq_along(conf_level)) {
        alpha_curr <- 1 - conf_level[iii]
        intervals[, , iii] <- apply(
          preds, 2, stats::quantile,
          probs = c(alpha_curr / 2, 1 - alpha_curr / 2)
        )
      }
    }

    return(list(
      samples = preds,
      preds = point_preds,
      intervals = intervals
    ))
  }

  preds <- validate_list_preds(preds, n_test, conf_level)
  if (is.null(preds$samples)) {
    mu_tmp <- as.numeric(preds$preds)
    s_tmp <- as.numeric(preds$sd)

    if (length(s_tmp) == 1) s_tmp <- rep(s_tmp, n_test)

    preds$samples <- sapply(seq_len(n_test), function(i) {
      stats::rnorm(1000, mean = mu_tmp[i], sd = s_tmp[i])
    })
  }

  if (is.null(preds$preds)) {
    preds$preds <- colMeans(preds$samples)
  }

  if (is.null(preds$intervals)) {
    n_conf <- length(conf_level)
    intervals <- array(NA_real_, dim = c(2, n_test, n_conf))
    for (iii in seq_along(conf_level)) {
      alpha_curr <- 1 - conf_level[iii]
      intervals[, , iii] <- apply(
        preds$samples, 2, stats::quantile,
        probs = c(alpha_curr / 2, 1 - alpha_curr / 2)
      )
    }
    preds$intervals <- intervals
  }


  list(
    samples = preds$samples,
    preds = as.numeric(preds$preds),
    intervals = preds$intervals
  )
}

compute_interval_metrics <- function(y_test, intervals, conf_level) {
  n_conf <- length(conf_level)

  coverages <- rep(NA_real_, n_conf)
  lengths <- rep(NA_real_, n_conf)
  mis <- rep(NA_real_, n_conf)

  for (iii in seq_along(conf_level)) {
    alpha_curr <- 1 - conf_level[iii]
    bounds <- intervals[, , iii]

    coverages[iii] <- mean((y_test >= bounds[1, ]) & (y_test <= bounds[2, ]))

    term1 <- apply(bounds, 2, diff)
    term2 <- 2 * (bounds[1, ] - y_test) * as.numeric(y_test < bounds[1, ]) / alpha_curr
    term3 <- 2 * (y_test - bounds[2, ]) * as.numeric(y_test > bounds[2, ]) / alpha_curr
    mis[iii] <- mean(term1 + term2 + term3)

    lengths[iii] <- mean(term1)
  }

  list(
    coverages = coverages,
    lengths = lengths,
    mis = mis,
    iae_alpha = mean(abs(coverages - conf_level))
  )
}

compute_crps_summary <- function(y_test, samples, verbose = FALSE) {
  if (verbose) cat("Computing CRPS")
  CRPS_vec <- unlist(lapply(seq_along(y_test), function(i) crpsf(y_test[i], samples[, i])))
  csumm <- summary(CRPS_vec)
  if (verbose) cat("\nDone.\n")

  list(
    CRPS = as.numeric(csumm[4]),
    CRPS_min = as.numeric(csumm[1]),
    CRPS_Q1 = as.numeric(csumm[2]),
    CRPS_med = as.numeric(csumm[3]),
    CRPS_Q3 = as.numeric(csumm[5]),
    CRPS_max = as.numeric(csumm[6])
  )
}

compute_metrics_from_prediction <- function(pred_obj,
                                            y_test_latent,
                                            y_test_obs,
                                            total_var,
                                            conf_level,
                                            score,
                                            verbose,
                                            use_latent_point = TRUE,
                                            use_latent_distr = TRUE) {

  out <- list()

  y_point <- if (use_latent_point) y_test_latent else y_test_obs
  y_dist  <- if (use_latent_distr) y_test_latent else y_test_obs

  y_hat <- pred_obj$preds
  rmse_curr <- rmsef(y_point, y_hat)
  mae_curr  <- maef(y_point, y_hat)
  bias_curr <- mean(y_hat - y_point)

  out$RMSE <- rmse_curr
  out$MAE  <- mae_curr
  out$Bias <- bias_curr
  out$FVU  <- rmse_curr^2 / total_var

  if (length(conf_level) > 0) {
    int_metrics <- compute_interval_metrics(y_dist, pred_obj$intervals, conf_level)

    for (iii in seq_along(conf_level)) {
      out[[paste0("COVER", round(conf_level[iii], 7))]] <- int_metrics$coverages[iii]
    }
    for (iii in seq_along(conf_level)) {
      out[[paste0("LENGTH", round(conf_level[iii], 7))]] <- int_metrics$lengths[iii]
    }
    for (iii in seq_along(conf_level)) {
      out[[paste0("MIS", round(conf_level[iii], 7))]] <- int_metrics$mis[iii]
    }

    out$IAE_alpha <- int_metrics$iae_alpha
  }

  if (score) {
    crps_metrics <- compute_crps_summary(y_dist, pred_obj$samples, verbose = verbose)
    out <- c(out, crps_metrics)
  }

  out
}

is_custom <- function(fn) {
  if (length(fn) != 1 || !is.character(fn)) return(FALSE)
  grepl("^custom_", fn)
}

expand_grid <- function(xx, k){
  p <- length(xx)
  indx <- rep(0, k)
  X <- matrix(NA, nrow=length(xx)^k, ncol=k)
  cnt <- 1
  while(indx[1] < length(xx)){
    X[cnt,] <- xx[1+indx]
    cnt <- cnt + 1

    indx[k] <- indx[k] + 1
    for(j in k:2){
      if(indx[j] == p){
        indx[j] <- 0
        indx[j-1] <- indx[j-1] + 1
      }
    }
  }
  return(X)
}

rmsef <- function(x, y){
  sqrt(mean((x-y)^2))
}

maef <- function(x, y){
  mean(abs(x - y))
}

# Speedup with cool sorting identity:
# sum of pairwise absolute differences = 2 \sum_{i=1}^n(2i-n-1)x_{()}
crpsf <- function(y, x, w = 0) {
  M <- length(x)
  term1 <- mean(abs(x-y))
  x <- sort(x)
  coef <- 2*(1:M) - M - 1
  term2 <- 2 * sum(coef*x)
  res <- term1 - (1 - w/M) * term2 / (2*M*(M - 1))
  return(res)
}


str2num <- function(str){
  # Rabin's Fingerprinting using Horner's Rule for safe computation
  B = 31
  M = 100030001

  chars <- as.numeric(unlist(iconv(str, to="ASCII", toRaw=TRUE)))
  hash <- 0L
  for (i in seq_along(chars)) {
    hash <- (hash * B + chars[i]) %% M
  }
  return(hash)
}

get_case_seed <- function(seed, n_train, design_type, NSR, fname, rep){
  s0 <- seed
  s1 <- n_train %% 1e15 # Should never get this big anyways
  s2 <- rep     %% 1e15
  s3 <- str2num(fname) # Rabin fingerprint
  if(NSR < 1){
    if(NSR == 0){
      s4 <- 99999
    }else{
      s4 <- round(-log(NSR) * 1e4)
    }
    s5 <- 0
  }else{
    s4 <- 0
    s5 <- round(NSR * 1e4)
  }
  s6 <- switch(design_type, LHS = 1, grid = 2, random = 3, custom=4, data=0)

  B <- 101
  ss <- 0
  for(i in 0:6) ss <- ss + B^(i-1) * get(paste0("s", i))
  ss <- ss %% 100030001
  return(ss)
}


# HELPERS FOR DATA STUDY
k.chunks = function(n, K){
  groups <- rep(1:K, times=ceiling(n/K))[1:n]
  groups <- sample(groups, n, FALSE)
  return(groups)
}

#' @keywords internal
resolve_dataset_inputs <- function(dnames, dsets, custom_data_names) {
  n_custom_data <- length(dsets)
  dnames_custom <- character(0)

  if (n_custom_data > 0) {
    if (!is.null(dsets$X)) {
      if (n_custom_data == 2) {
        tmp <- list()
        tmp[[1]] <- dsets
        dsets <- tmp
        n_custom_data <- 1
        warning("Trying to coerce. dsets should be a list of lists. Check documentation.")
      } else {
        stop("dsets should be a list of lists. Check documentation.")
      }
    }

    if (is.null(custom_data_names)) {
      if (is.null(names(dsets))) {
        dnames_custom <- paste0("custom", seq_len(n_custom_data))
      } else {
        dnames_custom <- names(dsets)
      }
    } else {
      if (length(custom_data_names) != n_custom_data) {
        stop("custom_data_names does not match dsets length")
      }
      dnames_custom <- custom_data_names
    }
  }

  list(
    dnames_all = c(dnames, dnames_custom),
    is_custom = c(rep(FALSE, length(dnames)), rep(TRUE, length(dnames_custom))),
    dsets = dsets
  )
}

#' @keywords internal
make_cv_groups <- function(n, folds, seed_t) {
  set.seed(seed_t)

  K <- folds
  if (K == 0) stop("Cannot have 0 folds")

  if (K > 0) {
    return(list(
      cv_type = "cv",
      K = K,
      groups = k.chunks(n, K)
    ))
  }

  # Bootstrap branch
  K <- -K
  groups <- vector("list", K)

  for (kk in seq_len(K)) {
    repeat {
      samp <- sample.int(n, n, replace = TRUE)
      oob <- setdiff(seq_len(n), unique(samp))
      if (length(oob) > 0) {
        groups[[kk]] <- samp
        break
      }
    }
  }

  list(
    cv_type = "boot",
    K = K,
    groups = groups
  )
}

#' @keywords internal
scale_range <- function(x, r = NULL){
  if (is.null(r))
    r <- range(x)
  if ((r[2] - r[1]) == 0)
    return(x - r[1])
  return((x - r[1])/(r[2] - r[1]))
}

#' @keywords internal
load_one_dataset <- function(dd, dnames_all, is_custom, dsets, x_scale01, custom_cnt) {
  dn <- dnames_all[dd]

  if (is_custom[dd]) {
    X <- dsets[[custom_cnt]]$X
    y <- dsets[[custom_cnt]]$y
    cdn <- dn
    custom_cnt <- custom_cnt + 1
  } else {
    curr <- get_emulation_data(dn)
    X <- as.matrix(curr$X)
    y <- curr$y
    cdn <- NA
  }

  if (x_scale01) {
    X <- apply(X, 2, scale_range)
  }

  list(
    X = X,
    y = y,
    dname = dn,
    custom_data_name = cdn,
    custom_cnt = custom_cnt
  )
}

lookup_sigma <- function(fname){
  sigma_lookup <- list(
    banana=3.710725e+02,
    borehole_low_fidelity=3.635870e+01,
    borehole=4.555280e+01,
    cantilever_D=3.286210e-01,
    cantilever_S=7.842194e+03,
    circuit=1.141898e+00,
    # arranged so that CRPS is roughly 0.56 for baseline after scaling, when NSR != 0
    const_fn=0.000000e+00   + 1e-7 * 0.7366 / 0.56, # Don't return 0 here
    const_fn15=0.000000e+00 + 1e-7 * 0.7366 / 0.56, # Don't return 0 here
    const_fn3=0.000000e+00  + 1e-7 * 0.7366 / 0.56, # Don't return 0 here
    crater=1.415364e+00,
    cube3_rotate=7.037742e-01,
    cube3=5.572555e-01,
    cube5=2.442026e+00,
    detpep_curve=3.610622e+01,
    detpep8=3.649000e+01,
    dms_additive=9.873234e-01,
    dms_complicated=1.002189e+00,
    dms_harmonic=9.218845e-01,
    dms_radial=9.845250e-01,
    dms_simple=9.619880e-01,
    ebola=4.904622e-01,
    forrester1_low_fidelity=6.083980e+00,
    forrester1=9.580872e+00,
    foursquare=3.829575e-01,
    friedman=4.826403e+00,
    friedman10=4.832760e+00,
    friedman20=4.831938e+00,
    gamma_mix=7.687958e-02,
    Gfunction=9.343063e-01,
    Gfunction12=9.162019e-01,
    Gfunction18=1.968464e+00,
    Gfunction6=8.893865e-01,
    grlee1=1.305325e+00,
    grlee2=7.703807e-02,
    grlee6=7.158600e-01,
    ignition=2.479687e+00,
    ishigami=3.716563e+00,
    lim_non_polynomial=1.951328e+00,
    lim_polynomial=1.634255e+00,
    multivalley=3.521684e-01,
    oo15=1.523091e+01,
    park4_low_fidelity=5.047334e+00,
    park4=4.749824e+00,
    permdb=4.478022e-02,
    piston=1.381331e-01,
    pollutant_uni=8.304634e-01,
    rabbits=2.857249e-01,
    ripples=7.754145e-02,
    robot=5.236244e-01,
    sharkfin=2.985767e-01,
    short_column=4.063448e+00,
    simple_poly=4.775166e-01,
    squiggle=5.377997e-01,
    star2=1.144571e+00,
    steel_column=1.064768e+02,
    sulfur=3.055872e+01,
    twin_galaxies=1.024249e+00,
    welch20=2.093332e+00,
    wingweight=4.807968e+01,
    onehundred=5.733751e+00,
    # STARTING DATA SETS HERE
    plate_deformation=1.449489e-02,
    wind_speed=1.027572e+00,
    strontium_plume_p4b=2.808191e-01,
    strontium_plume_p104=1.469877e-01,
    spectra1=1.000000e+00,
    spectra2=1.000000e+00,
    pbx9501_gold=6.320689e-03,
    pbx9501_ss304=6.826252e-03,
    pbx9501_nickel=6.838146e-03,
    pbx9501_uranium=6.980380e-03,
    ptw1=6.597825e-03,
    discoflux_flyer=3.579125e-02,
    taylor_foot=1.259407e-01,
    taylor_length=1.926734e-01,
    jc=4.089584e-03,
    flyerPTW1=1.589439e-02,
    flyerPTW2=2.508928e-03,
    flyer_plate104=6.348922e+01,
    diffusion_1D=2.766821e-02,
    "fair_climate_ssp1-2.6_year2200"=4.126652e-01,
    "fair_climate_ssp2-4.5_year2200"=7.037122e-01,
    "fair_climate_ssp3-7.0_year2200"=1.202978e+00,
    taylor_cylinder1=2.168441e-02,
    stochastic_sir=2.009268e+03,
    diffusion_2D=1.968937e-01,
    acme_climate=2.609752e+02,
    ptw2=7.001573e-03,
    SLOSH_low=7.484742e-01,
    SLOSH_mid=9.763949e-01,
    SLOSH_high=1.344907e+00,
    Z_machine_max_vel1=1.082886e+02,
    Z_machine_max_vel2=7.478024e+01,
    Z_machine_max_vel_all=5.396926e+02,
    e3sm_mnar=1.513867e+01,
    nuclear_data=1.068442e-02,
    e3sm_mcar=1.492739e+01,
    icf1=9.876422e-02,
    icf2=1.964187e-01,
    diablo_canyon_plume=4.958526e-01,
    okc_plume=6.473348e-01,
    #New functions
    higdon1=0.5546522,
    quad4=0.8130207,
    turb12=2.700776,
    linear21=7.967096,
    linear21_s10=5.351829,
    linear21_s1=2.886897,
    moon31=0.1863337,
    Gfunction40=1.829371
  )

  val <- sigma_lookup[[fname]]

  if (is.null(val)) {
    return(1)
  }

  return(val)
}

#' Human-friendly labels for derived metrics
#'
#' Converts internal column names like "CRPS_rank" or "CRPS_rel_log"
#' into nicer labels for plotting.
#'
#' @param metric Character; column name.
#'
#' @return A character label.
#' @export
metric_label <- function(metric) {
  if (metric == "time") {
    return("Time")
  }
  if (metric == "time_predict") {
    return("Prediction time")
  }
  if (metric == "time_fit") {
    return("Training time")
  }

  if (grepl("_rank$", metric)) {
    base <- sub("_rank$", "", metric)
    return(paste(metric_label(base), "Rank"))
  }
  if (grepl("_auc$", metric)) {
    base <- sub("_auc$", "", metric)
    return(paste("AUC of", metric_label(base)))
  }
  if (grepl("_rel_log$", metric)) {
    base <- sub("_rel_log$", "", metric)
    return(paste("log Relative", metric_label(base)))
  }
  if (grepl("_rel$", metric)) {
    base <- sub("_rel$", "", metric)
    return(paste("Relative", metric_label(base)))
  }
  if (grepl("_norm$", metric)) {
    base <- sub("_norm$", "", metric)
    return(paste(metric_label(base), "(z-score)"))
  }

  return(metric)
}
