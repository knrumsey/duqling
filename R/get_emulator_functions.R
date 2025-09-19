#' Access fit/predict functions for emulators
#'
#' Provides a consistent interface for retrieving the \code{fit_*} and \code{pred_*}
#' functions for one or more emulator methods supported in \pkg{duqling}.
#' This allows downstream code (e.g., \code{run_sim_study()}) to use emulators
#' without attaching all their required packages as dependencies.
#'
#' @param methods Character vector of method names. Must be drawn from the
#'   set of emulator methods implemented in the \pkg{duqling} paper comparison, e.g.
#'   \code{"bass"}, \code{"bart"}, \code{"bppr"}, \code{"blm"},
#'   \code{"bcart"}, \code{"btreelm"}, \code{"spce"}, \code{"apce"},
#'   \code{"confrf"}, \code{"fitcgp"}, \code{"rffgp"}, \code{"lagp"},
#'   \code{"alcgp"}, \code{"mpgp"}, \code{"svecgp"}, \code{"bnn"},
#'   \code{"tbass"}, \code{"qbass"}, \code{"rvm"}, \code{"ngboost"},
#'   \code{"blasso"}, \code{"gp"}, \code{"deepgp"}, \code{"treegp"},
#'   \code{"hetgp"}, \code{"bootrf"}, \code{"bcmgp"}, \code{"rgasp"},
#'   \code{"baseline"}.
#'
#' @return
#' If \code{methods} has length 1, a list with two components:
#' \describe{
#'   \item{\code{fit_func}}{The fitting function (e.g. `fit_bass`).}
#'   \item{\code{pred_func}}{The prediction function (e.g. `pred_bass`).}
#' }
#' If `methods` has length greater than 1, a list with two components:
#' \describe{
#'   \item{\code{fit_func}}{A named list of fit functions, keyed by method name.}
#'   \item{\code{pred_func}}{A named list of prediction functions, keyed by method name.}
#' }
#' This uniform structure allows both single and multiple method cases
#' to be passed directly to [run_sim_study()], e.g.
#' \preformatted{
#' emus <- get_emulator_functions("bass")
#' run_sim_study(emus$fit_func, emus$pred_func)
#'
#' emus <- get_emulator_functions(c("bass", "bart"))
#' run_sim_study(emus$fit_func, emus$pred_func)
#' }
#'
#' @details
#' Each emulator method is implemented via a pair of functions
#' \code{fit_<method>} and \code{pred_<method>}. These are defined in \pkg{duqling}
#' but may require additional packages to be installed. This accessor
#' checks for package availability using [requireNamespace()], and will
#' raise an error if required packages are missing.
#'
#' The list of required packages is documented in the source code. For
#' example:
#' \itemize{
#'   \item `"bass"` requires \pkg{BASS}
#'   \item `"bart"` requires \pkg{BART}
#'   \item `"bcart"`, `"btreelm"`, `"treegp"` require \pkg{tgp}
#'   \item `"rgasp"` requires \pkg{RobustGaSP}
#'   \item `"gp"` requires \pkg{hetGP}
#'   \item `"deepgp"` requires \pkg{deepgp}
#'   \item etc.
#' }
#'
#' Note: emulators based on \pkg{tgp} (e.g. \code{"bcart"}, \code{"btreelm"},
#' \code{"treegp"}) are \strong{not compatible} with the \code{mc_cores} argument
#' for parallelization, due to limitations in that package. Using them in
#' parallel contexts may cause errors.
#'
#' @examples
#' \dontrun{
#' # Single emulator
#' emu <- get_emulator_functions("bass")
#' str(emu)
#'
#' # Multiple emulators
#' emus <- get_emulator_functions(c("bass", "bart"))
#' str(emus$fit_func)
#' str(emus$pred_func)
#' }
#'
#' @export
get_emulator_functions <- function(methods) {
  stopifnot(!is.null(methods))

  # package requirements lookup
  required_pkgs <- list(
    bass    = c("BASS"),
    bart    = c("BART"),
    bppr    = c("BayesPPR"),
    blm     = character(0),
    bcart   = c("tgp"),
    btreelm = c("tgp"),
    spce    = c("khaos","glmnet"),
    apce    = c("khaos"),
    confrf  = c("conforest"),
    fitcgp  = c("gplite"),
    rffgp   = c("gplite"),
    lagp    = c("laGP"),
    alcgp   = c("laGP"),
    mpgp    = c("spareGParts","GpGp","GPvecchia","GPfit","cluster"),
    svecgp  = c("spareGParts","GpGp","GPvecchia","GPfit"),
    bnn     = c("bnns"),
    tbass   = c("GBASS"),
    qbass   = c("GBASS"),
    rvm     = c("spareGParts","glmnet"),
    ngboost = c("ngboost","reticulate"),
    blasso  = c("BayesianLasso"),
    gp      = c("hetGP"),
    deepgp  = c("deepgp"),
    treegp  = c("tgp"),
    hetgp   = c("hetGP"),
    bootrf  = c("randomForest"),
    bcmgp   = c("spareGParts"),
    rgasp   = c("RobustGaSP"),
    baseline= character(0)
  )

  if(length(intersect(c("bcart", "btreelm", "treegp"), methods)) > 0){
    warning("Note: tgp package is not compatible with duqlings internal parallelization. Make sure that you set mc_cores=1 for these emulators.")
  }

  if ("ngboost" %in% methods) {
    if (!reticulate::py_module_available("ngboost")) {
      stop("Method ngboost requires a working Python environment with ngboost installed.
         See https://github.com/Akai01/ngboost for installation instructions.")
    }
  }

  # check that requested methods are known
  unknown <- setdiff(methods, names(required_pkgs))
  if (length(unknown) > 0) {
    stop("Unknown method(s): ", paste(unknown, collapse = ", "))
  }

  # check package availability
  check_pkgs <- function(pkgs) {
    pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE,
                 FUN.VALUE = logical(1))]
  }

  # single method case
  if (length(methods) == 1) {
    m <- methods
    missing <- check_pkgs(required_pkgs[[m]])
    if (length(missing) > 0) {
      stop_missing_package(m, missing)
    }
    return(list(
      fit_func  = get(paste0("fit_", m), envir = asNamespace("duqling")),
      pred_func = get(paste0("pred_", m), envir = asNamespace("duqling"))
    ))
  }

  # multiple methods case
  fit_funcs <- list()
  pred_funcs <- list()
  for (m in methods) {
    missing <- check_pkgs(required_pkgs[[m]])
    if (length(missing) > 0) {
      stop_missing_package(m, missing)
    }
    fit_funcs[[m]]  <- get(paste0("fit_", m), envir = asNamespace("duqling"))
    pred_funcs[[m]] <- get(paste0("pred_", m), envir = asNamespace("duqling"))
  }

  list(fit_func = fit_funcs, pred_func = pred_funcs)
}


stop_missing_package <- function(m, missing){
  msg <- paste0("Method ", m, " requires packages: ",
                paste(missing, collapse = ", "))
  if("spareGParts" %in% missing){
    msg <- paste0(msg, "\n\n\tgithub.com/knrumsey/spareGParts")
  }
  if("ngboost" %in% missing){
    msg <- paste0(msg, "\n\n\tgithub.com/Akai01/ngboost")
  }
  if("conforest" %in% missing){
    msg <- paste0(msg, "\n\n\tgithub.com/knrumsey/conforest")
  }
  if("khaos" %in% missing){
    msg <- paste0(msg, "\n\n\tgithub.com/knrumsey/khaos")
  }
  if("BayesPPR" %in% missing){
    msg <- paste0(msg, "\n\n\tgithub.com/gqcollins/BayesPPR")
  }
  msg <- paste0(msg, "\n")
  stop(msg)
}


# ===================================================================
#.    EMULATOR FIT AND PRED FUNCS (PRIVATE)
# ===================================================================
# 1. BASS
fit_bass <- function(X, y) BASS::bass(X, y, verbose=FALSE)
pred_bass <- function(obj, Xt) predict(obj, Xt)

# 2. BART
fit_bart <- function(X, y){
  if(sd(y) ==0) y <- y + rnorm(y, 0, 1e-7)
  BART::wbart(X, y)
}
pred_bart <- function(obj, Xt) BART::predict(obj, Xt)

# 3. BayesPPR
fit_bppr <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(y, 0, 1e-7)
  BayesPPR::bppr(X, y)
}
pred_bppr <- function(obj, Xt){
  predict(obj, Xt)
}

# 4. Bayesian linear model
fit_blm <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(y, 0, 1e-7)
  ytrain <- y
  Xtrain <- cbind(1, X)
  n <- nrow(Xtrain)
  p <- ncol(Xtrain)
  XtX_inv <- solve(t(Xtrain) %*% Xtrain)
  beta_hat <- XtX_inv %*% t(Xtrain) %*% ytrain
  residuals <- ytrain - Xtrain %*% beta_hat
  sigma2_hat <- as.numeric((t(residuals) %*% residuals) / (n - p))
  list(
    beta_hat = beta_hat,
    XtX_inv = XtX_inv,
    sigma2_hat = sigma2_hat,
    n = n,
    p = p
  )
}
pred_blm <- function(obj, Xt){
  fit <- obj
  Xtest <- Xt
  Xtest <- cbind(1, Xtest)
  beta_hat <- fit$beta_hat
  XtX_inv <- fit$XtX_inv
  sigma2_hat <- fit$sigma2_hat
  num_draws <- 1000
  predictions <- matrix(NA, nrow = num_draws, ncol = nrow(Xtest))
  for (i in 1:nrow(Xtest)) {
    mu_pred <- as.numeric(Xtest[i, ] %*% beta_hat)
    pred_var <- sigma2_hat * (1 + as.numeric(Xtest[i, ] %*% XtX_inv %*% Xtest[i, ]))
    predictions[, i] <- rnorm(num_draws, mean = mu_pred, sd = sqrt(pred_var))
  }
  predictions
}

# 5. Bayesian CART
fit_bcart <- function(X, y){
  if(var(y) == 0) y <- y + rnorm(y, 0, 1e-7)
  out <- list(X=X, y=y)
  return(out)
}
pred_bcart <- function(obj, Xt){
  mod <- tgp::bcart(obj$X, obj$y, Xt, zcov=TRUE)
  yhat <- as.numeric(mod$ZZ.mean)           # mean vector (length n)
  Sigma <- mod$ZZ.s2                        # covariance matrix (n x n)
  n <- length(yhat)

  # Cholesky decomposition: Sigma = L %*% t(L)
  L <- chol(Sigma)

  # Generate nsamp samples of standard normals
  nsamp <- 1000
  z <- matrix(rnorm(nsamp * n), nrow = n)

  # Generate samples: Y = mu + L %*% z
  samples <- matrix(NA, nrow = nsamp, ncol = n)
  for(i in 1:nsamp){
    samples[i, ] <- yhat + L %*% z[,i]
  }

  return(samples)
}

# 6. Bayesian Treed Linear Model
fit_btreelm <- function(X, y){
  if(var(y) == 0) y <- y + rnorm(y, 0, 1e-7)
  out <- list(X=X, y=y)
  return(out)
}
pred_btreelm <- function(obj, Xt){
  mod <- tgp::btlm(obj$X, obj$y, Xt, zcov=TRUE)
  yhat <- as.numeric(mod$ZZ.mean)           # mean vector (length n)
  Sigma <- mod$ZZ.s2                        # covariance matrix (n x n)
  n <- length(yhat)

  # Cholesky decomposition: Sigma = L %*% t(L)
  L <- chol(Sigma)

  # Generate nsamp samples of standard normals
  nsamp <- 1000
  z <- matrix(rnorm(nsamp * n), nrow = n)

  # Generate samples: Y = mu + L %*% z
  samples <- matrix(NA, nrow = nsamp, ncol = n)
  for(i in 1:nsamp){
    samples[i, ] <- yhat + L %*% z[,i]
  }

  return(samples)
}

# 7. Sparse Khaos
fit_spce <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(y,0,1e-7)
  khaos::sparse_khaos(X,y, verbose=FALSE)
}
pred_spce <- function(obj, Xt) predict(obj, Xt)

# 8. Adaptive Khaos
fit_apce <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(y,0,1e-7)
  khaos::adaptive_khaos(X, y, verbose=FALSE)
}
pred_apce <- function(obj, Xt) predict(obj, Xt)

# 9. Conformal Random Forest
library(conforest)
fit_confrf <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(y,0,1e-9)
  conforest::rfok(X, y)
}
pred_confrf <- function(obj, Xt) predict(obj, Xt)

# 10. Sparse GP (Fully independent training conditionals; FITC)
fit_fitcgp <- function(X, y){
  if(sd(y) == 0){
    y <- y + rnorm(y, 0, 1e-7)
  }else{
    y <- y + rnorm(y, 0, 1e-7*sd(y)) # Do this for robustness
  }
  n <- length(y)
  ni <- 4*floor(sqrt(n))
  ni <- max(5, ni)
  ni <- min(100, ni, round(n*0.75))
  gp <- gplite::gp_init(method=method_fitc(num_inducing = ni))
  gp <- gplite::gp_optim(gp, X, y, restarts=2)
  gp$y <- y
  return(gp)
}
pred_fitcgp <- function(obj, Xt){
  jitter <- 1e-7
  while(TRUE){
    t_pred <- try({
      gplite::gp_draw(obj, Xt, draws=1000, jitter=jitter)
    }, silent = TRUE)
    if(!inherits(t_pred, "try-error")){
      return(t(t_pred))
    }else{
      print(t_pred)
      jitter <- jitter * 10
      if(jitter >= 1*var(obj$y)) stop("gp draw failed even with jitter > var(y)")
    }
  }
}

# 11. Basis GP (Random Fourier Features; Rahimi and Recht 2007)
fit_rffgp <- function(X, y){
  if(sd(y) == 0){
    y <- y + rnorm(y, 0, 1e-7)
  }else{
    y <- y + rnorm(y, 0, 1e-7*sd(y)) # Do this for robustness
  }
  n <- length(y)
  nb <- 2*floor(sqrt(n))
  nb <- min(512, nb)
  while(TRUE){
    gp <- try({
      gp1 <- gplite::gp_init(method=method_rf(num_basis=nb))
      gp1 <- gplite::gp_optim(gp1, X, y, restarts=2)
      gp1$y <- y
      gp1
    }, silent=TRUE)

    if(!inherits(gp, "try-error")){
      return(gp)
    }else{
      print(gp)
      if(nb <= 2) stop("numb basis 1 failed")
      nb <- floor(nb/2) + (floor(nb/2) %% 2)
    }
  }
}
pred_rffgp <- function(obj, Xt){
  jitter <- 1e-7
  while(TRUE){
    t_pred <- try({
      gplite::gp_draw(obj, Xt, draws=1000, jitter=jitter)
    }, silent = TRUE)
    if(!inherits(t_pred, "try-error")){
      return(t(t_pred))
    }else{
      print(t_pred)
      jitter <- jitter * 10
      if(jitter >= 100*var(obj$y)) stop("gp draw failed even with jitter > var(y)")
    }
  }
}

# 12. Local Approximate GP
fit_lagp <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(y, 0, 1e-7)
  if(max(table(y)) > 10) y <- y + rnorm(y, 0, 1e-7)
  list(X=X, y=y)
}
pred_lagp <- function(obj, Xt){
  n <- nrow(Xt)
  nn <- min(100, max(30, floor(sqrt(n))))

  preds <- matrix(NA, nrow=1000, ncol=nrow(Xt))
  for(i in 1:nrow(Xt)){
    mod <- laGP::laGP(Xt[i,,drop=FALSE], start=10, end=nn, X=obj$X, Z=obj$y)
    m <- mod$mean
    v <- mod$df
    s2 <- mod$s2 * (v-2) / v
    preds[,i] <- m + rt(1000, v) * sqrt(s2)
  }
  return(preds)
}

# 13. Local Approximate GP (SoD)
fit_alcgp <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(y, 0, 1e-7)
  list(X=X, y=y)
}
pred_alcgp <- function(obj, Xt){
  n <- nrow(Xt)
  nn <- max(100, min(300, n, 2 * round(sqrt(n))))
  mod <- laGP::laGP(Xref=Xt, obj$X, obj$y, start=10, end=nn)
  m <- mod$mean
  v <- mod$df
  s2 <- mod$s2 * (v-2) / v
  preds <- matrix(NA, nrow=1000, ncol=nrow(Xt))
  for(i in 1:nrow(Xt)){
    preds[,i] <- m[i] + rt(1000, v) * sqrt(s2[i])
  }
  return(preds)
}

# 14. Matching Pursuit SoD GP (Keerthi & Chu)
fit_mpgp <- function(X, y){
  if(sd(y) == 0){
    y <- y + rnorm(y, 0, 1e-7)
  }else{
    y <- y + rnorm(y, 0, sd(y)*1e-7)
  }

  n <- length(y)
  m <- min(1000, n-1, max(100, 2*floor(sqrt(n))))

  res <-  spareGParts::mpgp(X, y, m=m)
  return(res)
}
pred_mpgp <- function(obj, Xt){
  predict(obj, Xt)
}

# 15. Scaled Vecchia GP
fit_svecgp <- function(X, y){
  y_og <- y
  if (sd(y) == 0) y <- y + rnorm(y,0,1e-7)

  if(ncol(X) == 1){
    X <- cbind(X, rnorm(length(y), 1, 0.001)) # add a dummy column
  }

  nug <- NULL
  tries <- 0
  fit <- NULL

  while(TRUE){
    tries <- tries + 1
    fit <- try({
      spareGParts::svecgp(X, y, nug=nug)
    }, silent=TRUE)
    if(!inherits(fit, "try-error")){
      return(fit)
    }else{
      cat("svecgp failed with nug = ", nug, "on try", tries, "\n")
      cat("error says:", fit, "\n")
      if(is.null(nug)){
        nug <- var(y) / 100
      }else{
        nug <- nug * 10
        y <- y_og + rnorm(y, 0, sqrt(nug))
      }
    }
    if(tries >= 30) stop("too many nugget failures")
  }
}
pred_svecgp <- function(obj, Xt){
  if(ncol(Xt) == 1) Xt <- cbind(Xt, rnorm(nrow(Xt),1,0.001))
  predict(obj, Xt)
}

# 16. Bayesian NN
fit_bnn <- function(X,  y){
  data <- as.data.frame(X)
  data$y <- y
  obj <- bnns::bnns(y~., data=data, L=2, nodes=c(8,8),
              iter=1200, warmup=200, thin=2, chains=2, cores=1)
  return(obj)
}
pred_bnn <- function(obj, Xt){
  preds <- t(predict(obj, as.data.frame(Xt)))
  return(preds)
}

# 17. T-BASS
fit_tbass <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(y, 0, 1e-7)
  GBASS::tbass(X, y, df=3)
}
pred_tbass <- function(obj, Xt) predict(obj, Xt)

# 18. Median BASS
fit_qbass <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(y, 0, 1e-7)
  GBASS::qbass(X, y, q=0.5)
}
pred_qbass <- function(obj, Xt) predict(obj, Xt)

# 19. Relevance Vector Machine
fit_rvm <- function(X, y) spareGParts::rvm(X, y)
pred_rvm <- function(obj, Xt) predict(obj, Xt)

# 20. NGBoost
fit_ngboost <- function(X, y){
  # --- Checks ---
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required to use ngboost emulators.")
  }
  if (!reticulate::py_module_available("ngboost")) {
    stop("The Python module 'ngboost' was not found.\n",
         "Please install it in a Python environment accessible to reticulate.\n",
         "See https://github.com/Akai01/ngboost for installation instructions.")
  }
  if (!reticulate::py_module_available("sklearn")) {
    stop("The Python module 'scikit-learn' was not found.\n",
         "It is required as the base learner for ngboost.")
  }

  # --- Handle degenerate response ---
  if (sd(y) == 0) y <- y + rnorm(y, 0, 1e-7)

  # --- Construct and fit model ---
  model <- ngboost::NGBRegression$new(Dist = Dist("Normal"),
                             Base = sklearner(),
                             Score = Scores("MLE"),
                             natural_gradient = TRUE,
                             n_estimators = 600,
                             learning_rate = 0.002,
                             minibatch_frac = 0.8,
                             col_sample = 0.9,
                             verbose = TRUE,
                             verbose_eval = 100,
                             tol = 1e-5)

  model$fit(X = X, Y = y)
  return(model)
}
pred_ngboost <- function(obj, Xt){
  if (!inherits(obj, "python.builtin.object")) {
    stop("Object provided to pred_ngboost is not a Python ngboost model.")
  }

  samples <- 1000
  preds <- matrix(NA, nrow = samples, ncol = nrow(Xt))

  for (i in seq_len(nrow(Xt))) {
    distt <- obj$pred_dist(Xt[i, , drop = FALSE])
    mu <- distt$mean()
    sigma <- distt$std()
    preds[, i] <- rnorm(samples, mu, sigma)
  }

  return(preds)
}

# 21. Bayesian LASSO
fit_blasso <- function(X, y) {
  # BayesianLasso package requires at least 3 predictors
  if (ncol(X) < 3) {
    pad_cols <- 3 - ncol(X)
    for (i in seq_len(pad_cols)) {
      X <- cbind(rnorm(nrow(X), 0, 1), X)
    }
  }

  # Stabilize constant response
  mu_y <- mean(y)
  sd_y <- sd(y)
  if (sd_y == 0) {
    y <- y + rnorm(y, 0, 1e-7)
    mu_y <- mean(y)
    sd_y <- sd(y)
  }
  y_std <- (y - mu_y) / sd_y

  # Prior and initialization
  a1 <- 1; b1 <- 1; u1 <- 0.1; v1 <- 0.1
  nsamples <- 3000
  beta_init <- rep(0.1, ncol(X))
  lambda_init <- 1
  sigma2_init <- 1

  max_attempts <- 5
  attempt <- 1
  while (attempt <= max_attempts) {
    fit <- try(
      BayesianLasso::Modified_Hans_Gibbs(
        X, y_std, a1, b1, u1, v1,
        nsamples, beta_init, lambda_init, sigma2_init,
        verbose = FALSE
      ),
      silent = TRUE
    )

    fail <- inherits(fit, "try-error") ||
      is.null(fit$mBeta) ||
      ncol(as.matrix(fit$mBeta)) == 0 ||
      all(colSums(abs(as.matrix(fit$mBeta))) == 0)

    if (!fail) {
      fit$mu_y <- mu_y
      fit$sd_y <- sd_y
      fit$a1 <- a1
      fit$b1 <- b1
      fit$attempts <- attempt
      class(fit) <- "blasso"
      return(fit)
    }

    # Adjust priors for the next attempt
    a1 <- a1 / 2
    b1 <- b1 * 2
    lambda_init <- lambda_init / 4
    attempt <- attempt + 1
  }

  stop("Bayesian Lasso failed after ", max_attempts, " attempts.")
}


pred_blasso <- function(obj, Xt) {
  # Pad design matrix to at least 3 columns
  if (ncol(Xt) < 3) {
    pad_cols <- 3 - ncol(Xt)
    for (i in seq_len(pad_cols)) {
      Xt <- cbind(rnorm(nrow(Xt), 0, 1), Xt)
    }
  }

  nt <- nrow(Xt)
  ind <- seq(1001, 3000, by = 2)
  preds <- matrix(NA_real_, nrow = length(ind), ncol = nt)

  for (i in seq_along(ind)) {
    beta <- obj$mBeta[i, ]
    sigma <- sqrt(obj$vsigma2[i])
    preds[i, ] <- rnorm(nt, Xt %*% beta, sigma)
  }

  preds <- obj$mu_y + obj$sd_y * preds
  return(preds)
}

# 22. Full GP
fit_gp <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(y, 0, 1e-7)
  mu_y <- mean(y)
  sigma_y <- sd(y)
  y <- (y-mu_y)/sigma_y

  fit <- hetGP::mleHomGP(X, y, noiseControl=list(g_max=1, g_min=1e-9))
  fit$mu_y <- mu_y
  fit$sigma_y <- sigma_y
  return(fit)
}
pred_gp <- function(obj, Xt){
  tmp <- predict(obj, Xt)
  mu <- tmp$mean
  sigma <- sqrt(abs(tmp$sd2 + tmp$nugs))
  preds <- matrix(NA, nrow=1000, ncol=nrow(Xt))
  for(i in 1:nrow(Xt)){
    preds[,i] <- rnorm(1000, mu[i], sigma[i])
  }
  preds <- obj$mu_y + obj$sigma_y * preds
}

# 23. Deep GP
fit_deepgp <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(y, 0, 1e-7)
  mu_y <- mean(y)
  sigma_y <- sd(y)
  y <- (y-mu_y)/sigma_y
  cat("fitting")
  fit <- deepgp::fit_two_layer(X, y, nmcmc=1000, vecchia=TRUE)
  fit <- trim(fit, 500, 5)
  fit$mu_y <- mu_y
  fit$sigma_y <- sigma_y
  return(fit)
}
pred_deepgp <- function(obj, Xt){
  cat("predicting")
  tmp <- predict(obj, Xt)
  mu <- tmp$mean
  sigma <- sqrt(abs(tmp$s2))

  mu[is.na(mu)] <- obj$mu_y
  sigma[is.na(sigma)] <- obj$sigma_y

  preds <- matrix(NA, nrow=1000, ncol=nrow(Xt))
  for(i in 1:nrow(Xt)){
    preds[,i] <- rnorm(1000, mu[i], sigma[i])
  }
  preds <- obj$mu_y + obj$sigma_y * preds
}

# 24. Tree GP
fit_treegp <- function(X, y){
  if(var(y) == 0) y <- y + rnorm(y, 0, 1e-7)
  out <- list(X = X, y = y)
  return(out)
}
pred_treegp <- function(obj, Xt){
  mod <- tgp::btgp(obj$X, obj$y, Xt, zcov = TRUE)
  yhat <- as.numeric(mod$ZZ.mean)   # mean vector (length n)
  Sigma <- mod$ZZ.s2                # covariance matrix (n x n)
  n <- length(yhat)
  nsamp <- 1000

  # Cholesky decomposition
  nug_vec <- c(0, 1e-7, 1e-4, 1e-1)
  for(i in seq_along(nug_vec)){
    nugI <- diag(rep(nug_vec[i], n))
    L <- try(chol(Sigma + nugI), silent=TRUE)
    if(!inherits(L, "try-error")){
      break
    }
  }

  # Sample with cholesky
  z <- matrix(rnorm(nsamp * n), nrow = n)
  samples <- t(yhat + crossprod(L, z))
  return(samples)
}

# 25. Het GP
fit_hetgp <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(y, 0, 1e-7)
  mu_y <- mean(y)
  sigma_y <- sd(y)
  y <- (y-mu_y)/sigma_y

  fit <- hetGP::mleHetGP(X, y, noiseControl=list(g_max=1, g_min=1e-9))
  fit$mu_y <- mu_y
  fit$sigma_y <- sigma_y
  return(fit)
}
pred_hetgp <- function(obj, Xt){
  tmp <- predict(obj, Xt)
  mu <- tmp$mean
  sigma <- sqrt(abs(tmp$sd2 + tmp$nugs))
  preds <- matrix(NA, nrow=1000, ncol=nrow(Xt))
  for(i in 1:nrow(Xt)){
    preds[,i] <- rnorm(1000, mu[i], sigma[i])
  }
  preds <- obj$mu_y + obj$sigma_y * preds
}

# 26. Random Forest + Bagging (bootstrap)
fit_bootrf <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(y, 0, 1e-7)
  n_bags = 100
  bag_models <- vector("list", n_bags)
  n <- nrow(X)
  for (b in 1:n_bags) {
    idx <- sample(n, n, replace = TRUE)
    bag_models[[b]] <- randomForest::randomForest(
      x = X[idx, , drop = FALSE],
      y = y[idx])
  }
  bag_models
}

pred_bootrf <- function(obj, Xt){
  n_bags <- length(obj)
  n_test <- nrow(Xt)
  preds_mat <- matrix(NA, nrow = n_bags, ncol = n_test)
  for (b in 1:n_bags) {
    preds_mat[b, ] <- predict(obj[[b]], newdata = Xt)
  }
  preds_mat
}

# 27. Bayesian Committee Machine
fit_bcmgp <- function(X, y){
  if(sd(y) == 0){
    y <- y + rnorm(y, 0, 1e-7)
  }else{
    y <- y + rnorm(y, 0, sd(y)*1e-7)
  }
  spareGParts::bcmgp(X, y)
}
pred_bcmgp <- function(obj, Xt){
  predict(obj, Xt)
}

# 28. Robust GaSP
fit_rgasp <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(y, 0, 1e-7)
  print("trying gasp")
  RobustGaSP::rgasp(X, y, nugget.est=TRUE)
}
pred_rgasp <- function(obj, Xt){
  print("predicitng gasp")
  out <- predict.rgasp(obj, Xt)
  mu <- out$mean
  s  <- out$sd
  preds <- matrix(NA, nrow=1000, ncol=length(mu))
  for(i in seq_along(mu)){
    preds[,i] <- rnorm(1000, mu[i], s[i])
  }
  print("returning")
  return(preds)
}

# 29. Baseline (fallback model)
fit_baseline <- function(X, y){
  stop("Use fallback model")
}
pred_baseline <- function(obj, Xt){
  stop("Shouldn't even get here")
}







