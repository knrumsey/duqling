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
#'   \item `"bcart"`, `"btreelm"` require \pkg{tgp}; `"treegp"` requires \pkg{GPTreeO}
#'   \item `"rgasp"` requires \pkg{RobustGaSP}
#'   \item `"gp"` requires \pkg{hetGP}
#'   \item `"deepgp"` requires \pkg{deepgp}
#'   \item etc.
#' }
#'
#' Note: emulators based on \pkg{tgp} (e.g. \code{"bcart"}, \code{"btreelm"}) are \strong{not compatible} with the \code{mc_cores} argument
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
    treegp  = c("GPTreeO", "DiceKriging"),
    hetgp   = c("hetGP"),
    bootrf  = c("randomForest"),
    bcmgp   = c("spareGParts"),
    rgasp   = c("RobustGaSP"),
    baseline= character(0),
    khaos   = c("khaos")
  )

  if(length(intersect(c("bcart", "btreelm"), methods)) > 0){
    warning("Note: tgp-based emulators are not compatible with internal parallelization; use mc_cores=1.")
  }

  if ("ngboost" %in% methods) {
    stop(
      "Method 'ngboost' is not provided directly by the duqling package.\n",
      "It requires a working Python environment with the ngboost library installed.\n\n",
      "See:\n",
      "  https://github.com/knrumsey/duqling_results\n",
      "for implementation details.\n\n",
      "Please refer to:\n",
      "  https://github.com/Akai01/ngboost\n",
      "for installation instructions."
    )
  }

  if ("gbc" %in% methods) {
    stop(
      "Method 'gbc' is not provided directly by the duqling package.\n",
      "It requires an external Python environment and the GBC implementation.\n\n",
      "See:\n",
      "  https://github.com/knrumsey/duqling_results\n",
      "for implementation details.\n\n",
      "Please refer to:\n",
      "  https://github.com/VadimSokolov/gbc-surrogate/\n",
      "for installation instructions."
    )
  }

  if (any(c("gpytorch", "svigp") %in% methods)) {
    stop(
      "Methods 'gpytorch' and 'svigp' rely on external Python libraries and are not fully provided by duqling.\n\n",
      "See:\n",
      "  https://github.com/knrumsey/duqling_results\n",
      "for implementation details.\n\n",
      "Please install the required Python dependencies (e.g., gpytorch) and ensure they are available via reticulate.\n\n",
      "See:\n",
      "  https://gpytorch.ai/",
      call. = FALSE
    )
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
pred_bass <- function(obj, Xt) predict(obj, Xt, nugget=TRUE)

# 2. BART
fit_bart <- function(X, y){
  if(sd(y) ==0) y <- y + rnorm(length(y), 0, 1e-7)
  BART::wbart(X, y)
}
pred_bart <- function(obj, Xt){
  fdraws <- predict(obj, Xt)
  noise <- matrix(rnorm(length(fdraws), sd=rep(obj$sigma, ncol(fdraws))),
                  nrow=nrow(fdraws), ncol=ncol(fdraws))
  fdraws + noise
}

# 3. BayesPPR
fit_bppr <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(length(y), 0, 1e-7)
  BayesPPR::bppr(X, y)
}
pred_bppr <- function(obj, Xt){
  fdraws <- predict(obj, Xt)
  sigma_draws <- as.numeric(obj$sd_resid)
  noise <- matrix(rnorm(length(fdraws), sd=rep(sigma_draws, ncol(fdraws))),
                  nrow=nrow(fdraws), ncol=ncol(fdraws))
  fdraws + noise
}

# 4. Bayesian linear model
fit_blm <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(length(y), 0, 1e-7)
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
  if(var(y) == 0) y <- y + rnorm(length(y), 0, 1e-7)
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
  if(var(y) == 0) y <- y + rnorm(length(y), 0, 1e-7)
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
  if(sd(y) == 0) y <- y + rnorm(length(y),0,1e-7)
  khaos::sparse_khaos(X,y, verbose=FALSE)
}
pred_spce <- function(obj, Xt) predict(obj, Xt, nugget=TRUE)

# 8. Adaptive Khaos
fit_apce <- function(X, y){
  if(sd(y)==0) y <- y + rnorm(length(y),0,1e-7)
  khaos::adaptive_khaos(X,y,prior_type="ridge",verbose=FALSE)
}
pred_apce <- function(obj, Xt) predict(obj, Xt, nugget=TRUE)

# 9. Conformal Random Forest
fit_confrf <- function(X,y){
  if(sd(y)==0) y <- y + rnorm(length(y),0,1e-7)
  conforest::rfok(X,y)
}
pred_confrf <- function(obj, Xt) predict(obj, Xt)

# 10. Sparse GP (Fully independent training conditionals; FITC)
fit_fitcgp <- function(X, y){
  if(sd(y)==0) y <- y + rnorm(length(y),0,1e-7)
  else y <- y + rnorm(length(y),0,1e-7*sd(y))
  mu_y <- mean(y); sd_y <- sd(y)
  if(sd_y==0) sd_y <- 1
  y_std <- (y-mu_y)/sd_y
  n <- length(y_std)
  ni <- min(100, max(5, 4*floor(sqrt(n))), round(n*0.75))
  gp <- try({
    gp1 <- gplite::gp_init(method=gplite::method_fitc(num_inducing=ni))
    gplite::gp_optim(gp1,X,y_std,restarts=2)
  }, silent=TRUE)
  if(inherits(gp,"try-error")){
    ni2 <- max(5,floor(ni/2))
    gp <- gplite::gp_init(method=gplite::method_fitc(num_inducing=ni2))
    gp <- gplite::gp_optim(gp,X,y_std,restarts=2)
    gp$num_inducing_used <- ni2
  } else gp$num_inducing_used <- ni
  gp$mu_y <- mu_y; gp$sd_y <- sd_y
  gp$pred_jitter <- max(1e-9,1e-7*var(y_std))
  gp
}
pred_fitcgp <- function(obj,Xt){
  out <- try(gplite::gp_draw(obj,Xt,draws=1000,target=TRUE,
                             marginal=TRUE,jitter=obj$pred_jitter),silent=TRUE)
  if(inherits(out,"try-error"))
    out <- gplite::gp_draw(obj,Xt,draws=1000,target=TRUE,marginal=TRUE,
                           jitter=max(1e-6,100*obj$pred_jitter))
  obj$mu_y + obj$sd_y*t(out)
}

# 11. Basis GP (Random Fourier Features; Rahimi and Recht 2007)
fit_rffgp <- function(X,y){
  if(sd(y)==0) y <- y + rnorm(length(y),0,1e-7)
  else y <- y + rnorm(length(y),0,1e-7*sd(y))
  mu_y <- mean(y); sd_y <- sd(y)
  if(sd_y==0) sd_y <- 1
  y_std <- (y-mu_y)/sd_y
  n <- length(y_std); nb <- min(512,2*floor(sqrt(n)))
  gp <- try({
    gp1 <- gplite::gp_init(method=gplite::method_rf(num_basis=nb))
    gplite::gp_optim(gp1,X,y_std,restarts=2)
  },silent=TRUE)
  if(inherits(gp,"try-error")){
    nb2 <- max(16,floor(nb/2))
    gp <- try({
      gp1 <- gplite::gp_init(method=gplite::method_rf(num_basis=nb2))
      gplite::gp_optim(gp1,X,y_std,restarts=2)
    },silent=TRUE)
    if(!inherits(gp,"try-error")) gp$num_basis_used <- nb2
    else {
      nb3 <- max(8,floor(nb2/2))
      gp <- gplite::gp_init(method=gplite::method_rf(num_basis=nb3))
      gp <- gplite::gp_optim(gp,X,y_std,restarts=1)
      gp$num_basis_used <- nb3
    }
  } else gp$num_basis_used <- nb
  gp$mu_y <- mu_y; gp$sd_y <- sd_y
  gp$pred_jitter <- max(1e-9,1e-7*var(y_std))
  gp
}
pred_rffgp <- function(obj,Xt){
  out <- try(gplite::gp_draw(obj,Xt,draws=1000,target=TRUE,
                             marginal=TRUE,jitter=obj$pred_jitter),silent=TRUE)
  if(inherits(out,"try-error"))
    out <- gplite::gp_draw(obj,Xt,draws=1000,target=TRUE,marginal=TRUE,
                           jitter=max(1e-6,100*obj$pred_jitter))
  obj$mu_y + obj$sd_y*t(out)
}

# 12. Local Approximate GP
fit_lagp <- function(X,y){
  if(sd(y)==0) y <- y + rnorm(length(y),0,1e-7)
  if(max(table(y))>2) y <- y + rnorm(length(y),0,1e-7*sd(y))
  list(X=X,y=y)
}
pred_lagp <- function(obj,Xt){
  n <- nrow(obj$X); nn <- min(100,max(30,floor(sqrt(n))))
  base_nug <- max(1e-7*var(obj$y),1e-9)
  preds <- matrix(NA_real_,1000,nrow(Xt))
  for(i in seq_len(nrow(Xt))){
    mod <- NULL
    for(nug in c(base_nug,100*base_nug,1e4*base_nug)){
      mod_try <- try(laGP::laGP(Xt[i,,drop=FALSE],start=10,end=nn,
                                X=obj$X,Z=obj$y,g=nug),silent=TRUE)
      if(!inherits(mod_try,"try-error")){mod<-mod_try;break}
    }
    if(is.null(mod)){
      mod_try <- try(laGP::laGP(Xt[i,,drop=FALSE],start=10,end=31,
                                X=obj$X,Z=obj$y),silent=TRUE)
      if(!inherits(mod_try,"try-error")) mod <- mod_try
    }
    if(is.null(mod)) stop("laGP failed with explicit g and default g")
    v <- mod$df; s2 <- mod$s2*(v-2)/v
    preds[,i] <- mod$mean + rt(1000,v)*sqrt(abs(s2))
  }
  preds
}

# 13. Local Approximate GP (SoD)
fit_alcgp <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(length(y), 0, 1e-7)
  list(X=X, y=y)
}
pred_alcgp <- function(obj,Xt){
  n <- nrow(obj$X)
  nn <- max(100,min(300,n,2*round(sqrt(n))))
  base_nug <- max(1e-7*var(obj$y),1e-9)
  mod <- NULL
  for(nug in c(base_nug,100*base_nug,1e4*base_nug)){
    mod_try <- try(laGP::laGP(Xref=Xt,obj$X,obj$y,start=10,end=nn,g=nug),silent=TRUE)
    if(!inherits(mod_try,"try-error")){mod<-mod_try;break}
  }
  if(is.null(mod)) stop("alcgp failed for all nugget values")
  m <- mod$mean; v <- mod$df; s2 <- mod$s2*(v-2)/v
  preds <- matrix(NA_real_,1000,nrow(Xt))
  for(i in seq_len(nrow(Xt))) preds[,i] <- m[i]+rt(1000,v)*sqrt(abs(s2[i]))
  preds
}

# 14. Matching Pursuit SoD GP (Keerthi & Chu)
fit_mpgp <- function(X, y){
  if(sd(y) == 0){
    y <- y + rnorm(length(y), 0, 1e-7)
  }else{
    y <- y + rnorm(length(y), 0, sd(y)*1e-7)
  }

  n <- length(y)
  m <- min(1000, n-1, max(100, 2*floor(sqrt(n))))

  res <-  spareGParts::mpgp(X, y, m=m)
  return(res)
}
pred_mpgp <- function(obj,Xt) predict(obj,Xt,nugget=TRUE)

# 15. Scaled Vecchia GP
fit_svecgp <- function(X,y){
  if(sd(y)==0) y <- y + rnorm(length(y),0,1e-7)
  spareGParts::svecgp(X,y,nug=NULL)
}
pred_svecgp <- function(obj,Xt) predict(obj,Xt,nugget=TRUE)

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
  if(sd(y) == 0) y <- y + rnorm(length(y), 0, 1e-7)
  GBASS::tbass(X, y, df=3)
}
pred_tbass <- function(obj,Xt) predict(obj,Xt,predictive=TRUE)

# 18. Median BASS
fit_qbass <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(length(y), 0, 1e-7)
  GBASS::qbass(X, y, q=0.5)
}
pred_qbass <- function(obj,Xt) predict(obj,Xt,predictive=TRUE)

# 19. Relevance Vector Machine
fit_rvm <- function(X,y){
  if(sd(y)==0) y <- y + rnorm(length(y),0,1e-7)
  spareGParts::rvm(X,y)
}
pred_rvm <- function(obj,Xt) predict(obj,Xt,nugget=TRUE)

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
  if (sd(y) == 0) y <- y + rnorm(length(y), 0, 1e-7)

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
fit_blasso <- function(X,y){
  X <- as.matrix(X)
  pad <- max(0,3-ncol(X))
  if(pad>0) X_aug <- cbind(matrix(rnorm(nrow(X)*pad),nrow(X),pad),X)
  else X_aug <- X
  mu_y <- mean(y); sd_y <- sd(y)
  if(sd_y==0){y <- y+rnorm(length(y),0,1e-7);mu_y<-mean(y);sd_y<-sd(y)}
  y_std <- (y-mu_y)/sd_y
  fit <- BayesianLasso::Modified_Hans_Gibbs(X_aug,y_std,
    a1=1,b1=1,u1=0.1,v1=0.1,nsamples=5000,
    beta_init=rep(0.1,ncol(X_aug)),lambda_init=1,sigma2_init=1,
    verbose=FALSE)
  fit$mu_y <- mu_y; fit$sd_y <- sd_y; fit$pad <- pad
  class(fit) <- "blasso"
  fit
}


pred_blasso <- function(obj,Xt){
  Xt <- as.matrix(Xt)
  if(obj$pad>0) Xt <- cbind(matrix(0,nrow(Xt),obj$pad),Xt)
  nt <- nrow(Xt); ind <- seq(2001,5000,by=3)
  preds <- matrix(NA_real_,length(ind),nt)
  for(i in seq_along(ind)){
    beta <- obj$mBeta[ind[i],]
    sigma <- sqrt(obj$vsigma2[ind[i]])
    preds[i,] <- rnorm(nt, Xt%*%beta, sigma)
  }
  obj$mu_y+obj$sd_y*preds
}

# 22. Full GP
fit_gp <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(length(y), 0, 1e-7)
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
fit_deepgp <- function(X,y){
  if(sd(y)==0) y <- y+rnorm(length(y),0,1e-7)
  mu_y <- mean(y); sigma_y <- sd(y); y <- (y-mu_y)/sigma_y
  fit <- deepgp::fit_two_layer(X,y,nmcmc=1000,vecchia=TRUE)
  fit <- deepgp::trim(fit,500,5)
  fit$mu_y <- mu_y; fit$sigma_y <- sigma_y
  fit
}
pred_deepgp <- function(obj,Xt){
  tmp <- predict(obj,Xt)
  mu <- tmp$mean; sigma <- sqrt(abs(tmp$s2))
  mu[is.na(mu)] <- obj$mu_y; sigma[is.na(sigma)] <- obj$sigma_y
  preds <- matrix(NA_real_,1000,nrow(Xt))
  for(i in seq_len(nrow(Xt))) preds[,i] <- rnorm(1000,mu[i],sigma[i])
  obj$mu_y+obj$sigma_y*preds
}

# 24. Tree GP (GPTreeO version)
fit_treegp <- function(X,y){
  X <- as.matrix(X); y <- as.numeric(y)
  if(sd(y)==0) y <- y+rnorm(length(y),0,1e-7)
  y_var <- rep(max(1e-7*var(y),1e-9),length(y))
  Nbar <- min(nrow(X),max(200,3*ncol(X)+25))
  obj <- GPTreeO::GPTree$new(
    Nbar=Nbar,
    retrain_buffer_length=min(200,nrow(X)),
    wrapper="DiceKriging",
    gp_control=list(covtype="matern3_2",iso=FALSE,lower=1e-3,upper=10)
  )
  for(i in seq_len(nrow(X))) obj$update(X[i,],y[i],y_var[i])
  obj
}
pred_treegp <- function(obj,Xt){
  Xt <- as.matrix(Xt); n_test <- nrow(Xt); B <- 1000
  preds <- matrix(NA_real_,B,n_test)
  for(i in seq_len(n_test)){
    pr <- obj$joint_prediction(Xt[i,],return_std=TRUE)
    if(is.list(pr)){
      if(!is.null(pr$mean) && !is.null(pr$std)){
        mu <- as.numeric(pr$mean); sd <- as.numeric(pr$std)
      } else if(length(pr)>=2){
        mu <- as.numeric(pr[[1]]); sd <- as.numeric(pr[[2]])
      } else stop("Unexpected return object from GPTreeO joint_prediction")
    } else {
      pr <- as.numeric(pr)
      if(length(pr)<2) stop("Unexpected return value from GPTreeO joint_prediction")
      mu <- pr[1]; sd <- pr[2]
    }
    preds[,i] <- rnorm(B,mean=mu,sd=sd)
  }
  preds
}

# 25. Het GP
fit_hetgp <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(length(y), 0, 1e-7)
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
  if(sd(y) == 0) y <- y + rnorm(length(y), 0, 1e-7)
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
    y <- y + rnorm(length(y), 0, 1e-7)
  }else{
    y <- y + rnorm(length(y), 0, sd(y)*1e-7)
  }
  spareGParts::bcmgp(X, y)
}
pred_bcmgp <- function(obj,Xt) predict(obj,Xt,nugget=TRUE)

# 28. Robust GaSP
fit_rgasp <- function(X, y){
  if(sd(y) == 0) y <- y + rnorm(length(y), 0, 1e-7)
  print("trying gasp")
  RobustGaSP::rgasp(X, y, nugget.est=TRUE)
}
pred_rgasp <- function(obj,Xt){
  out <- RobustGaSP::predict.rgasp(obj,Xt)
  mu <- out$mean; s <- out$sd
  preds <- matrix(NA_real_,1000,length(mu))
  for(i in seq_along(mu)) preds[,i] <- rnorm(1000,mu[i],s[i])
  preds
}

# 29. Baseline (fallback model)
fit_baseline <- function(X, y){
  stop("Use fallback model")
}
pred_baseline <- function(obj, Xt){
  stop("Shouldn't even get here")
}








# 33. Adaptive Khaos (g-prior)
fit_khaos <- function(X,y){
  if(sd(y)==0) y <- y+rnorm(length(y),0,1e-7)
  khaos::adaptive_khaos(X,y,prior_type="gprior",verbose=FALSE)
}
pred_khaos <- function(obj,Xt) predict(obj,Xt,nugget=TRUE)
