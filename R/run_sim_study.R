#' Reproducible Simulation Study
#'
#' Reproducible code for simulation studies.
#'
#' @param my_fit If \code{my_pred} is specified, then \code{my_fit} should take two arguments called \code{X_train} and \code{y_train}, and should return a fitted model object which will be passed to \code{my_pred}. Otherwise, \code{my_fit} should take a third and fourth argument, \code{X_test, conf_level}, and should return predictions as specified below.
#' @param my_pred A function taking three arguments, the object returned by \code{my_fit}, \code{X_test} and a confidence level \code{alpha} (if coverage is a desired metric, \code{interval = TRUE}). If coverage is to be computed, \code{my_pred} should return a matrix with columns names c("preds", "lb", "ub"). Otherwise, it should return a vector of predictions.
#' @param fnames A vector of function names from the \code{duqling} package. See \code{quack()} for details.
#' @param interval Do we track interval estimates (or just point estimates)?
#' @param n_train the sample size (or vector of sample sizes) for each training set
#' @param n_test the sample size for each testing set
#' @param NSR the noise to signal ratio (inverse of the more-standard signal to noise ratio).
#' @param design_type How should the training and testing designs be generated? Options are "LHS", "grid" and "random"
#' @param replications How many replications should be repeated for each data set?
#' @param seed Seed for random number generators. For reproducibility, we discourage the use of this argument.
#' @param conf_level Confidence level for interval estimates. If \code{length(conf_level) > 1}, then \code{my_pred} should return a matrix with \code{1 + 2*length(conf_level)} columns, with 2 columns of lower and upper bounds for each value in \code{conf_}
#' @param method_names A vector of method names, length equal to \code{length(my_fit)}. If NULL, the indexed names \code{my_method<i>} will be used.
#' @param mc_cores How many cores to use for parallelization over replications.
#' @param verbose should progress be reported?
#' @return See details
#' @details Code to conduct a reproducible simulation study to compare emulators. By reporting the parameters to the study, other authors can compare their results directly.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#' @export
#' @examples
#' library(BASS)
#'
#' my_fit <- function(X, y){
#'   bass(X, y, g1=0.001, g2=0.001)
#' }
#'
#' my_pred <- function(obj, Xt, conf_level=0.95){
#'   alpha <- 1 - conf_level
#'   preds <- predict(obj, Xt)
#'   yhat <- apply(preds, 2, mean)
#'   sd <- sqrt(apply(preds, 2, var) + mean(obj$s2))
#'   res <- cbind(yhat, yhat-2*sd, yhat + 2*sd)
#'   return(res)
#' }
#'
#' run_sim_study(my_fit, my_pred,
#'    fnames=get_sim_functions_tiny(),
#'    n_train=50)
run_sim_study <- quack_off <- function(my_fit, my_pred=NULL,
                          fnames=quack(input_dims = 1)$fname,
                          interval=TRUE,
                          n_train = 100,
                          n_test = 1e4,
                          NSR = 0,
                          design_type = "LHS",
                          replications = 1,
                          seed = 42,
                          conf_level = 0.95,
                          method_names=NULL,
                          mc_cores=1,
                          verbose=TRUE){

  # error handling here



  DF_full <- NULL
  for(ff in seq_along(fnames)){
    fn <- fnames[ff]
    p <- quack(fn)$input_dim
    fnum <- which(fn == quack()$fname)
    if(verbose) cat("Starting function", fn, "\n")
    for(ii in seq_along(n_train)){
      n <- n_train[ii]
      if(verbose) cat("\t Running all combinations and replications for n =", n, "\n")
      for(jj in seq_along(NSR)){
        for(kk in seq_along(design_type)){

          if(mc_cores == 1){
            results <- lapply(1:replications, run_one_sim_case,
                   seed=seed, fn=fn, fnum=fnum, p=p, n=n, conf_level=conf_level,
                   nsr=NSR[jj], dsgn=design_type[kk], n_test=n_test, interval=interval,
                   my_fit=my_fit, my_pred=my_pred)
          }else{
            results <- parallel::mclapply(1:replications, run_one_sim_case,
                              seed=seed, fn=fn, fnum=fnum, p=p, n=n, conf_level=conf_level,
                              nsr=NSR[jj], dsgn=design_type[kk], n_test=n_test, interval=interval,
                              my_fit=my_fit, my_pred=my_pred,
                              mc.cores=mc_cores)
          }

          # Collect results for current setting
          DF_curr <- results[[1]]
          if(length(results) >= 2){
            for(rr in 2:length(results)){
              DF_curr <- rbind(DF_curr, results[[rr]])
            }
          }
          # Store data
          if(is.null(DF_full)){
            DF_full <- DF_curr
          }else{
            DF_full <- rbind(DF_full, DF_curr)
          }
                }
      }
    }
  }
  return(DF_full)
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

transform_seed <- function(seed, n, dt, NSR, fnum, rr){
  design_num <- switch(dt, LHS = 1, grid = 2, random = 3)
  SNR_num    <- ifelse(round(1/NSR) == Inf, 0, round(1/NSR))
  BASE <- max(rr, n, SNR_num, 3) + 1
  seed_transform <- (n + BASE*seed + BASE^2*SNR_num + BASE^3*fnum + BASE^4*rr + BASE^5*design_num) %% 100030001
  return(seed_transform)
}


run_one_sim_case <- function(rr, seed, fn, fnum, p, n, nsr, dsgn, n_test, conf_level, interval, my_fit, my_pred){
    # Store case
    DF_curr <- data.frame(fname=fn, input_dim=p,
                          n=n,
                          NSR=nsr,
                          design_type=dsgn,
                          rep=rr)

    # Generate training data
    seed_t <- transform_seed(seed, n, dsgn, nsr, fnum, rr)
    set.seed(seed_t)
    if(dsgn == "LHS"){
      if(n <= 1200){
        X_train <- lhs::maximinLHS(n, p)
        X_test  <- lhs::randomLHS(n_test, p)
      }else{
        X_train <- lhs::randomLHS(n, p)
        X_test  <- lhs::randomLHS(n_test, p)
      }
    }else{
      if(dsgn == "random"){
        X_train <- matrix(runif(n*p), ncol=p)
        X_test <- matrix(runif(n_test*p), ncol=p)
      }else{ # grid
        ni = ceiling(n^(1/p))
        xx <- seq(0, 1, length.out = ni)
        X_train <- expand_grid(xx, p)
        n <- nrow(X_train)

        nit = ceiling(n_test^(1/p))
        xxt <- seq(0, 1, length.out = nit)
        X_test <- expand_grid(xxt, p)
      }
    }
    f <- get(fn, loadNamespace("duqling"))

    y_train <- apply(X_train, 1, f, scale01=TRUE)
    if(var(y_train) == 0){
      noise_lvl <- 1 # Cannot make sense of SNR when there is no signal
    }else{
      noise_lvl <- sqrt(var(y_train) * nsr)
    }
    y_train <- y_train + rnorm(n, 0, noise_lvl)
    y_test <- apply(X_test, 1, f, scale01=TRUE) # no noise for testing data


    # Call my_fit()
    if(is.null(my_pred)){
      tictoc::tic()
      preds <- my_fit(X_train, y_train, X_test, conf_level)
      t_tot <- tictoc::toc()

      DF_curr$t_tot <- t_tot$toc - t_tot$tic
    }else{
      tictoc::tic()
      fitted_object <- my_fit(X_train, y_train)
      t_fit <- tictoc::toc()

      tictoc::tic()
      preds <- my_pred(fitted_object, X_test, conf_level)
      t_pred <- tictoc::toc()

      DF_curr$t_fit <- t_fit$toc - t_fit$tic
      DF_curr$t_pred <- t_pred$toc - t_pred$tic
      DF_curr$t_tot <- DF_curr$t_fit + DF_curr$t_pred
    }

    # RMSE and coverage
    if(interval == TRUE){
      rmse_curr <- rmsef(y_test, preds[,1])
      DF_curr$RMSE <- rmse_curr
      DF_curr$FVU  <- rmse_curr^2/var(y_test)

      # Compute empirical coverage for each value in conf_level
      n_conf <- length(conf_level)
      nms <- names(DF_curr)
      for(iii in 1:n_conf){
        DF_curr[,ncol(DF_curr)+1] <- mean((y_test >= preds[,2*iii]) * (y_test <= preds[,2*iii+1]))
      }
      colnames(DF_curr) <- c(nms, lapply(as.character(conf_level), function(zz) paste0("CONF", zz)))

    }else{
      rmse_curr <- rmsef(y_test, preds)
      DF_curr$RMSE <- rmse_curr
      DF_curr$FVU  <- rmse_curr^2/var(y_test)
    }
  return(DF_curr)
}
