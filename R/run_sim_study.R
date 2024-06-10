#' Reproducible Simulation Study (Test Functions)
#'
#' Reproducible code for simulation studies.
#'
#' @param fit_func If \code{pred_func} is specified, the \code{fit_func} should take two arguments called \code{X_train} and \code{y_train}, and should return an object which will be passed to \code{pred_func}. If \code{pred_func} is NOT specified, then \code{fit_func} should take a third argument called \code{X_test}, and should return predictive samples (see pred_func documentation).
#' @param pred_func A function taking two arguments: (i) the object returned by \code{fit_func} and a matrix \code{X_test}. The function should return an matrix of samples from the predictive distribution, with one column per test point. For additional flexibility, see the details below.
#' @param fnames A vector of function names from the \code{duqling} package. See \code{quack()} for details.
#' @param conf_level A vector of confidence levels.
#' @param n_train the sample size (or vector of sample sizes) for each training set
#' @param n_test the sample size for each testing set
#' @param NSR the noise to signal ratio (inverse of the more-standard signal to noise ratio).
#' @param design_type How should the training and testing designs be generated? Options are "LHS", "grid" and "random"
#' @param replications How many replications should be repeated for each data set?
#' @param seed Seed for random number generators. For reproducibility, we discourage the use of this argument.
#' @param method_names A vector of method names, length equal to \code{length(fit_func)}. If NULL, the indexed names \code{my_method<i>} will be used.
#' @param mc_cores How many cores to use for parallelization over replications.
#' @param verbose should progress be reported?
#' @return See details
#' @details Code to conduct a reproducible simulation study to compare emulators. By reporting the parameters to the study, other authors can compare their results directly.
#' Only \code{fit_func} needs to be specified, but only the total time will be reported. The simplest (and recommended) approach is that the \code{fit_func} (or \code{pred_func}) should return a matrix of posterior samples, with one column per test point (e.g., per row in \code{X_test}). Any number of rows (predictive samples) is allowed. In this case, the mean of the samples is used as a prediction and the R \code{quantile} function is used to construct confidence intervals. This default behavior can be changed by instead allowing \code{fit_func} (or \code{pred_func}) to return a named list with fields i) \code{samples} (required), ii) \code{preds} (optional; a vector of predictions), and iii) intervals (optional; a 2 by n by k array of interval bounds where n is the number of test points and k is \code{length(conf_level)}).
#'
#' @references Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#' @export
#' @examples
#' library(BASS)
#'
#' my_fit <- function(X, y){
#'   bass(X, y, g1=0.001, g2=0.001)
#' }
#' my_pred <- function(obj, Xt){
#'   predict(obj, Xt)
#' }
#' run_sim_study(my_fit, my_pred,
#'              fnames=get_sim_functions_tiny()[1:2],
#'              n_train=50)
run_sim_study <- function(fit_func, pred_func=NULL,
                          fnames=quack(input_dims = 1)$fname,
                          conf_level = NULL,
                          n_train = 100,
                          n_test = 1e4,
                          NSR = 0,
                          design_type = "LHS",
                          replications = 1,
                          seed = 42,
                          method_names=NULL,
                          mc_cores=1,
                          verbose=TRUE){

  # error handling here
  if(is.null(method_names)){
      method_names <- names(fit_func)
  }

  DF_full <- NULL
  for(ff in seq_along(fnames)){
    fn <- fnames[ff]
    p <- quack(fn)$input_dim
    #fnum <- which(fn == quack(sorted=FALSE)$fname)
    fnum <- str2num(fn)
    if(verbose) cat("Starting function ", ff, "/", length(fnames), ": ", fn, "\n", sep="")
    for(ii in seq_along(n_train)){
      n <- n_train[ii]
      if(verbose) cat("\t Running all combinations and replications for n =", n, "\n")
      for(jj in seq_along(NSR)){
        for(kk in seq_along(design_type)){

          if(mc_cores == 1){
            results <- lapply(1:replications, run_one_sim_case,
                   seed=seed, fn=fn, fnum=fnum, p=p, n=n, conf_level=conf_level,
                   nsr=NSR[jj], dsgn=design_type[kk], n_test=n_test,
                   method_names=method_names, fit_func=fit_func, pred_func=pred_func, verbose=verbose)
          }else{
            results <- parallel::mclapply(1:replications, run_one_sim_case,
                              seed=seed, fn=fn, fnum=fnum, p=p, n=n, conf_level=conf_level,
                              nsr=NSR[jj], dsgn=design_type[kk], n_test=n_test,
                              method_names=method_names, fit_func=fit_func, pred_func=pred_func, verbose=verbose,
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

str2num <- function(str){
  sum((1:nchar(str))*as.numeric(unlist(iconv(str, to="ASCII", toRaw=TRUE))))
}

rmsef <- function(x, y){
  sqrt(mean((x-y)^2))
}

transform_seed <- function(seed, n, dt, NSR, fnum, rr){
 s1 <- seed
 s2 <- round(log(n))
 s3 <- round(100*(log(n) %% 1))
 s4 <- switch(dt, LHS = 1, grid = 2, random = 3)
 s5 <- fnum
 s6 <- round(100*NSR)
 s7 <- round(100*((100*NSR) %% 1))
 s8 <- floor(rr/100)
 s8 <- rr %% 100
 B <- 101
 ss <- 0
 for(i in 1:8) ss <- ss + B^(i-1)*get(paste0("s", i))
 ss <- ss %% 100030001
 return(ss)
}

run_one_sim_case <- function(rr, seed, fn, fnum, p, n, nsr, dsgn, n_test, conf_level, method_names, fit_func, pred_func, verbose){
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

    # ==================================================
    # Fit models
    # ==================================================
    for(ii in seq_along(fit_func)){
      my_method <- ifelse(is.null(method_names[ii]), paste0("method", ii), method_names[ii])
      #browser()
      DF_curr <- data.frame(method=my_method,
                            fname=fn, input_dim=p,
                            n=n,
                            NSR=nsr,
                            design_type=dsgn,
                            rep=rr)

      fit_func_curr <- ifelse(is.function(fit_func), fit_func, fit_func[[ii]])
      if(is.function(pred_func)){
        pred_func_curr <- pred_func
      }else{
        if(is.null(pred_func[[ii]])){
          pred_func_curr <- NULL
        }else{
          pred_func_curr <- pred_func[[ii]]
        }
      }
      #browser()
      # Call fit_func()
      if(is.null(pred_func_curr)){
        tictoc::tic()
        preds <- fit_func_curr(X_train, y_train, X_test)
        t_tot <- tictoc::toc(quiet=!verbose)

        DF_curr$t_tot <- t_tot$toc - t_tot$tic
      }else{
        tictoc::tic()
        fitted_object <- fit_func_curr(X_train, y_train)
        t_fit <- tictoc::toc(quiet=!verbose)

        tictoc::tic()
        preds <- pred_func_curr(fitted_object, X_test)
        t_pred <- tictoc::toc(quiet=!verbose)

        DF_curr$t_fit <- t_fit$toc - t_fit$tic
        DF_curr$t_pred <- t_pred$toc - t_pred$tic
        DF_curr$t_tot <- DF_curr$t_fit + DF_curr$t_pred
      }

      #browser()
      # Compute RMSE, coverage(s), and CRPS
      # CASE: function returns matrix or vector of predictions
      if(!is.numeric(preds) & !is.list(preds)){
        stop("fit/pred functions must return a vector, matrix, or a list. See help file for details. ")
      }
      if(is.numeric(preds)){
        if(is.null(dim(preds))){
          preds <- matrix(preds, ncol=1)
        }
        # CALCULATE RMSE
        y_hat <- colMeans(preds)
        rmse_curr <- rmsef(y_test, y_hat)
        DF_curr$RMSE <- rmse_curr
        DF_curr$FVU  <- rmse_curr^2/var(y_test)

        # CALCULATE COVERAGES
        n_conf <- length(conf_level)
        if(n_conf > 0){
          nms <- names(DF_curr)
          for(iii in seq_along(conf_level)){
            alpha_curr <- 1 - conf_level[iii]
            bounds <- apply(preds, 2, quantile, probs=c(alpha_curr/2, 1-alpha_curr/2))
            DF_curr[,ncol(DF_curr)+1] <- mean((y_test >= bounds[1,]) * (y_test <= bounds[2,]))
          }
          colnames(DF_curr) <- c(nms, unlist(lapply(as.character(round(conf_level, 10)), function(zz) paste0("CONF", zz))))
        }

        # CALUCLATE CRPS
        CRPS_vec <- rep(NA, n_test)
        for(iii in 1:n_test){
          print(preds[,iii])
          y_pred <- preds[,iii]
          range_curr <- range(c(y_pred, y_test[iii]))
          xx <- matrix(seq(range_curr[1]*(1-1e-7), range_curr[2], length.out=1000), ncol=1)
          Fhat <- apply(xx, 1, function(xx) mean(y_pred <= xx))
          Ihat <- as.numeric(xx >= y_test[iii])
          CRPS_vec[iii] <- mean((Fhat-Ihat)^2)
        }
        DF_curr$CRPS <- mean(CRPS_vec)
        DF_curr$CRPS_sd <- sd(CRPS_vec)
      }

      # CASE: Function returns a named list
      if(is.list(preds)){
        if(is.null(preds$samples)){
          stop("If pred/fit function returns a list, the samples field must be specified.")
        }
        if(is.null(preds$preds)){
          preds$preds <- rowMeans(preds$samples)
        }
        if(is.null(preds$intervals)){
          n_conf <- length(conf_level)
          intervals <- array(NA, dim=c(2, n_test, n_conf))
          for(iii in seq_along(conf_level)){
            alpha_curr <- 1 - conf_level[iii]
            intervals[,,iii] <- apply(preds$samples, 1, quantile, probs=c(alpha_curr/2, 1-alpha_curr/2))
          }
          if(n_conf == 1) intervals <- intervals[,,1]
          preds$intervals <- intervals
        }

        # CALCULATE RMSE
        y_hat <- colMeans(preds$preds)
        rmse_curr <- rmsef(y_test, y_hat)
        DF_curr$RMSE <- rmse_curr
        DF_curr$FVU  <- rmse_curr^2/var(y_test)

        # CALCULATE COVERAGES
        n_conf <- length(conf_level)
        if(n_conf > 0){
          nms <- names(DF_curr)
          for(iii in seq_along(conf_level)){
            alpha_curr <- 1 - conf_level[iii]
            if(is.matrix(preds$intervals)){
              bounds <- preds$intervals
            }else{
              bounds <- preds$intervals[,,iii]
            }
            DF_curr[,ncol(DF_curr)+1] <- mean((y_test >= bounds[1,]) * (y_test <= bounds[2,]))
          }
          colnames(DF_curr) <- c(nms, unlist(lapply(as.character(round(conf_level, 10)), function(zz) paste0("CONF", zz))))
        }

        # CALCULATE CRPS
        CRPS_vec <- rep(NA, n_test)
        for(iii in 1:n_test){
          y_pred <- preds$samples[,iii]
          range_curr <- range(c(y_pred, y_test[iii]))
          xx <- matrix(seq(range_curr[1]*(1-1e-7), range_curr[2], length.out=1000), ncol=1)
          Fhat <- apply(xx, 1, function(xx) mean(y_pred <= xx))
          Ihat <- as.numeric(xx >= y_test[iii])
          CRPS_vec[iii] <- mean((Fhat-Ihat)^2)*diff(range_curr)
        }
        DF_curr$CRPS <- mean(CRPS_vec)
        DF_curr$CRPS_sd <- sd(CRPS_vec)
      }
      if(ii == 1){
        DF_res <- DF_curr
      }else{
        DF_res <- rbind(DF_res, DF_curr)
      }
    }# End loop over methods
  return(DF_res)
}

#' Details:
#'
#' fit_func can return any of the following
#'   A) an M by n matrix of predictive samples (recommended)
#'   B) a n-vector of predictions (will be converted to an n by 1 matrix)
#'   C) a named list with fields
#'      - preds: An n-vector of predictions. If NULL, then rowMeans(samples) will be used.
#'      - intervals: A 2 by n by K array (or a matrix, if K = 1) where K = length(conf_level). If NULL, then quantiles of the samples field will be used.
#'      - samples: An n by M matrix of samples from the predictive distribution





