#' Reproducible Simulation Study (Test Functions)
#'
#' Reproducible code for simulation studies.
#'
#' @param fit_func If \code{pred_func} is specified, the \code{fit_func} should take two arguments called \code{X_train} and \code{y_train}, and should return an object which will be passed to \code{pred_func}. If \code{pred_func} is NOT specified, then \code{fit_func} should take a third argument called \code{X_test}, and should return predictive samples (see pred_func documentation).
#' @param pred_func A function taking two arguments: (i) the object returned by \code{fit_func} and a matrix \code{X_test}. The function should return an matrix of samples from the predictive distribution, with one column per test point. For additional flexibility, see the details below.
#' @param fnames A vector of function names from the \code{duqling} package. See \code{quack()} for details.
#' @param conf_level A vector of confidence levels.
#' @param score Logical. Should CRPS be computed?
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
#' run_sim_study(my_fit, my_pred,
#'              fnames=c("dms_additive", "borehole"),
#'              n_train=50,
#'              replications=2,
#'              method_names=c("lm", "ppr") # Optional
#'              )
#'
run_sim_study <- function(fit_func, pred_func=NULL,
                          fnames=NULL,
                          conf_level = c(0.8, 0.9, 0.95, 0.99),
                          score = TRUE,
                          n_train = 100,
                          n_test = 1000,
                          NSR = 0,
                          design_type = "LHS",
                          replications = 1,
                          seed = 42,
                          method_names=NULL,
                          mc_cores=1,
                          verbose=TRUE){

  # error handling here
  if(is.null(fnames)){
    fnames = quack(input_dim = 1)$fname
  }
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
                   seed=seed, fn=fn, fnum=fnum, p=p, n=n, conf_level=conf_level, score=score,
                   nsr=NSR[jj], dsgn=design_type[kk], n_test=n_test,
                   method_names=method_names, fit_func=fit_func, pred_func=pred_func, verbose=verbose)
          }else{
            results <- parallel::mclapply(1:replications, run_one_sim_case,
                              seed=seed, fn=fn, fnum=fnum, p=p, n=n, conf_level=conf_level, score=score,
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

# crpsf <- function(y, x, w=0){
#   M <- length(x)
#   term1 <- mean(abs(x-y))
#   if(M <= 6500){
#     term2 <- sum(outer(x, x, function(a, b) abs(a-b))) # Fastest way for small M
#   }else{
#     idx <- unlist(lapply(2:M, function(i) 1:i))
#     term2 <- 2*sum(abs(x[rep(2:M, 2:M)] - x[idx]))     # Faster for big M
#   }
#   res <- term1 - (1 - w/M)*term2/(2*M*(M-1))
#   return(res)
# }

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

run_one_sim_case <- function(rr, seed, fn, fnum, p, n, nsr, dsgn, n_test, conf_level, score, method_names, fit_func, pred_func, verbose){
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
        X_train <- matrix(stats::runif(n*p), ncol=p)
        X_test <- matrix(stats::runif(n_test*p), ncol=p)
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
    v_train <- stats::var(y_train)
    if(v_train == 0){
      noise_lvl <- 1e-6 * sqrt(nsr)  # Cannot make sense of SNR when there is no signal
      total_var <- 1e-12 * (1 + nsr)
    }else{
      noise_lvl <- sqrt(v_train * nsr)
      total_var <- v_train * (1 + nsr)
    }
    y_train <- y_train + stats::rnorm(n, 0, noise_lvl)
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
        DF_curr$FVU  <- rmse_curr^2 / total_var

        # CALCULATE COVERAGES
        n_conf <- length(conf_level)
        if(n_conf > 0){
          nms <- names(DF_curr)
          for(iii in seq_along(conf_level)){
            alpha_curr <- 1 - conf_level[iii]
            bounds <- apply(preds, 2, stats::quantile, probs=c(alpha_curr/2, 1-alpha_curr/2))

            # Coverage
            DF_curr[,ncol(DF_curr)+1] <- mean((y_test >= bounds[1,]) * (y_test <= bounds[2,]))
          }

          # INTERVAL SCORES
          for(iii in seq_along(conf_level)){
            alpha_curr <- 1 - conf_level[iii]
            bounds <- apply(preds, 2, stats::quantile, probs=c(alpha_curr/2, 1-alpha_curr/2))

            # Negatively oriented mean interval score (Raftery & Gneiting)
            term1 <- apply(bounds, 2, diff)
            term2 <- 2*(bounds[,1] - y_test)*as.numeric(y_test < bounds[,1])/alpha_curr
            term3 <- 2*(y_test - bounds[,2])*as.numeric(y_test > bounds[,2])/alpha_curr
            DF_curr[,ncol(DF_curr)+1] <- mean(term1 + term2 + term3)
          }
          nms <- c(nms, paste0("COVER", round(conf_level, 7)), paste0("MIS", round(conf_level, 7)))
          colnames(DF_curr) <- nms
        }

        # CALUCLATE CRPS
        if(score){
          if(verbose) cat("Computing CRPS")
          CRPS_vec <- unlist(lapply(1:n_test, function(i) crpsf(y_test[i], preds[,i])))
          csumm <- summary(CRPS_vec)
          DF_curr$CRPS <- csumm[4]
          DF_curr$CRPS_min <- csumm[1]
          DF_curr$CRPS_Q1 <- csumm[2]
          DF_curr$CRPS_med <- csumm[3]
          DF_curr$CRPS_Q3 <- csumm[5]
          DF_curr$CRPS_max <- csumm[6]
          if(verbose) cat("\nDone.")
        }
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
            intervals[,,iii] <- apply(preds$samples, 1, stats::quantile, probs=c(alpha_curr/2, 1-alpha_curr/2))
          }
          if(n_conf == 1) intervals <- intervals[,,1]
          preds$intervals <- intervals
        }

        # CALCULATE RMSE
        y_hat <- colMeans(preds$preds)
        rmse_curr <- rmsef(y_test, y_hat)
        DF_curr$RMSE <- rmse_curr
        DF_curr$FVU  <- rmse_curr^2/stats::var(y_test)

        # CALCULATE COVERAGES
        n_conf <- length(conf_level)
        if(n_conf > 0){
          nms <- names(DF_curr)
          #COVERAGES
          for(iii in seq_along(conf_level)){
            alpha_curr <- 1 - conf_level[iii]
            bounds <- apply(preds, 2, stats::quantile, probs=c(alpha_curr/2, 1-alpha_curr/2))

            DF_curr[,ncol(DF_curr)+1] <- mean((y_test >= bounds[1,]) * (y_test <= bounds[2,]))
          }

          # INTERVAL SCORES
          for(iii in seq_along(conf_level)){
            alpha_curr <- 1 - conf_level[iii]
            bounds <- apply(preds, 2, stats::quantile, probs=c(alpha_curr/2, 1-alpha_curr/2))

            term1 <- apply(bounds, 2, diff)
            term2 <- 2*(bounds[,1] - y_test)*as.numeric(y_test < bounds[,1])/alpha_curr
            term3 <- 2*(y_test - bounds[,2])*as.numeric(y_test > bounds[,2])/alpha_curr
            DF_curr[,ncol(DF_curr)+1] <- mean(term1 + term2 + term3)
          }
          nms <- c(nms, paste0("COVER", round(conf_level, 7)), paste0("MIS", round(conf_level, 7)))
          colnames(DF_curr) <- nms
        }

        # CALCULATE CRPS
        if(score){
          if(verbose) cat("Computing CRPS")
          CRPS_vec <- unlist(lapply(1:n_test, function(i) crpsf(y_test[i], preds[,i])))
          csumm <- summary(CRPS_vec)
          DF_curr$CRPS <- csumm[4]
          DF_curr$CRPS_min <- csumm[1]
          DF_curr$CRPS_Q1 <- csumm[2]
          DF_curr$CRPS_med <- csumm[3]
          DF_curr$CRPS_Q3 <- csumm[5]
          DF_curr$CRPS_max <- csumm[6]
          if(verbose) cat("\nDone.")
        }
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





