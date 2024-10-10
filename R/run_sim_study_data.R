#' Reproducible Simulation Study (Data)
#'
#' Reproducible code for simulation studies with real data sets.
#'
#' @param fit_func If \code{pred_func} is specified, the \code{fit_func} should take two arguments called \code{X_train} and \code{y_train}, and should return an object which will be passed to \code{pred_func}. If \code{pred_func} is NOT specified, then \code{fit_func} should take a third argument called \code{X_test}, and should return predictive samples (see pred_func documentation).
#' @param pred_func A function taking two arguments: (i) the object returned by \code{fit_func} and a matrix \code{X_test}. The function should return an matrix of samples from the predictive distribution, with one column per test point. For additional flexibility, see the details below.
#' @param dnames A vector of dataset names from the \code{duqling} package. See \code{data_quack()} for details.
#' @param dsets A list of datasets. Each list component is a list with elements \code{X} (a matrix) and \code{y} (a vector).
#' @param folds Number of folds in cross validation. Can be a scalar or a vector (with \code{length(folds) == length(dnames)}). If \code{folds} is negative, then a Bootstrap Cross-Validation procedure is run \code{-folds} times.
#' @param seed Seed for random number generators. For reproducibility, we discourage the use of this argument.
#' @param conf_level Confidence level for interval estimates. If \code{length(conf_level) > 1}, then \code{pred_func} should return a matrix with \code{1 + 2*length(conf_level)} columns, with 2 columns of lower and upper bounds for each value in \code{conf_}
#' @param score Logical. Should CRPS be computed?
#' @param method_names A vector of method names, length equal to \code{length(fit_func)}. If NULL, the indexed names \code{my_method<i>} will be used.
#' @param custom_data_names An optional vector of dataset names corresponding to the argument \code{dsets}.
#' @param mc_cores How many cores to use for parallelization over replications.
#' @param verbose should progress be reported?
#' @return See details
#' @details Code to conduct a reproducible simulation study to compare emulators. By reporting the parameters to the study, other authors can compare their results directly.
#' Only \code{fit_func} needs to be specified, but only the total time will be reported. The simplest (and recommended) approach is that the \code{fit_func} (or \code{pred_func}) should return a matrix of posterior samples, with one column per test point (e.g., per row in \code{X_test}). Any number of rows (predictive samples) is allowed. In this case, the mean of the samples is used as a prediction and the R \code{quantile} function is used to construct confidence intervals. This default behavior can be changed by instead allowing \code{fit_func} (or \code{pred_func}) to return a named list with fields i) \code{samples} (required), ii) \code{preds} (optional; a vector of predictions), and iii) intervals (optional; a 2 by n by k array of interval bounds where n is the number of test points and k is \code{length(conf_level)}).
#'
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
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
#'
#' run_sim_study_data(fit_func, pred_func,
#'    dnames=c("pbx9501_gold", "strontium_plume_p104"),
#'    folds=c(2, 3))
run_sim_study_data <- function(fit_func, pred_func=NULL,
                          dnames=get_sim_data_tiny(),
                          dsets=NULL,
                          folds=20,
                          seed = 42,
                          conf_level = c(0.8, 0.9, 0.95, 0.99),
                          score=TRUE,
                          method_names=NULL,
                          custom_data_names=NULL,
                          mc_cores=1,
                          verbose=TRUE){

  # error handling here
  if(is.null(method_names)){
    method_names <- names(fit_func)
  }
  if(length(folds) == 1){
    folds <- rep(folds, length(dnames))
  }
  n_custom_data <- length(dsets)
  if(n_custom_data > 0){
    if(!is.null(dsets$X)){
      if(n_custom_data == 2){
        # Try to fix it
        tmp <- list()
        tmp[[1]] <- dsets
        dsets <- tmp
        n_custom_data <- 1
        warning("Trying to coerce. dsets should be a list of lists. Check documentation.")
      }else{
        stop("dsets should be a list of lists. Check documentation.")
      }
    }
    dnames <- c(dnames, paste0("custom", 1:n_custom_data))
    custom_cnt <- 1
  }

  DF_full <- NULL
  for(dd in seq_along(dnames)){
    dn <- dnames[dd]
    dnum <- str2num(dn)
    set.seed(seed + str2num(dn))
    if(verbose) cat("Starting dataset ", dd, "/", length(dnames), ": ", dn, "\n", sep="")

    # Check if this is a custom dataset
    if(grep("custom", dn)){
      X <- dsets[[custom_cnt]]$X
      y <- dsets[[custom_cnt]]$y
      cdn <- custom_data_names[custom_cnt]
      custom_cnt <- custom_cnt + 1
    }else{
      # Load data from UQDataverse
      curr <- get_emulation_data(dn)
      X <- as.matrix(curr$X)
      y <- curr$y
    }
    n <- nrow(X)
    p <- ncol(X)
    # Get CV information
    K <- folds[dd]
    if(K == 0) stop("Cannot have 0 folds")
    if(K > 0){
      # Cross Validation
      cv_type <- "cv"
      groups <- k.chunks(n, K)
    }else{
      # Bootstrap Cross-Validation
      cv_type <- "boot"
      K <- -K
      groups <- list()
      for(kk in 1:K){
        groups[[k]] <- sample(n, n, TRUE)
      }
    }

    if(mc_cores == 1){
      results <- lapply(X=1:K, FUN=run_one_sim_case_data,
                        XX=X, yy=y, groups=groups, cv_type=cv_type,
                        dn=dn, score=score,
                        conf_level=conf_level,
                        method_names=method_names,
                        custom_data_name=cdn,
                        fit_func=fit_func, pred_func=pred_func, verbose=verbose)
    }else{
      results <- parallel::mclapply(1:K, run_one_sim_case_data,
                                    XX=X, yy=y, groups=groups, cv_type=cv_type,
                                    dn=dn, score=score,
                                    conf_level=conf_level,
                                    method_names=method_names,
                                    custom_data_name=cdn,
                                    fit_func=fit_func, pred_func=pred_func, verbose=verbose,
                                    mc_cores=mc_cores)
    }
    # Collect results for current setting
    DF_curr <- results[[1]]
    if(length(results) >= 2){
      for(kk in 2:length(results)){
        DF_curr <- rbind(DF_curr, results[[kk]])
      }
    }
    # Store data
    if(is.null(DF_full)){
      DF_full <- DF_curr
    }else{
      DF_full <- rbind(DF_full, DF_curr)
    }
  }
  return(DF_full)
}


k.chunks = function(n, K){
  groups <- rep(1:K, times=ceiling(n/K))[1:n]
  groups <- sample(groups, n, FALSE)
  return(groups)
}

run_one_sim_case_data <- function(k, XX, yy, groups, cv_type,
                                  dn, score,
                                  conf_level, interval, method_names, custom_data_name,
                                  fit_func, pred_func,
                                  verbose){
  # Partition data
  #K <- length(unique(groups))
  if(cv_type == "cv"){
    indx <- which(groups == k)
    mk <- length(indx)
  }
  if(cv_type == "boot"){
    indx <- groups[[kk]]
    mk <- length(unique(indx))
  }

  X_test <- XX[indx,]
  y_test <- yy[indx]
  X_train <- XX[-unique(indx),]
  y_train <- yy[-unique(indx)]
  n <- n_test <- nrow(X_test)
  p <- ncol(X_test)

  # Fit models
  for(ii in seq_along(fit_func)){
    my_method <- ifelse(is.null(method_names[ii]), paste0("method", ii), method_names[ii])
    #browser()
    if(grep("custom", dn)){
      cdn <- custom_data_name
    }else{
      cdn <- dn
    }
    DF_curr <- data.frame(method=my_method,
                          dname=cdn, input_dim=p, n=n,
                          fold=k, fold_size=mk)

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
    # # RMSE and coverage
    # if(interval == TRUE){
    #   rmse_curr <- rmsef(y_test, preds[,1])
    #   DF_curr$RMSE <- rmse_curr
    #   DF_curr$FVU  <- rmse_curr^2/var(y_test)
    #
    #   # Compute empirical coverage for each value in conf_level
    #   n_conf <- length(conf_level)
    #   nms <- names(DF_curr)
    #   for(iii in 1:n_conf){
    #     DF_curr[,ncol(DF_curr)+1] <- mean((y_test >= preds[,2*iii]) * (y_test <= preds[,2*iii+1]))
    #   }
    #   colnames(DF_curr) <- c(nms, lapply(as.character(conf_level), function(zz) paste0("CONF", zz)))
    #
    # }else{
    #   rmse_curr <- rmsef(y_test, preds)
    #   DF_curr$RMSE <- rmse_curr
    #   DF_curr$FVU  <- rmse_curr^2/var(y_test)
    # }
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

      # CALCULATE COVERAGES and IS's
      n_conf <- length(conf_level)
      if(n_conf > 0){
        nms <- names(DF_curr)
        #COVERAGES
        for(iii in seq_along(conf_level)){
          alpha_curr <- 1 - conf_level[iii]
          bounds <- apply(preds, 2, quantile, probs=c(alpha_curr/2, 1-alpha_curr/2))

          DF_curr[,ncol(DF_curr)+1] <- mean((y_test >= bounds[1,]) * (y_test <= bounds[2,]))
        }

        # INTERVAL SCORES
        for(iii in seq_along(conf_level)){
          alpha_curr <- 1 - conf_level[iii]
          bounds <- apply(preds, 2, quantile, probs=c(alpha_curr/2, 1-alpha_curr/2))

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
        #COVERAGES
        for(iii in seq_along(conf_level)){
          alpha_curr <- 1 - conf_level[iii]
          bounds <- apply(preds, 2, quantile, probs=c(alpha_curr/2, 1-alpha_curr/2))

          DF_curr[,ncol(DF_curr)+1] <- mean((y_test >= bounds[1,]) * (y_test <= bounds[2,]))
        }

        # INTERVAL SCORES
        for(iii in seq_along(conf_level)){
          alpha_curr <- 1 - conf_level[iii]
          bounds <- apply(preds, 2, quantile, probs=c(alpha_curr/2, 1-alpha_curr/2))

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
    #browser()
    if(ii == 1){
      DF_res <- DF_curr
    }else{
      DF_res <- rbind(DF_res, DF_curr)
    }
  }# End loop over methods
  return(DF_res)
}



