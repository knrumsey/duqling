#' Reproducible Simulation Study
#'
#' Reproducible code for simulation studies.
#'
#' @param my_fit If \code{my_pred} is specified, then \code{my_fit} should take two arguments called \code{X_train} and \code{y_train}, and should return a fitted model object which will be passed to \code{my_pred}. Otherwise, \code{my_fit} should take a third and fourth argument, \code{X_test, conf_level}, and should return predictions as specified below.
#' @param my_pred A function taking three arguments, the object returned by \code{my_fit}, \code{X_test} and a confidence level \code{alpha} (if coverage is a desired metric, \code{interval = TRUE}). If coverage is to be computed, \code{my_pred} should return a matrix with columns names c("preds", "lb", "ub"). Otherwise, it should return a vector of predictions.
#' @param dnames A vector of dataset names from the \code{duqling} package. See \code{data_quack()} for details.
#' @param interval Do we track interval estimates (or just point estimates)?
#' @param folds Number of folds in cross validation. Can be a scalar or a vector (with \code{length(folds) == length(dnames)}).
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
#' run_sim_study_data(my_fit, my_pred,
#'    dnames=c("pbx9501_gold", "strontium_plume_p104"),
#'    folds=c(5, 10))
run_sim_study_data <- function(my_fit, my_pred=NULL,
                          dnames=c("strontium_plume_p104", "pbx9501_gold"),
                          interval=TRUE,
                          folds=20,
                          replications = 1,
                          seed = 42,
                          conf_level = 0.95,
                          method_names=NULL,
                          mc_cores=1,
                          verbose=TRUE){

  # error handling here
  if(is.null(method_names)){
    method_names <- names(my_fit)
  }
  if(length(folds) == 1){
    folds <- rep(folds, length(dnames))
  }

  DF_full <- NULL
  for(dd in seq_along(dnames)){
    dn <- dnames[dd]
    dnum <- str2num(dn)
    set.seed(seed + str2num(dn))
    if(verbose) cat("Starting dataset ", dd, "/", length(dnames), ": ", dn, "\n", sep="")
    curr <- get_emulation_data(dn)
    X <- curr$X
    y <- curr$y
    n <- nrow(X)
    p <- ncol(X)
    # Get CV information
    K <- folds[dd]
    groups <- k.chunks(n, K)
    if(mc_cores == 1){
      results <- lapply(X=1:K, FUN=run_one_sim_case_data,
                        XX=X, yy=y, groups=groups,
                        dn=dn,
                        conf_level=conf_level, interval=interval,
                        method_names=method_names,
                        my_fit=my_fit, my_pred=my_pred, verbose=verbose)
    }else{
      results <- parallel::mclapply(1:K, run_one_sim_case_data,
                                    XX=X, yy=y, groups=groups,
                                    dn=dn,
                                    conf_level=conf_level, interval=interval,
                                    method_names=method_names,
                                    my_fit=my_fit, my_pred=my_pred, verbose=verbose,
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

run_one_sim_case_data <- function(k, XX, yy, groups,
                                  dn,
                                  conf_level, interval, method_names,
                                  my_fit, my_pred,
                                  verbose){
  # Partition data
  #K <- length(unique(groups))
  indx <- which(groups == k)
  mk <- length(indx)
  X_test <- XX[indx,]
  y_test <- yy[indx]
  X_train <- XX[-indx,]
  y_train <- yy[-indx]
  n <- nrow(X_test)
  p <- ncol(X_test)

  # Fit models
  for(ii in seq_along(my_fit)){
    my_method <- ifelse(is.null(method_names[ii]), paste0("method", ii), method_names[ii])
    #browser()
    DF_curr <- data.frame(method=my_method,
                          dname=dn, input_dim=p, n=n,
                          fold=k, fold_size=mk)

    my_fit_curr <- ifelse(is.function(my_fit), my_fit, my_fit[[ii]])
    if(is.function(my_pred)){
      my_pred_curr <- my_pred
    }else{
      if(is.null(my_pred[[ii]])){
        my_pred_curr <- NULL
      }else{
        my_pred_curr <- my_pred[[ii]]
      }
    }
    #browser()
    # Call my_fit()
    if(is.null(my_pred_curr)){
      tictoc::tic()
      preds <- my_fit_curr(X_train, y_train, X_test, conf_level)
      t_tot <- tictoc::toc(quiet=!verbose)

      DF_curr$t_tot <- t_tot$toc - t_tot$tic
    }else{
      tictoc::tic()
      fitted_object <- my_fit_curr(X_train, y_train)
      t_fit <- tictoc::toc(quiet=!verbose)

      tictoc::tic()
      preds <- my_pred_curr(fitted_object, X_test, conf_level)
      t_pred <- tictoc::toc(quiet=!verbose)

      DF_curr$t_fit <- t_fit$toc - t_fit$tic
      DF_curr$t_pred <- t_pred$toc - t_pred$tic
      DF_curr$t_tot <- DF_curr$t_fit + DF_curr$t_pred
    }

    #browser()
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
    #browser()
    if(ii == 1){
      DF_res <- DF_curr
    }else{
      DF_res <- rbind(DF_res, DF_curr)
    }
  }# End loop over methods
  return(DF_res)
}



