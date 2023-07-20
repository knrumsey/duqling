#' Reproducible Simulation Study
#'
#' Reproducible code for simulation studies.
#'
#' @param my_fit If \code{my_pred} is specified, then \code{my_fit} should take two arguments called \code{X_train} and \code{y_train}, and should return an object which will be passed to \code{my_pred}. Otherwise, \code{my_fit} should take a third argument, \code{X_test}, and should return predictions as specified below.
#' @param my_pred A function taking three arguments, the object returned by \code{my_fit}, \code{X_test} and a confidence level \code{alpha} (if coverage is a desired metric, \code{interval = TRUE}). If coverage is to be computed, \code{my_pred} should return a matrix with columns names c("preds", "lb", "ub"). Otherwise, it should rturn a vector of predictions.
#' @param fnames A vector of function names from the \code{duqling} package. See \code{quack()} for details.
#' @param interval Do we track interval estimates (or just point estimates)?
#' @param n_train the sample size (or vector of sample sizes) for each training set
#' @param n_test the sample size for each testing set
#' @param NSR the noise to signal ratio (inverse of the more-standard signal to noise ratio).
#' @param design_type How should the training and testing designs be generated? Options are "LHS", "grid" and "random"
#' @param replications How many replications should be repeated for each data set?
#' @param seed Seed for random number generators. For reproducibility, we discourage the use of this argument.
#' @param alpha Confidence level for interval estimates.
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
#'    fnames=c("dms_additive", "dms_simple"),
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
                          alpha = 0.95,
                          verbose=TRUE){

  # error handling here



  DF_full <- NULL
  for(ff in seq_along(fnames)){
    fn <- fnames[ff]
    p <- quack(fn)$input_dim
    if(verbose) cat("Starting function", fn, "\n")
    for(ii in seq_along(n_train)){
      n <- n_train[ii]
      if(verbose) cat("\t Running all combinations and replications for n =", n, "\n")
      for(jj in seq_along(NSR)){
        for(kk in seq_along(design_type)){
          for(rr in 1:replications){
            # Store case
            DF_curr <- data.frame(fname=fn, input_dim=p,
                                  n=n_train[ii],
                                  NSR=NSR[jj],
                                  design_type=design_type[kk],
                                  rep=rr)


            # Generate training data
            set.seed(seed + rr - 1)
            if(design_type[kk] == "LHS"){
              if(n_train[ii] <= 1200){
                X_train <- lhs::maximinLHS(n, p)
                X_test <- lhs::randomLHS(n_test, p)
              }else{
                X_train <- lhs::randomLHS(n, p)
                X_test <- lhs::randomLHS(n_test, p)
              }
            }else{
              if(design_type[kk] == "random"){
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
            noise_lvl <- sqrt(var(y_train) * NSR[jj])
            y_train <- y_train + rnorm(n, 0, noise_lvl)
            y_test <- apply(X_test, 1, f, scale01=TRUE) # no noise for testing data


            # Call my_fit()
            if(is.null(my_pred)){
              tictoc::tic()
              preds <- my_fit(X_train, y_train, X_test, alpha)
              t_tot <- tictoc::toc()

              DF_curr$t_tot <- t_tot$toc - t_tot$tic
            }else{
              tictoc::tic()
              fitted_object <- my_fit(X_train, y_train)
              t_fit <- tictoc::toc()

              tictoc::tic()
              preds <- my_pred(fitted_object, X_test, alpha)
              t_pred <- tictoc::toc()

              DF_curr$t_fit <- t_fit$toc - t_fit$tic
              DF_curr$t_pred <- t_pred$toc - t_pred$tic
            }

            # RMSE and coverage
            if(interval == TRUE){
              DF_curr$RMSE <- rmsef(y_test, preds[,1])
              DF_curr$COVR <- mean((y_test >= preds[,2]) * (y_test <= preds[,3]))
            }else{
              DF_curr$RMSE <- rmsef(y_test, preds)
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





