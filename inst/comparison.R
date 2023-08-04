library(BASS)
library(GBASS)
library(laGP)
library(leapgp)
library(BART)
library(BayesPPR)

my_fit <- list()

#BART
my_fit_tmp <- function(X_train, y_train, X_test, conf_level=0.95){
  deleteme = capture.output(mod <- wbart(X_train, y_train, X_test))
  pred <- mod$yhat.test
  yhat <- apply(pred, 2, mean)
  sd_post_mean <- apply(pred, 2, sd)
  sd_post_eps  <- mean(mod$sigma)
  sd <- sqrt(sd_post_mean^2 + sd_post_eps^2)
  cbind(yhat,
        yhat - 2*sd,
        yhat + 2*sd)
}
my_fit$BART = my_fit_tmp

# BASS
my_fit_tmp <- function(X, y, X_test, conf_level=0.95){
  obj <- bass(X, y, verbose=FALSE)
  alpha <- 1 - conf_level
  preds <- predict(obj, X_test)
  yhat <- apply(preds, 2, mean)
  sd <- sqrt(apply(preds, 2, var) + mean(obj$s2))
  res <- cbind(yhat,
               yhat-2*sd,
               yhat + 2*sd)
  return(res)
}
my_fit$BASS = my_fit_tmp

# BASSv2
my_fit_tmp <- function(X, y, X_test, conf_level=0.95){
  obj <- bass(X, y, verbose=FALSE, h1=4, h2=40/nrow(X))
  alpha <- 1 - conf_level
  preds <- predict(obj, X_test)
  yhat <- apply(preds, 2, mean)
  sd <- sqrt(apply(preds, 2, var) + mean(obj$s2))
  res <- cbind(yhat,
               yhat-2*sd,
               yhat + 2*sd)
  return(res)
}
my_fit$BASSv2 = my_fit_tmp



#BPPR
my_fit_tmp <- function(X_train, y_train, X_test, conf_level=0.95){
  mod <- bppr(X_train, y_train, print_every=0)
  pred <- predict(mod, X_test)
  yhat <- apply(pred, 2, mean)
  sd_post_mean <- apply(pred, 2, sd)
  sd_post_eps  <- mean(mod$sd_resid)
  sd <- sqrt(sd_post_mean^2 + sd_post_eps^2)
  cbind(yhat,
        yhat - 2*sd,
        yhat + 2*sd)
}
my_fit$BPPR = my_fit_tmp


results <- run_sim_study(my_fit, fnames=get_sim_functions_medium(),
                         n_train = c(100, 1000), n_test=2500,
                         NSR=0, replications = 10,
                         mc_cores=5)

#save(results, file="data/results.Rda")


# Make figure
library(quack)
library(tidyverse)
library(plyr)
results_gg <- quack::ggsummary(results, measurevar="FVU", groupvars=c("method", "fname"))
ggplot(results_gg, aes(x=fname, y=mean, fill=method)) +
  geom_bar(linewidth=1, stat="identity", col="black", position="dodge") +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), linetype=1, lwd=1, width=0.25, position=position_dodge(0.9))
