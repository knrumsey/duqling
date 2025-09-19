#' Quack (Gives Information about functions in this package)
#'
#' Returns information about the functions found in this package.
#'
#' @param fname A string, the name of a function in this package
#' @param input_dim a vector specifying the input dimension of the function
#' @param has_categorical logical, should functions with categorical inputs be included?
#' @param response a string in the set c("all", "univariate", "multivariate", "functional") specifying which response type is requested.
#' @param stochastic logical. Is function response stochastic?
#' @param sorted Should results be sorted (by input dimension and then alphabetically)
#' @param show_sigma Should we try to retrieve the variance of the function response across its default input range? Not available for all functions. (Not actively supported, use with caution).
#' @return See details
#' @details If fname is specified, this function returns a list with the number of input dimensions and a p x 2 matrix of input ranges. If fname is not specified, then `quackquack` returns a list of function names which satisfy the requirements specified by the other inputs. If no arguments are specified, then a list of all functions is returned.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#' @export
#' @examples
#' quack("borehole")
#'
#' quack(input_dim=c(1, 2), has_categorical = FALSE, response = "univariate")
quack <- function(fname=NULL, input_dim=NULL, response=NULL, stochastic=NULL, has_categorical=NULL, sorted=TRUE, show_sigma=FALSE, ...){
  quackquack2(fname, input_dim, response, stochastic, has_categorical, sorted, show_sigma, ...)
}



#' Not ready to delete this just yet, but I made a much cleaner version of it. On a trial period.
# quackquack <- function(fname=NULL, input_dim=NULL, has_categorical=NULL, response=NULL, stochastic=NULL, sorted=TRUE, ...){
#   dots <- list(...)
#   if (!is.null(dots$response)) {
#     if (is.null(response)) {
#       response <- dots$response
#     }
#     dots$response <- NULL
#   }
#
#   tmp_fname <- fname
#   tmp_input_dim <- input_dim
#   tmp_has_categorical <- has_categorical
#   tmp_response <- response
#   tmp_stochastic <- stochastic
#
#   if(!is.null(tmp_fname)){
#     # Check to see that function exists
#     if(tmp_fname %in% quack()$fname){
#       fff <- get(paste0("quackquack_", fname))
#       return(fff())
#     }else{
#       stop("fname not recognized. Use quack() for full list of functions.")
#     }
#   }
#
#
#   # CREATE MASTER LIST OF FUNCTIONS
#   # add function borehole
#   master_list <- data.frame(fname="borehole", input_dim=8, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   # add function borehole_low_fidelity
#   new_func <- data.frame(fname="borehole_low_fidelity", input_dim=8, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add functions dms
#   new_func <- data.frame(fname=c("dms_simple", "dms_radial", "dms_harmonic", "dms_additive", "dms_complicated"),
#                          input_dim=rep(2, 5), has_categorical=rep(FALSE, 5), response=rep("univariate", 5), stochastic=rep(FALSE, 5))
#   master_list <- rbind(master_list, new_func)
#   # add function piston
#   new_func <- data.frame(fname="piston", input_dim=7, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add function stochastic_piston
#   new_func <- data.frame(fname="stochastic_piston", input_dim=5, has_categorical=FALSE, response="univariate", stochastic=TRUE)
#   master_list <- rbind(master_list, new_func)
#   # add pollutant functions
#   new_func <- data.frame(fname=c("pollutant", "pollutant_uni"), input_dim=c(4,4), has_categorical=c(FALSE, FALSE), response=c("functional", "univariate"), stochastic=c(FALSE, FALSE))
#   master_list <- rbind(master_list, new_func)
#   # add grlee functions
#   new_func <- data.frame(fname=c("grlee1", "grlee2", "grlee6"), input_dim=c(1,2,6), has_categorical=rep(FALSE,3), response=rep("univariate",3), stochastic=rep(FALSE, 3))
#   master_list <- rbind(master_list, new_func)
#   # add lim functions
#   new_func <- data.frame(fname=c("lim_polynomial", "lim_non_polynomial"), input_dim=c(2,2), has_categorical=c(FALSE, FALSE), response=c("univariate", "univariate"), stochastic=c(FALSE, FALSE))
#   master_list <- rbind(master_list, new_func)
#   # add ripples function
#   new_func <- data.frame(fname="ripples", input_dim=2, has_categorical = FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add twin_galaxies function
#   new_func <- data.frame(fname="twin_galaxies", input_dim=2, has_categorical = FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add simple machine functions
#   new_func <- data.frame(fname=c("simple_machine", "simple_machine_cm"), input_dim=c(3, 5), has_categorical = rep(FALSE,2), response=rep("functional",2), stochastic=c(FALSE, FALSE))
#   master_list <- rbind(master_list, new_func)
#   # add ocean circ
#   new_func <- data.frame(fname="ocean_circ", input_dim=4, has_categorical = FALSE, response="univariate", stochastic=TRUE)
#   master_list <- rbind(master_list, new_func)
#   # add squiggle function
#   new_func <- data.frame(fname="squiggle", input_dim=2, has_categorical=FALSE, response="univariate", stochastic = FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add friedman function
#   new_func <- data.frame(fname="friedman", input_dim=5, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   new_func <- data.frame(fname="friedman10", input_dim=10, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   new_func <- data.frame(fname="friedman20", input_dim=20, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add sirs model
#   new_func <- data.frame(fname="dts_sirs", input_dim=9, has_categorical=FALSE, response="functional", stochastic=TRUE)
#   master_list <- rbind(master_list, new_func)
#   # add detpep functions
#   new_func <- data.frame(fname=c("detpep_curve", "detpep8", "welch20"),
#                          input_dim=c(3, 8, 20), has_categorical=c(FALSE, FALSE, FALSE),
#                          response=rep("univariate", 3), stochastic=rep(FALSE, 3))
#   master_list <- rbind(master_list, new_func)
#   # add circuit model
#   new_func <- data.frame(fname="circuit", input_dim=6, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add robot model
#   new_func <- data.frame(fname="robot", input_dim=8, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add wingweight model
#   new_func <- data.frame(fname="wingweight", input_dim=10, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add constant functions
#   new_func <- data.frame(fname=c("const_fn", "const_fn3", "const_fn15"),
#                          input_dim=c(1, 3, 15), has_categorical=c(FALSE, FALSE, FALSE),
#                          response=rep("univariate", 3), stochastic=rep(FALSE, 3))
#   master_list <- rbind(master_list, new_func)
#   # add michalewicz model
#   new_func <- data.frame(fname="multivalley", input_dim=2, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add vinet eos model
#   new_func <- data.frame(fname="vinet", input_dim=3, has_categorical=FALSE, response="functional", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add sharkfin
#   new_func <- data.frame(fname="sharkfin", input_dim=3, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add foursquare
#   new_func <- data.frame(fname="foursquare", input_dim=2, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add Rosenbrocks banana
#   new_func <- data.frame(fname="banana", input_dim=2, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add ishigami function
#   new_func <- data.frame(fname="ishigami", input_dim=3, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add simple poly
#   new_func <- data.frame(fname="simple_poly", input_dim=2, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add cube functions
#   new_func <- rbind(data.frame(fname="cube3", input_dim=3, has_categorical=FALSE, response="univariate", stochastic=FALSE),
#                     data.frame(fname="cube3_rotate", input_dim=3, has_categorical=FALSE, response="univariate", stochastic=FALSE),
#                     data.frame(fname="cube5", input_dim=5, has_categorical=FALSE, response="univariate", stochastic=FALSE))
#   master_list <- rbind(master_list, new_func)
#   # add mock-ignition function
#   new_func <- data.frame(fname="ignition", input_dim=10, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # Add beam-deflection function
#   new_func <- data.frame(fname="beam_deflection", input_dim=5, has_categorical=FALSE, response="functional", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # Add crater function
#   new_func <- data.frame(fname="crater", input_dim=7, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # Add cantilever functions
#   new_func <- rbind(data.frame(fname="cantilever_D", input_dim=6, has_categorical=FALSE, response="univariate", stochastic=FALSE),
#                     data.frame(fname="cantilever_S", input_dim=6, has_categorical=FALSE, response="univariate", stochastic=FALSE))
#   master_list <- rbind(master_list, new_func)
#   # Add short and steel column
#   new_func <- data.frame(fname="steel_column", input_dim=9, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   new_func <- data.frame(fname="short_column", input_dim=5, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # Add sulfur
#   new_func <- data.frame(fname="sulfur", input_dim=9, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add G functions
#   new_func <- rbind(data.frame(fname="Gfunction", input_dim=3, has_categorical=FALSE, response="univariate", stochastic=FALSE),
#                     data.frame(fname="Gfunction6", input_dim=6, has_categorical=FALSE, response="univariate", stochastic=FALSE),
#                     data.frame(fname="Gfunction12", input_dim=12, has_categorical=FALSE, response="univariate", stochastic=FALSE),
#                     data.frame(fname="Gfunction18", input_dim=18, has_categorical=FALSE, response="univariate", stochastic=FALSE))
#   master_list <- rbind(master_list, new_func)
#   # Add park functions
#   new_func <- data.frame(fname="park4", input_dim=4, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add function borehole_low_fidelity
#   new_func <- data.frame(fname="park4_low_fidelity", input_dim=4, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add 100D function
#   new_func <- data.frame(fname="onehundred", input_dim=100, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add gradient 100D function
#   new_func <- data.frame(fname="d_onehundred", input_dim=100, has_categorical=FALSE, response="multivariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add ebola function
#   new_func <- data.frame(fname="ebola", input_dim=8, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add star2 function
#   new_func <- data.frame(fname="star2", input_dim=2, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # add forrester functions
#   new_func <- data.frame(fname="forrester1", input_dim=1, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   new_func <- data.frame(fname="forrester1_low_fidelity", input_dim=1, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # Add Oakley and O'hagan function
#   new_func <- data.frame(fname="oo15", input_dim=15, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # Add rabbits
#   new_func <- data.frame(fname="rabbits", input_dim=3, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # Add gamma_mix
#   new_func <- data.frame(fname="gamma_mix", input_dim=7, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # Add permdb
#   new_func <- data.frame(fname="permdb", input_dim=16, has_categorical=FALSE, response="univariate", stochastic=FALSE)
#   master_list <- rbind(master_list, new_func)
#   # Add blackscholes
#   new_func <- data.frame(fname=c("bs_call", "bs_put"), input_dim=c(6,6), has_categorical=c(FALSE, FALSE), response=c("functional", "functional"), stochastic=c(TRUE, TRUE))
#   master_list <- rbind(master_list, new_func)
#
#   # Sort list
#   if(sorted){
#     master_list <- master_list[order(master_list$fname),]
#     master_list <- master_list[order(master_list$input_dim),]
#     rownames(master_list) <- 1:nrow(master_list)
#   }
#
#   # Not supported anymore
#   ## Add noise levels
#   # recompute_noise <- FALSE
#   # if(recompute_noise){
#   #   tab <- master_list
#   #   Nsims <- 1000
#   #   X <- lhs::randomLHS(Nsims, 20)
#   #   noise_vec <- rep(NA, nrow(tab))
#   #   for(i in 1:nrow(tab)){
#   #     fname <- tab$fname[i]
#   #     ff <- get(fname, envir=asNamespace("duqling"))
#   #     tmp <- get(paste0("quackquack_", fname), envir=asNamespace("duqling"))
#   #     if(tmp()$response == "univariate"){
#   #       XX <- X[,1:tmp()$input_dim, drop=FALSE]
#   #       y <- apply(XX, 1, ff, scale01=TRUE)
#   #       noise_vec[i] <- sd(y)
#   #     }
#   #   }
#   #   master_list$noise <- round(noise_vec, 4)
#   #   for(i in 1:length(noise_vec)) cat(noise_vec[i], ", ")
#   #   return(TRUE)
#   # }else{
#   #   noise_vec <- c(0 , 1.305875 , 360.2585 , 1.006089 , 1.00005 , 0.9132416 , 1.009811 , 0.9707915 , 0.3843342 , 0.08473954 , 1.91615 , 1.633466 , 0.354706 , 0.0797161 , 0.4775317 , 0.5098403 , 1.011876 , 0 , 35.84058 , 3.721238 , 0.2873336 , NA , NA , 13.5659 , NA , 0.8257302 , 4.857915 , NA , 0.1355501 , 1.174008 , 0.7268092 , 0.1357079 , 112.6912 , 89.67627 , 36.31331 , 0.5275391 , NA , 4.857915 , 46.78881 , 0 , 4.857915 , 2.089928)
#   #   master_list$noise <- round(noise_vec, 4)
#   # }
#
#
#   return_list <- master_list
#   #PROCESS FUNCTION DATA FRAME BASED ON REQUIREMENTS
#   if(!is.null(tmp_input_dim)){
#     tmp <- tmp_input_dim
#     return_list <- subset(return_list, subset=input_dim %in% tmp)
#   }
#
#   if(!is.null(tmp_has_categorical)){
#     tmp <- tmp_has_categorical
#     return_list <- subset(return_list, subset=has_categorical == tmp)
#   }
#
#   #if(!is.null(tmp_response)){
#   #  tmp <- tmp_response
#   #  return_list <- subset(return_list, subset=response %in% tmp)
#   #}
#
#   if (!is.null(tmp_response)) {
#     tmp <- grep(paste(tmp_response, collapse="|"),
#                 return_list$response,
#                 ignore.case = TRUE, value = TRUE)
#     return_list <- subset(return_list, subset=response %in% tmp)
#   }
#
#   if(!is.null(tmp_stochastic)){
#     tmp <- tmp_stochastic
#     if (is.character(tmp)) {
#       tmp <- (tmp == "y")
#     }
#     return_list <- subset(return_list, subset=stochastic %in% tmp)
#   }
#
#   if(nrow(return_list) == 0){
#     return("No functions found meeting the criteria")
#   }else{
#     return(return_list)
#   }
# }

