#' Quack (Gives Information about functions in this package)
#'
#' Returns information about the functions found in this package.
#'
#' @param fname A string, the name of a function in this package
#' @param input_dim a vector specifying the input dimension of the function
#' @param input_cat logical, should functions with categorical inputs be included?
#' @param response_type a string in the set c("all", "uni", "multi", "func") specifying which response type is requested.
#' @param stochastic logical. Is function response stochastic?
#' @param sorted Should results be sorted (by input dimension and then alphabetically)
#' @return See details
#' @details If fname is specified, this function returns a list with the number of input dimensions and a p x 2 matrix of input ranges. If fname is not specified, then `quackquack` returns a list of function names which satisfy the requirements specified by the other inputs. If no arguments are specified, then a list of all functions is returned.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#' @export
#' @examples
#' quack("borehole")
#'
#' quack(input_dim=c(1, 2), input_cat = FALSE, response_type = "uni")
quack <- function(fname=NULL, input_dim=NULL, input_cat=NULL, response_type=NULL, stochastic=NULL, sorted=TRUE){
  quackquack(fname, input_dim, input_cat, response_type, stochastic, sorted)
}


quackquack <- function(fname=NULL, input_dim=NULL, input_cat=NULL, response_type=NULL, stochastic=NULL, sorted=TRUE){
  tmp_fname <- fname
  tmp_input_dim <- input_dim
  tmp_input_cat <- input_cat
  tmp_response_type <- response_type
  tmp_stochastic <- stochastic

  if(!is.null(tmp_fname)){
    # Check to see that function exists
    if(tmp_fname %in% quack()$fname){
      fff <- get(paste0("quackquack_", fname))
      return(fff())
    }else{
      stop("fname not recognized. Use quack() for full list of functions.")
    }
  }


  # CREATE MASTER LIST OF FUNCTIONS
  # add function borehole
  master_list <- data.frame(fname="borehole", input_dim=8, input_cat=FALSE, response_type="uni", stochastic="n")
  # add function borehole_low_fidelity
  new_func <- data.frame(fname="borehole_low_fidelity", input_dim=8, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add functions dms
  new_func <- data.frame(fname=c("dms_simple", "dms_radial", "dms_harmonic", "dms_additive", "dms_complicated"),
                         input_dim=rep(2, 5), input_cat=rep(FALSE, 5), response_type=rep("uni", 5), stochastic=rep("n", 5))
  master_list <- rbind(master_list, new_func)
  # add function piston
  new_func <- data.frame(fname="piston", input_dim=7, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add function stochastic_piston
  new_func <- data.frame(fname="stochastic_piston", input_dim=5, input_cat=FALSE, response_type="uni", stochastic="y")
  master_list <- rbind(master_list, new_func)
  # add pollutant functions
  new_func <- data.frame(fname=c("pollutant", "pollutant_uni"), input_dim=c(4,4), input_cat=c(FALSE, FALSE), response_type=c("func", "uni"), stochastic=c("n", "n"))
  master_list <- rbind(master_list, new_func)
  # add grlee functions
  new_func <- data.frame(fname=c("grlee1", "grlee2", "grlee6"), input_dim=c(1,2,6), input_cat=rep(FALSE,3), response_type=rep("uni",3), stochastic=rep("n", 3))
  master_list <- rbind(master_list, new_func)
  # add lim functions
  new_func <- data.frame(fname=c("lim_polynomial", "lim_non_polynomial"), input_dim=c(2,2), input_cat=c(FALSE, FALSE), response_type=c("uni", "uni"), stochastic=c("n", "n"))
  master_list <- rbind(master_list, new_func)
  # add ripples function
  new_func <- data.frame(fname="ripples", input_dim=2, input_cat = FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add twin_galaxies function
  new_func <- data.frame(fname="twin_galaxies", input_dim=2, input_cat = FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add simple machine functions
  new_func <- data.frame(fname=c("simple_machine", "simple_machine_cm"), input_dim=c(3, 5), input_cat = rep(FALSE,2), response_type=rep("func",2), stochastic=c("n", "n"))
  master_list <- rbind(master_list, new_func)
  # add ocean circ
  new_func <- data.frame(fname="ocean_circ", input_dim=4, input_cat = FALSE, response_type="uni", stochastic="y")
  master_list <- rbind(master_list, new_func)
  # add squiggle function
  new_func <- data.frame(fname="squiggle", input_dim=2, input_cat=FALSE, response_type="uni", stochastic = "n")
  master_list <- rbind(master_list, new_func)
  # add friedman function
  new_func <- data.frame(fname="friedman", input_dim=5, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  new_func <- data.frame(fname="friedman10", input_dim=10, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  new_func <- data.frame(fname="friedman20", input_dim=20, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add sirs model
  new_func <- data.frame(fname="dts_sirs", input_dim=9, input_cat=FALSE, response_type="func", stochastic="y")
  master_list <- rbind(master_list, new_func)
  # add detpep functions
  new_func <- data.frame(fname=c("detpep_curve", "detpep8", "welch20"),
                         input_dim=c(3, 8, 20), input_cat=c(FALSE, FALSE, FALSE),
                         response_type=rep("uni", 3), stochastic=rep("n", 3))
  master_list <- rbind(master_list, new_func)
  # add circuit model
  new_func <- data.frame(fname="circuit", input_dim=6, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add robot model
  new_func <- data.frame(fname="robot", input_dim=8, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add wingweight model
  new_func <- data.frame(fname="wingweight", input_dim=10, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add constant functions
  new_func <- data.frame(fname=c("const_fn", "const_fn3", "const_fn15"),
                         input_dim=c(1, 3, 15), input_cat=c(FALSE, FALSE, FALSE),
                         response_type=rep("uni", 3), stochastic=rep("n", 3))
  master_list <- rbind(master_list, new_func)
  # add michalewicz model
  new_func <- data.frame(fname="multivalley", input_dim=2, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add vinet eos model
  new_func <- data.frame(fname="vinet", input_dim=3, input_cat=FALSE, response_type="func", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add sharkfin
  new_func <- data.frame(fname="sharkfin", input_dim=3, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add foursquare
  new_func <- data.frame(fname="foursquare", input_dim=2, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add Rosenbrocks banana
  new_func <- data.frame(fname="banana", input_dim=2, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add ishigami function
  new_func <- data.frame(fname="ishigami", input_dim=3, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add simple poly
  new_func <- data.frame(fname="simple_poly", input_dim=2, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add cube functions
  new_func <- rbind(data.frame(fname="cube3", input_dim=3, input_cat=FALSE, response_type="uni", stochastic="n"),
                    data.frame(fname="cube3_rotate", input_dim=3, input_cat=FALSE, response_type="uni", stochastic="n"),
                    data.frame(fname="cube5", input_dim=5, input_cat=FALSE, response_type="uni", stochastic="n"))
  master_list <- rbind(master_list, new_func)
  # add mock-ignition function
  new_func <- data.frame(fname="ignition", input_dim=10, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # Add beam-deflection function
  new_func <- data.frame(fname="beam_deflection", input_dim=5, input_cat=FALSE, response_type="func", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # Add crater function
  new_func <- data.frame(fname="crater", input_dim=7, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # Add cantilever functions
  new_func <- rbind(data.frame(fname="cantilever_D", input_dim=6, input_cat=FALSE, response_type="uni", stochastic="n"),
                    data.frame(fname="cantilever_S", input_dim=6, input_cat=FALSE, response_type="uni", stochastic="n"))
  master_list <- rbind(master_list, new_func)
  # Add short and steel column
  new_func <- data.frame(fname="steel_column", input_dim=9, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  new_func <- data.frame(fname="short_column", input_dim=5, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # Add sulfur
  new_func <- data.frame(fname="sulfur", input_dim=9, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add G functions
  new_func <- rbind(data.frame(fname="Gfunction", input_dim=3, input_cat=FALSE, response_type="uni", stochastic="n"),
                    data.frame(fname="Gfunction6", input_dim=6, input_cat=FALSE, response_type="uni", stochastic="n"),
                    data.frame(fname="Gfunction12", input_dim=12, input_cat=FALSE, response_type="uni", stochastic="n"),
                    data.frame(fname="Gfunction18", input_dim=18, input_cat=FALSE, response_type="uni", stochastic="n"))
  master_list <- rbind(master_list, new_func)
  # Add park functions
  new_func <- data.frame(fname="park4", input_dim=4, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add function borehole_low_fidelity
  new_func <- data.frame(fname="park4_low_fidelity", input_dim=4, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add 100D function
  new_func <- data.frame(fname="onehundred", input_dim=100, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add ebola function
  new_func <- data.frame(fname="ebola", input_dim=8, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add star2 function
  new_func <- data.frame(fname="star2", input_dim=2, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add forrester functions
  new_func <- data.frame(fname="forrester1", input_dim=1, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  new_func <- data.frame(fname="forrester1_low_fidelity", input_dim=1, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # Add Oakley and O'hagan function
  new_func <- data.frame(fname="oo15", input_dim=15, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # Add rabbits
  new_func <- data.frame(fname="rabbits", input_dim=3, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # Add permdb
  new_func <- data.frame(fname="gamma_mix", input_dim=7, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # Add permdb
  new_func <- data.frame(fname="permdb", input_dim=16, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)

  # Sort list
  if(sorted){
    master_list <- master_list[order(master_list$fname),]
    master_list <- master_list[order(master_list$input_dim),]
    rownames(master_list) <- 1:nrow(master_list)
  }

  # Not supported anymore
  ## Add noise levels
  # recompute_noise <- FALSE
  # if(recompute_noise){
  #   tab <- master_list
  #   Nsims <- 1000
  #   X <- lhs::randomLHS(Nsims, 20)
  #   noise_vec <- rep(NA, nrow(tab))
  #   for(i in 1:nrow(tab)){
  #     fname <- tab$fname[i]
  #     ff <- get(fname, envir=asNamespace("duqling"))
  #     tmp <- get(paste0("quackquack_", fname), envir=asNamespace("duqling"))
  #     if(tmp()$response_type == "uni"){
  #       XX <- X[,1:tmp()$input_dim, drop=FALSE]
  #       y <- apply(XX, 1, ff, scale01=TRUE)
  #       noise_vec[i] <- sd(y)
  #     }
  #   }
  #   master_list$noise <- round(noise_vec, 4)
  #   for(i in 1:length(noise_vec)) cat(noise_vec[i], ", ")
  #   return(TRUE)
  # }else{
  #   noise_vec <- c(0 , 1.305875 , 360.2585 , 1.006089 , 1.00005 , 0.9132416 , 1.009811 , 0.9707915 , 0.3843342 , 0.08473954 , 1.91615 , 1.633466 , 0.354706 , 0.0797161 , 0.4775317 , 0.5098403 , 1.011876 , 0 , 35.84058 , 3.721238 , 0.2873336 , NA , NA , 13.5659 , NA , 0.8257302 , 4.857915 , NA , 0.1355501 , 1.174008 , 0.7268092 , 0.1357079 , 112.6912 , 89.67627 , 36.31331 , 0.5275391 , NA , 4.857915 , 46.78881 , 0 , 4.857915 , 2.089928)
  #   master_list$noise <- round(noise_vec, 4)
  # }


  return_list <- master_list
  #PROCESS FUNCTION DATA FRAME BASED ON REQUIREMENTS
  if(!is.null(tmp_input_dim)){
    tmp <- tmp_input_dim
    return_list <- subset(return_list, subset=input_dim %in% tmp)
  }

  if(!is.null(tmp_input_cat)){
    tmp <- tmp_input_cat
    return_list <- subset(return_list, subset=input_cat == tmp)
  }

  if(!is.null(tmp_response_type)){
    tmp <- tmp_response_type
    return_list <- subset(return_list, subset=response_type %in% tmp)
  }

  if(!is.null(tmp_stochastic)){
    tmp <- c(tmp_stochastic, "y/n")
    return_list <- subset(return_list, subset=stochastic %in% tmp)
  }
  if(nrow(return_list) == 0){
    return("No functions found meeting the criteria")
  }else{
    return(return_list)
  }
}








