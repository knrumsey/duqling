#' Quack (Gives Information about functions in this package)
#'
#' Returns information about the functions found in this package.
#'
#' @param fname A string, the name of a function in this package
#' @param input_dims a vector specifying the input dimension of the function
#' @param input_cat logical, should functions with categorical inputs be included?
#' @param response_type a string in the set c("all", "uni", "multi", "func") specifying which response type is requested.
#' @param stochastic logical. Is function response stochastic?
#' @return See details
#' @details If fname is specified, this function returns a list with the number of input dimensions and a p x 2 matrix of input ranges. If fname is not specified, then `quackquack` returns a list of function names which satisfy the requirements specified by the other inputs. If no arguments are specified, then a list of all functions is returned.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#' @export
#' @examples
#' quack("borehole")
#'
#' quack(input_dims=c(1, 2), input_cat = FALSE, response_type = "uni")
quack <- function(fname=NULL, input_dims=NULL, input_cat=NULL, response_type=NULL, stochastic=NULL){
  quackquack(fname, input_dims, input_cat, response_type, stochastic)
}


quackquack <- function(fname=NULL, input_dims=NULL, input_cat=NULL, response_type=NULL, stochastic=NULL){
  tmp_fname <- fname
  tmp_input_dims <- input_dims
  tmp_input_cat <- input_cat
  tmp_response_type <- response_type
  tmp_stochastic <- stochastic

  if(!is.null(tmp_fname)){
    fff <- get(paste0("quackquack_", fname))
    return(fff())
  }



  # CREATE MASTER LIST OF FUNCTIONS
  # add function borehole
  master_list <- data.frame(fname="borehole", input_dims=8, input_cat=FALSE, response_type="uni", stochastic="n")
  # add function borehole_low_fidelity
  new_func <- data.frame(fname="borehole_low_fidelity", input_dims=8, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add functions dms
  new_func <- data.frame(fname=c("dms_simple", "dms_radial", "dms_harmonic", "dms_additive", "dms_complicated"),
                         input_dims=rep(2, 5), input_cat=rep(FALSE, 5), response_type=rep("uni", 5), stochastic=rep("n", 5))
  master_list <- rbind(master_list, new_func)
  # add function piston
  new_func <- data.frame(fname="piston", input_dims=7, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add function stochastic_piston
  new_func <- data.frame(fname="stochastic_piston", input_dims=5, input_cat=FALSE, response_type="uni", stochastic="y")
  master_list <- rbind(master_list, new_func)
  # add pollutant functions
  new_func <- data.frame(fname=c("pollutant", "pollutant_uni"), input_dims=c(4,4), input_cat=c(FALSE, FALSE), response_type=c("func", "uni"), stochastic=c("n", "n"))
  master_list <- rbind(master_list, new_func)
  # add grlee functions
  new_func <- data.frame(fname=c("grlee1", "grlee2", "grlee4"), input_dims=c(1,2,4), input_cat=rep(FALSE,3), response_type=rep("uni",3), stochastic=rep("n", 3))
  master_list <- rbind(master_list, new_func)
  # add lim functions
  new_func <- data.frame(fname=c("lim_polynomial", "lim_non_polynomial"), input_dims=c(2,2), input_cat=c(FALSE, FALSE), response_type=c("uni", "uni"), stochastic=c("n", "n"))
  master_list <- rbind(master_list, new_func)
  # add ripples function
  new_func <- data.frame(fname="ripples", input_dims=2, input_cat = FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add twin_galaxies function
  new_func <- data.frame(fname="twin_galaxies", input_dims=2, input_cat = FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add simple machine functions
  new_func <- data.frame(fname=c("simple_machine", "simple_machine_cm"), input_dims=c(3, 5), input_cat = rep(FALSE,2), response_type=rep("func",2), stochastic=c("n", "n"))
  master_list <- rbind(master_list, new_func)
  # add ocean circ
  new_func <- data.frame(fname="ocean_circ", input_dims=4, input_cat = FALSE, response_type="uni", stochastic="y")
  master_list <- rbind(master_list, new_func)
  # add squiggle function
  new_func <- data.frame(fname="squiggle", input_dims=2, input_cat=FALSE, response_type="uni", stochastic = "n")
  master_list <- rbind(master_list, new_func)
  # add friedman function
  new_func <- data.frame(fname="friedman", input_dims=5, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  new_func <- data.frame(fname="friedman10", input_dims=10, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  new_func <- data.frame(fname="friedman20", input_dims=20, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add sirs model
  new_func <- data.frame(fname="dts_sirs", input_dims=9, input_cat=FALSE, response_type="func", stochastic="y")
  master_list <- rbind(master_list, new_func)
  # add detpep functions
  new_func <- data.frame(fname=c("detpep_curve", "detpep8", "welch20"),
                         input_dims=c(3, 8, 20), input_cat=c(FALSE, FALSE, FALSE),
                         response_type=rep("uni", 3), stochastic=rep("n", 3))
  master_list <- rbind(master_list, new_func)
  # add circuit model
  new_func <- data.frame(fname="circuit", input_dims=6, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add robot model
  new_func <- data.frame(fname="robot", input_dims=8, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add wingweight model
  new_func <- data.frame(fname="wingweight", input_dims=10, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add constant functions
  new_func <- data.frame(fname=c("const_fn", "const_fn3", "const_fn15"),
                         input_dims=c(1, 3, 15), input_cat=c(FALSE, FALSE, FALSE),
                         response_type=rep("uni", 3), stochastic=rep("n", 3))
  master_list <- rbind(master_list, new_func)
  # add michalewicz model
  new_func <- data.frame(fname="micwicz", input_dims=2, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add vinet eos model
  new_func <- data.frame(fname="vinet", input_dims=3, input_cat=FALSE, response_type="func", stochastic="n")
  master_list <- rbind(master_list, new_func)
  # add sharkfin
  new_func <- data.frame(fname="sharkfin", input_dims=3, input_cat=FALSE, response_type="uni", stochastic="n")
  master_list <- rbind(master_list, new_func)


  #master_list <- master_list[order(master_list$fname),]
  master_list <- master_list[order(master_list$input_dims),]

  return_list <- master_list
  #PROCESS FUNCTION DATA FRAME BASED ON REQUIREMENTS
  if(!is.null(tmp_input_dims)){
    tmp <- tmp_input_dims
    return_list <- subset(return_list, subset=input_dims %in% tmp)
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


