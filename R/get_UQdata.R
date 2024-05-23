#' Get UQ data
#'
#' Load in UQ datasets from the UQdataverse (harvard)
#'
#' @param name String. Name of dataset (see documentation)
#' @param control A list of control variables (currently not used)
#' @return Data frame
#' @details Wrapper to read in various UQ datasets (from harvard dataverse)
#' \itemize{
#'  \item{"strontium_plume"}{Input dimension = 20, observations = 300}
#'  \item{"parameter 2"}{Stuff}
#' }
#' @export
#' @examples
#' data <- get_UQdata("strontium_plume")
get_UQdata <- function(dname, control=list()){
  if(dname == "strontium_plume"){
    url <- "https://dataverse.harvard.edu/api/access/datafile/10210723"
    data <- read.table(url, header=TRUE)
    return(data)
  }

  if(dname == "pbx9501"){
    url <- "https://dataverse.harvard.edu/api/access/datafile/10223920"
    data <- read.table(url, header=TRUE)
    return(data)
  }
  warning("Found no dataset by that name")
  return(FALSE)
}

#' Get UQ data for emulation
#'
#' Load in UQ datasets from the UQdataverse (harvard)
#'
#' @param name String. Name of dataset (see documentation)
#' @return A list with elements `y` (a univariate response) and `X` (a matrix of inputs).
#' @details Wrapper to read in various UQ datasets (from harvard dataverse)
#' #' @details Wrapper to read in various UQ datasets (from harvard dataverse)
#' \itemize{
#'  \item{"strontium_plume_"}{Input dimension = 20, observations = 300}
#'  \item{"parameter 2"}{Stuff}
#' }
#' @export
#' @examples
#' get_emulation_data
get_emulation_data <- function(dname){
  res <- list()
  if(dname == "strontium_plume_p4b"){
    tmp <- get_UQdata("strontium_plume")
    res$X <- tmp[,1:20]
    res$y <- tmp$p4b
    return(res)
  }
  if(dname == "strontium_plume_p104"){
    tmp <- get_UQdata("strontium_plume")
    res$X <- tmp[,1:20]
    res$y <- tmp$p104
    return(res)
  }
  if(dname == "pbx9501_gold"){
    tmp <- get_UQdata("pbx9501")
    tmp <- subset(tmp, material == "gold")
    res$X <- tmp[,c(11,14,15,12,13,16)]
    res$y <- tmp$v5
    return(res)
  }
  if(dname == "pbx9501_ss304"){
    tmp <- get_UQdata("pbx9501")
    tmp <- subset(tmp, material == "ss304")
    res$X <- tmp[,c(11,14,15,12,13,16)]
    res$y <- tmp$v5
    return(res)
  }
  if(dname == "pbx9501_nickel"){
    tmp <- get_UQdata("pbx9501")
    tmp <- subset(tmp, material == "nickel")
    res$X <- tmp[,c(11,14,15,12,13,16)]
    res$y <- tmp$v5
    return(res)
  }
  if(dname == "pbx9501_uranium"){
    tmp <- get_UQdata("pbx9501")
    tmp <- subset(tmp, material == "uranium")
    res$X <- tmp[,c(11,14,15,12,13,16)]
    res$y <- tmp$v5
    return(res)
  }

  warning("Found no dataset by that name")
  return(FALSE)
}


#' Get UQ data for emulation
#'
#' Load in UQ datasets from the UQdataverse (harvard)
#'
#' @param raw Logical. When TRUE, full dataset information is given. Otherwise, returns list of scalar-response datasets for emulation.
#' @param dname Vector of function names to filter.
#' @param input_dim Vector of input dimensions (quantitative only) to filter.
#' @param input_cat_dim Vector of input dimensions (categorical only) to filter.
#' @param n Vector of sample sizes to filter.
#' @param output_dim Vector of output dimensions to filter.
#' @return A table with data set names meeting the filter requirements
#' @details Wrapper to read in various UQ datasets (from harvard dataverse)
#' @export
#' @examples
#' tab <- data_quack(raw=TRUE)
#' get_UQdata(tab$dname[1])
#'
#' tab <- data_quack(raw=FALSE)
#' get_emulation_data(tab$dname[1])
data_quack <- function(raw=TRUE, dname=NULL, input_dim=NULL, input_cat_dim=NULL, n=NULL, output_dim=NULL){
  if(raw){
    data_quack_raw(dname)
  }else{
    data_quack_emulator(dname)
  }
}

data_quack_emulator <- function(dname, input_dim=NULL, input_cat_dim=NULL, n=NULL){
  tab <- NULL

  tmp <- data.frame(dname="strontium_plume_p4b", input_dim=20, input_cat_dim=0, n=300)
  tab <- rbind(tab, tmp)
  tmp <- data.frame(dname="strontium_plume_p104", input_dim=20, input_cat_dim=0, n=300)
  tab <- rbind(tab, tmp)

  tmp <- data.frame(dname="pbx9501_gold", input_dim=6, input_cat_dim=0, n=500)
  tab <- rbind(tab, tmp)
  tmp <- data.frame(dname="pbx9501_ss304", input_dim=6, input_cat_dim=0, n=500)
  tab <- rbind(tab, tmp)
  tmp <- data.frame(dname="pbx9501_nickel", input_dim=6, input_cat_dim=0, n=500)
  tab <- rbind(tab, tmp)
  tmp <- data.frame(dname="pbx9501_uranium", input_dim=6, input_cat_dim=0, n=500)
  tab <- rbind(tab, tmp)

  # Subset the master table
  res <- tab
  if(!is.null(dname)){
    tmp <- dname
    res <- subset(res, res$dname %in% tmp)
  }
  if(!is.null(input_dim)){
    tmp <- input_dim
    res <- subset(res, res$input_dim %in% tmp)
  }
  if(!is.null(input_cat_dim)){
    tmp <- input_cat_dim
    res <- subset(res, res$input_cat_dim %in% tmp)
  }
  if(!is.null(n)){
    tmp <- n
    res <- subset(res, res$n %in% tmp)
  }
  return(res)
}


data_quack_raw <- function(dname=NULL, input_dim=NULL, output_dim=NULL, n=NULL, input_cat_dim=NULL){
  tab <- NULL

  tmp <- data.frame(dname="strontium_plume", input_dim=20, output_dim=10, n=300, input_cat_dim=0)
  tab <- rbind(tab, tmp)

  tmp <- data.frame(dname="pbx9501", input_dim=6, output_dim=10, n=7000, input_cat_dim=1)
  tab <- rbind(tab, tmp)


  # Subset the master table
  res <- tab
  if(!is.null(dname)){
    tmp <- dname
    res <- subset(res, res$dname %in% tmp)
  }
  if(!is.null(input_dim)){
    tmp <- input_dim
    res <- subset(res, res$input_dim %in% tmp)
  }
  if(!is.null(input_cat_dim)){
    tmp <- input_cat_dim
    res <- subset(res, res$input_cat_dim %in% tmp)
  }
  if(!is.null(output_dim)){
    tmp <- output_dim
    res <- subset(res, res$output_dim %in% tmp)
  }
  if(!is.null(n)){
    tmp <- n
    res <- subset(res, res$n %in% tmp)
  }
  return(res)
}





