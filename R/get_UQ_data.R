#' Get UQ data
#'
#' Load in UQ datasets from the UQdataverse (harvard)
#'
#' @param dname String. Name of dataset (see documentation)
#' @param control A list of control variables (currently not used)
#' @return Data frame
#' @details Wrapper to read in various UQ datasets (from harvard dataverse)
#' \itemize{
#'  \item{"strontium_plume"}{Input dimension = 20, observations = 300}
#'  \item{"parameter 2"}{Stuff}
#' }
#' @export
#' @examples
#' data <- get_UQ_data("strontium_plume")
get_UQ_data <- function(dname, control=list()){
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
  if(dname == "stochastic_sir"){
    url <- "https://dataverse.harvard.edu/api/access/datafile/10228544"
    data <- read.table(url, header=TRUE)
    return(data)
  }
  if(dname == "e3sm"){
    url <- "https://dataverse.harvard.edu/api/access/datafile/10232219"
    data <- read.table(url, header=TRUE)
    return(data)
  }
  if(dname == "fair_climate"){
    url <- "https://dataverse.harvard.edu/api/access/datafile/10243053"
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
#' @param dname String. Name of dataset (see documentation)
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
    tmp <- get_UQ_data("strontium_plume")
    res$X <- tmp[,1:20]
    res$y <- tmp$p4b
    return(res)
  }
  if(dname == "strontium_plume_p104"){
    tmp <- get_UQ_data("strontium_plume")
    res$X <- tmp[,1:20]
    res$y <- tmp$p104
    return(res)
  }
  if(dname == "pbx9501_gold"){
    tmp <- get_UQ_data("pbx9501")
    tmp <- subset(tmp, material == "gold")
    res$X <- tmp[,c(11,14,15,12,13,16)]
    res$y <- tmp$v5
    return(res)
  }
  if(dname == "pbx9501_ss304"){
    tmp <- get_UQ_data("pbx9501")
    tmp <- subset(tmp, material == "ss304")
    res$X <- tmp[,c(11,14,15,12,13,16)]
    res$y <- tmp$v5
    return(res)
  }
  if(dname == "pbx9501_nickel"){
    tmp <- get_UQ_data("pbx9501")
    tmp <- subset(tmp, material == "nickel")
    res$X <- tmp[,c(11,14,15,12,13,16)]
    res$y <- tmp$v5
    return(res)
  }
  if(dname == "pbx9501_uranium"){
    tmp <- get_UQ_data("pbx9501")
    tmp <- subset(tmp, material == "uranium")
    res$X <- tmp[,c(11,14,15,12,13,16)]
    res$y <- tmp$v5
    return(res)
  }
  if(dname == "stochastic_sir"){
    tmp <- get_UQ_data("stochastic_sir")
    res$X <- tmp[,-1]
    res$y <- tmp[,1]
    return(res)
  }
  if(dname == "e3sm"){
    tmp <- get_UQ_data("e3sm")
    res$X <- tmp[,2:3]
    res$y <- tmp$temperature
  }
  if(dname == "fair_climate_ssp1-2.6"){
    tmp <- get_UQ_data("fair_climate")
    tmp <- subset(tmp, ssp == "1-2.6")
    res$X <- tmp[,2:47]
    res$y <- tmp$temp_anomaly
  }
  if(dname == "fair_climate_ssp2-4.5"){
    tmp <- get_UQ_data("fair_climate")
    tmp <- subset(tmp, ssp == "2-4.5")
    res$X <- tmp[,2:47]
    res$y <- tmp$temp_anomaly
  }
  if(dname == "fair_climate_ssp3-7.0"){
    tmp <- get_UQ_data("fair_climate")
    tmp <- subset(tmp, ssp == "3-7.0")
    res$X <- tmp[,2:47]
    res$y <- tmp$temp_anomaly
  }
  if(dname == "fair_climate_ssp1-2.6_year2200"){
    tmp <- get_UQ_data("fair_climate")
    tmp <- subset(tmp, ssp == "1-2.6" & year == 2200)
    res$X <- tmp[,3:47]
    res$y <- tmp$temp_anomaly
  }
  if(dname == "fair_climate_ssp2-4.5_year2200"){
    tmp <- get_UQ_data("fair_climate")
    tmp <- subset(tmp, ssp == "2-4.5" & year == 2200)
    res$X <- tmp[,3:47]
    res$y <- tmp$temp_anomaly
  }
  if(dname == "fair_climate_ssp3-7.0_year2200"){
    tmp <- get_UQ_data("fair_climate")
    tmp <- subset(tmp, ssp == "3-7.0" & year == 2200)
    res$X <- tmp[,3:47]
    res$y <- tmp$temp_anomaly
  }

  warning("Found no dataset by that name")
  return(FALSE)
}


