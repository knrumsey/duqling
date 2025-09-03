#' Get UQ data
#'
#' Load in UQ datasets from the UQdataverse (harvard)
#'
#' @param dname String. Name of dataset (see documentation)
#' @param control A list of control variables (currently not used)
#' @return Data frame
#' @details Wrapper to read in various UQ datasets (from harvard dataverse)
#' \itemize{
#'  \item{"strontium_plume": Input dimension = 20, observations = 300}
#'  \item{"parameter 2": Stuff}
#' }
#' @export
#' @examples
#' data <- get_UQ_data("strontium_plume")
get_UQ_data <- function(dname, control=list()){
  if(dname == "strontium_plume"){
    url <- "https://dataverse.harvard.edu/api/access/datafile/10210723"
    data <- readr::read_delim(url, delim="\t")
    return(data)
  }
  if(dname == "pbx9501"){
    url <- "https://dataverse.harvard.edu/api/access/datafile/10223920"
    data <- readr::read_delim(url, delim="\t")
    return(data)
  }
  if(dname == "stochastic_sir"){
    url <- "https://dataverse.harvard.edu/api/access/datafile/10228544"
    data <- readr::read_delim(url, delim="\t")
    return(data)
  }
  if(dname == "e3sm"){
    url <- "https://dataverse.harvard.edu/api/access/datafile/10232219"
    data <- readr::read_delim(url, delim="\t")
    return(data)
  }
  if(dname == "fair_climate"){
    url <- "https://dataverse.harvard.edu/api/access/datafile/10243053"
    data <- readr::read_delim(url, delim="\t")
    return(data)
  }
  if(dname == "Z_machine_exp"){
    url <- "https://dataverse.harvard.edu/api/access/datafile/10274145"
    data <- readr::read_table(url, col_types=c("iccdd"))
    return(data)
  }
  if(dname == "Z_machine_sim"){
    url <- "https://dataverse.harvard.edu/api/access/datafile/10274144"
    data <- readr::read_csv(url)
    url2 <- "https://dataverse.harvard.edu/api/access/datafile/10274664"
    params <- readr::read_table(url2)
    obj <- list(data=data, params=params)
    return(obj)
  }
  if(dname == "flyer_plate104"){
    url <- "https://dataverse.harvard.edu/api/access/datafile/10338350"
    inputs <- readr::read_delim(url, delim="\t")
    url <- "https://dataverse.harvard.edu/api/access/datafile/10338343"
    outputs <- readr::read_delim(url, delim="\t")
    colnames(outputs) <- paste0("y", 1:ncol(outputs))
    data <- cbind(inputs, outputs)
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
#' \itemize{
#'  \item {"strontium_plume_": Input dimension = 20, observations = 300}
#'  \item {"parameter 2": Stuff}
#' }
#' @export
#' @examples
#' get_emulation_data
get_emulation_data <- function(dname){
  material <- ssp <- year <- NULL
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
    res$y <- unlist(tmp[,1])
    return(res)
  }
  if(dname == "e3sm"){
    tmp <- get_UQ_data("e3sm")
    res$X <- tmp[,2:3]
    res$y <- tmp$temperature
    return(res)
  }
  if(dname == "e3sm_mcar"){
    tmp <- get_UQ_data("e3sm")
    X <- tmp[,2:3]
    y <- tmp$temperature
    n <- length(y)

    # Take a pseudo-random set of indices
    M <- 11217
    samp <- rep(1, M)
    for(i in 2:M){
      samp[i] <- (1664525 * samp[i-1] + 1013904223) %% (2^32)
    }
    samp <- unique(samp %% n)
    res$X <- X[samp,]
    res$y <- y[samp]
    return(res)
  }
  if(dname == "e3sm_mnar"){
    tmp <- get_UQ_data("e3sm")
    X <- tmp[,2:3]
    y <- tmp$temperature

    # Get pseudorandom numbers
    n <- length(y)
    prob <-rep(0.5, n)
    for(i in 2:n){
      prob[i] <- (1664525 * prob[i-1] + 1013904223) %% (2^32)
    }
    prob <- prob / (2^32)

    # Remove points probabilistically,
    # with a higher probability to remove close to the center location
    center <- c(285, 15)
    lscale <- c(28, 15)
    thresh <- stats::dnorm(unlist(X[,1]), center[1], lscale[1]) * stats::dnorm(unlist(X[,2]), center[2], lscale[2])
    thresh <- thresh/max(thresh) + 4
    thresh <- thresh/max(thresh)
    ind    <- which(thresh > prob)
    res$X <- X[-ind,]
    res$y <- y[-ind]
    return(res)
  }
  if(dname == "fair_climate_ssp1-2.6"){
    tmp <- get_UQ_data("fair_climate")
    tmp <- subset(tmp, ssp == "1-2.6")
    res$X <- tmp[,2:47]
    res$y <- tmp$temp_anomaly
    return(res)
  }
  if(dname == "fair_climate_ssp2-4.5"){
    tmp <- get_UQ_data("fair_climate")
    tmp <- subset(tmp, ssp == "2-4.5")
    res$X <- tmp[,2:47]
    res$y <- tmp$temp_anomaly
    return(res)
  }
  if(dname == "fair_climate_ssp3-7.0"){
    tmp <- get_UQ_data("fair_climate")
    tmp <- subset(tmp, ssp == "3-7.0")
    res$X <- tmp[,2:47]
    res$y <- tmp$temp_anomaly
    return(res)
  }
  if(dname == "fair_climate_ssp1-2.6_year2200"){
    tmp <- get_UQ_data("fair_climate")
    tmp <- subset(tmp, ssp == "1-2.6" & year == 2200)
    res$X <- tmp[,3:47]
    res$y <- tmp$temp_anomaly
    return(res)
  }
  if(dname == "fair_climate_ssp2-4.5_year2200"){
    tmp <- get_UQ_data("fair_climate")
    tmp <- subset(tmp, ssp == "2-4.5" & year == 2200)
    res$X <- tmp[,3:47]
    res$y <- tmp$temp_anomaly
    return(res)
  }
  if(dname == "fair_climate_ssp3-7.0_year2200"){
    tmp <- get_UQ_data("fair_climate")
    tmp <- subset(tmp, ssp == "3-7.0" & year == 2200)
    res$X <- tmp[,3:47]
    res$y <- tmp$temp_anomaly
    return(res)
  }
  if(dname == "Z_machine_max_vel1"){
    tmp <- get_UQ_data("Z_machine_sim")
    res$y <- stats::aggregate(velocity1~id, max, data=tmp$data)$velocity1
    res$X <- as.matrix(tmp$params[,c(2:4, 5+c(0,9,18))])
    return(res)
  }
  if(dname == "Z_machine_max_vel2"){
    tmp <- get_UQ_data("Z_machine_sim")
    res$y <- stats::aggregate(velocity2~id, max, data=tmp$data)$velocity2
    res$X <- as.matrix(tmp$params[,c(2:4, 6+c(0,9,18))])
    return(res)
  }
  if(dname == "Z_machine_max_vel_all"){
    tmp <- get_UQ_data("Z_machine_sim")
    res$y <- stats::aggregate(velocity1~id, max, data=tmp$data)[,2]
    res$y <- res$y + stats::aggregate(velocity2~id, max, data=tmp$data)[,2]
    res$y <- res$y + stats::aggregate(velocity3~id, max, data=tmp$data)[,2]
    res$y <- res$y + stats::aggregate(velocity4~id, max, data=tmp$data)[,2]
    res$y <- res$y + stats::aggregate(velocity5~id, max, data=tmp$data)[,2]
    res$y <- res$y + stats::aggregate(velocity6~id, max, data=tmp$data)[,2]
    res$y <- res$y + stats::aggregate(velocity7~id, max, data=tmp$data)[,2]
    res$y <- res$y + stats::aggregate(velocity8~id, max, data=tmp$data)[,2]
    res$y <- res$y + stats::aggregate(velocity9~id, max, data=tmp$data)[,2]
    res$X <- as.matrix(tmp$param[,-1])
    return(res)
  }
  # SLOSH DATA IS GIVEN AT 3 LOCATIONS
  # Low:  index = 9946 (for original data)
  #       coord = (-74.77213, 39.16568)
  #
  # Mid:  index = 35158 (for original data)
  #       coord = (-74.89213, 39.03168)
  #
  # High: index = 11975 (for original data)
  #       coord = (-74.68413, 39.15668)
  if(dname == "SLOSH_low"){
    url <- "https://dataverse.harvard.edu/api/access/datafile/10400109"
    outputs <- readr::read_delim(url, delim="\t")
    url <- "https://dataverse.harvard.edu/api/access/datafile/10338342"
    inputs <- readr::read_delim(url, delim="\t")
    res$y <- unlist(outputs[,1])
    res$X <- as.matrix(inputs)
    return(res)
  }
  if(dname == "SLOSH_mid"){
    url <- "https://dataverse.harvard.edu/api/access/datafile/10400109"
    outputs <- readr::read_delim(url, delim="\t")
    url <- "https://dataverse.harvard.edu/api/access/datafile/10338342"
    inputs <- readr::read_delim(url, delim="\t")
    res$y <- unlist(outputs[,2])
    res$X <- as.matrix(inputs)
    return(res)
  }
  if(dname == "SLOSH_high"){
    url <- "https://dataverse.harvard.edu/api/access/datafile/10400109"
    outputs <- readr::read_delim(url, delim="\t")
    url <- "https://dataverse.harvard.edu/api/access/datafile/10338342"
    inputs <- readr::read_delim(url, delim="\t")
    res$y <- unlist(outputs[,3])
    res$X <- as.matrix(inputs)
    return(res)
  }
  if(dname == "flyer_plate104"){
    data <- get_UQ_data("flyer_plate104")
    res$y <- data[,11 + 1 + 54]
    res$X <- as.matrix(data[,1:11])
    return(res)
  }

  warning("Found no dataset by that name")
  return(FALSE)
}


