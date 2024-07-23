#' Get UQ Data for Emulation
#'
#' Load in UQ datasets from the UQdataverse (harvard)
#'
#' @param raw Logical. When TRUE, full dataset information is given. Otherwise, returns list of scalar-response datasets for emulation.
#' @param dname Vector of function names to filter.
#' @param input_dim Vector of input dimensions (quantitative only) to filter.
#' @param input_cat_dim Vector of input dimensions (categorical only) to filter.
#' @param n Vector of sample sizes to filter.
#' @param output_dim Vector of output dimensions to filter.
#' @param response_type String. Used only when \code{raw=FALSE}. Can be "uni" for univariate response or "func" for functional response.
#' @return A table with data set names meeting the filter requirements
#' @details Wrapper to read in various UQ datasets (from harvard dataverse)
#' @export
#' @examples
#' tab <- data_quack(raw=TRUE)
#' get_UQ_data(tab$dname[1])
#'
#' tab <- data_quack(raw=FALSE)
#' get_emulation_data(tab$dname[1])
data_quack <- function(raw=FALSE, dname=NULL, input_dim=NULL, input_cat_dim=NULL, n=NULL, output_dim=NULL, response_type=NULL){
  if(raw){
    data_quack_raw(dname, input_dim=input_dim, output_dim=output_dim, n=n, input_cat_dim=input_cat_dim)
  }else{
    data_quack_emulator(dname, input_dim=input_dim, n=n, input_cat_dim=input_cat_dim, response_type=response_type)
  }
}

data_quack_raw <- function(dname=NULL, input_dim=NULL, output_dim=NULL, n=NULL, input_cat_dim=NULL){
  tab <- NULL

  tmp <- data.frame(dname="strontium_plume", input_dim=20, output_dim=10, n=300, input_cat_dim=0)
  tab <- rbind(tab, tmp)

  tmp <- data.frame(dname="pbx9501", input_dim=6, output_dim=10, n=7000, input_cat_dim=1)
  tab <- rbind(tab, tmp)

  tmp <- data.frame(dname="stochastic_sir", input_dim=4, output_dim=1, n=2000, input_cat_dim=0)
  tab <- rbind(tab, tmp)

  tmp <- data.frame(dname="e3sm", input_dim=2, output_dim=1, n=48602, input_cat_dim=1)
  tab <- rbind(tab, tmp)

  tmp <- data.frame(dname="fair_climate", input_dim=46, output_dim=1, n=168168, input_cat_dim=1)
  tab <- rbind(tab, tmp)

  tmp <- data.frame(dname="Z_machine_exp", input_dim=1, output_dim=1, n=23224, input_cat_dim=3)
  tab <- rbind(tab, tmp)

  tmp <- data.frame(dname="Z_machine_sim", input_dim=40, output_dim=9, n=5000000, input_cat_dim=0)
  tab <- rbind(tab, tmp)

  tmp <- data.frame(dname="flyer_plate104", input_dim=11, output_dim=200, n=1000, input_cat_dim=0)
  tab <- rbind(tab, tmp)

  # Subset the master table
  ord <- order(tab$input_dim)
  tab <- tab[ord,]
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
  rownames(res) <- 1:nrow(res)
  return(res)
}

data_quack_emulator <- function(dname, input_dim=NULL, input_cat_dim=NULL, n=NULL, response_type=NULL){
  tab <- NULL

  tmp <- data.frame(dname="strontium_plume_p4b", input_dim=20, input_cat_dim=0, n=300, response_type="uni")
  tab <- rbind(tab, tmp)
  tmp <- data.frame(dname="strontium_plume_p104", input_dim=20, input_cat_dim=0, n=300, response_type="uni")
  tab <- rbind(tab, tmp)

  tmp <- data.frame(dname="pbx9501_gold", input_dim=6, input_cat_dim=0, n=500, response_type="uni")
  tab <- rbind(tab, tmp)
  tmp <- data.frame(dname="pbx9501_ss304", input_dim=6, input_cat_dim=0, n=500, response_type="uni")
  tab <- rbind(tab, tmp)
  tmp <- data.frame(dname="pbx9501_nickel", input_dim=6, input_cat_dim=0, n=500, response_type="uni")
  tab <- rbind(tab, tmp)
  tmp <- data.frame(dname="pbx9501_uranium", input_dim=6, input_cat_dim=0, n=500, response_type="uni")
  tab <- rbind(tab, tmp)

  tmp <- data.frame(dname="stochastic_sir", input_dim=4, input_cat_dim=0, n=2000, response_type="uni")
  tab <- rbind(tab, tmp)

  tmp <- data.frame(dname="e3sm", input_dim=2, input_cat_dim=0, n=48602, response_type="uni")
  tab <- rbind(tab, tmp)
  tmp <- data.frame(dname="e3sm_mcar", input_dim=2, input_cat_dim=0, n=10000, response_type="uni")
  tab <- rbind(tab, tmp)
  tmp <- data.frame(dname="e3sm_mnar", input_dim=2, input_cat_dim=0, n=9122, response_type="uni")
  tab <- rbind(tab, tmp)

  tmp <- rbind(data.frame(dname="fair_climate_ssp1-2.6", input_dim=46, input_cat_dim=0, n=56056, response_type="uni"),
               data.frame(dname="fair_climate_ssp2-4.5", input_dim=46, input_cat_dim=0, n=56056, response_type="uni"),
               data.frame(dname="fair_climate_ssp3-7.0", input_dim=46, input_cat_dim=0, n=56056, response_type="uni"),
               data.frame(dname="fair_climate_ssp1-2.6_year2200", input_dim=45, input_cat_dim=0, n=1001, response_type="uni"),
               data.frame(dname="fair_climate_ssp2-4.5_year2200", input_dim=45, input_cat_dim=0, n=1001, response_type="uni"),
               data.frame(dname="fair_climate_ssp3-7.0_year2200", input_dim=45, input_cat_dim=0, n=1001, response_type="uni")
  )
  tab <- rbind(tab, tmp)

  tmp <- data.frame(dname="Z_machine_max_vel1", input_dim=6, input_cat_dim=0, n=5000, response_type="uni")
  tab <- rbind(tab, tmp)
  tmp <- data.frame(dname="Z_machine_max_vel2", input_dim=6, input_cat_dim=0, n=5000, response_type="uni")
  tab <- rbind(tab, tmp)
  tmp <- data.frame(dname="Z_machine_max_vel_all", input_dim=30, input_cat_dim=0, n=5000, response_type="uni")
  tab <- rbind(tab, tmp)

  tmp <- rbind(data.frame(dname="SLOSH_low", input_dim=5, input_cat_dim=0, n=4000, response_type="uni"),
               data.frame(dname="SLOSH_mid", input_dim=5, input_cat_dim=0, n=4000, response_type="uni"),
               data.frame(dname="SLOSH_high", input_dim=5, input_cat_dim=0, n=4000, response_type="uni"))
  tab <- rbind(tab, tmp)

  tmp <- data.frame(dname="flyer_plate104", input_dim=11, input_cat_dim=0, n=1000, response_type="uni")
  tab <- rbind(tab, tmp)

  # Subset the master table
  ord <- order(tab$input_dim)
  tab <- tab[ord,]
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
  if(!is.null(response_type)){
    tmp <- response_type
    res <- subset(res, res$response_type %in% tmp)
  }
  rownames(res) <- 1:nrow(res)
  return(res)
}

# Workflow
# Add data to UQDataverse (harvard)
# Add data read to get_UQ_data
# Add wrappers for get_emulation_data
# Add function info to both data_quack_raw() and data_quack_emulation().
