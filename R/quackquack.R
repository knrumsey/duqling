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

quackquack <- function(fname=NULL, input_dim=NULL,
                       response=NULL, stochastic=NULL, has_categorical=NULL,
                       sorted=TRUE, show_sigma=FALSE, ...){
  dots <- list(...)

  # Backward compatibility: response_type arg
  if (!is.null(dots$response_type) && is.null(response)) {
    response <- dots$response_type
    dots$response_type <- NULL
  }

  if (is.character(stochastic)){
    stochastic <- (stochastic == "y")
  }

  # Case 1: fname specified -> return info for one or more functions
  if (!is.null(fname)) {
    fnames <- as.character(fname)
    out <- list()
    for(i in seq_along(fnames)){
      qq_name <- paste0("quackquack_", fnames[i])
      if (!exists(qq_name, envir = asNamespace("duqling"))){
        stop("fname ", fnames[i], " is not recognized.")
      }
      tmp <- get(qq_name, envir = asNamespace("duqling"))()
      tmp <- .format_quack(tmp, fnames[i])
      print_quack(tmp)
      out[[i]] <- tmp
    }
    if(length(fnames) == 1){
      out <- out[[1]]
    }
    return(invisible(out))
  }

  # Hard code in the list of stochastic functions
  STOCHASTIC_LIST <- c( "ocean_circ",
                        "stochastic_piston",
                        "bs_call",
                        "bs_put",
                        "dts_sirs")

  ns <- asNamespace("duqling")
  all_funs <- ls(ns, pattern = "^quackquack_")
  fnames <- sub("^quackquack_", "", all_funs)
  master_list <- NULL
  for(i in seq_along(fnames)){
    qq_name <- all_funs[i]
    tmp <- get(qq_name, envir = asNamespace("duqling"))()
    tmp <- .format_quack(tmp, fnames[i])
    curr_row <- data.frame(fname=fnames[i],
                           input_dim = tmp$input_dim,
                           response = tmp$response,
                           has_categorical = tmp$has_categorical)

    curr_row$stochastic <- tmp$stochastic
    if(fnames[i] %in% STOCHASTIC_LIST){
      curr_row$stochastic <- TRUE
    }else{
      if(is.null(curr_row$stochastic)){
        curr_row$stochastic <- FALSE
      }
      if(is.character(curr_row$stochastic)){
        curr_row$stochastic <- curr_row$stochastic == "y"
      }
    }

    if(show_sigma){
      sig <- lookup_sigma(fnames[i])
      if(!is.null(sig)){
        curr_row$sigma <- sig
      }else{
        curr_row$sigma <- NA
      }
    }

    # Check conditions here
    conditions_met <- TRUE
    tmp <- curr_row

    # Filter by input_dim (exact match against vector of allowed dims)
    if (!is.null(input_dim)) {
      if (!(tmp$input_dim %in% input_dim)) conditions_met <- FALSE
    }

    # Filter by has_categorical
    if (!is.null(has_categorical)) {
      if (tmp$has_categorical != has_categorical) conditions_met <- FALSE
    }

    # Filter by response (with partial matching, backward compat for response_type)
    if (!is.null(response)) {
      resp_val <- tolower(tmp$response)
      # allow partial matching like "uni" or "u"
      ok <- any(sapply(response, function(r) grepl(tolower(r), resp_val)))
      if (!ok) conditions_met <- FALSE
    }

    # Filter by stochastic (accepts TRUE/FALSE or "y"/"n")
    if (!is.null(stochastic)) {
      stoch_val <- tmp$stochastic
      # normalize inputs
      if (is.character(stochastic)) {
        stochastic <- tolower(stochastic) == "y"
      }
      if (!(stoch_val %in% stochastic)) conditions_met <- FALSE
    }

    if(conditions_met){
      if(is.null(master_list)){
        master_list <- curr_row
      }else{
        master_list <- rbind(master_list, curr_row)
      }
    }
  }

  if (is.null(master_list)) {
    return("No functions found meeting the criteria")
  } else {
    if(sorted){
      master_list <- master_list[order(master_list$input_dim, master_list$fname),]
      rownames(master_list) <- 1:nrow(master_list)
    }
    return(master_list)
  }
}


# Private helper to pretty-print individual quackquack_<fname> objects
.format_quack <- function(obj, fname) {
  out <- list(
    fname        = fname,
    input_dim    = obj$input_dim,
    has_categorical = if(!is.null(obj$input_cat)) obj$input_cat else NA,
    response     = switch(obj$response_type,
                          "uni"  = "univariate",
                          "func" = "functional",
                          "multi"= "multivariate",
                          obj$response_type),
    stochastic   = if (is.character(obj$stochastic)) tolower(obj$stochastic) == "y" else obj$stochastic,
    input_range  = obj$input_range
  )
  invisible(out)
}

# Print method for duqling_quack objects
print_quack <- function(x, ...) {
  if(is.null(x$stochastic)) x$stochastic <- "No"

  cat("Function:", x$fname, "\n")
  cat("  Input dimension:", x$input_dim, "\n")
  cat("  Response type:", x$response, "\n")
  cat("  Stochastic:", x$stochastic, "\n")
  cat("  Has categorical inputs:", x$has_categorical, "\n\n")

  if (!is.null(x$input_range)) {
    cat("Input ranges:\n")
    print(x$input_range)
  }
}
