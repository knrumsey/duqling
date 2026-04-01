#' Get UQ Dataset Information
#'
#' Returns information about datasets available through `duqling`.
#'
#' @param dname Character vector of dataset names to filter.
#' @param raw Logical. When `TRUE`, return information on raw datasets.
#'   Otherwise, return information on emulation-ready datasets.
#' @param input_dim Vector of quantitative input dimensions to filter.
#' @param input_cat_dim Vector of categorical input dimensions to filter.
#' @param n Vector of sample sizes to filter.
#' @param output_dim Vector of output dimensions to filter. Used only when
#'   `raw = TRUE`.
#' @param response Character string specifying response type for emulation-ready
#'   datasets. Can be `"all"`, `"univariate"`, `"multivariate"`, or
#'   `"functional"`. Backward-compatible aliases such as `response_type = "uni"`
#'   are also accepted.
#' @param sorted Logical. Should results be sorted by input dimension and then
#'   alphabetically?
#' @param verbose Logical. Print information for a single dataset?
#' @return A data frame of datasets meeting the filter requirements, or an
#'   invisible named list for a single dataset when `dname` has length 1.
#' @export
#' @examples
#' data_quack(raw = TRUE)
#' data_quack(raw = FALSE)
#' data_quack(response = "univariate")
#' data_quack("pbx9501_gold")
data_quack <- function(dname = NULL,
                       raw = FALSE,
                       input_dim = NULL,
                       input_cat_dim = NULL,
                       n = NULL,
                       output_dim = NULL,
                       response = NULL,
                       sorted = TRUE,
                       verbose = TRUE,
                       ...) {
  dots <- list(...)

  # Backward compatibility: response_type
  if (!is.null(dots$response_type) && is.null(response)) {
    response <- dots$response_type
  }

  # Backward compatibility: old abbreviations
  if (!is.null(response)) {
    response <- tolower(response)
    response <- sapply(response, function(x) {
      if (x %in% c("uni", "u")) return("univariate")
      if (x %in% c("multi", "m")) return("multivariate")
      if (x %in% c("func", "f")) return("functional")
      x
    }, USE.NAMES = FALSE)
  }

  tab <- if (raw) {
    .data_quack_raw_table()
  } else {
    .data_quack_emulator_table()
  }

  # Single dataset lookup
  if (!is.null(dname) && length(dname) == 1) {
    if (!(dname %in% tab$dname)) {
      stop("dname ", dname, " is not recognized.", call. = FALSE)
    }
    out <- tab[tab$dname == dname, , drop = FALSE]
    out <- .format_data_quack(out, raw = raw)
    if (verbose) print_data_quack(out)
    return(invisible(out))
  }

  # Filtering
  res <- tab

  if (!is.null(dname)) {
    res <- subset(res, dname %in% dname)
  }
  if (!is.null(input_dim)) {
    res <- subset(res, input_dim %in% input_dim)
  }
  if (!is.null(input_cat_dim)) {
    res <- subset(res, input_cat_dim %in% input_cat_dim)
  }
  if (!is.null(n)) {
    res <- subset(res, n %in% n)
  }
  if (raw && !is.null(output_dim)) {
    res <- subset(res, output_dim %in% output_dim)
  }
  if (!raw && !is.null(response)) {
    ok <- sapply(res$response, function(resp_val) {
      any(sapply(response, function(r) grepl(r, tolower(resp_val), fixed = TRUE)))
    })
    res <- res[ok, , drop = FALSE]
  }

  if (nrow(res) == 0) {
    return("No datasets found meeting the criteria")
  }

  if (sorted) {
    ord <- order(res$input_dim, res$dname)
    res <- res[ord, , drop = FALSE]
    rownames(res) <- seq_len(nrow(res))
  }

  res
}

.data_quack_raw_table <- function() {
  rbind(
    data.frame(dname = "strontium_plume", input_dim = 20, output_dim = 10, n = 300, input_cat_dim = 0),
    data.frame(dname = "pbx9501",          input_dim = 6,  output_dim = 10, n = 7000, input_cat_dim = 1),
    data.frame(dname = "stochastic_sir",   input_dim = 4,  output_dim = 1,  n = 2000, input_cat_dim = 0),
    data.frame(dname = "e3sm",             input_dim = 2,  output_dim = 1,  n = 48602, input_cat_dim = 1),
    data.frame(dname = "fair_climate",     input_dim = 46, output_dim = 1,  n = 168168, input_cat_dim = 1),
    data.frame(dname = "Z_machine_exp",    input_dim = 1,  output_dim = 1,  n = 23224, input_cat_dim = 3),
    data.frame(dname = "Z_machine_sim",    input_dim = 40, output_dim = 9,  n = 5000000, input_cat_dim = 0),
    data.frame(dname = "flyer_plate104",   input_dim = 11, output_dim = 200, n = 1000, input_cat_dim = 0)
  )
}

.data_quack_emulator_table <- function() {
  tab <- rbind(
    data.frame(dname = "strontium_plume_p4b",         input_dim = 20, input_cat_dim = 0, n = 300,   response = "univariate"),
    data.frame(dname = "strontium_plume_p104",        input_dim = 20, input_cat_dim = 0, n = 300,   response = "univariate"),
    data.frame(dname = "pbx9501_gold",                input_dim = 6,  input_cat_dim = 0, n = 500,   response = "univariate"),
    data.frame(dname = "pbx9501_ss304",               input_dim = 6,  input_cat_dim = 0, n = 500,   response = "univariate"),
    data.frame(dname = "pbx9501_nickel",              input_dim = 6,  input_cat_dim = 0, n = 500,   response = "univariate"),
    data.frame(dname = "pbx9501_uranium",             input_dim = 6,  input_cat_dim = 0, n = 500,   response = "univariate"),
    data.frame(dname = "stochastic_sir",              input_dim = 4,  input_cat_dim = 0, n = 2000,  response = "univariate"),
    data.frame(dname = "e3sm",                        input_dim = 2,  input_cat_dim = 0, n = 48602, response = "univariate"),
    data.frame(dname = "e3sm_mcar",                   input_dim = 2,  input_cat_dim = 0, n = 10000, response = "univariate"),
    data.frame(dname = "e3sm_mnar",                   input_dim = 2,  input_cat_dim = 0, n = 9122,  response = "univariate"),
    data.frame(dname = "fair_climate_ssp1-2.6",       input_dim = 46, input_cat_dim = 0, n = 56056, response = "univariate"),
    data.frame(dname = "fair_climate_ssp2-4.5",       input_dim = 46, input_cat_dim = 0, n = 56056, response = "univariate"),
    data.frame(dname = "fair_climate_ssp3-7.0",       input_dim = 46, input_cat_dim = 0, n = 56056, response = "univariate"),
    data.frame(dname = "fair_climate_ssp1-2.6_year2200", input_dim = 45, input_cat_dim = 0, n = 1001, response = "univariate"),
    data.frame(dname = "fair_climate_ssp2-4.5_year2200", input_dim = 45, input_cat_dim = 0, n = 1001, response = "univariate"),
    data.frame(dname = "fair_climate_ssp3-7.0_year2200", input_dim = 45, input_cat_dim = 0, n = 1001, response = "univariate"),
    data.frame(dname = "Z_machine_max_vel1",          input_dim = 6,  input_cat_dim = 0, n = 5000,  response = "univariate"),
    data.frame(dname = "Z_machine_max_vel2",          input_dim = 6,  input_cat_dim = 0, n = 5000,  response = "univariate"),
    data.frame(dname = "Z_machine_max_vel_all",       input_dim = 30, input_cat_dim = 0, n = 5000,  response = "univariate"),
    data.frame(dname = "SLOSH_low",                   input_dim = 5,  input_cat_dim = 0, n = 4000,  response = "univariate"),
    data.frame(dname = "SLOSH_mid",                   input_dim = 5,  input_cat_dim = 0, n = 4000,  response = "univariate"),
    data.frame(dname = "SLOSH_high",                  input_dim = 5,  input_cat_dim = 0, n = 4000,  response = "univariate"),
    data.frame(dname = "flyer_plate104",              input_dim = 11, input_cat_dim = 0, n = 1000,  response = "univariate")
  )
  tab
}

.format_data_quack <- function(obj, raw = FALSE) {
  if (raw) {
    list(
      dname = obj$dname,
      input_dim = obj$input_dim,
      input_cat_dim = obj$input_cat_dim,
      output_dim = obj$output_dim,
      n = obj$n,
      response = if (obj$output_dim == 1) "univariate" else "multivariate"
    )
  } else {
    list(
      dname = obj$dname,
      input_dim = obj$input_dim,
      input_cat_dim = obj$input_cat_dim,
      n = obj$n,
      response = obj$response
    )
  }
}

print_data_quack <- function(x, ...) {
  cat("Dataset:", x$dname, "\n")
  cat("  Input dimension:", x$input_dim, "\n")
  cat("  Categorical input dimension:", x$input_cat_dim, "\n")
  cat("  Sample size:", x$n, "\n")
  if (!is.null(x$output_dim)) {
    cat("  Output dimension:", x$output_dim, "\n")
  }
  cat("  Response type:", x$response, "\n")
}
