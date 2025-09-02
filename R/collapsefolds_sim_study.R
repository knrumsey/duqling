#' Collapse Cross-Validation Folds for Simulation Study Results
#'
#' Aggregates multiple CV folds per method × dataset into a single summary row.
#'
#' @param df A data frame from \code{run_sim_study_data()}.
#'
#' @return A data frame with one row per dataset × method combination. Metrics are aggregated across folds using fold-size weights.
#'
#' @details
#' RMSE is aggregated as the square root of the weighted average of squared RMSE terms. All other metrics are aggregated using weighted means, with weights proportional to fold size. The \code{failure_type} column is set to \code{"none"} if all folds succeeded, otherwise reports a string summary of failures (e.g., \code{"fit=1, pred=2"}).
#'
#' @export
collapsefolds_sim_study <- function(df) {
  if ("fname" %in% names(df)) {
    warning("collapsefolds_sim_study() is intended for run_sim_study_data() results, not synthetic test functions.")
  }

  required_cols <- c("method", "dname", "fold", "fold_size", "RMSE", "input_dim", "n")
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  # Define groups
  key_cols <- c("method", "dname", "input_dim", "n")
  group_keys <- unique(df[key_cols])

  output <- list()

  for (i in seq_len(nrow(group_keys))) {
    key <- group_keys[i, , drop = FALSE]

    idx <- which(
      df$method == key$method &
        df$dname == key$dname &
        df$input_dim == key$input_dim &
        df$n == key$n
    )

    group_df <- df[idx, , drop = FALSE]
    weights <- group_df$fold_size / sum(group_df$fold_size)

    # Start result row
    row <- key
    row$fold <- length(idx)

    # failure_type summary
    ftab <- table(group_df$failure_type)
    if (length(ftab) == 1 && names(ftab)[1] == "none") {
      row$failure_type <- "none"
    } else {
      row$failure_type <- paste(names(ftab), ftab, sep = "=", collapse = ", ")
    }

    # RMSE: special handling
    row$RMSE <- sqrt(sum(weights * group_df$RMSE^2, na.rm = TRUE))

    # All other numeric metrics → weighted mean
    numeric_cols <- names(group_df)[
      sapply(group_df, is.numeric) &
        !names(group_df) %in% c("RMSE", "fold_size", "input_dim", "n", "fold")
    ]

    for (col in numeric_cols) {
      row[[col]] <- sum(weights * group_df[[col]], na.rm = TRUE)
    }

    output[[i]] <- row
  }

  result_df <- do.call(rbind, lapply(output, as.data.frame))
  rownames(result_df) <- NULL
  return(result_df)
}
