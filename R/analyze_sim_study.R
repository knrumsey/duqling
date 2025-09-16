#' Summarize Emulator Performance from a Simulation Study
#'
#' Computes summary statistics from the output of \code{run_sim_study()}.
#'
#' @param df A data frame returned by \code{run_sim_study()}, containing emulator performance metrics across simulation scenarios.
#' @param group_by Optional. A character vector of column names to group summaries by (e.g., \code{"fname"}, \code{"NSR"}, \code{"n_train"}, \code{"design_type"} or (rarely of interest) \code{"replication"}).
#' @param methods Optional. A character vector specifying a subset of methods to include in the summary.
#' @param topX Integer. The number of top-performing methods (by CRPS) used to compute the \code{topX_rate}. Default is 5.
#' @param good_enough_pct Numeric. The threshold (as a proportion above the best CRPS) to define a "good enough" emulator. Default is 0.01 (i.e., within 1\% of best).
#' @param fvu_thresh Numeric. The largest value of fraction of variance unexplained that is acceptable.
#' @param ties_method For computing ranks in each simulation scenario.
#'
#' @return A data frame where each row corresponds to a unique emulator method and contains the following columns:
#'   \itemize{
#'     \item \code{method}: The method name.
#'     \item \code{n_scenarios}: Number of simulation scenarios in which the method was evaluated (excluding failures).
#'     \item \code{avg_CRPS}: Average CRPS across all valid simulation scenarios.
#'     \item \code{avg_RMSE}: Average RMSE across all valid simulation scenarios.
#'     \item \code{avg_time}: Average total time (\code{t_tot}) in seconds.
#'     \item \code{avg_rank_CRPS}: Average rank of the method based on CRPS within each scenario.
#'     \item \code{win_rate}: Proportion of scenarios where the method achieved the best CRPS.
#'     \item \code{topX_rate}: Proportion of scenarios where the method ranked in the top \code{X} by CRPS.
#'     \item \code{failure_rate}: Proportion of all runs in which the method failed (either at fitting or prediction).
#'     \item \code{good_enough_rate}: Proportion of scenarios where the method's CRPS was within \code{(1 + good_enough_pct)} of the best.
#'     \item \code{rel_CRPS}: Average CRPS relative to the best method in each scenario.
#'     \item \code{rel_RMSE}: Average RMSE relative to the best method in each scenario.
#'   }
#'
#' @details This function summarizes emulator performance across all simulation scenarios in a \code{run_sim_study()} output. Each scenario is defined by the combination of test function, training set size, noise-to-signal ratio, design type, and replication index. Failures are excluded from accuracy and timing metrics but included in failure rate calculation.
#'
#' @examples
#' \dontrun{
#' res <- run_sim_study(...)
#' summarize_sim_study(res)
#' summarize_sim_study(res, methods = c("GP", "BART"), topX = 3)
#' }
#' @export
summarize_sim_study_deprecated <- function(df,
                                group_by = NULL,
                                methods = NULL,
                                topX = 5,
                                good_enough_pct = 0.01,
                                fvu_thresh = 0.01,
                                ties_method="min"){

  # In case df comes from run_sim_study_data()
  if (!"fname" %in% names(df) && "dname" %in% names(df)) {
    df$fname <- df$dname
    df$rep <- df$fold
    df$replication <- df$fold
    df$n_train <- NA
    df$NSR <- NA
    df$design_type <- NA
  }

  # Check for required columns
  required_cols <- c("method", "fname", "n_train", "NSR", "design_type", "replication", "RMSE", "CRPS", "t_tot", "failure_type")
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in input data frame: ", paste(missing_cols, collapse = ", "))
  }

  # Filter methods if requested
  if (!is.null(methods)) {
    df <- df[df$method %in% methods, , drop = FALSE]
  }

  # Recursively handle group_by argument
  if (!is.null(group_by)) {
    if (!all(group_by %in% names(df))) {
      stop("Some 'group_by' variables are not in the data frame.")
    }

    # Create grouping keys
    group_keys <- split(df, interaction(df[, group_by], drop = TRUE))

    # Recursive call for each group
    out_list <- lapply(group_keys, function(dsub) {
      out <- summarize_sim_study(dsub, methods = methods, topX = topX, good_enough_pct = good_enough_pct)

      # Attach group_by columns to output
      for (g in group_by) {
        out[[g]] <- unique(dsub[[g]])
      }

      return(out)
    })

    # Combine all grouped summaries
    results <- do.call(rbind, out_list)
    rownames(results) <- NULL
    return(results)
  }

  # Define simulation scenario ID (collapse all columns that define a scenario)
  df$scenario_id <- apply(df[c("fname", "n_train", "NSR", "design_type", "replication")], 1, paste, collapse = "_")

  # Split by scenario
  scenario_list <- split(df, df$scenario_id)

  # Initialize per-method accumulators
  methods_all <- unique(df$method)
  method_stats <- setNames(vector("list", length(methods_all)), methods_all)

  for (m in methods_all) {
    method_stats[[m]] <- list(
      n_scenarios = 0,
      sum_crps = 0,
      sum_rmse = 0,
      sum_time = 0,
      n_failures = 0,
      win_count = 0,
      topX_count = 0,
      good_enough_count = 0,
      low_fvu_count = 0,
      sum_rel_crps = 0,
      sum_rel_rmse = 0,
      rank_sum_crps = 0
    )
  }

  # Loop over scenarios
  for (scenario in scenario_list) {
    # Only consider methods that didn't fail
    valid <- scenario$failure_type %in% c("none", "fit", "pred", "time")
    if (sum(valid) < 1) next  # Skip if no valid methods

    scenario_valid <- scenario[valid, , drop = FALSE]
    crps_vals <- scenario_valid$CRPS
    rmse_vals <- scenario_valid$RMSE
    fvu_vals  <- scenario_valid$FVU
    time_vals <- scenario_valid$t_tot
    methods_in_scenario <- scenario_valid$method

    # Find best values
    best_crps <- max(1e-16, min(crps_vals))
    best_rmse <- max(1e-16, min(rmse_vals))

    # Rank methods by CRPS
    ranks <- rank(crps_vals, ties.method = ties_method)
    names(ranks) <- methods_in_scenario
    best_rank <- min(ranks)

    for (i in seq_len(nrow(scenario_valid))) {
      row <- scenario_valid[i, ]
      m <- row$method
      stats <- method_stats[[m]]
      stats$n_scenarios <- stats$n_scenarios + 1
      stats$sum_crps <- stats$sum_crps + row$CRPS
      stats$sum_rmse <- stats$sum_rmse + row$RMSE
      stats$sum_time <- stats$sum_time + row$t_tot
      stats$sum_rel_crps <- stats$sum_rel_crps + (row$CRPS / best_crps)
      stats$sum_rel_rmse <- stats$sum_rel_rmse + (row$RMSE / best_rmse)
      stats$rank_sum_crps <- stats$rank_sum_crps + ranks[m]

      if (ranks[m] == best_rank) stats$win_count <- stats$win_count + 1
      if (ranks[m] <= topX) stats$topX_count <- stats$topX_count + 1
      if (row$CRPS <= (1 + good_enough_pct) * best_crps) stats$good_enough_count <- stats$good_enough_count + 1
      if (row$FVU <= fvu_thresh) stats$low_fvu_count <- stats$low_fvu_count + 1

      method_stats[[m]] <- stats
    }
  }

  # Now count failures
  for (m in methods_all) {
    method_stats[[m]]$n_failures <- sum(df$method == m & df$failure_type != "none")
  }

  # Compile results
  results <- data.frame(
    method = character(),
    n_scenarios = integer(),
    avg_CRPS = numeric(),
    avg_RMSE = numeric(),
    avg_time = numeric(),
    avg_rank_CRPS = numeric(),
    win_rate = numeric(),
    topX_rate = numeric(),
    failure_rate = numeric(),
    good_enough_rate = numeric(),
    low_fvu_rate = numeric(),
    rel_CRPS = numeric(),
    rel_RMSE = numeric(),
    stringsAsFactors = FALSE
  )

  for (m in methods_all) {
    stats <- method_stats[[m]]
    n <- stats$n_scenarios
    results <- rbind(results, data.frame(
      method = m,
      n_scenarios = n,
      avg_CRPS = stats$sum_crps / n,
      avg_RMSE = stats$sum_rmse / n,
      avg_time = stats$sum_time / n,
      avg_rank_CRPS = stats$rank_sum_crps / n,
      win_rate = stats$win_count / n,
      topX_rate = stats$topX_count / n,
      failure_rate = stats$n_failures / (n + stats$n_failures),
      good_enough_rate = stats$good_enough_count / n,
      low_fvu_rate = stats$low_fvu_count / n,
      rel_CRPS = stats$sum_rel_crps / n,
      rel_RMSE = stats$sum_rel_rmse / n,
      stringsAsFactors = FALSE
    ))
  }
  rownames(results) <- NULL
  return(results)
}


#' Filter Simulation Study Results
#'
#' Filters the results from \code{run_sim_study()} by method, function, sample size, etc.
#'
#' @param df A data frame from \code{run_sim_study()}.
#' @param methods Optional character vector of methods to include.
#' @param fname Optional character vector of function names to include.
#' @param n_train Optional numeric vector of training set sizes.
#' @param NSR Optional numeric vector of noise-to-signal ratios.
#' @param design_type Optional character vector of design types to include.
#' @param replication Optional integer vector of rep indices to include.
#'
#' @return A filtered data frame.
#' @examples
#' \dontrun{
#' filtered <- filter_sim_study(results, methods = c("GP", "BART"), fname = "borehole")
#' rankplot_sim_study(filtered, metric = "CRPS")
#' }
#' @export
filter_sim_study_deprecated <- function(df,
                             methods = NULL,
                             fname = NULL,
                             n_train = NULL,
                             NSR = NULL,
                             design_type = NULL,
                             replication = NULL) {

  # In case df comes from run_sim_study_data()
  if (!"fname" %in% names(df) && "dname" %in% names(df)) {
    df$fname <- df$dname
  }

  # Rename "n" to "n_train" internally if needed
  if ("n" %in% names(df) && !"n_train" %in% names(df)) {
    names(df)[names(df) == "n"] <- "n_train"
  }

  if (!is.null(methods)) {
    df <- df[df$method %in% methods, , drop = FALSE]
  }

  if (!is.null(fname)) {
    df <- df[df$fname %in% fname, , drop = FALSE]
  }

  if (!is.null(n_train)) {
    df <- df[df$n_train %in% n_train, , drop = FALSE]
  }

  if (!is.null(NSR)) {
    df <- df[df$NSR %in% NSR, , drop = FALSE]
  }

  if (!is.null(design_type)) {
    df <- df[df$design_type %in% design_type, , drop = FALSE]
  }

  rep_col <- if ("replication" %in% names(df)) "replication" else if ("rep" %in% names(df)) "rep" else NULL
  if (!is.null(replication) && !is.null(rep_col)) {
    df <- df[df[[rep_col]] %in% replication, , drop = FALSE]
  }

  return(df)
}


#' Join Simulation Study Results
#'
#' Joins two simulation study result data frames, allowing for mismatched columns.
#'
#' @param df1 A data frame from \code{run_sim_study()}.
#' @param df2 Another data frame to join with \code{df1}.
#' @param on_mismatch Character string specifying how to handle mismatched columns.
#'   Options are \code{"union"} (default) to include all columns and pad with \code{NA}s as needed,
#'   or \code{"intersect"} to retain only columns common to both data frames.
#'
#' @return A combined data frame.
#' @examples
#' df1 <- data.frame(a = 1:3, b = letters[1:3])
#' df2 <- data.frame(b = letters[4:5], c = 10:11)
#'
#' # Keep all columns (union)
#' join_sim_study(df1, df2, on_mismatch = "union")
#'
#' # Keep only common columns (intersect)
#' join_sim_study(df1, df2, on_mismatch = "intersect")
#'
#' @export
join_sim_study_deprecated <- function(df1, df2, on_mismatch = c("union", "intersect")) {
  on_mismatch <- match.arg(on_mismatch)

  # Unify run_sim_study() and run_sim_study_data() if needed
  # We do this by renaming $dname -> $fname by safely merging the two columns
  if ("dname" %in% names(df1)) {
    if (!"fname" %in% names(df1)) {
      df1$fname <- df1$dname
    } else {
      # Fill NAs in fname from dname
      df1$fname[is.na(df1$fname)] <- df1$dname[is.na(df1$fname)]
    }

    if(!all(c("n_train", "NSR", "design_type") %in% names(df1))){
      df1$replication <- df1$fold
      df1$n_train <- NA
      df1$NSR <- NA
      df1$design_type <- NA
    }
  }

  if ("dname" %in% names(df2)) {
    if (!"fname" %in% names(df2)) {
      df2$fname <- df2$dname
    } else {
      df2$fname[is.na(df2$fname)] <- df2$dname[is.na(df2$fname)]
    }

    if(!all(c("n_train", "NSR", "design_type") %in% names(df2))){
      df2$replication <- df2$fold
      df2$n_train <- NA
      df2$NSR <- NA
      df2$design_type <- NA
    }
  }

  cols1 <- names(df1)
  cols2 <- names(df2)

  if (on_mismatch == "intersect") {
    common_cols <- intersect(cols1, cols2)
    df1 <- df1[common_cols]
    df2 <- df2[common_cols]
  } else if (on_mismatch == "union") {
    all_cols <- union(cols1, cols2)

    add_missing_cols <- function(df, target_cols) {
      for (col in setdiff(target_cols, names(df))) {
        df[[col]] <- NA
      }
      # Reorder to match target_cols
      df <- df[target_cols]
      return(df)
    }

    df1 <- add_missing_cols(df1, all_cols)
    df2 <- add_missing_cols(df2, all_cols)
  }

  result <- rbind(df1, df2)
  return(result)
}

