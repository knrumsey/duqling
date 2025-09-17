#' Process simulation study results (duqling)
#'
#' Unifies outputs from \code{run_sim_study()} and \code{run_sim_study_data()},
#' adds an ID and a simulation scenario string, and (optionally) scales CRPS
#' in place using \code{lookup_sigma(id)}.
#'
#' @param df data.frame from \code{run_sim_study()} or \code{run_sim_study_data()}.
#' @param scale_CRPS logical; if TRUE (default) scale CRPS by sigma and keep column name "CRPS".
#'
#' @return An object of class \code{duq_sim_study} with:
#' \itemize{
#'   \item \code{$df} processed data frame
#'   \item \code{$meta} list with source_type, scenario_keys, scaling notes, etc.
#' }
#' @export
process_sim_study <- function(df, scale_CRPS = TRUE) {
  if (!is.data.frame(df)) stop("df must be a data.frame", call. = FALSE)

  # Detect source type
  source_type <- if ("fname" %in% names(df)) {
    "fun"
  } else if ("dname" %in% names(df)) {
    "data"
  } else {
    "unknown"
  }

  # Canonical ID + repetition
  df$id <- if ("fname" %in% names(df)) df$fname else if ("dname" %in% names(df)) df$dname else NA_character_
  df$replication <- if ("replication" %in% names(df)) df$replication else if ("fold" %in% names(df)) df$fold else NA

  # Scenario string
  if (source_type == "fun") {
    scen_keys <- c("id","n_train","NSR","design_type","replication")
  } else if (source_type == "data") {
    scen_keys <- c("id","fold","fold_size","cv_type")
  } else {
    scen_keys <- c("id","replication")
  }
  scen_keys <- intersect(scen_keys, names(df))
  scen_mat <- df[, scen_keys, drop = FALSE]
  for (k in scen_keys) scen_mat[[k]] <- as.character(scen_mat[[k]])
  scen_mat[is.na(scen_mat)] <- "NA"
  df$sim_scenario <- apply(scen_mat, 1, paste, collapse = "*")

  # Scale CRPS if requested
  scaling_notes <- list(applied = FALSE, n_scaled = 0L)
  if (scale_CRPS) {
    if (!"CRPS" %in% names(df)) {
      warning("scale_CRPS=TRUE but no 'CRPS' column found; skipping.")
    } else {
      if (!exists("lookup_sigma", mode = "function")) {
        stop("lookup_sigma(fname) is not available. Please define it or attach the package that provides it.", call. = FALSE)
      }
      sig <- vapply(df$id, lookup_sigma, numeric(1))
      df$sigma <- sig
      df$CRPS <- df$CRPS / df$sigma
      df$CRPS_min <- df$CRPS_min / df$sigma
      df$CRPS_Q1 <- df$CRPS_Q1 / df$sigma
      df$CRPS_med <- df$CRPS_med / df$sigma
      df$CRPS_Q3 <- df$CRPS_Q3 / df$sigma
      df$CRPS_max <- df$CRPS_max / df$sigma
      scaling_notes$applied <- TRUE
      scaling_notes$n_scaled <- sum(!is.na(df$CRPS))
    }
  }

  # Add source column
  df$source <- source_type

  # Drop original fname/dname/fold
  df$fname <- NULL
  df$dname <- NULL
  df$fold  <- NULL

  # Rename time fields
  rename_map <- c(
    t_tot  = "time",
    t_pred = "time_predict",
    t_fit  = "time_fit"
  )
  for (nm in names(rename_map)) {
    if (nm %in% names(df)) {
      names(df)[names(df) == nm] <- rename_map[[nm]]
    }
  }

  meta <- list(
    source_type = source_type,
    scenario_keys = scen_keys,
    scaled = scale_CRPS && scaling_notes$applied,
    scaling_notes = scaling_notes,
    n_rows = nrow(df)
  )

  structure(list(df = df, meta = meta), class = "duq_sim_study")
}


#' Print method for duq_sim_study
#' @export
print.duq_sim_study <- function(x, ...) {
  cat("<duq_sim_study>\n")
  cat(" Source:", x$meta$source_type, "\n")
  cat(" Rows:", x$meta$n_rows, "\n")
  cat(" Scaled CRPS applied:", isTRUE(x$meta$scaled), "\n")
  #if (!is.null(x$meta$scaling_notes) && isTRUE(x$meta$scaled)) {
  #  cat("  - n_scaled:", x$meta$scaling_notes$n_scaled, "\n")
  #}
  cat(" Scenario keys:", paste(x$meta$scenario_keys, collapse = ", "), "\n")
  if (!is.null(x$meta$collapsed)) {
    cat(" Collapsed by:", paste(x$meta$collapsed, collapse = ", "), "\n")
  }
  invisible(x)
}


#' Rank methods within each simulation scenario
#'
#' Adds ranking columns to a \code{duq_sim_study} object.
#' Each specified metric gets a new column (e.g., \code{rank_CRPS})
#' with ranks assigned across methods within each unique \code{sim_scenario}.
#'
#' @param obj A \code{duq_sim_study} object from \code{process_sim_study()}.
#' @param metric Character vector of metric column names (default "CRPS").
#' @param ties_method Passed to \code{base::rank()} (default "min").
#' @param auc Logical (default \code{FALSE}). Should the rankings be converted to
#' area under curve scale? See \code{rankplot_sim_study()} for details.
#'
#' @return The same \code{duq_sim_study} object, with new ranking column(s)
#'   appended to \code{obj$df}.
#' @export
rank_sim_study <- function(obj,
                           metric = "CRPS",
                           ties_method = "min",
                           auc = FALSE) {
  if (!inherits(obj, "duq_sim_study")) {
    stop("Please provide a duq_sim_study object (from process_sim_study()).", call. = FALSE)
  }

  df <- obj$df

  if (!all(metric %in% names(df))) {
    missing <- setdiff(metric, names(df))
    stop("Metric(s) not found in data frame: ", paste(missing, collapse = ", "))
  }

  # For each metric, create a rank column
  for (m in metric) {
    if(auc){
      rank_col <- paste0(m, "_auc")
    }else{
      rank_col <- paste0(m, "_rank")
    }

    df[[rank_col]] <- NA_real_

    # Split by sim_scenario
    split_idx <- split(seq_len(nrow(df)), df$sim_scenario)
    for (idx in split_idx) {
      vals <- df[[m]][idx]
      if (all(is.na(vals))) next
      # Lower values are better
      ranked_vals <- rank(vals, ties.method = ties_method)
      if(auc){
        ranked_vals <- 1 - (ranked_vals - 1) / (length(ranked_vals) - 1)
      }
      df[[rank_col]][idx] <- ranked_vals
    }
  }

  obj$df <- df
  obj
}

#' Normalize a metric within each simulation scenario
#'
#' Adds standardized (z-score) columns to a \code{duq_sim_study} object.
#' The metric is normalized within each \code{sim_scenario}, optionally
#' using trimming when computing mean/sd.
#'
#' @param obj A \code{duq_sim_study} object.
#' @param metric Character vector of metric column names (default "CRPS").
#' @param trim Numeric proportion (0-0.5). If >0, mean/sd are computed on the
#'   central (1-2*trim) fraction of values within each scenario, but all values
#'   are scored relative to those statistics.
#'
#' @return The same \code{duq_sim_study} object with new columns
#'   appended (e.g., \code{CRPS_norm}).
#' @export
normalize_sim_study <- function(obj,
                                metric = "CRPS",
                                trim = 0) {
  if (!inherits(obj, "duq_sim_study")) {
    stop("Please provide a duq_sim_study object (from process_sim_study()).", call. = FALSE)
  }
  if (trim < 0 || trim >= 0.5) {
    stop("trim must be between 0 and 0.5")
  }

  df <- obj$df
  if (!all(metric %in% names(df))) {
    missing <- setdiff(metric, names(df))
    stop("Metric(s) not found in data frame: ", paste(missing, collapse = ", "))
  }

  for (m in metric) {
    new_col <- paste0(m, "_norm")
    df[[new_col]] <- NA_real_

    split_idx <- split(seq_len(nrow(df)), df$sim_scenario)
    for (idx in split_idx) {
      vals <- df[[m]][idx]
      if (all(is.na(vals))) next

      mu <- mean(vals, trim = trim, na.rm = TRUE)
      sdv <- stats::sd(vals, na.rm = TRUE)
      if (sdv == 0 || is.na(sdv)) {
        df[[new_col]][idx] <- 0
      } else {
        df[[new_col]][idx] <- (vals - mu) / sdv
      }
    }
  }

  obj$df <- df
  obj
}

#' Relative performance within each simulation scenario
#'
#' Adds relative-score columns to a \code{duq_sim_study} object.
#' For each scenario, the metric is divided by the minimum value
#' (with optional epsilon smoothing). Results can be log-transformed
#' (default) for stability, and optionally truncated at an upper bound
#' to reduce the impact of extreme outliers.
#'
#' @param obj A \code{duq_sim_study} object.
#' @param metric Character vector of metric column names (default "CRPS").
#' @param epsilon Optional numeric smoothing constant. If not NULL,
#'   computes (\code{metric + epsilon}) / (\code{min(metric) + epsilon}).
#'   Default = NULL.
#' @param log Logical; if TRUE (default), take the natural logarithm of the
#'   relative values. This compresses large ratios and often gives cleaner plots.
#' @param upper_bound Optional numeric. If not NULL, relative values larger
#'   than this bound are truncated to \code{upper_bound} (applied after log
#'   transformation if \code{log = TRUE}).
#'
#' @return The same \code{duq_sim_study} object with new columns
#'   appended (e.g., \code{CRPS_rel_log} if \code{log=TRUE},
#'   or \code{CRPS_rel} otherwise).
#' @export
relativize_sim_study <- function(obj,
                               metric = "CRPS",
                               epsilon = NULL,
                               log = FALSE,
                               upper_bound = NULL) {
  if (!inherits(obj, "duq_sim_study")) {
    stop("Please provide a duq_sim_study object (from process_sim_study()).", call. = FALSE)
  }

  df <- obj$df
  if (!all(metric %in% names(df))) {
    missing <- setdiff(metric, names(df))
    stop("Metric(s) not found in data frame: ", paste(missing, collapse = ", "))
  }

  for (m in metric) {
    new_col <- if (log) paste0(m, "_rel_log") else paste0(m, "_rel")
    df[[new_col]] <- NA_real_

    split_idx <- split(seq_len(nrow(df)), df$sim_scenario)
    for (idx in split_idx) {
      vals <- df[[m]][idx]
      if (all(is.na(vals))) next

      min_val <- min(vals, na.rm = TRUE)
      ratio <- if (!is.null(epsilon)) {
        (vals + epsilon) / (min_val + epsilon)
      } else {
        vals / min_val
      }

      if (log) {
        ratio <- log(ratio)
      }

      if (!is.null(upper_bound)) {
        ratio <- pmin(ratio, upper_bound)
      }

      df[[new_col]][idx] <- ratio
    }
  }

  obj$df <- df
  obj
}


#' Ensure a derived metric column exists in a duq_sim_study
#'
#' If the requested metric column is missing, compute it based on its suffix.
#' Supports \code{_rank}, \code{_auc}, \code{_rel}, \code{_rel_log}, and \code{_norm}.
#'
#' @param obj A \code{duq_sim_study} object.
#' @param metric Character; name of the column to check or compute.
#' @param ties_method Passed to ranking functions (default "min").
#' @param epsilon Optional smoothing constant for relative metrics.
#' @param log Logical; for relative metrics, whether to use log scale.
#' @param trim Numeric; for normalize, fraction to trim when computing mean/sd.
#'
#' @return The same \code{duq_sim_study} object with the requested metric column present.
#' @export
ensure_metric <- function(obj,
                          metric,
                          ties_method = "min",
                          epsilon = NULL,
                          upper_bound = NULL,
                          log = TRUE,
                          trim = 0) {
  if (!inherits(obj, "duq_sim_study")) {
    stop("Please provide a duq_sim_study object (from process_sim_study()).", call. = FALSE)
  }

  df <- obj$df
  if (metric %in% names(df)) return(obj)  # already exists

  # Figure out suffix
  if (grepl("_rank$", metric)) {
    base <- sub("_rank$", "", metric)
    warning("Column '", metric, "' not found. Computing via rank_sim_study().")
    obj <- rank_sim_study(obj, metric = base, ties_method = ties_method, auc = FALSE)

  } else if (grepl("_auc$", metric)) {
    base <- sub("_auc$", "", metric)
    warning("Column '", metric, "' not found. Computing via rank_sim_study(auc=TRUE).")
    obj <- rank_sim_study(obj, metric = base, ties_method = ties_method, auc = TRUE)

  } else if (grepl("_rel_log$", metric)) {
    base <- sub("_rel_log$", "", metric)
    warning("Column '", metric, "' not found. Computing via relative_sim_study(log=TRUE).")
    obj <- relativize_sim_study(obj, metric = base, epsilon = epsilon, upper_bound = upper_bound, log = TRUE)

  } else if (grepl("_rel$", metric)) {
    base <- sub("_rel$", "", metric)
    warning("Column '", metric, "' not found. Computing via relative_sim_study(log=FALSE).")
    obj <- relativize_sim_study(obj, metric = base, epsilon = epsilon, upper_bound = upper_bound, log = FALSE)

  } else if (grepl("_norm$", metric)) {
    base <- sub("_norm$", "", metric)
    warning("Column '", metric, "' not found. Computing via normalize_sim_study().")
    obj <- normalize_sim_study(obj, metric = base, trim = trim)

  } else {
    stop("Don't know how to ensure metric '", metric, "'.")
  }

  obj
}

#' Combine two duq_sim_study objects
#'
#' Stacks the results of two processed simulation studies. All scenario
#' columns are preserved (unioned), and metric columns can be restricted to
#' the intersection (default) or union (with NA padding).
#'
#' @param obj1,obj2 duq_sim_study objects.
#' @param keep_extra_metrics Logical; if TRUE, keep metric columns unique to
#'   one object and pad with NA. If FALSE (default), keep only metrics present
#'   in both.
#'
#' @return A new \code{duq_sim_study} object.
#' @export
join_sim_study <- function(obj1, obj2, keep_extra_metrics = FALSE) {
  if (!inherits(obj1, "duq_sim_study") || !inherits(obj2, "duq_sim_study")) {
    stop("Both inputs must be duq_sim_study objects.", call. = FALSE)
  }

  joined_source_type <- if (identical(obj1$meta$source_type, obj2$meta$source_type)) {
    obj1$meta$source_type
  } else {
    "mixed"
  }

  df1 <- obj1$df
  df2 <- obj2$df

  # Identify common "identity" columns
  id_cols <- c("id", "method", "sim_scenario", "replication")

  # Scenario + metric candidates
  non_metric <- union(id_cols, union(obj1$meta$scenario_keys, obj2$meta$scenario_keys))
  metric_cols1 <- setdiff(names(df1), non_metric)
  metric_cols2 <- setdiff(names(df2), non_metric)

  # Decide which metrics to keep
  if (keep_extra_metrics) {
    all_metrics <- union(metric_cols1, metric_cols2)
  } else {
    all_metrics <- intersect(metric_cols1, metric_cols2)
  }

  # Build unified column set
  all_cols <- union(non_metric, all_metrics)

  # Pad missing columns with NA
  add_missing <- function(df, needed) {
    miss <- setdiff(needed, names(df))
    for (m in miss) df[[m]] <- NA
    df[, needed, drop = FALSE]
  }

  df1_sub <- add_missing(df1, all_cols)
  df2_sub <- add_missing(df2, all_cols)

  # Row bind
  df_joined <- rbind(df1_sub, df2_sub)

  meta <- list(
    source_type = joined_source_type,
    scenario_keys = union(obj1$meta$scenario_keys, obj2$meta$scenario_keys),
    metrics_kept = all_metrics,
    n_rows = nrow(df_joined),
    joined = TRUE
  )

  structure(list(df = df_joined, meta = meta), class = "duq_sim_study")
}

#' Filter a duq_sim_study object
#'
#' Filters the rows of a simulation study while preserving class and metadata.
#'
#' @param obj A \code{duq_sim_study} object from \code{process_sim_study()}.
#' @param ids Character vector of IDs to keep (matches \code{id}).
#' @param methods Character vector of methods to keep (matches \code{method}).
#' @param n_train Numeric vector of training sizes to keep.
#' @param design_type Character vector of design types to keep.
#' @param NSR Numeric vector of noise-to-signal ratios to keep.
#' @param replication Integer vector of replication indices to keep.
#' @param fold_size Integer vector of fold sizes to keep.
#' @param source Character vector of sources to keep ("function", "dataset").
#' @param failure_type Character vector of failure types to keep (if present).
#' @param metrics Character vector of metric columns to retain. If \code{NULL}
#'   (default) all metric columns are kept.
#' @param expr Optional expression evaluated inside \code{obj$df} for custom
#'   filtering, e.g. \code{expr = n_train <= 500}.
#' @param rerank Logical; if TRUE, recompute ranks (via \code{rank_sim_study()})
#'   for any rank columns present. Default is FALSE.
#'
#' @return A \code{duq_sim_study} object with filtered rows (and possibly fewer
#'   columns if \code{metrics} is specified).
#' @export
filter_sim_study <- function(obj,
                             id = NULL,
                             method = NULL,
                             n_train = NULL,
                             design_type = NULL,
                             NSR = NULL,
                             replication = NULL,
                             fold_size = NULL,
                             source = NULL,
                             failure_type = NULL,
                             metrics = NULL,
                             expr = NULL,
                             rerank = FALSE) {
  if (!inherits(obj, "duq_sim_study")) {
    stop("Please provide a duq_sim_study object (from process_sim_study()).", call. = FALSE)
  }
  # Backwards compatability preserved
  ids <- id
  methods <- method

  # Get df
  df <- obj$df

  # Build logical mask (start all TRUE)
  keep <- rep(TRUE, nrow(df))

  # Helper to filter by vector if column exists
  filter_vec <- function(col, vals) {
    if (!col %in% names(df)) return(rep(TRUE, nrow(df)))
    df[[col]] %in% vals
  }

  if (!is.null(ids))          keep <- keep & filter_vec("id", ids)
  if (!is.null(methods))      keep <- keep & filter_vec("method", methods)
  if (!is.null(n_train))      keep <- keep & filter_vec("n_train", n_train)
  if (!is.null(design_type))  keep <- keep & filter_vec("design_type", design_type)
  if (!is.null(NSR))          keep <- keep & filter_vec("NSR", NSR)
  if (!is.null(replication))  keep <- keep & filter_vec("replication", replication)
  if (!is.null(fold_size))    keep <- keep & filter_vec("fold_size", fold_size)
  if (!is.null(source))       keep <- keep & filter_vec("source", source)
  if (!is.null(failure_type)) keep <- keep & filter_vec("failure_type", failure_type)

  # Apply expr if provided
  if (!is.null(expr)) {
    expr_keep <- eval(substitute(expr), envir = df, enclos = parent.frame())
    if (!is.logical(expr_keep) || length(expr_keep) != nrow(df)) {
      stop("expr must evaluate to a logical vector of length nrow(df).", call. = FALSE)
    }
    keep <- keep & expr_keep
  }

  df <- df[keep, , drop = FALSE]

  # Restrict to selected metrics if requested
  if (!is.null(metrics)) {
    core_cols <- c("id","method","sim_scenario","replication","source",
                   obj$meta$scenario_keys)
    core_cols <- intersect(core_cols, names(df))
    keep_cols <- union(core_cols, metrics)
    df <- df[, intersect(keep_cols, names(df)), drop = FALSE]
  }

  # Warn if nothing left
  if (nrow(df) == 0) {
    warning("Filtering removed all rows.", call. = FALSE)
  }

  # Update metadata
  obj$df <- df
  obj$meta$n_rows <- nrow(df)

  # Optional rerank
  if (rerank) {
    rank_cols <- grep("_rank$", names(df), value = TRUE)
    if (length(rank_cols) == 0) {
      warning("rerank=TRUE but no *_rank columns found; skipping re-ranking.")
    } else {
      metrics_to_rank <- sub("_rank$", "", rank_cols)
      for (m in metrics_to_rank) {
        obj <- rank_sim_study(obj, metric = m)
      }
    }
  }

  obj
}


#' Collapse a duq_sim_study across replications (or other scenario keys)
#'
#' Averages (or otherwise aggregates) metrics across specified scenario keys,
#' typically dropping \code{replication} to get per-scenario summaries.
#'
#' @param obj A \code{duq_sim_study} object from \code{process_sim_study()}.
#' @param collapse_by Character vector of columns to collapse over. Default is "replication".
#' @param fun Aggregation function to apply (default = mean).
#' @param weight_by_folds Should \code{fun} be weighted by \code{fold_size} when applicable?
#' @param ... Additional arguments passed to \code{fun}.
#'
#' @return A collapsed \code{duq_sim_study} object with updated metadata.
#' @export
collapse_sim_study <- function(obj,
                               collapse_by = "replication",
                               fun = mean,
                               weight_by_folds=TRUE,
                               ...) {
  if (!inherits(obj, "duq_sim_study")) {
    stop("Please provide a duq_sim_study object (from process_sim_study()).", call. = FALSE)
  }

  df <- obj$df
  collapse_by <- intersect(collapse_by, names(df))

  # Scenario keys to keep
  group_keys <- setdiff(c("id","method", obj$meta$scenario_keys), collapse_by)
  group_keys <- intersect(group_keys, names(df))

  # Metric columns = all numeric columns not in group_keys
  metric_cols <- setdiff(names(df), c(group_keys, collapse_by))

  # Aggregate
  split_idx <- split(seq_len(nrow(df)), df[group_keys], drop = TRUE)
  rows <- lapply(split_idx, function(idx) {
    row <- df[idx[1], group_keys, drop = FALSE]
    for (m in metric_cols) {
      if (is.numeric(df[[m]])) {
        if (weight_by_folds && all(df$source[idx] == "data")) {
          wts <- df$fold_size[idx]
          vals <- rep(df[[m]][idx], times = wts)
          row[[m]] <- fun(vals, ...)
        } else {
          row[[m]] <- fun(df[[m]][idx], ...)
        }
      } else {
        if (m == "failure_type"){
          vals <- df[[m]][idx]
          if (all(vals == "none")) {
            row[[m]] <- "none"
          } else if (all(vals != "none")) {
            row[[m]] <- "all"
          } else {
            row[[m]] <- "some"
          }
        }else{
          row[[m]] <- df[[m]][idx[1]]  # carry first value for non-numeric cols
        }
      }
    }
    row
  })
  df_new <- do.call(rbind, rows)

  # Update metadata
  meta <- obj$meta
  meta$n_rows <- nrow(df_new)
  meta$scenario_keys <- setdiff(meta$scenario_keys, collapse_by)
  meta$collapsed <- collapse_by

  structure(list(df = df_new, meta = meta), class = "duq_sim_study")
}



