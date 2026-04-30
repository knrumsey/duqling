#' Summarize performance of methods in a duq_sim_study
#'
#' Produces summary tables (by default, one row per method) with flexible
#' metrics such as averages, win rates, top-K rates, effective rates,
#' soft-relative averages, and failure rates.
#'
#' @param obj A \code{duq_sim_study} object from \code{process_sim_study()}.
#' @param summarize Character vector of metrics to summarize via \code{summary_fun}.
#' @param summary_fun A function applied to each column in \code{summarize}.
#'   Must return either a single value or a named vector. Names become suffixes
#'   in the resulting columns.
#' @param win_rate Metrics for which to compute the proportion of rank-1 cases.
#' @param topK_rate Metrics for which to compute the proportion of rank <= K.
#' @param K Integer vector of K values for top-K rate (default = 5).
#' @param effective_rate Metrics for which to compute "good-enough" rates.
#' @param tolerance Numeric vector of tolerances (same length as effective_rate).
#' @param failure_rate Logical; if TRUE, include proportion of times
#'   \code{failure_type != "none"}.
#' @param soft_rel Metrics for which to compute average soft-relative scores.
#' @param epsilon Numeric vector of epsilons (same length as soft_rel).
#' @param upper_bound Numeric vector of upper bounds (same length as soft_rel)
#' @param group_by Character vector of columns to split summaries by
#'   (e.g. \code{c("n_train","NSR")}). Default = NULL (collapse across all scenarios).
#' @param split_tables Logical; if TRUE (default), return a list of tables
#'   (one per group). If FALSE, return a single combined data.frame with
#'   grouping columns retained.
#' @param ties_method Character vector for how to handle ties for ranking. Default "min".
#' @param latex Logical; if TRUE, return LaTeX tables via \code{stargazer}.
#'   Requires \pkg{stargazer}.
#' @param ... Additional arguments passed to \code{stargazer()}.
#'
#' @return A data.frame (if \code{split_tables = FALSE}) or a list of
#'   data.frames/LaTeX tables (if \code{split_tables = TRUE}).
#' @export
summarize_sim_study <- function(obj,
                                summarize = "time",
                                summary_fun = function(val) c("mean" = mean(val)),
                                win_rate = "CRPS",
                                topK_rate = NULL,
                                K = 5,
                                effective_rate = NULL,
                                tolerance = NULL,
                                failure_rate = FALSE,
                                soft_rel = "CRPS",
                                epsilon = 0.001,
                                upper_bound = Inf,
                                group_by = NULL,
                                split_tables = TRUE,
                                ties_method="min",
                                latex = FALSE,
                                ...) {
  if (!inherits(obj, "duq_sim_study")) {
    stop("Please provide a duq_sim_study object (from process_sim_study()).", call. = FALSE)
  }

  # Ensure derived metrics exist if requested in "summarize"
  for (m in summarize) {
    obj <- ensure_metric(obj, m, ties_method = ties_method)
  }
  df <- obj$df

  # ---- Helper to build one summary table given a subset of df ----
  summarize_one <- function(dsub) {
    out <- data.frame(method = unique(dsub$method), stringsAsFactors = FALSE)

    # Summarize metrics with summary_fun
    for (m in summarize) {
      if (!m %in% names(dsub)) next
      vals <- split(dsub[[m]], dsub$method)
      res <- lapply(vals, function(xx) {
        ans <- summary_fun(xx)
        if (is.null(names(ans))) names(ans) <- deparse(substitute(summary_fun))
        ans
      })
      res <- do.call(rbind, res)
      colnames(res) <- paste0(m, "_", colnames(res))
      out <- cbind(out, res[match(out$method, rownames(res)), , drop=FALSE])
    }

    # Win rate
    if (!is.null(win_rate)) {
      obj_sub <- list(df = dsub, meta = obj$meta)
      class(obj_sub) <- "duq_sim_study"
      for (m in win_rate) {
        obj_sub <- rank_sim_study(obj_sub, metric = m, ties_method = ties_method)
        rank_col <- paste0(m, "_rank")
        rates <- tapply(obj_sub$df[[rank_col]] == 1,
                        obj_sub$df$method, mean, na.rm = TRUE)
        out[[paste0(m, "_winrate")]] <- as.numeric(rates[out$method])
      }
    }

    # Top-K rate
    if (!is.null(topK_rate)) {
      obj_sub <- list(df = dsub, meta = obj$meta)
      class(obj_sub) <- "duq_sim_study"
      for (m in topK_rate) {
        obj_sub <- rank_sim_study(obj_sub, metric = m, ties_method=ties_method)
        rank_col <- paste0(m, "_rank")
        for (k in K) {
          rates <- tapply(obj_sub$df[[rank_col]] <= k,
                          obj_sub$df$method, mean, na.rm = TRUE)
          out[[paste0(m, "_top", k, "rate")]] <- as.numeric(rates[out$method])
        }
      }
    }

    # Effective rate
    if (!is.null(effective_rate)) {
      stopifnot(length(tolerance) == length(effective_rate))
      for (j in seq_along(effective_rate)) {
        m <- effective_rate[j]
        tol <- tolerance[j]
        vals <- split(dsub[[m]], dsub$sim_scenario)
        mins <- vapply(vals, min, numeric(1), na.rm = TRUE)
        ok <- dsub[[m]] <= mins[dsub$sim_scenario] * (1 + tol)
        rates <- tapply(ok, dsub$method, mean, na.rm = TRUE)
        out[[paste0(m, "_effective", tol)]] <- as.numeric(rates[out$method])
      }
    }

    # Failure rate
    if (failure_rate && "failure_type" %in% names(dsub)) {
      rates <- tapply(dsub$failure_type != "none",
                      dsub$method, mean, na.rm = TRUE)
      out[["failure_rate"]] <- as.numeric(rates[out$method])
    }

    # Soft relative
    if (!is.null(soft_rel)) {
      if (length(epsilon) == 1) epsilon <- rep(epsilon, length(soft_rel))
      for (j in seq_along(soft_rel)) {
        m <- soft_rel[j]
        eps <- epsilon[j]
        vals <- split(dsub[[m]], dsub$sim_scenario)
        mins <- vapply(vals, min, numeric(1), na.rm = TRUE)
        score <- (dsub[[m]] + eps) / (mins[dsub$sim_scenario] + eps)
        score <- pmin(score, upper_bound)
        rates <- tapply(score, dsub$method, mean, na.rm = TRUE)
        out[[paste0(m, "_softrel")]] <- as.numeric(rates[out$method])
      }
    }

    out
  }

  # ---- Split into groups if requested ----
  if (is.null(group_by)) {
    res <- summarize_one(df)
  } else {
    split_idx <- split(seq_len(nrow(df)), df[group_by], drop = TRUE)
    res <- lapply(split_idx, function(idx) summarize_one(df[idx, , drop=FALSE]))
    if (!split_tables) {
      # Add group-by columns back in
      for (nm in names(res)) {
        parts <- strsplit(nm, "\\.")[[1]]
        for (j in seq_along(group_by)) {
          res[[nm]][[group_by[j]]] <- parts[j]
        }
      }
      res <- do.call(rbind, res)
    }
  }

  rownames(res) <- NULL

  # ---- LaTeX output ----
  if (latex) {
    if (!requireNamespace("stargazer", quietly = TRUE)) {
      stop("Please install stargazer to use latex=TRUE.", call. = FALSE)
    }
    if (!is.data.frame(res) && split_tables) {
      # List of tables
      ltx <- lapply(res, function(tbl) {
        capture.output(
          stargazer::stargazer(tbl, summary = FALSE, rownames = FALSE)
        )
      })
    } else {
      # Single table
      ltx <- capture.output(
        stargazer::stargazer(res, summary = FALSE, rownames = FALSE)
      )
    }
    cat(ltx, sep = "\n")
    invisible(res)
  }else{
    return(res)
  }
}

#' Boxplots (or violins) for a duq_sim_study metric
#'
#' Visualize the distribution of a metric across methods, optionally grouped
#' by a scenario variable.
#'
#' @param obj A \code{duq_sim_study} object from \code{process_sim_study()}.
#' @param metric Character; a single metric column to plot.
#' @param y_scale_fun Optional transformation function for the y-axis
#'   (e.g. \code{log10}, \code{sqrt}, \code{identity}). Default = NULL.
#' @param ylim Optional numeric vector of length 2 with y-axis limits.
#' @param show_mean Logical; if TRUE, add a point for the mean in each box.
#' @param notches Logical; if TRUE, add notches to boxplots.
#' @param outliers Logical; if FALSE, hide outlier points in boxplots.
#' @param violin Logical; if TRUE, use violin plots instead of boxplots.
#' @param sort_by Either \code{"performance"}, \code{"alphabetical"}, or a
#'   character vector of method names giving the explicit order. If
#'   \code{"performance"}, methods are ordered by the median of the metric.
#'   Partial matching is allowed.
#' @param title Plot title. If NULL (default), use "Distribution of <metric>".
#'   If FALSE, suppress the title.
#' @param group_by Optional column name for grouping (e.g. \code{"NSR"} or
#'   \code{"source"}). If provided, plots grouped distributions side by side
#'   within each method.
#' @param ... additional arguments for ensure_metric.
#'
#' @return A \code{ggplot} object.
#' @export
boxplots_sim_study <- function(obj,
                               metric="CRPS",
                               y_scale_fun = NULL,
                               ylim = NULL,
                               show_mean = FALSE,
                               notches = FALSE,
                               outliers = TRUE,
                               violin = FALSE,
                               sort_by = c("performance","alphabetical"),
                               title = NULL,
                               group_by = NULL,
                               ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install ggplot2 to use this plotting function.", call. = FALSE)
  }
  if (!inherits(obj, "duq_sim_study")) {
    stop("Please provide a duq_sim_study object (from process_sim_study()).", call. = FALSE)
  }

  # Ensure the requested metric column exists
  obj <- ensure_metric(obj, metric, ...)
  df  <- obj$df

  if (!metric %in% names(df)) {
    stop("Metric '", metric, "' not found in data.", call. = FALSE)
  }


  # Check scale function
  if(is.null(y_scale_fun)) y_scale_fun = "identity"

  scale_type <- "ggscale"
  if(is.function(y_scale_fun)){
    fname <- deparse(substitute(y_scale_fun))
    if (fname %in% c("identity","log","log10","log2","sqrt","reverse")) {
      scale_type <- "ggscale"
      y_scale_fun <- fname
    } else {
      scale_type <- "custom"
    }
  }

  # Drop rows with missing metric
  dsub <- df[!is.na(df[[metric]]), ]

  # Handle method ordering robustly
  mode <- tryCatch(
    match.arg(sort_by, c("performance","alphabetical")),
    error = function(e) NULL
  )

  if (!is.null(mode)) {
    # sort_by is a mode ("performance" or "alphabetical"); default vector picks "performance"
    if (mode == "performance") {
      medians <- tapply(dsub[[metric]], dsub$method, median, na.rm = TRUE)
      dsub$method <- factor(dsub$method, levels = names(sort(medians)))
    } else { # "alphabetical"
      dsub$method <- factor(dsub$method, levels = sort(unique(dsub$method)))
    }
  } else {
    # sort_by is an explicit vector of methods (manual order)
    keep <- dsub$method %in% sort_by
    dsub <- dsub[keep, , drop = FALSE]
    dsub$method <- factor(dsub$method, levels = sort_by)
  }
  n_methods <- length(levels(dsub$method))

  # Build aes
  if(scale_type == "custom"){
    #pretty_labs <- pretty(dsub[[metric]], n = 5)   # original scale
    #dsub[[metric]] <- y_scale_fun(dsub[[metric]])  # transform the data
    br_fun <- scales::pretty_breaks(n = 5)
    pretty_labs <- br_fun(dsub[[metric]])          # labels on original scale
    dsub[[metric]] <- y_scale_fun(dsub[[metric]])
  }

  aes_args <- list(x = quote(method), y = as.name(metric))
  if (!is.null(group_by) && group_by %in% names(dsub)) {
    if(!is.factor(dsub[[group_by]])){
      dsub[[group_by]] <- factor(dsub[[group_by]])
    }
    aes_args$fill <- as.name(group_by)
    aes_args$group <- interaction(dsub$method, dsub[[group_by]], drop = TRUE)
  }

  # Make plot
  p <- ggplot2::ggplot(dsub, do.call(ggplot2::aes, aes_args)) +
    ggplot2::theme_minimal(base_size = 14)

  if (n_methods < 8) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  } else {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  }

  if (violin) {
    p <- p + ggplot2::geom_violin(trim = FALSE, position = "dodge")
  } else {
    p <- p + ggplot2::geom_boxplot(
      notch = notches,
      outlier.shape = if (outliers) 19 else NA,
      position = if (!is.null(group_by)) ggplot2::position_dodge(width = 0.8) else "dodge"
    )
  }

  if (show_mean) {
    p <- p + ggplot2::stat_summary(
      fun = mean, geom = "point",
      shape = 23, size = 1, fill = "orange",
      position = if (!is.null(group_by)) ggplot2::position_dodge(width = 0.8) else "identity"
    )
  }

  # Y-axis scale and limits
  if(scale_type == "custom"){
    p <- p + ggplot2::scale_y_continuous(
      breaks = y_scale_fun(pretty_labs),  # tick positions after transform
      labels = pretty_labs
    )
  }else{
    p <- p + ggplot2::scale_y_continuous(trans = y_scale_fun)
  }

  if (!is.null(ylim)) {
    p <- p + ggplot2::coord_cartesian(ylim = ylim)
  }

  # Use a human-friendly label for the y axis
  p <- p + ggplot2::labs(y = metric_label(metric))

  # Title
  if (is.null(title)) {
    p <- p + ggplot2::ggtitle(paste("Distribution of", metric))
  } else if (is.character(title)) {
    p <- p + ggplot2::ggtitle(title)
  } # if FALSE, suppress
  p
}


#' Cumulative Rank Plot for Emulator Performance
#'
#' Create a cumulative rank plot for a given performance metric
#' (e.g., CRPS, RMSE, time).
#'
#' @param obj A \code{duq_sim_study} object from \code{process_sim_study()}.
#' @param metric Character; the name of the metric to rank by (e.g. "CRPS").
#' @param ties_method Passed to \code{rank_sim_study()} (how to handle ties).
#'   Default is "min".
#' @param use_shapes Logical; whether to use different point shapes in addition
#'   to color. Default is TRUE.
#' @param title Optional title for plot. If NULL (default), generates a title
#'   from \code{metric}. If FALSE, suppresses the title.
#' @param ... Additional arguments passed to ensure_metric
#'
#' @return A \code{ggplot} object.
#' @export
rankplot_sim_study <- function(obj,
                               metric = "CRPS",
                               use_shapes = TRUE,
                               title = NULL,
                               ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install ggplot2 to use this plotting function.", call. = FALSE)
  }
  if (!inherits(obj, "duq_sim_study")) {
    stop("Please provide a duq_sim_study object (from process_sim_study()).", call. = FALSE)
  }

  # Ensure rank column exists
  # Ensure metric column exists (rank or derived)
  rank_col <- paste0(metric, "_rank")
  obj <- ensure_metric(obj, rank_col, ...)
  df  <- obj$df
  if (!rank_col %in% names(df)) {
    stop("Failed to compute ranks for metric '", metric, "'.", call. = FALSE)
  }

  n_by_scenario <- tapply(df[[rank_col]], df$sim_scenario, length)
  if (length(unique(n_by_scenario)) > 1) {
    warning("Not all scenarios have the same number of methods.
           Some methods may have been ranked against fewer competitors.
           Consider filtering to a consistent subset with filter_sim_study().")
  }

  # Build cumulative rank distribution
  tab <- table(df$method, df[[rank_col]])
  pct_cumul <- t(apply(tab, 1, function(x) cumsum(x) / sum(x)))

  methods <- rownames(pct_cumul)
  rank_vals <- as.integer(colnames(pct_cumul))

  cumul_ranks <- data.frame(
    method = rep(methods, each = length(rank_vals)),
    rank   = rep(rank_vals, times = length(methods)),
    pct    = as.vector(t(pct_cumul)),
    stringsAsFactors = FALSE
  )

  # Order methods by area under the cumulative curve
  auc <- tapply(cumul_ranks$pct, cumul_ranks$method, sum)
  method_order <- names(sort(auc, decreasing = TRUE))
  cumul_ranks$method <- factor(cumul_ranks$method, levels = method_order)

  # Plot
  shapes_vec <- c(1, 0, 2, 5, 4)
  p <- ggplot2::ggplot(cumul_ranks,
                       ggplot2::aes(x = rank, y = pct, color = method)) +
    ggplot2::geom_line(size = 1)

  if (use_shapes) {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(shape = method), size = 2) +
      ggplot2::scale_shape_manual(values = rep(shapes_vec, length.out = length(unique(cumul_ranks$method))))
  }

  plot_title <- if (is.null(title)) {
    paste(metric_label(metric), "Results (Rank-Based)")
  } else if (identical(title, FALSE)) {
    NULL
  } else {
    title
  }

  p <- p +
    ggplot2::scale_color_viridis_d(option = "turbo") +
    ggplot2::labs(
      x = "Rank or better",
      y = "% of scenarios",
      title = plot_title,
      color = "Method",
      shape = if (use_shapes) "Method" else NULL
    ) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
    ggplot2::theme_minimal(base_size = 16)

  p
}


#' Pareto Plot of Accuracy vs Time
#'
#' Plot tradeoff between performance and runtime for each method using
#' derived metrics (e.g. ranks, relative scores, normalized scores).
#'
#' @param obj A \code{duq_sim_study} object.
#' @param metric Length-2 character vector. The performance and runtime metrics
#'   to compare. Default is \code{c("CRPS_auc","time_auc")}.
#' @param show_pfront Logical; if TRUE (default), draw the Pareto front.
#' @param show_legend Logical; if TRUE (default), show legend for Pareto front.
#' @param xlim,ylim Optional numeric vectors of length 2 to control axis limits.
#' @param show_direction Logical; if TRUE (default), append arrows to axis labels
#'   when metric direction is known (e.g. "← Worse   Better →").
#' @param title Plot title. If NULL, auto-generate; if FALSE, suppress.
#' @param ... Additional arguments passed to \code{ensure_metric()}.
#'
#' @return A \code{ggplot} object.
#' @export
paretoplot_sim_study <- function(obj,
                                 metric = c("CRPS_auc","time_auc"),
                                 show_pfront = TRUE,
                                 show_legend = TRUE,
                                 xlim = NULL,
                                 ylim = NULL,
                                 show_direction = TRUE,
                                 title = NULL,
                                 ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install ggplot2 to use this plotting function.", call. = FALSE)
  }
  if (!inherits(obj, "duq_sim_study")) {
    stop("Please provide a duq_sim_study object (from process_sim_study()).", call. = FALSE)
  }
  stopifnot(length(metric) == 2)

  # Ensure metrics exist
  #for (m in metric) {
  #  obj <- ensure_metric(obj, m, ...)
  #}
  obj <- ensure_metric(obj, metric, ...)
  df <- obj$df

  # Compute average across scenarios for each method
  method_stats <- aggregate(df[metric],
                            by = list(method = df$method),
                            FUN = mean, na.rm = TRUE)

  # Helper: direction of "better"
  metric_direction <- function(m) {
    if (grepl("_auc$", m)) return("up")
    if (grepl("_rank$", m)) return("down")
    if (grepl("_rel", m))  return("down")
    if (grepl("_norm$", m)) return("down")
    if (grepl("CRPS|RMSE|FVU|MIS", m, ignore.case = TRUE)) return("down")
    if (grepl("time", m, ignore.case = TRUE)) return("down")
    return("unknown")
  }

  dir_x <- metric_direction(metric[2])
  dir_y <- metric_direction(metric[1])

  # If metric unknown, disable direction arrows
  if ("unknown" %in% c(dir_x, dir_y)) {
    show_direction <- FALSE
  }

  # Prepare labels
  make_axis_lab <- function(m, dir) {
    if (!show_direction) return(metric_label(m))
    if (dir == "up") {
      paste0(metric_label(m), "\n", "\u2190 Worse     Better \u2192")
    } else {
      paste0(metric_label(m), "\n", "\u2190 Better     Worse \u2192")
    }
  }

  x_lab <- make_axis_lab(metric[2], dir_x)
  y_lab <- make_axis_lab(metric[1], dir_y)

  # Transform for Pareto detection (so bigger=better)
  pareto_df <- method_stats
  pareto_df$x_val <- pareto_df[[metric[2]]]
  pareto_df$y_val <- pareto_df[[metric[1]]]
  if (dir_x == "down") pareto_df$x_val <- -pareto_df$x_val
  if (dir_y == "down") pareto_df$y_val <- -pareto_df$y_val

  # Pareto front detection
  is_dominated <- function(i, mat) {
    any(mat[-i, "x_val"] >= mat[i, "x_val"] &
          mat[-i, "y_val"] >= mat[i, "y_val"] &
          (mat[-i, "x_val"] > mat[i, "x_val"] |
             mat[-i, "y_val"] > mat[i, "y_val"]))
  }
  pareto_idx <- which(!sapply(seq_len(nrow(pareto_df)), function(i) is_dominated(i, pareto_df)))
  pareto_df$pareto <- FALSE
  pareto_df$pareto[pareto_idx] <- TRUE

  # Highlight color when show_pfront
  pareto_df$plot_col <- if (show_pfront) {
    ifelse(pareto_df$pareto, "dodgerblue3", "black")
  } else {
    rep("black", nrow(pareto_df))
  }

  # Plot base
  p <- ggplot2::ggplot(pareto_df,
                       ggplot2::aes(x = .data[[metric[2]]],
                                    y = .data[[metric[1]]])) +
    ggplot2::geom_point(ggplot2::aes(shape = pareto), size = 3, color = "#1f78b4") +
    ggplot2::theme_minimal(base_size = 14)

  if (show_pfront && sum(pareto_df$pareto) > 1) {
    pf <- pareto_df[pareto_df$pareto, ]

    # Order along the x-axis according to direction
    if (dir_x == "up") {
      pf <- pf[order(pf[[metric[2]]]), ]
    } else {
      pf <- pf[order(-pf[[metric[2]]]), ]
    }

    p <- p + ggplot2::geom_path(
      data = pf,
      ggplot2::aes(x = .data[[metric[2]]], y = .data[[metric[1]]]),
      color = "black",
      linewidth = 0.5,
      linetype = "dashed",
      inherit.aes = FALSE
    )
  }

  if (show_legend) {
    p <- p + ggplot2::scale_shape_manual(
      values = c(`FALSE` = 16, `TRUE` = 17),
      guide = ggplot2::guide_legend(title = "Pareto Front")
    )
  } else {
    p <- p + ggplot2::guides(shape = "none")
  }

  # Add labels
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p + ggrepel::geom_text_repel(
      ggplot2::aes(label = method, color = plot_col),
      size = 3.5,
      max.overlaps = Inf,
      show.legend = FALSE
    )
  } else {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = method, color = plot_col),
      vjust = -0.5, hjust = 0.5,
      size = 3.5,
      check_overlap = TRUE,
      show.legend = FALSE
    ) +
      ggplot2::coord_cartesian(clip = "off")
  }

  # Title
  plot_title <- if (is.null(title)) {
    paste("Pareto Plot:", metric_label(metric[1]), "vs", metric_label(metric[2]))
  } else if (identical(title, FALSE)) {
    NULL
  } else {
    title
  }

  p <- p + ggplot2::labs(
    x = x_lab,
    y = y_lab,
    title = plot_title
  )

  # Axis limits
  if (!is.null(xlim)) {
    p <- p + ggplot2::coord_cartesian(xlim = xlim)
  }
  if (!is.null(ylim)) {
    p <- p + ggplot2::coord_cartesian(ylim = ylim)
  }

  p
}


#' Heatmap of Method Performance Across Simulation Scenarios
#'
#' Visualize the average performance of methods across grouped simulation
#' scenarios using a heatmap. Supports both rank-based and raw metrics
#' (and other derived metrics like relative or normalized scores).
#'
#' @param obj A \code{duq_sim_study} object from \code{process_sim_study()}.
#' @param metric Character; the performance metric column to plot (default "CRPS_rank").
#'   Can include derived suffixes like \code{"_rank"}, \code{"_auc"},
#'   \code{"_rel"}, \code{"_rel_log"}, or \code{"_norm"}.
#' @param group_by Character vector of columns that define each simulation
#'   scenario group (default = "id").
#' @param show_values Logical; if TRUE, overlay numeric values (rounded to 2 decimals).
#'   If NULL (default), values are shown only for small grids (<= 8 × 8).
#' @param color_scale Either a single viridis option
#'   ("magma","inferno","plasma","viridis","cividis","rocket","mako","turbo"),
#'   or a 2-element character vector of colors for a low-high gradient.
#'   Default = "turbo".
#' @param orientation Either "vertical" (methods on x-axis, scenarios on y-axis)
#'   or "horizontal" (scenarios on x-axis, methods on y-axis).
#' @param y_scale_fun Optional function applied to aggregated values (e.g., discretizer).
#' @param colorbar_labels Optional list with \code{$breaks} and \code{$labels}
#'   for the colorbar.
#' @param discrete_colorbar Logical; if TRUE, force discrete colorbar (values converted
#'   to factor). Default = FALSE.
#' @param title Optional plot title. If NULL, auto-generate; if FALSE, suppress.
#'
#' @return A \code{ggplot} heatmap.
#' @export
heatmap_sim_study <- function(obj,
                              metric = "CRPS_rank",
                              group_by = "id",
                              show_values = NULL,
                              color_scale = "turbo",
                              orientation = c("vertical", "horizontal"),
                              y_scale_fun = NULL,
                              colorbar_labels = NULL,
                              discrete_colorbar = FALSE,
                              title = NULL,
                              ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install ggplot2 to use this plotting function.", call. = FALSE)
  }
  if (!inherits(obj, "duq_sim_study")) {
    stop("Please provide a duq_sim_study object (from process_sim_study()).", call. = FALSE)
  }

  orientation = match.arg(orientation)

  # Ensure metric exists
  obj <- ensure_metric(obj, metric, ...)
  df  <- obj$df

  if (!all(group_by %in% names(df))) {
    stop("Some group_by columns are missing in df: ", paste(group_by, collapse=", "))
  }

  # Aggregate by group_id and method
  df$group_id <- apply(df[, group_by, drop=FALSE], 1, paste, collapse=" \u00D7 ")
  agg <- stats::aggregate(df[[metric]],
                          by = list(group_id = df$group_id, method = df$method),
                          FUN = mean, na.rm = TRUE)
  colnames(agg)[3] <- "value"

  # Optional transform (e.g. discretizer)
  if (!is.null(y_scale_fun)) {
    agg$value <- y_scale_fun(agg$value)
  }

  # Order methods by average performance (best at bottom if smaller=better)
  method_means <- aggregate(value ~ method, data = agg, mean, na.rm = TRUE)
  method_levels <- method_means$method[order(method_means$value)]
  n_methods <- length(method_levels)

  agg$method <- factor(agg$method, levels = method_levels)
  if(!is.factor(agg$group_id)){
    agg$group_id <- factor(agg$group_id, levels = unique(agg$group_id))
  }


  # Decide whether to show values
  if (is.null(show_values)) {
    n_rows <- length(levels(agg$group_id))
    n_cols <- length(levels(agg$method))
    show_values <- (n_rows <= 8 && n_cols <= 8)
  }

  # Axis labels
  id_label <- switch(obj$meta$source_type,
                     "fun"   = "Function",
                     "data"  = "Dataset",
                     "mixed" = "Function/Dataset",
                     "unknown" = "Function/Dataset")
  group_pretty_map <- c(
    "id" = id_label,
    "n_train" = "Training Size",
    "NSR" = "Noise Ratio",
    "design_type" = "Design",
    "replication" = "Replication",
    "fold_size" = "Fold Size",
    "source" = "Source"
  )
  group_by_pretty <- group_by
  group_by_pretty[group_by %in% names(group_pretty_map)] <-
    group_pretty_map[group_by[group_by %in% names(group_pretty_map)]]

  # Color scale
  if (discrete_colorbar) {
    agg$value <- factor(agg$value)

    if (is.character(color_scale) && length(color_scale) == 1 &&
        color_scale %in% c("magma","inferno","plasma","viridis","cividis",
                           "rocket","mako","turbo")) {
      fill_scale <- ggplot2::scale_fill_viridis_d(
        option = color_scale,
        na.value = "grey90",
        breaks = if (!is.null(colorbar_labels)) colorbar_labels$breaks else ggplot2::waiver(),
        labels = if (!is.null(colorbar_labels)) colorbar_labels$labels else ggplot2::waiver()
      )
    } else if (is.character(color_scale) && length(color_scale) == 2) {
      fill_scale <- ggplot2::scale_fill_manual(
        values = color_scale,
        na.value = "grey90"
      )
    } else {
      stop("Invalid color_scale for discrete colorbar.")
    }

  } else {
    limits <- range(as.numeric(agg$value), na.rm = TRUE)

    if (is.character(color_scale) && length(color_scale) == 1 &&
        color_scale %in% c("magma","inferno","plasma","viridis","cividis",
                           "rocket","mako","turbo")) {
      fill_scale <- ggplot2::scale_fill_viridis_c(
        option = color_scale,
        direction = -1,
        na.value = "grey90",
        limits = limits,
        breaks = if (!is.null(colorbar_labels)) colorbar_labels$breaks else ggplot2::waiver(),
        labels = if (!is.null(colorbar_labels)) colorbar_labels$labels else ggplot2::waiver()
      )
    } else if (is.character(color_scale) && length(color_scale) == 2) {
      fill_scale <- ggplot2::scale_fill_gradient(
        low = color_scale[1],
        high = color_scale[2],
        na.value = "grey90",
        limits = limits,
        breaks = if (!is.null(colorbar_labels)) colorbar_labels$breaks else ggplot2::waiver(),
        labels = if (!is.null(colorbar_labels)) colorbar_labels$labels else ggplot2::waiver()
      )
    } else {
      stop("Invalid color_scale: must be a viridis option or a 2-element color vector.")
    }
  }

  # Orientation
  if (orientation == "vertical") {
    x_angle <- ifelse(n_methods <= 8, 45, 90)
    aes_args <- ggplot2::aes(x = method, y = group_id, fill = value)
    x_label <- "Method"
    y_label <- paste(group_by_pretty, collapse=" \u00D7 ")
    theme <- ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 8),
        axis.text.x = ggplot2::element_text(angle = x_angle, hjust = 1, vjust = 0.5)
      )
  } else if (orientation == "horizontal") {
    aes_args <- ggplot2::aes(x = group_id, y = method, fill = value)
    x_label <- paste(group_by_pretty, collapse=" \u00D7 ")
    y_label <- "Method"
    theme <- ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 8),
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
      )
  } else {
    stop("orientation must be either 'vertical' or 'horizontal'")
  }

  # Title
  plot_title <- if (is.null(title)) {
    paste("Average", metric_label(metric), "by", paste(group_by_pretty, collapse=" \u00D7 "))
  } else if (identical(title, FALSE)) {
    NULL
  } else {
    title
  }

  # Build plot
  p <- ggplot2::ggplot(agg, aes_args) +
    ggplot2::geom_tile(color = "white") +
    fill_scale +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      fill = metric_label(metric),
      title = plot_title
    ) +
    theme

  if (show_values) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", as.numeric(value))),
      size = 3
    )
  }

  p
}




#' Performance Profile Plot for Emulator Comparison
#'
#' Create a performance profile plot for a given metric. For each method and
#' each simulation scenario, the metric is divided by the best value attained in
#' that scenario. The resulting curve shows the fraction of scenarios for which
#' a method is within a factor \eqn{\tau} of the best method.
#'
#' @param obj A \code{duq_sim_study} object from \code{process_sim_study()}.
#' @param metric Character; the name of the metric to compare (e.g. "CRPS").
#'   May refer to a derived metric handled by \code{ensure_metric()}.
#' @param tau_lim Optional numeric vector of length 2 giving the range of
#'   \eqn{\tau} values to plot. If \code{NULL} (default), limits are chosen from
#'   the observed relative values.
#' @param tau_grid Optional numeric vector of \eqn{\tau} values at which to
#'   evaluate the profile. If \code{NULL} (default), a grid is constructed
#'   automatically from \code{tau_lim}.
#' @param tau_log Logical; should the \eqn{\tau} axis be plotted on the log10
#'   scale? Default is \code{TRUE}.
#' @param use_shapes Logical; whether to use different point shapes in addition
#'   to color. Default is \code{TRUE}.
#' @param relative_epsilon Optional numeric smoothing constant passed to
#'   \code{relativize_sim_study()}. If not \code{NULL}, relative values are
#'   computed as \code{(metric + epsilon) / (best + epsilon)}.
#' @param color_scale Character; viridis option for line colors. Default is
#'   \code{"turbo"}.
#' @param title Optional title for plot. If \code{NULL} (default), generates a
#'   title from \code{metric}. If \code{FALSE}, suppresses the title.
#' @param ... Additional arguments passed to \code{ensure_metric()}.
#'
#' @return A \code{ggplot} object.
#' @export
perfprofile_sim_study <- function(obj,
                                  metric = "CRPS",
                                  tau_lim = NULL,
                                  tau_grid = NULL,
                                  tau_log = TRUE,
                                  use_shapes = TRUE,
                                  relative_epsilon = NULL,
                                  color_scale = "turbo",
                                  title = NULL,
                                  ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install ggplot2 to use this plotting function.", call. = FALSE)
  }
  if (!inherits(obj, "duq_sim_study")) {
    stop("Please provide a duq_sim_study object (from process_sim_study()).", call. = FALSE)
  }

  # Ensure metric exists, then compute relative version
  obj <- ensure_metric(obj, metric, ...)
  obj <- relativize_sim_study(obj,
                              metric = metric,
                              epsilon = relative_epsilon,
                              log = FALSE)

  df <- obj$df
  rel_col <- paste0(metric, "_rel")

  if (!rel_col %in% names(df)) {
    stop("Failed to compute relative values for metric '", metric, "'.", call. = FALSE)
  }

  rel_vals_all <- df[[rel_col]][is.finite(df[[rel_col]]) & !is.na(df[[rel_col]])]
  if (length(rel_vals_all) == 0) {
    stop("No finite relative values found for metric '", metric, "'.", call. = FALSE)
  }

  # Profile domain
  if (is.null(tau_lim)) {
    tau_lim <- c(1, max(rel_vals_all, na.rm = TRUE))
  }
  if (length(tau_lim) != 2 || any(!is.finite(tau_lim)) || tau_lim[1] >= tau_lim[2]) {
    stop("tau_lim must be a numeric vector of length 2 with tau_lim[1] < tau_lim[2].", call. = FALSE)
  }
  if (tau_log && any(tau_lim <= 0)) {
    stop("tau_lim must be strictly positive when tau_log = TRUE.", call. = FALSE)
  }

  if (is.null(tau_grid)) {
    if (tau_log) {
      tau_grid <- 10^seq(log10(tau_lim[1]), log10(tau_lim[2]), length.out = 200)
    } else {
      tau_grid <- seq(tau_lim[1], tau_lim[2], length.out = 200)
    }
  } else {
    tau_grid <- sort(unique(as.numeric(tau_grid)))
    tau_grid <- tau_grid[is.finite(tau_grid)]
    tau_grid <- tau_grid[tau_grid >= tau_lim[1] & tau_grid <= tau_lim[2]]
    if (length(tau_grid) == 0) {
      stop("tau_grid has no values inside tau_lim.", call. = FALSE)
    }
    if (tau_log && any(tau_grid <= 0)) {
      stop("tau_grid must be strictly positive when tau_log = TRUE.", call. = FALSE)
    }
  }

  # Check whether all methods appear in the same number of scenarios
  n_by_method <- tapply(!is.na(df[[rel_col]]), df$method, sum)
  if (length(unique(n_by_method)) > 1) {
    warning("Not all methods appear in the same number of scenarios.
           Performance profiles are computed using available scenarios only.
           Consider filtering to a consistent subset with filter_sim_study().")
  }

  # Build performance profile
  methods <- sort(unique(df$method))
  prof_list <- vector("list", length(methods))
  names(prof_list) <- methods

  for (m in methods) {
    vals <- df[[rel_col]][df$method == m]
    vals <- vals[is.finite(vals) & !is.na(vals)]

    if (length(vals) == 0) next

    prof_list[[m]] <- data.frame(
      method = m,
      tau = tau_grid,
      pct = vapply(tau_grid, function(tt) mean(vals <= tt), numeric(1)),
      stringsAsFactors = FALSE
    )
  }

  prof_df <- do.call(rbind, prof_list)
  if (is.null(prof_df) || nrow(prof_df) == 0) {
    stop("Could not construct performance profile.", call. = FALSE)
  }

  # Order methods by area under the profile
  auc <- tapply(prof_df$pct, prof_df$method, sum)
  method_order <- names(sort(auc, decreasing = TRUE))
  prof_df$method <- factor(prof_df$method, levels = method_order)

  # Plot
  shapes_vec <- c(1, 0, 2, 5, 4)
  p <- ggplot2::ggplot(prof_df,
                       ggplot2::aes(x = tau, y = pct, color = method)) +
    ggplot2::geom_line(linewidth = 1)

  if (use_shapes) {
    point_idx <- unique(as.integer(round(seq(1, length(tau_grid),
                                             length.out = min(8, length(tau_grid))))))
    point_tau <- tau_grid[point_idx]
    prof_pts <- prof_df[prof_df$tau %in% point_tau, , drop = FALSE]

    p <- p +
      ggplot2::geom_point(data = prof_pts,
                          ggplot2::aes(shape = method),
                          size = 2) +
      ggplot2::scale_shape_manual(values = rep(shapes_vec, length.out = length(unique(prof_df$method))))
  }

  plot_title <- if (is.null(title)) {
    paste(metric_label(metric), "Performance Profile")
  } else if (identical(title, FALSE)) {
    NULL
  } else {
    title
  }

  if (is.character(color_scale) &&
      length(color_scale) == 1 &&
      color_scale %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {
    p <- p + ggplot2::scale_color_viridis_d(option = color_scale)
  } else {
    stop("color_scale must be one of the standard viridis options.", call. = FALSE)
  }

  p <- p +
    ggplot2::labs(
      x = expression(tau),
      y = "% of scenarios",
      title = plot_title,
      color = "Method",
      shape = if (use_shapes) "Method" else NULL
    ) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(100 * x, "%"),
                                limits = c(0, 1)) +
    ggplot2::theme_minimal(base_size = 16)

  if (tau_log) {
    p <- p + ggplot2::scale_x_log10(limits = tau_lim)
  } else {
    p <- p + ggplot2::scale_x_continuous(limits = tau_lim)
  }

  p
}

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

  # Detect source type, but preserve existing processed objects when possible
  source_type <- if ("fname" %in% names(df)) {
    "fun"
  } else if ("dname" %in% names(df)) {
    "data"
  } else if ("source" %in% names(df) && all(df$source %in% c("fun", "data", "mixed"), na.rm = TRUE)) {
    # already processed-like data frame
    unique_src <- unique(stats::na.omit(df$source))
    if (length(unique_src) == 1) unique_src else "unknown"
  } else {
    "unknown"
  }

  # Canonical ID: preserve if already present
  if (!("id" %in% names(df))) {
    df$id <- if ("fname" %in% names(df)) {
      df$fname
    } else if ("dname" %in% names(df)) {
      df$dname
    } else {
      NA_character_
    }
  }

  # Canonical repetition: preserve if already present, otherwise use fold
  if (!("replication" %in% names(df)) || all(is.na(df$replication))) {
    df$replication <- if ("fold" %in% names(df)) df$fold else NA
  }

  # Check if data was already scaled.
  if ("crps_scaled" %in% names(df)) {
    if (any(df$crps_scaled, na.rm = TRUE) && scale_CRPS) {
      warning("Some rows already have crps_scaled=TRUE; You may be double-scaling.")
    }
  }

  # Scenario keys
  if (source_type == "fun") {
    scen_keys <- c("id", "n_train", "NSR", "design_type", "replication")
  } else if (source_type == "data") {
    scen_keys <- c("id", "fold", "fold_size", "cv_type")
  } else {
    # preserve existing scenario if already present
    scen_keys <- c("id", "replication")
  }

  # Preserve existing sim_scenario if already present and non-missing
  if (!("sim_scenario" %in% names(df)) || all(is.na(df$sim_scenario))) {
    scen_keys_use <- intersect(scen_keys, names(df))
    scen_mat <- df[, scen_keys_use, drop = FALSE]
    for (k in scen_keys_use) scen_mat[[k]] <- as.character(scen_mat[[k]])
    scen_mat[is.na(scen_mat)] <- "NA"
    df$sim_scenario <- apply(scen_mat, 1, paste, collapse = "*")
  } else {
    scen_keys_use <- intersect(scen_keys, names(df))
  }

  # Track CRPS scaling status
  if (!("crps_scaled" %in% names(df))) {
    df$crps_scaled <- FALSE
  }

  scaling_notes <- list(applied = FALSE, n_scaled = 0L)

  # Only scale if requested and not already scaled
  if (scale_CRPS) {
    if (!"CRPS" %in% names(df)) {
      warning("scale_CRPS=TRUE but no 'CRPS' column found; skipping.")
    } else if (all(isTRUE(df$crps_scaled))) {
      # already scaled; do nothing
      scaling_notes$applied <- FALSE
      scaling_notes$n_scaled <- 0L
    } else {
      if (!exists("lookup_sigma", mode = "function")) {
        stop("lookup_sigma(fname) is not available. Please define it or attach the package that provides it.", call. = FALSE)
      }

      sig <- vapply(df$id, lookup_sigma, numeric(1))
      df$sigma <- sig

      scale_cols <- c("CRPS", "CRPS_min", "CRPS_Q1", "CRPS_med", "CRPS_Q3", "CRPS_max")
      scale_cols <- intersect(scale_cols, names(df))
      for (cc in scale_cols) {
        df[[cc]] <- df[[cc]] / df$sigma
      }

      df$crps_scaled <- TRUE
      scaling_notes$applied <- TRUE
      scaling_notes$n_scaled <- sum(!is.na(df$CRPS))
    }
  } else {
    # leave as-is; if sigma missing, don't invent it
    if (!("sigma" %in% names(df))) df$sigma <- NA_real_
  }

  # Add source column if missing
  if (!("source" %in% names(df))) {
    df$source <- source_type
  }

  # Drop original raw-identifying columns if present
  if ("fname" %in% names(df)) df$fname <- NULL
  if ("dname" %in% names(df)) df$dname <- NULL
  if ("fold"  %in% names(df)) df$fold  <- NULL

  # Rename time fields, but preserve already-renamed names
  rename_map <- c(
    t_tot  = "time",
    t_pred = "time_predict",
    t_fit  = "time_fit"
  )
  for (nm in names(rename_map)) {
    if (nm %in% names(df) && !(rename_map[[nm]] %in% names(df))) {
      names(df)[names(df) == nm] <- rename_map[[nm]]
    }
  }

  meta <- list(
    source_type = source_type,
    scenario_keys = scen_keys_use,
    scaled = isTRUE(scale_CRPS) && isTRUE(any(df$crps_scaled)),
    crps_scaled = isTRUE(all(df$crps_scaled)),
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
#' @param ... additional arguments passed onto ensure_metric
#' area under curve scale? See \code{rankplot_sim_study()} for details.
#'
#' @return The same \code{duq_sim_study} object, with new ranking column(s)
#'   appended to \code{obj$df}.
#' @export
rank_sim_study <- function(obj,
                           metric = "CRPS",
                           ties_method = "min",
                           auc = FALSE,
                           ...) {
  if (!inherits(obj, "duq_sim_study")) {
    stop("Please provide a duq_sim_study object (from process_sim_study()).", call. = FALSE)
  }

  # Ensure base metric(s) exist before trying to rank them
  obj <- ensure_metric(obj, metric, ties_method = ties_method, ...)
  df <- obj$df

  if (!all(metric %in% names(df))) {
    missing <- setdiff(metric, names(df))
    stop("Metric(s) not found in data frame: ", paste(missing, collapse = ", "))
  }

  # For each metric, create a rank column
  for (m in metric) {
    if (auc) {
      rank_col <- paste0(m, "_auc")
    } else {
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
      if (auc) {
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

      if (!is.null(upper_bound)) {
        ratio <- pmin(ratio, upper_bound)
      }

      if (log) {
        ratio <- log10(ratio)
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
                          epsilon = 0,
                          upper_bound = Inf,
                          log = TRUE,
                          trim = 0,
                          verbose=FALSE) {
  if (!inherits(obj, "duq_sim_study")) {
    stop("Please provide a duq_sim_study object (from process_sim_study()).", call. = FALSE)
  }

  # Normalize inputs to be same length as metric
  n <- length(metric)
  ties_method <- rep_len(ties_method, n)
  epsilon     <- rep_len(epsilon, n)
  upper_bound <- rep_len(upper_bound, n)
  log         <- rep_len(log, n)
  trim        <- rep_len(trim, n)

  for (i in seq_along(metric)) {
    m <- metric[i]
    tm <- ties_method[i]
    eps <- epsilon[i]
    ub <- upper_bound[i]
    lg <- log[i]
    tr <- trim[i]

    df <- obj$df
    if (m %in% names(df)) next  # already exists

    if (grepl("_rank$", m)) {
      base <- sub("_rank$", "", m)
      if(verbose)
        message("Column '", m, "' not found. Computing via rank_sim_study().")
      obj <- rank_sim_study(
        obj,
        metric = base,
        ties_method = tm,
        auc = FALSE,
        epsilon = eps,
        upper_bound = ub,
        log = lg,
        trim = tr,
        verbose = verbose
      )

    } else if (grepl("_auc$", m)) {
      base <- sub("_auc$", "", m)
      if(verbose)
        message("Column '", m, "' not found. Computing via rank_sim_study(auc=TRUE).")
      obj <- rank_sim_study(
        obj,
        metric = base,
        ties_method = tm,
        auc = TRUE,
        epsilon = eps,
        upper_bound = ub,
        log = lg,
        trim = tr,
        verbose = verbose
      )

    } else if (grepl("_rel_log$", m)) {
      base <- sub("_rel_log$", "", m)
      if(verbose)
        message("Column '", m, "' not found. Computing via relative_sim_study(log=TRUE).")
      obj <- relativize_sim_study(obj, metric = base, epsilon = eps, upper_bound = ub, log = TRUE)

    } else if (grepl("_rel$", m)) {
      base <- sub("_rel$", "", m)
      if(verbose)
        message("Column '", m, "' not found. Computing via relative_sim_study(log=FALSE).")
      obj <- relativize_sim_study(obj, metric = base, epsilon = eps, upper_bound = ub, log = FALSE)

    } else if (grepl("_norm$", m)) {
      base <- sub("_norm$", "", m)
      if(verbose)
        message("Column '", m, "' not found. Computing via normalize_sim_study().")
      obj <- normalize_sim_study(obj, metric = base, trim = tr)

    } else {
      stop("Don't know how to ensure metric '", m, "'.")
    }
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

  # Detect method column name
  method_col <- if ("method_name" %in% names(df1) || "method_name" %in% names(df2)) {
    "method_name"
  } else if ("method" %in% names(df1) || "method" %in% names(df2)) {
    "method"
  } else {
    NULL
  }

  # Suffix overlapping obj2 method names
  # if (!is.null(method_col) &&
  #     method_col %in% names(df1) &&
  #     method_col %in% names(df2)) {
  #   overlap <- intersect(unique(df1[[method_col]]), unique(df2[[method_col]]))
  #   hit <- df2[[method_col]] %in% overlap
  #   df2[[method_col]][hit] <- paste0(df2[[method_col]][hit], "_obj2")
  # }
  # Suffix overlapping obj2 method names only within the same simulation scenario
  if (!is.null(method_col) &&
      method_col %in% names(df1) &&
      method_col %in% names(df2) &&
      "sim_scenario" %in% names(df1) &&
      "sim_scenario" %in% names(df2)) {

    key1 <- unique(paste(df1$sim_scenario, df1[[method_col]], sep = "___"))
    key2 <- paste(df2$sim_scenario, df2[[method_col]], sep = "___")

    hit <- key2 %in% key1
    df2[[method_col]][hit] <- paste0(df2[[method_col]][hit], "_obj2")
  }

  # Identify common "identity" columns
  id_cols <- c("id", method_col, "sim_scenario", "replication")

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
#' @param id Character vector of IDs to keep (matches \code{id}).
#' @param method Character vector of methods to keep (matches \code{method}).
#' @param n_train Numeric vector of training sizes to keep.
#' @param design_type Character vector of design types to keep.
#' @param NSR Numeric vector of noise-to-signal ratios to keep.
#' @param replication Integer vector of replication indices to keep.
#' @param fold_size Integer vector of fold sizes to keep.
#' @param source Character vector of sources to keep (e.g. \code{"function"},
#'   \code{"dataset"}).
#' @param failure_type Character vector of failure types to keep (if present).
#' @param metrics Character vector of metric columns to retain. If \code{NULL}
#'   (default), all columns are kept.
#' @param expr Optional unquoted expression evaluated inside \code{obj$df} for
#'   custom filtering, e.g. \code{expr = n_train <= 500}.
#' @param rerank Logical; if \code{TRUE}, recompute ranks (via
#'   \code{rank_sim_study()}) for any rank columns present. Default is
#'   \code{FALSE}.
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
  # Backwards compatibility preserved
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
  expr_sub <- substitute(expr)
  if (!identical(expr_sub, NULL)) {
    expr_keep <- eval(expr_sub, envir = df, enclos = parent.frame())
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

#' Add or modify columns in a duq_sim_study object
#'
#' Evaluates one or more expressions inside \code{obj$df} and returns the
#' modified \code{duq_sim_study} object. This is useful for creating custom
#' metrics or transformed columns without modifying \code{obj$df} directly.
#'
#' Expressions are evaluated sequentially, so later expressions may refer to
#' columns created earlier in the same call.
#'
#' @param obj A \code{duq_sim_study} object from \code{process_sim_study()}.
#' @param ... Named expressions to evaluate inside \code{obj$df}. Each name
#'   becomes a new or modified column in the returned object. For example,
#'   \code{log_FVU = log10(FVU)} or \code{speed = 1 / t_tot}.
#' @param overwrite Logical; if \code{FALSE} (default), an error is thrown when
#'   attempting to overwrite an existing column. If \code{TRUE}, existing
#'   columns may be replaced.
#'
#' @return A \code{duq_sim_study} object with updated columns in \code{obj$df}.
#'
#' @examples
#' \dontrun{
#' duq2 <- mutate_sim_study(
#'   duq,
#'   log_FVU = log10(FVU),
#'   speed = 1 / t_tot
#' )
#'
#' duq3 <- mutate_sim_study(
#'   duq,
#'   RMSE_over_time = RMSE / t_tot,
#'   log_ratio = log10(RMSE_over_time)
#' )
#' }
#'
#' @export
mutate_sim_study <- function(obj, ..., overwrite = FALSE) {
  if (!inherits(obj, "duq_sim_study")) {
    stop(
      "Please provide a duq_sim_study object (from process_sim_study()).",
      call. = FALSE
    )
  }

  exprs <- as.list(substitute(list(...)))[-1]

  if (length(exprs) == 0) {
    warning("No expressions supplied; returning object unchanged.", call. = FALSE)
    return(obj)
  }

  expr_names <- names(exprs)
  if (is.null(expr_names) || any(expr_names == "")) {
    stop(
      "All expressions passed to mutate_sim_study() must be named.",
      call. = FALSE
    )
  }

  df <- obj$df

  for (nm in expr_names) {
    if (!overwrite && nm %in% names(df)) {
      stop(
        "Column '", nm, "' already exists. Set overwrite = TRUE to replace it.",
        call. = FALSE
      )
    }

    value <- tryCatch(
      eval(exprs[[nm]], envir = df, enclos = parent.frame()),
      error = function(e) {
        stop(
          "Failed to compute column '", nm, "': ", conditionMessage(e),
          call. = FALSE
        )
      }
    )

    # Allow scalar values and recycle them to nrow(df)
    if (length(value) == 1L) {
      value <- rep(value, nrow(df))
    }

    if (length(value) != nrow(df)) {
      stop(
        "Expression for column '", nm, "' must evaluate to length 1 or nrow(obj$df).",
        call. = FALSE
      )
    }

    df[[nm]] <- value
  }

  obj$df <- df
  obj$meta$n_rows <- nrow(df)
  obj
}

