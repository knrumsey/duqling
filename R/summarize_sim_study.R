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
        out[[paste0(m, "_winrate")]] <- rates[out$method]
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
          out[[paste0(m, "_top", k, "rate")]] <- rates[out$method]
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
        out[[paste0(m, "_effective", tol)]] <- rates[out$method]
      }
    }

    # Failure rate
    if (failure_rate && "failure_type" %in% names(dsub)) {
      rates <- tapply(dsub$failure_type != "none",
                      dsub$method, mean, na.rm = TRUE)
      out[["failure_rate"]] <- rates[out$method]
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
        rates <- tapply(score, dsub$method, mean, na.rm = TRUE)
        out[[paste0(m, "_softrel")]] <- rates[out$method]
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
                               group_by = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install ggplot2 to use this plotting function.", call. = FALSE)
  }
  if (!inherits(obj, "duq_sim_study")) {
    stop("Please provide a duq_sim_study object (from process_sim_study()).", call. = FALSE)
  }

  # Ensure the requested metric column exists
  obj <- ensure_metric(obj, metric, ties_method = ties_method)
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
#'
#' @return A \code{ggplot} object.
#' @export
rankplot_sim_study <- function(obj,
                               metric = "CRPS",
                               ties_method = "min",
                               use_shapes = TRUE,
                               title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install ggplot2 to use this plotting function.", call. = FALSE)
  }
  if (!inherits(obj, "duq_sim_study")) {
    stop("Please provide a duq_sim_study object (from process_sim_study()).", call. = FALSE)
  }

  # Ensure rank column exists
  if(is.null(obj$df[[paste0(metric, "_rank")]])){
    obj <- rank_sim_study(obj, metric = metric, ties_method = ties_method)
  }
  df <- obj$df

  rank_col <- paste0(metric, "_rank")
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
#' @param ... Additional arguments passed to \code{ensure_method()}.
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
  for (m in metric) {
    obj <- ensure_metric(obj, m, ...)
  }
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
      ggplot2::aes(label = method),
      size = 3.5,
      max.overlaps = Inf
    )
  } else {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = method),
      vjust = -0.5, hjust = 0.5,
      size = 3.5,
      check_overlap = TRUE
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
#' @param metric Character; the performance metric column to plot (default "CRPS").
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
                              metric = "CRPS",
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
  agg$group_id <- factor(agg$group_id, levels = unique(agg$group_id))

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




