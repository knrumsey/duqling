#' Pareto Plot of Accuracy vs Time
#'
#' Plot tradeoff between performance and runtime for each method using rank-based scores.
#'
#' @param df Data frame from \code{run_sim_study()}.
#' @param metric Length-2 character vector. The performance and runtime metrics to compare. Default is \code{c("CRPS", "t_tot")}.
#' @param methods Optional character vector of methods to include.
#' @param highlight_method Character vector of methods to label and color. Use \code{NA} to color all (default), or \code{NULL} to color none.
#' @param pin_axes Logical. Should axes be pinned at (0, 1) ?
#' @param path Optional file path to save plot (e.g., "figs/pareto_plot.png"). If NULL, the plot is returned instead.
#' @return A ggplot object.
#' @export
paretoplot_sim_study <- function(df,
                                 metric = c("CRPS", "t_tot"),
                                 methods = NULL,
                                 highlight_method = NA,
                                 pin_axes = TRUE,
                                 path = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install 'ggplot2'.")

  # Validate inputs
  if (length(metric) != 2 || !all(metric %in% names(df))) {
    stop("Please provide two valid metric names found in df.")
  }

  perf_metric <- metric[1]
  time_metric <- metric[2]

  df[[perf_metric]] <- as.numeric(df[[perf_metric]])
  df[[time_metric]] <- as.numeric(df[[time_metric]])

  # Optional filter
  if (!is.null(methods)) {
    df <- df[df$method %in% methods, , drop = FALSE]
  }

  # Scenario ID
  scenario_cols <- c("fname", "n_train", "NSR", "design_type", "replication")
  df$scenario_id <- apply(df[, scenario_cols, drop = FALSE], 1, paste, collapse = "_")

  # Rank methods within each scenario
  scenario_list <- split(df, df$scenario_id)
  ranked_list <- lapply(scenario_list, function(sdf) {
    sdf$rank <- rank(sdf[[perf_metric]], ties.method = "average")
    sdf
  })
  ranked_df <- do.call(rbind, ranked_list)

  # Average rank and time per method
  method_stats <- aggregate(cbind(rank = ranked_df$rank, time = df[[time_metric]]),
                            by = list(method = df$method), FUN = mean, na.rm = TRUE)
  names(method_stats) <- c("method", "avg_rank", "avg_time")

  # Compute scores: 1 = best, 0 = worst
  n_methods <- nrow(method_stats)
  method_stats$score_perf <- 1 - (method_stats$avg_rank - 1) / (n_methods - 1)
  ranks_time <- rank(method_stats$avg_time, ties.method = "average")
  method_stats$score_time <- 1 - (ranks_time - 1) / (n_methods - 1)

  # Highlight
  if (is.na(highlight_method[1])) {
    method_stats$highlight <- TRUE
  } else if (is.null(highlight_method)) {
    method_stats$highlight <- FALSE
  } else {
    method_stats$highlight <- method_stats$method %in% highlight_method
  }

  # Pareto front using raw metrics (lower is better)
  is_dominated <- function(i, mat) {
    any(mat[-i, "avg_time"] <= mat[i, "avg_time"] &
          mat[-i, "avg_rank"] <= mat[i, "avg_rank"] &
          (mat[-i, "avg_time"] < mat[i, "avg_time"] |
             mat[-i, "avg_rank"] < mat[i, "avg_rank"]))
  }
  pareto_idx <- which(!sapply(seq_len(nrow(method_stats)), function(i) is_dominated(i, method_stats)))
  method_stats$pareto <- FALSE
  method_stats$pareto[pareto_idx] <- TRUE

  # Pareto path
  pareto_df <- method_stats[method_stats$pareto, ]
  pareto_df <- pareto_df[order(pareto_df$score_time), ]

  # Plot
  p <- ggplot2::ggplot(method_stats, ggplot2::aes(x = score_time, y = score_perf)) +
    ggplot2::geom_point(
      ggplot2::aes(color = highlight, shape = pareto),
      size = 3
    ) +
    ggplot2::scale_color_manual(
      values = c("TRUE" = "#1f78b4", "FALSE" = "gray60"),
      guide = "none"
    ) +
    ggplot2::geom_path(
      data = pareto_df,
      ggplot2::aes(x = score_time, y = score_perf),
      color = "black",
      linewidth = 0.5,
      linetype = "dashed",
      inherit.aes = FALSE
    ) +
    ggplot2::scale_shape_manual(
      values = c(16, 17),
      guide = ggplot2::guide_legend(title = "Pareto Front")
    ) +
    ggplot2::labs(
      x = paste("Speed Score (1 = Fastest)", "\n← Slower     Faster →"),
      y = paste("Accuracy Score (1 = Best)", "\n↓ Worse      Better ↑"),
      title = paste("Pareto Plot (Rank-Based):", perf_metric, "vs", time_metric)
    ) +
    ggplot2::theme_minimal(base_size = 14)

  if(pin_axes){
    p <- p + ggplot2::coord_cartesian(clip = "off", xlim=c(0,1), ylim=c(0,1))
  }else{
    p <- p + ggplot2::coord_cartesian(clip = "off")
  }

  # Labels for highlighted methods
  if (any(method_stats$highlight)) {
    p <- p + ggplot2::geom_text(
      data = method_stats[method_stats$highlight, ],
      ggplot2::aes(
        x = score_time,
        y = score_perf,
        label = method
      ),
      vjust = -0.5,
      hjust = 0.5,
      size = 3.5,
      inherit.aes = FALSE
    )
  }

  # Save or return
  if (!is.null(path)) {
    ggplot2::ggsave(filename = path, plot = p, width = 12, height = 6, units = "in", dpi = 350)
    invisible(p)
  } else {
    return(p)
  }
}








