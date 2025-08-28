#' Pareto Plot of Accuracy vs Time
#'
#' Plot tradeoff between performance and runtime for each method.
#'
#' @param df Data frame from \code{run_sim_study()}.
#' @param metric Length-2 character vector. The x and y metrics to compare. Default is \code{c("CRPS", "t_tot")}.
#' @param methods Optional character vector of methods to include.
#' @param highlight_method Character vector of methods to label and color. Use \code{NA} to color all (default), or \code{NULL} to color none.
#' @param path Optional file path to save plot (e.g., "figs/CRPS_results.png"). If NULL, the plot is returned instead of saved.
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' paretoplot_sim_study(results)
#' paretoplot_sim_study(results, highlight_method = c("GP", "BART"))
#' paretoplot_sim_study(results, metric = c("RMSE", "t_tot"), highlight_method = NULL)
#' }
#' @export
paretoplot_sim_study <- function(df,
                                 metric = c("CRPS", "t_tot"),
                                 methods = NULL,
                                 highlight_method = NA,
                                 path=NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install 'ggplot2'.")

  # Validate input
  if (length(metric) != 2 || !all(metric %in% names(df))) {
    stop("Please provide two valid metric names found in df.")
  }

  perf_metric <- metric[1]  # e.g., CRPS
  time_metric <- metric[2]  # e.g., t_tot

  df[[perf_metric]] <- as.numeric(df[[perf_metric]])
  df[[time_metric]] <- as.numeric(df[[time_metric]])

  # Filter methods if needed
  if (!is.null(methods)) {
    df <- df[df$method %in% methods, , drop = FALSE]
  }

  method_levels <- sort(unique(df$method))
  n_methods <- length(method_levels)

  # Compute average performance/time per method
  method_stats <- aggregate(df[, metric], by = list(method = df$method), FUN = mean, na.rm = TRUE)
  colnames(method_stats)[2:3] <- c(perf_metric, time_metric)

  # Compute rank-based AUC-style scores for both axes (scale: 0 = worst, 1 = best)
  ranks_perf <- rank(method_stats[[perf_metric]], ties.method = "average")  # lower = better
  ranks_time <- rank(method_stats[[time_metric]], ties.method = "average")  # lower = better

  method_stats$score_perf <- 1 - (ranks_perf - 1) / (n_methods - 1)
  method_stats$score_time <- 1 - (ranks_time - 1) / (n_methods - 1)

  # Handle highlight
  if (is.na(highlight_method[1])) {
    method_stats$highlight <- TRUE
  } else if (is.null(highlight_method)) {
    method_stats$highlight <- FALSE
  } else {
    method_stats$highlight <- method_stats$method %in% highlight_method
  }

  # Pareto logic (using raw metrics, not AUC-scores)
  is_dominated <- function(i, mat) {
    any(mat[-i, time_metric] <= mat[i, time_metric] &
          mat[-i, perf_metric] <= mat[i, perf_metric] &
          (mat[-i, time_metric] < mat[i, time_metric] | mat[-i, perf_metric] < mat[i, perf_metric]))
  }
  pareto_idx <- which(!sapply(seq_len(nrow(method_stats)), function(i) is_dominated(i, method_stats)))
  method_stats$pareto <- FALSE
  method_stats$pareto[pareto_idx] <- TRUE

  # Extract Pareto-optimal points and order left to right
  pareto_df <- method_stats[method_stats$pareto, ]
  pareto_df <- pareto_df[order(pareto_df$score_time), ]  # left to right

  # Plot
  p <- ggplot2::ggplot(method_stats, ggplot2::aes(x = score_time, y = score_perf)) +
    #ggplot2::geom_point(aes(shape = pareto), size = 3, color = "black") +
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
      linetype = "dashed"
    ) +
    ggplot2::scale_shape_manual(values = c(16, 17), guide = ggplot2::guide_legend(title = "Pareto Front")) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      x = paste("Speed Score (1 = Fastest)", "\n← Slower     Faster →"),
      y = paste("Accuracy Score (1 = Best)", "\n↓ Worse      Better ↑"),
      title = paste("Pareto Plot (AUC-style):", perf_metric, "vs", time_metric)
    ) +
    ggplot2::theme_minimal(base_size = 14)

  # Add labels only for highlighted methods
  if (any(method_stats$highlight)) {
    p <- p + ggplot2::geom_text(
      data = method_stats[method_stats$highlight, ],
      ggplot2::aes(label = method),
      vjust = -0.5,
      hjust = 0.5,
      size = 3.5
    )
  }

  if (!is.null(path)) {
    ggplot2::ggsave(filename = path, plot = p, width = 12, height = 6, units = "in", dpi = 350)
    invisible(p)
  } else {
    return(p)
  }
}
