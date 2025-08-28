#' Rank Plot for Emulator Performance
#'
#' Create a cumulative rank plot for a given performance metric (e.g., CRPS, RMSE).
#'
#' @param df A data frame (typically from \code{run_sim_study()} or aggregated results).
#' @param metric Character. The name of the metric to rank by (e.g., "CRPS", "RMSE", "t_tot").
#' @param methods Optional character vector of method names to include. Default is all.
#' @param ties_method How to handle ties in ranking (passed to \code{rank()}). Default is "min".
#' @param use_shapes Logical. Whether to use different point shapes in addition to color. Default is TRUE.
#' @param path Optional file path to save plot (e.g., "figs/CRPS_results.png"). If NULL, the plot is returned instead of saved.
#'
#' @return A ggplot object (invisibly), or writes a file if \code{path} is specified.
#'
#' @examples
#' \dontrun{
#' rankplot_sim_study(results, metric = "CRPS")
#' rankplot_sim_study(results, metric = "RMSE", methods = c("BART", "GP"), path = "figs/rmse.png")
#' }
#' @export
rankplot_sim_study <- function(df,
                               metric = "CRPS",
                               methods = NULL,
                               ties_method = "min",
                               use_shapes = TRUE,
                               path = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install 'ggplot2'.")

  # Check metric
  if (!metric %in% names(df)) stop(paste("Metric", metric, "not found."))

  # Optional filter
  if (!is.null(methods)) {
    df <- df[df$method %in% methods, , drop = FALSE]
  }

  # Convert metric to numeric (in case it's a table or factor)
  df[[metric]] <- as.numeric(df[[metric]])

  # Scenario ID: defined by (fname, n_train, NSR, design_type)
  df$scenario_id <- apply(df[, c("fname", "n_train", "NSR", "design_type", "replication")], 1, paste, collapse = "_")

  # Split into scenarios
  scenarios <- split(df, df$scenario_id)

  # Compute rank per scenario
  rank_list <- lapply(scenarios, function(scenario_df) {
    scenario_df$rank <- rank(scenario_df[[metric]], ties.method = ties_method)
    scenario_df
  })

  ranked_df <- do.call(rbind, rank_list)

  # Count how often each method gets each rank
  rank_table <- table(ranked_df$method, ranked_df$rank)

  # Convert to cumulative percentages
  pct_cumul <- t(apply(rank_table, 1, function(x) cumsum(x) / sum(x)))

  # Convert to long format for ggplot
  method_names <- rownames(pct_cumul)
  rank_vals <- as.integer(colnames(pct_cumul))

  cumul_ranks <- data.frame(
    method = rep(method_names, each = length(rank_vals)),
    rank = rep(rank_vals, times = length(method_names)),
    pct = as.vector(t(pct_cumul))
  )

  # Order methods by area under curve (i.e., better average rank)
  auc <- tapply(cumul_ranks$pct, cumul_ranks$method, sum)
  method_order <- names(sort(auc, decreasing = TRUE))
  cumul_ranks$method <- factor(cumul_ranks$method, levels = method_order)

  # Plot
  shapes_vec <- c(1, 0, 2, 5, 4)
  p <- ggplot2::ggplot(cumul_ranks, ggplot2::aes(x = rank, y = pct, color = method)) +
    ggplot2::geom_line(size = 1)

  if (use_shapes) {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(shape = method), size = 2) +
      ggplot2::scale_shape_manual(values = rep(shapes_vec, length.out = length(unique(cumul_ranks$method))))
  }

  p <- p +
    ggplot2::scale_color_viridis_d(option = "turbo") +
    ggplot2::labs(
      x = "Rank or better",
      y = "% of scenarios",
      title = paste(metric, "Results"),
      color = "Emulator",
      shape = if (use_shapes) "Emulator" else NULL
    ) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
    ggplot2::theme_minimal(base_size = 16)

  if (!is.null(path)) {
    ggplot2::ggsave(filename = path, plot = p, width = 12, height = 6, units = "in", dpi = 350)
    invisible(p)
  } else {
    return(p)
  }
}
