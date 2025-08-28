#' Heatmap of Method Rankings Across Simulation Scenarios
#'
#' @param df A data frame from \code{run_sim_study()}.
#' @param metric Character. The performance metric to use (e.g., "CRPS", "RMSE").
#' @param methods Optional character vector of methods to include.
#' @param group_by Character vector of columns that define each simulation scenario (default = "fname").
#' @param ties_method How to handle ties in ranking. Passed to \code{rank()}. Default = "min".
#' @param color_scale Either a string specifying one of the 8 viridis color map options
#'   ("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo"),
#'   or a character vector of two colors (e.g., \code{c("white", "black")}) to be used
#'   as a custom gradient from low to high rank. Default is \code{"turbo"}.
#' @param path Optional file path to save plot (e.g., "figs/CRPS_results.png"). If NULL, the plot is returned instead of saved.
#' @return A ggplot heatmap.
#'
#' @examples
#' \dontrun{
#' heatmap_sim_study(df, metric = "CRPS", group_by = c("fname", "NSR"))
#' }
#' @export
heatmap_sim_study <- function(df,
                              metric = "CRPS",
                              methods = NULL,
                              group_by = "fname",
                              ties_method = "min",
                              color_scale = "turbo",
                              orientation = "vertical",
                              path = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install 'ggplot2'.")

  # Check inputs
  required_fields <- c("fname", "n_train", "NSR", "design_type")
  rep_col <- if ("replication" %in% names(df)) "replication" else "rep"
  scenario_fields <- c(required_fields, rep_col)

  if (!all(scenario_fields %in% names(df))) stop("Simulation scenario columns are missing.")
  if (!all(group_by %in% names(df))) stop("Some group_by columns are missing.")
  if (!metric %in% names(df)) stop(paste("Metric", metric, "not found in df."))

  # Filter methods
  if (!is.null(methods)) {
    df <- df[df$method %in% methods, , drop = FALSE]
  }

  df[[metric]] <- as.numeric(df[[metric]])

  # Create scenario ID for exact simulation scenarios
  df$scenario_id <- apply(df[, scenario_fields, drop = FALSE], 1, paste, collapse = "_")

  # Rank methods within each scenario
  scenario_list <- split(df, df$scenario_id)
  ranked_list <- lapply(scenario_list, function(scenario_df) {
    scenario_df$rank <- rank(scenario_df[[metric]], ties.method = ties_method)
    scenario_df
  })
  ranked_df <- do.call(rbind, ranked_list)

  # Create group ID based on group_by
  ranked_df$group_id <- apply(ranked_df[, group_by, drop = FALSE], 1, paste, collapse = "_")

  # Average rank per method per group
  agg <- aggregate(rank ~ group_id + method, data = ranked_df, FUN = mean)

  # Get full grid of group × method
  group_levels <- unique(agg$group_id)
  method_levels <- if (!is.null(methods)) methods else sort(unique(agg$method))
  grid <- expand.grid(group_id = group_levels, method = method_levels, stringsAsFactors = FALSE)

  full_data <- merge(grid, agg, by = c("group_id", "method"), all.x = TRUE)
  full_data$method <- factor(full_data$method, levels = method_levels)
  full_data$group_id <- factor(full_data$group_id, levels = group_levels)

  # Color scale stuff
  n_methods <- length(unique(full_data$method))
  if (is.character(color_scale) && length(color_scale) == 1 && color_scale %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {
    fill_scale <- ggplot2::scale_fill_viridis_c(
      option = color_scale,
      direction = -1,
      na.value = "grey90",
      limits = c(1, n_methods)
    )
  } else if (is.character(color_scale) && length(color_scale) == 2) {
    fill_scale <- ggplot2::scale_fill_gradient(
      low = color_scale[1],
      high = color_scale[2],
      na.value = "grey90",
      limits = c(1, n_methods)
    )
  } else {
    stop("Invalid color_scale: must be a viridis option or a 2-element color vector.")
  }

  # Orientation
  if (orientation == "vertical") {
    aes_args <- ggplot2::aes(x = method, y = group_id, fill = rank)
    x_label <- "Method"
    y_label <- paste(group_by, collapse = " × ")
    theme <- ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8))
  } else if (orientation == "horizontal") {
    aes_args <- ggplot2::aes(x = group_id, y = method, fill = rank)
    x_label <- paste(group_by, collapse = " × ")
    y_label <- "Method"
    theme <- ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 8),
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
      )
  } else {
    stop("orientation must be either 'vertical' or 'horizontal'")
  }

  # Plot
  p <- ggplot2::ggplot(full_data, aes_args) +
    ggplot2::geom_tile(color = "white") +
    fill_scale +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      fill = "Avg. Rank",
      title = paste("Average Rank by", metric)
    ) +
    theme

  if (!is.null(path)) {
    ggplot2::ggsave(filename = path, plot = p, width = 12, height = 6, units = "in", dpi = 350)
    invisible(p)
  } else {
    return(p)
  }
}
