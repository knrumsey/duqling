#' Heatmap of Method Rankings Across Simulation Scenarios
#'
#' @param df A data frame from \code{run_sim_study()}.
#' @param metric Character. The performance metric to plot (e.g., \code{"CRPS"}, \code{"RMSE"}). By default, ranks are shown; use \code{"CRPS_raw"} or similar to show raw values.
#' @param methods Optional character vector of methods to include.
#' @param group_by Character vector of columns that define each simulation scenario (default = "fname").
#' @param ties_method How to handle ties in ranking. Passed to \code{rank()}. Default = "min".
#' @param show_values Logical. Should text given the average "metric" value (rounded to 2 decimal places) be added to the plot?
#' @param color_scale Either a string specifying one of the 8 viridis color map options
#'   ("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo"),
#'   or a character vector of two colors (e.g., \code{c("white", "black")}) to be used
#'   as a custom gradient from low to high rank. Default is \code{"turbo"}.
#' @param orientation Optional: "horizontal" or "vertical"
#' @param transformation A function defining a transformation to be applied to the metric of choice.
#' @param colorbar_labels A named list with fields \code{$breaks} and \code{$labels} for the colorbar.
#' @param title Optional title for plot. Set to \code{FALSE} to suppress title.
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
                              show_values = NULL,
                              color_scale = "turbo",
                              orientation = "vertical",
                              transformation=NULL,
                              colorbar_labels=NULL,
                              title = NULL,
                              path = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install 'ggplot2'.")

  # In case df comes from run_sim_study_data()
  if (!"fname" %in% names(df) && "dname" %in% names(df)) {
    df$fname <- df$dname
    df$rep <- df$fold
    df$replication <- df$fold
    df$n_train <- NA
    df$NSR <- NA
    df$design_type <- NA
  }

  # Determine if we're ranking or showing raw values
  use_rank <- !grepl("_raw$", metric)
  base_metric <- sub("_raw$", "", metric)

  # Check inputs
  required_fields <- c("fname", "n_train", "NSR", "design_type")
  rep_col <- if ("replication" %in% names(df)) "replication" else "rep"
  scenario_fields <- c(required_fields, rep_col)

  if (!all(scenario_fields %in% names(df))) stop("Simulation scenario columns are missing.")
  if (!all(group_by %in% names(df))) stop("Some group_by columns are missing.")
  if (!base_metric %in% names(df)) stop(paste("Metric", base_metric, "not found in df."))

  # Filter methods
  if (!is.null(methods)) {
    df <- df[df$method %in% methods, , drop = FALSE]
  }

  df[[base_metric]] <- as.numeric(df[[base_metric]])

  # Create scenario ID for exact simulation scenarios
  df$scenario_id <- apply(df[, scenario_fields, drop = FALSE], 1, paste, collapse = "_")

  # Initialize placeholder for aggregated data
  agg <- NULL

  if (use_rank) {
    # Rank methods within each scenario
    scenario_list <- split(df, df$scenario_id)
    ranked_list <- lapply(scenario_list, function(scenario_df) {
      scenario_df$rank <- rank(scenario_df[[base_metric]], ties.method = ties_method)
      scenario_df
    })
    ranked_df <- do.call(rbind, ranked_list)
    ranked_df$group_id <- apply(ranked_df[, group_by, drop = FALSE], 1, paste, collapse = " \u00D7 ")
    agg <- aggregate(rank ~ group_id + method, data = ranked_df, FUN = mean)
    agg$value <- agg$rank
  } else {
    df$group_id <- apply(df[, group_by, drop = FALSE], 1, paste, collapse = " \u00D7 ")
    agg <- aggregate(df[[base_metric]], by = list(group_id = df$group_id, method = df$method), FUN = mean)
    colnames(agg)[3] <- "value"
  }

  # Get full grid of group × method
  group_levels <- unique(agg$group_id)
  if ("fname" %in% group_by) {
    fname_order <- unique(df$fname)
    group_levels <- group_levels[order(match(group_levels, fname_order))]
  }

  # if (use_rank) {
  #   method_means <- aggregate(value ~ method, data = agg, mean)
  #   method_levels <- method_means$method[order(method_means$value)]  # best ranks first
  # } else {
  #   method_levels <- if (!is.null(methods)) methods else sort(unique(agg$method))
  # }
  method_means <- aggregate(value ~ method, data = agg, mean)
  method_levels <- method_means$method[order(method_means$value)]
  n_methods <- length(method_levels)

  grid <- expand.grid(group_id = group_levels, method = method_levels, stringsAsFactors = FALSE)
  full_data <- merge(grid, agg, by = c("group_id", "method"), all.x = TRUE)
  full_data$method <- factor(full_data$method, levels = method_levels)
  full_data$group_id <- factor(full_data$group_id, levels = group_levels)

  # Optionally transform values (after averaging)
  if (!is.null(transformation)) {
    full_data$value <- transformation(full_data$value)
  }

  # Auto show values only for small grids
  if (is.null(show_values)) {
    n_rows <- length(unique(full_data$group_id))
    n_cols <- length(unique(full_data$method))
    show_values <- (n_rows <= 8 && n_cols <= 8)
  }

  # Color scale limits
  if (use_rank) {
    limits <- c(1, length(method_levels))
  } else {
    limits <- range(full_data$value, na.rm = TRUE)
  }

  # Color scale
  if (is.character(color_scale) && length(color_scale) == 1 &&
      color_scale %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {

    fill_scale <- ggplot2::scale_fill_viridis_c(
      option = color_scale,
      direction = -1,
      na.value = "grey90",
      limits = limits,
      breaks = if (!is.null(colorbar_labels)) colorbar_labels$breaks else waiver(),
      labels = if (!is.null(colorbar_labels)) colorbar_labels$labels else waiver()
    )

  } else if (is.character(color_scale) && length(color_scale) == 2) {

    fill_scale <- ggplot2::scale_fill_gradient(
      low = color_scale[1],
      high = color_scale[2],
      na.value = "grey90",
      limits = limits,
      breaks = if (!is.null(colorbar_labels)) colorbar_labels$breaks else waiver(),
      labels = if (!is.null(colorbar_labels)) colorbar_labels$labels else waiver()
    )

  } else {
    stop("Invalid color_scale: must be a viridis option or a 2-element color vector.")
  }

  # Pretty axis label
  group_pretty_map <- c(
    "fname" = "Function",
    "n_train" = "Training Size",
    "NSR" = "Noise Ratio",
    "design_type" = "Design",
    "replication" = "Replication"
  )

  group_by_pretty <- group_by
  group_by_pretty[group_by %in% names(group_pretty_map)] <- group_pretty_map[group_by[group_by %in% names(group_pretty_map)]]

  # Orientation
  if (orientation == "vertical") {
    x_angle <- ifelse(n_methods <= 8, 45, 90)
    aes_args <- ggplot2::aes(x = method, y = group_id, fill = value)
    x_label <- "Method"
    y_label <- paste(group_by_pretty, collapse = " \u00D7 ")
    theme <- ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 8),
        axis.text.x = ggplot2::element_text(angle = x_angle, hjust = 1, vjust = 0.5)
      )
  } else if (orientation == "horizontal") {
    aes_args <- ggplot2::aes(x = group_id, y = method, fill = value)
    x_label <- paste(group_by_pretty, collapse = " \u00D7 ")
    y_label <- "Method"
    theme <- ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 8),
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
      )
  } else {
    stop("orientation must be either 'vertical' or 'horizontal'")
  }

  plot_title <- if (is.null(title)) {
    paste("Average", if (use_rank) "Rank" else "Value", "by", base_metric)
  } else if (identical(title, FALSE)) {
    NULL
  } else {
    title
  }

  # Plot
  p <- ggplot2::ggplot(full_data, aes_args) +
    ggplot2::geom_tile(color = "white") +
    fill_scale +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      fill = if (use_rank) "Avg. Rank" else base_metric,
      title = plot_title
    ) +
    theme

  if (show_values) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", value)),
      size = 3
    )
  }

  if (!is.null(path)) {
    ggplot2::ggsave(filename = path, plot = p, width = 12, height = 6, units = "in", dpi = 350)
    invisible(p)
  } else {
    return(p)
  }
}
