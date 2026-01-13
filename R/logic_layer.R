# Logic Layer - Plotting and Calculation Functions
# Purpose: Core business logic for data processing, analysis, and visualization
# Created: 2026-01-13

# ============================================================================
# CALCULATION FUNCTIONS
# ============================================================================

#' Calculate Summary Statistics
#'
#' @param data A numeric vector or data frame column
#' @param na.rm Logical. Remove NA values? Default is TRUE
#'
#' @return A list containing mean, median, sd, min, and max
#'
#' @examples
#' calculate_summary_stats(c(1, 2, 3, 4, 5))
#'
calculate_summary_stats <- function(data, na.rm = TRUE) {
  list(
    mean = mean(data, na.rm = na.rm),
    median = median(data, na.rm = na.rm),
    sd = sd(data, na.rm = na.rm),
    min = min(data, na.rm = na.rm),
    max = max(data, na.rm = na.rm),
    n = length(data)
  )
}

#' Calculate Growth Rate
#'
#' @param initial Initial value
#' @param final Final value
#' @param periods Number of periods
#'
#' @return Numeric growth rate as percentage
#'
#' @examples
#' calculate_growth_rate(100, 150, 5)
#'
calculate_growth_rate <- function(initial, final, periods = 1) {
  ((final / initial) ^ (1 / periods) - 1) * 100
}

#' Calculate Percentage Change
#'
#' @param old Old value
#' @param new New value
#'
#' @return Numeric percentage change
#'
#' @examples
#' calculate_percent_change(100, 120)
#'
calculate_percent_change <- function(old, new) {
  ((new - old) / old) * 100
}

#' Aggregate Data by Groups
#'
#' @param data Data frame
#' @param group_col Column name for grouping
#' @param value_col Column name with values to aggregate
#' @param fun Function to apply (e.g., "mean", "sum", "median")
#'
#' @return Data frame with aggregated results
#'
aggregate_data <- function(data, group_col, value_col, fun = "mean") {
  aggregate(
    data[[value_col]] ~ data[[group_col]],
    FUN = match.fun(fun),
    data = data
  )
}

# ============================================================================
# PLOTTING FUNCTIONS
# ============================================================================

#' Create a Time Series Plot
#'
#' @param data Data frame with time series data
#' @param x Column name for x-axis (typically time)
#' @param y Column name for y-axis (value)
#' @param title Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param color Line color
#'
#' @return ggplot2 object or base plot
#'
plot_time_series <- function(data, x, y, title = "Time Series Plot",
                             xlab = "Time", ylab = "Value", color = "steelblue") {
  plot(data[[x]], data[[y]],
    type = "l",
    main = title,
    xlab = xlab,
    ylab = ylab,
    col = color,
    lwd = 2
  )
  grid()
}

#' Create a Distribution Plot
#'
#' @param data Numeric vector
#' @param title Plot title
#' @param bins Number of histogram bins
#' @param color Bar color
#'
#' @return Base plot object
#'
plot_distribution <- function(data, title = "Distribution Plot",
                              bins = 30, color = "steelblue") {
  hist(data,
    main = title,
    xlab = "Value",
    ylab = "Frequency",
    breaks = bins,
    col = color,
    border = "white"
  )
  grid(nx = NA, ny = NULL)
}

#' Create a Comparison Bar Plot
#'
#' @param data Data frame with categories and values
#' @param categories Column name for categories
#' @param values Column name for values
#' @param title Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param color Bar color
#'
#' @return Base plot object
#'
plot_comparison <- function(data, categories, values, title = "Comparison Plot",
                            xlab = "Category", ylab = "Value", color = "steelblue") {
  barplot(data[[values]],
    names.arg = data[[categories]],
    main = title,
    xlab = xlab,
    ylab = ylab,
    col = color,
    border = "white"
  )
  grid(nx = NA, ny = NULL)
}

#' Create a Scatter Plot
#'
#' @param data Data frame
#' @param x Column name for x-axis
#' @param y Column name for y-axis
#' @param title Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param color Point color
#' @param size Point size
#'
#' @return Base plot object
#'
plot_scatter <- function(data, x, y, title = "Scatter Plot",
                         xlab = "X", ylab = "Y", color = "steelblue", size = 2) {
  plot(data[[x]], data[[y]],
    main = title,
    xlab = xlab,
    ylab = ylab,
    col = color,
    pch = 16,
    cex = size
  )
  grid()
}

#' Create a Box Plot
#'
#' @param data Data frame
#' @param group_col Column name for grouping
#' @param value_col Column name with values
#' @param title Plot title
#' @param ylab Y-axis label
#' @param color Box color
#'
#' @return Base plot object
#'
plot_boxplot <- function(data, group_col, value_col, title = "Box Plot",
                         ylab = "Value", color = "steelblue") {
  boxplot(data[[value_col]] ~ data[[group_col]],
    main = title,
    ylab = ylab,
    col = color,
    border = "darkgray"
  )
  grid(nx = NA, ny = NULL)
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Validate Data Input
#'
#' @param data Data to validate
#' @param type Expected data type
#'
#' @return Logical indicating validity
#'
validate_data <- function(data, type = "numeric") {
  if (is.null(data)) return(FALSE)
  if (type == "numeric") return(is.numeric(data))
  if (type == "dataframe") return(is.data.frame(data))
  TRUE
}

#' Format Numbers with Commas
#'
#' @param x Numeric value
#' @param digits Number of decimal places
#'
#' @return Formatted string
#'
format_number <- function(x, digits = 2) {
  format(round(x, digits = digits), big.mark = ",", scientific = FALSE)
}

#' Remove Missing Values
#'
#' @param data Data frame or vector
#'
#' @return Data with NA values removed
#'
clean_missing <- function(data) {
  if (is.data.frame(data)) {
    na.omit(data)
  } else {
    data[!is.na(data)]
  }
}
