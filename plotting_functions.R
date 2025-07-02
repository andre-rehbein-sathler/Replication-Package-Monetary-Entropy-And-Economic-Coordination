# ============================================================================
# PLOTTING AND VISUALIZATION FUNCTIONS
# "Monetary Entropy and Economic Coordination: An Impossibility Theorem"
# Author: Anonymous Author
# Journal of Political Economy - Replication Package
# ============================================================================

# Publication-quality plotting functions for entropy analysis and 
# experimental results visualization

# Required packages
suppressPackageStartupMessages({
  library(ggplot2)
  library(gridExtra)
  library(scales)
  library(RColorBrewer)
  library(cowplot)
  library(viridis)
  library(lubridate)
  library(zoo)
})

# ============================================================================
# THEME AND COLOR CONFIGURATION
# ============================================================================

#' Create JPE publication theme
#' 
#' @param base_size Base font size
#' @param base_family Font family
#' @return ggplot2 theme object
theme_jpe <- function(base_size = 12, base_family = "serif") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Text elements
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5, margin = margin(b = 20)),
      plot.subtitle = element_text(size = base_size, hjust = 0.5, color = "gray40", margin = margin(b = 10)),
      plot.caption = element_text(size = base_size - 2, color = "gray50", hjust = 0, margin = margin(t = 10)),
      
      # Axis elements
      axis.title = element_text(size = base_size, face = "bold"),
      axis.text = element_text(size = base_size - 1),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      
      # Legend elements
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 10),
      
      # Panel elements
      panel.grid.major = element_line(color = "gray90", size = 0.5),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5),
      
      # Strip text for facets
      strip.text = element_text(size = base_size, face = "bold"),
      strip.background = element_rect(fill = "gray95", color = "black"),
      
      # Plot margins
      plot.margin = margin(20, 20, 20, 20)
    )
}

#' Get JPE color palette
#' 
#' @param n Number of colors needed
#' @param type Type of palette ("categorical", "sequential", "diverging")
#' @return Vector of hex colors
get_jpe_colors <- function(n = 6, type = "categorical") {
  
  if (type == "categorical") {
    # Colorblind-friendly categorical palette
    colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
    return(colors[1:min(n, length(colors))])
    
  } else if (type == "sequential") {
    # Blue sequential palette
    return(brewer.pal(min(n, 9), "Blues"))
    
  } else if (type == "diverging") {
    # Red-Blue diverging palette
    return(brewer.pal(min(n, 11), "RdBu"))
    
  } else {
    stop("Unknown palette type. Use 'categorical', 'sequential', or 'diverging'")
  }
}

# ============================================================================
# ENTROPY TIME SERIES PLOTS
# ============================================================================

#' Plot entropy time series with smoothing
#' 
#' @param data Data frame with 'date' and 'entropy' columns
#' @param smooth_window Window for moving average (default 10)
#' @param add_events Optional data frame with event dates and labels
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return ggplot object
plot_entropy_timeseries <- function(data, smooth_window = 10, add_events = NULL, 
                                   title = "Monetary Entropy Index", 
                                   subtitle = "Time series with smoothed trend") {
  
  # Validate input
  required_cols <- c("date", "entropy")
  if (!all(required_cols %in% names(data))) {
    stop("Data must contain 'date' and 'entropy' columns")
  }
  
  # Prepare data
  plot_data <- data %>%
    filter(!is.na(entropy) & !is.na(date)) %>%
    arrange(date) %>%
    mutate(
      smooth_entropy = zoo::rollapply(entropy, width = smooth_window, FUN = mean, 
                                     align = "center", fill = NA, na.rm = TRUE)
    )
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = date)) +
    geom_line(aes(y = entropy), color = "lightblue", alpha = 0.6, size = 0.5) +
    geom_line(aes(y = smooth_entropy), color = get_jpe_colors(1), size = 1.2) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Date",
      y = "Monetary Entropy",
      caption = "Note: Blue line shows raw entropy, thick line shows smoothed trend"
    ) +
    theme_jpe() +
    scale_y_continuous(labels = number_format(accuracy = 0.001))
  
  # Add event markers if provided
  if (!is.null(add_events)) {
    if (all(c("date", "label") %in% names(add_events))) {
      p <- p +
        geom_vline(data = add_events, aes(xintercept = as.numeric(date)), 
                   linetype = "dashed", color = "red", alpha = 0.7) +
        geom_text(data = add_events, aes(x = date, y = Inf, label = label),
                  vjust = 1.1, hjust = 0.5, angle = 90, size = 3, color = "red")
    }
  }
  
  return(p)
}

#' Plot entropy with volatility overlay
#' 
#' @param data Data frame with 'date', 'entropy', and 'volatility' columns
#' @param title Plot title
#' @return ggplot object
plot_entropy_volatility_overlay <- function(data, title = "Entropy and Volatility Over Time") {
  
  # Validate input
  required_cols <- c("date", "entropy", "volatility")
  if (!all(required_cols %in% names(data))) {
    stop("Data must contain 'date', 'entropy', and 'volatility' columns")
  }
  
  # Prepare data for dual y-axis
  plot_data <- data %>%
    filter(!is.na(entropy) & !is.na(volatility) & !is.na(date)) %>%
    arrange(date)
  
  # Scale volatility to match entropy range approximately
  entropy_range <- max(plot_data$entropy) - min(plot_data$entropy)
  vol_range <- max(plot_data$volatility) - min(plot_data$volatility)
  vol_scaling <- entropy_range / vol_range
  
  plot_data <- plot_data %>%
    mutate(volatility_scaled = (volatility - min(volatility)) * vol_scaling + min(entropy))
  
  # Create plot
  p <- ggplot(plot_data, aes(x = date)) +
    geom_line(aes(y = entropy, color = "Entropy"), size = 1) +
    geom_line(aes(y = volatility_scaled, color = "Volatility"), size = 1, alpha = 0.8) +
    scale_color_manual(values = c("Entropy" = get_jpe_colors(1), "Volatility" = get_jpe_colors(2)[2])) +
    labs(
      title = title,
      x = "Date",
      y = "Entropy",
      color = "Measure",
      caption = "Note: Volatility scaled to match entropy range for visualization"
    ) +
    theme_jpe() +
    scale_y_continuous(
      labels = number_format(accuracy = 0.001),
      sec.axis = sec_axis(~ (. - min(plot_data$entropy))/vol_scaling + min(plot_data$volatility), 
                         name = "Volatility", labels = number_format(accuracy = 0.001))
    )
  
  return(p)
}

#