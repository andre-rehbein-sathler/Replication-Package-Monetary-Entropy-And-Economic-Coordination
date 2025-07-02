# ============================================================================
# WELFARE LOSS CALCULATION FUNCTIONS
# "Monetary Entropy and Economic Coordination: An Impossibility Theorem"
# Author: Anonymous Author
# Journal of Political Economy - Replication Package
# ============================================================================

# Functions for calculating welfare losses based on the Monetary Impossibility
# Theorem and various empirical proxies

# Required packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(zoo)
  library(MASS)
})

# ============================================================================
# THEORETICAL WELFARE LOSS FUNCTIONS
# ============================================================================

#' Calculate theoretical welfare loss from entropy using Impossibility Theorem
#' 
#' Implements the core relationship: ΔW ≥ κ × E²
#' 
#' @param entropy Vector of entropy values
#' @param kappa Structural parameter (default estimated from literature)
#' @param baseline_welfare Baseline welfare level for scaling
#' @return Vector of welfare loss estimates
calculate_theoretical_welfare_loss <- function(entropy, kappa = 0.5, baseline_welfare = 1) {
  
  if (any(entropy < 0, na.rm = TRUE)) {
    warning("Negative entropy values detected, setting to zero")
    entropy[entropy < 0] <- 0
  }
  
  # Theoretical relationship: ΔW = κ × E²
  welfare_loss <- kappa * entropy^2 * baseline_welfare
  
  # Handle potential numerical issues
  welfare_loss[is.infinite(welfare_loss) | is.nan(welfare_loss)] <- NA
  
  return(welfare_loss)
}

#' Calculate welfare loss difference between two entropy states
#' 
#' @param entropy_initial Initial entropy level
#' @param entropy_final Final entropy level  
#' @param kappa Structural parameter
#' @param baseline_welfare Baseline welfare scaling
#' @return Welfare change (negative = welfare improvement)
calculate_welfare_change <- function(entropy_initial, entropy_final, 
                                   kappa = 0.5, baseline_welfare = 1) {
  
  welfare_initial <- calculate_theoretical_welfare_loss(entropy_initial, kappa, baseline_welfare)
  welfare_final <- calculate_theoretical_welfare_loss(entropy_final, kappa, baseline_welfare)
  
  welfare_change <- welfare_final - welfare_initial
  
  return(welfare_change)
}

#' Estimate structural parameter κ from entropy-welfare data
#' 
#' @param entropy Vector of entropy values
#' @param welfare_loss Vector of observed welfare losses
#' @param method Estimation method ("ols", "robust", "constrained")
#' @return List with estimated κ and diagnostics
estimate_kappa_parameter <- function(entropy, welfare_loss, method = "ols") {
  
  # Remove missing values and prepare data
  complete_data <- data.frame(entropy = entropy, welfare = welfare_loss) %>%
    na.omit() %>%
    filter(entropy >= 0, welfare >= 0)
  
  if (nrow(complete_data) < 5) {
    stop("Insufficient data for κ estimation (need at least 5 observations)")
  }
  
  # Estimate κ using specified method
  if (method == "ols") {
    # Linear regression: W = κ × E²
    model <- lm(welfare ~ I(entropy^2) - 1, data = complete_data)
    kappa_est <- coef(model)[1]
    se <- summary(model)$coefficients[1, 2]
    r_squared <- summary(model)$r.squared
    
  } else if (method == "robust") {
    # Robust regression to handle outliers
    model <- rlm(welfare ~ I(entropy^2) - 1, data = complete_data)
    kappa_est <- coef(model)[1]
    se <- summary(model)$coefficients[1, 2]
    r_squared <- NA  # R² not directly available for robust regression
    
  } else if (method == "constrained") {
    # Constrained estimation (κ > 0)
    if (require(quadprog, quietly = TRUE)) {
      # Quadratic programming approach
      X <- cbind(complete_data$entropy^2)
      y <- complete_data$welfare
      
      # Solve: min ||y - X*kappa||² subject to kappa >= 0
      D <- t(X) %*% X
      d <- t(X) %*% y
      A <- matrix(1, 1, 1)  # Constraint matrix
      b <- 0  # kappa >= 0
      
      result <- solve.QP(D, d, A, b)
      kappa_est <- result$solution[1]
      
      # Calculate standard error
      residuals <- y - X %*% kappa_est
      mse <- sum(residuals^2) / (nrow(complete_data) - 1)
      var_kappa <- mse * solve(D)[1,1]
      se <- sqrt(var_kappa)
      
      # Calculate R²
      tss <- sum((y - mean(y))^2)
      rss <- sum(residuals^2)
      r_squared <- 1 - rss/tss
      
    } else {
      warning("quadprog package not available, falling back to OLS")
      model <- lm(welfare ~ I(entropy^2) - 1, data = complete_data)
      kappa_est <- max(0, coef(model)[1])  # Ensure non-negative
      se <- summary(model)$coefficients[1, 2]
      r_squared <- summary(model)$r.squared
    }
    
  } else {
    stop("Unknown method. Use 'ols', 'robust', or 'constrained'")
  }
  
  # Diagnostic tests
  fitted_values <- kappa_est * complete_data$entropy^2
  residuals <- complete_data$welfare - fitted_values
  
  # Durbin-Watson test for autocorrelation (if time series)
  dw_stat <- if (nrow(complete_data) > 3) {
    dw <- sum(diff(residuals)^2) / sum(residuals^2)
    dw
  } else {
    NA
  }
  
  results <- list(
    kappa = kappa_est,
    standard_error = se,
    r_squared = r_squared,
    n_observations = nrow(complete_data),
    method = method,
    fitted_values = fitted_values,
    residuals = residuals,
    durbin_watson = dw_stat,
    economic_significance = kappa_est > 0.1,  # Arbitrary threshold
    statistical_significance = if (!is.na(se)) abs(kappa_est/se) > 1.96 else NA
  )
  
  return(results)
}

# ============================================================================
# EMPIRICAL WELFARE LOSS PROXIES
# ============================================================================

#' Calculate welfare loss from price deviations (market microstructure)
#' 
#' @param prices Vector of observed prices
#' @param efficient_prices Vector of efficient/benchmark prices (e.g., VWAP)
#' @param method Method for calculating loss ("relative", "absolute", "squared")
#' @return Vector of welfare loss proxies
calculate_price_deviation_welfare_loss <- function(prices, efficient_prices, method = "relative") {
  
  if (length(prices) != length(efficient_prices)) {
    stop("Price vectors must have same length")
  }
  
  if (method == "relative") {
    # Relative deviation: |P - P_efficient| / P_efficient
    loss <- abs(prices - efficient_prices) / efficient_prices
    
  } else if (method == "absolute") {
    # Absolute deviation: |P - P_efficient|
    loss <- abs(prices - efficient_prices)
    
  } else if (method == "squared") {
    # Squared deviation: (P - P_efficient)²
    loss <- (prices - efficient_prices)^2
    
  } else {
    stop("Unknown method. Use 'relative', 'absolute', or 'squared'")
  }
  
  # Handle potential division by zero or infinite values
  loss[is.infinite(loss) | is.nan(loss)] <- NA
  
  return(loss)
}

#' Calculate welfare loss from bid-ask spreads
#' 
#' @param bid Vector of bid prices
#' @param ask Vector of ask prices
#' @param method Method for spread calculation ("relative", "absolute")
#' @return Vector of welfare loss estimates
calculate_spread_welfare_loss <- function(bid, ask, method = "relative") {
  
  if (length(bid) != length(ask)) {
    stop("Bid and ask vectors must have same length")
  }
  
  if (any(ask < bid, na.rm = TRUE)) {
    warning("Some ask prices are below bid prices - check data quality")
  }
  
  if (method == "relative") {
    # Relative spread: (Ask - Bid) / Midpoint
    midpoint <- (bid + ask) / 2
    loss <- (ask - bid) / midpoint
    
  } else if (method == "absolute") {
    # Absolute spread: Ask - Bid
    loss <- ask - bid
    
  } else {
    stop("Unknown method. Use 'relative' or 'absolute'")
  }
  
  # Ensure non-negative spreads
  loss[loss < 0] <- 0
  loss[is.infinite(loss) | is.nan(loss)] <- NA
  
  return(loss)
}

#' Calculate welfare loss from volume-weighted price deviations
#' 
#' @param prices Vector of transaction prices
#' @param volumes Vector of transaction volumes
#' @param benchmark_price Benchmark price (e.g., opening price, VWAP)
#' @param window Rolling window for VWAP calculation
#' @return Vector of volume-weighted welfare loss measures
calculate_volume_weighted_welfare_loss <- function(prices, volumes, benchmark_price = NULL, window = 20) {
  
  if (length(prices) != length(volumes)) {
    stop("Prices and volumes must have same length")
  }
  
  # Calculate VWAP if benchmark not provided
  if (is.null(benchmark_price)) {
    vwap <- zoo::rollapply(
      cbind(prices, volumes), 
      width = window, 
      FUN = function(x) sum(x[,1] * x[,2]) / sum(x[,2]),
      align = "right", 
      fill = NA,
      by.column = FALSE
    )
    benchmark_price <- vwap
  }
  
  # Volume-weighted deviation
  total_volume <- sum(volumes, na.rm = TRUE)
  if (total_volume == 0) {
    warning("Total volume is zero")
    return(rep(NA, length(prices)))
  }
  
  # Calculate weighted welfare loss
  volume_weights <- volumes / total_volume
  price_deviations <- abs(prices - benchmark_price) / benchmark_price
  welfare_loss <- price_deviations * volume_weights
  
  # Handle missing values
  welfare_loss[is.infinite(welfare_loss) | is.nan(welfare_loss)] <- NA
  
  return(welfare_loss)
}

# ============================================================================
# WELFARE LOSS AGGREGATION FUNCTIONS
# ============================================================================

#' Aggregate welfare losses across multiple markets or time periods
#' 
#' @param welfare_losses List of welfare loss vectors
#' @param weights Vector of market/period weights (must sum to 1)
#' @param method Aggregation method ("weighted_mean", "sum", "max")
#' @return Aggregated welfare loss measure
aggregate_welfare_losses <- function(welfare_losses, weights = NULL, method = "weighted_mean") {
  
  if (is.null(weights)) {
    weights <- rep(1/length(welfare_losses), length(welfare_losses))
  }
  
  if (length(welfare_losses) != length(weights)) {
    stop("Number of welfare loss series must match number of weights")
  }
  
  if (abs(sum(weights) - 1) > 1e-6) {
    stop("Weights must sum to 1")
  }
  
  # Ensure all series have same length
  min_length <- min(sapply(welfare_losses, length))
  welfare_losses <- lapply(welfare_losses, function(x) x[1:min_length])
  
  if (method == "weighted_mean") {
    # Weighted average across markets
    aggregated <- Reduce("+", Map("*", welfare_losses, weights))
    
  } else if (method == "sum") {
    # Simple sum (appropriate if losses are additive)
    aggregated <- Reduce("+", welfare_losses)
    
  } else if (method == "max") {
    # Maximum loss (worst-case scenario)
    welfare_matrix <- do.call(cbind, welfare_losses)
    aggregated <- apply(welfare_matrix, 1, max, na.rm = TRUE)
    
  } else {
    stop("Unknown method. Use 'weighted_mean', 'sum', or 'max'")
  }
  
  return(aggregated)
}

#' Calculate welfare loss statistics and confidence intervals
#' 
#' @param welfare_loss Vector of welfare loss values
#' @param confidence_level Confidence level for intervals (default 0.95)
#' @param bootstrap_n Number of bootstrap samples for CI
#' @return List with welfare loss statistics
calculate_welfare_statistics <- function(welfare_loss, confidence_level = 0.95, bootstrap_n = 1000) {
  
  # Remove missing values
  welfare_clean <- welfare_loss[!is.na(welfare_loss) & is.finite(welfare_loss)]
  
  if (length(welfare_clean) == 0) {
    warning("No valid welfare loss observations")
    return(list(
      n = 0,
      mean = NA,
      median = NA,
      sd = NA,
      min = NA,
      max = NA,
      ci_lower = NA,
      ci_upper = NA
    ))
  }
  
  # Basic statistics
  stats <- list(
    n = length(welfare_clean),
    mean = mean(welfare_clean),
    median = median(welfare_clean),
    sd = sd(welfare_clean),
    min = min(welfare_clean),
    max = max(welfare_clean),
    skewness = if (require(moments, quietly = TRUE)) moments::skewness(welfare_clean) else NA,
    kurtosis = if (require(moments, quietly = TRUE)) moments::kurtosis(welfare_clean) else NA
  )
  
  # Confidence intervals
  alpha <- 1 - confidence_level
  
  if (length(welfare_clean) >= 30) {
    # Normal approximation for large samples
    se <- stats$sd / sqrt(stats$n)
    z_critical <- qnorm(1 - alpha/2)
    stats$ci_lower <- stats$mean - z_critical * se
    stats$ci_upper <- stats$mean + z_critical * se
    stats$ci_method <- "normal"
    
  } else if (bootstrap_n > 0 && length(welfare_clean) >= 5) {
    # Bootstrap confidence intervals for small samples
    bootstrap_means <- replicate(bootstrap_n, {
      bootstrap_sample <- sample(welfare_clean, replace = TRUE)
      mean(bootstrap_sample)
    })
    
    stats$ci_lower <- quantile(bootstrap_means, alpha/2)
    stats$ci_upper <- quantile(bootstrap_means, 1 - alpha/2)
    stats$ci_method <- "bootstrap"
    
  } else {
    # No CI for very small samples
    stats$ci_lower <- NA
    stats$ci_upper <- NA
    stats$ci_method <- "insufficient_data"
  }
  
  return(stats)
}

# ============================================================================
# WELFARE LOSS VALIDATION FUNCTIONS
# ============================================================================

#' Validate welfare loss measures against economic theory
#' 
#' @param welfare_loss Vector of welfare loss values
#' @param entropy Vector of corresponding entropy values
#' @return List with validation results
validate_welfare_measures <- function(welfare_loss, entropy) {
  
  # Remove missing values
  complete_data <- data.frame(welfare = welfare_loss, entropy = entropy) %>%
    na.omit()
  
  if (nrow(complete_data) < 5) {
    warning("Insufficient data for welfare validation")
    return(list(valid = FALSE, message = "Insufficient data"))
  }
  
  validation_results <- list()
  
  # 1. Non-negativity check
  validation_results$non_negative <- all(complete_data$welfare >= 0)
  
  # 2. Monotonicity check (welfare should increase with entropy)
  if (nrow(complete_data) > 10) {
    correlation <- cor(complete_data$entropy, complete_data$welfare)
    validation_results$monotonic <- correlation > 0
    validation_results$correlation <- correlation
  } else {
    validation_results$monotonic <- NA
    validation_results$correlation <- NA
  }
  
  # 3. Theoretical relationship test (quadratic)
  if (nrow(complete_data) > 5) {
    # Test linear vs quadratic fit
    linear_model <- lm(welfare ~ entropy, data = complete_data)
    quadratic_model <- lm(welfare ~ I(entropy^2), data = complete_data)
    
    validation_results$linear_r2 <- summary(linear_model)$r.squared
    validation_results$quadratic_r2 <- summary(quadratic_model)$r.squared
    validation_results$quadratic_superior <- validation_results$quadratic_r2 > validation_results$linear_r2
  }
  
  # 4. Economic magnitude check (welfare losses should be reasonable)
  welfare_range <- max(complete_data$welfare) - min(complete_data$welfare)
  welfare_cv <- sd(complete_data$welfare) / mean(complete_data$welfare)
  
  validation_results$reasonable_magnitude <- welfare_cv > 0.1 & welfare_cv < 10  # Arbitrary bounds
  validation_results$coefficient_variation <- welfare_cv
  
  # 5. Overall validation
  validation_results$overall_valid <- all(c(
    validation_results$non_negative,
    validation_results$monotonic %||% TRUE,  # TRUE if NA
    validation_results$quadratic_superior %||% TRUE,
    validation_results$reasonable_magnitude
  ), na.rm = TRUE)
  
  return(validation_results)
}

#' Compare different welfare loss measures
#' 
#' @param welfare_measures List of different welfare loss vectors
#' @param measure_names Vector of names for the measures
#' @return Comparison matrix and statistics
compare_welfare_measures <- function(welfare_measures, measure_names = NULL) {
  
  if (is.null(measure_names)) {
    measure_names <- paste0("Measure_", 1:length(welfare_measures))
  }
  
  # Ensure equal length
  min_length <- min(sapply(welfare_measures, length))
  welfare_matrix <- do.call(cbind, lapply(welfare_measures, function(x) x[1:min_length]))
  colnames(welfare_matrix) <- measure_names
  
  # Calculate correlations
  correlation_matrix <- cor(welfare_matrix, use = "complete.obs")
  
  # Calculate relative statistics
  comparison_stats <- data.frame(
    Measure = measure_names,
    Mean = apply(welfare_matrix, 2, mean, na.rm = TRUE),
    SD = apply(welfare_matrix, 2, sd, na.rm = TRUE),
    Min = apply(welfare_matrix, 2, min, na.rm = TRUE),
    Max = apply(welfare_matrix, 2, max, na.rm = TRUE),
    CV = apply(welfare_matrix, 2, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
  )
  
  # Test for significant differences
  if (ncol(welfare_matrix) == 2) {
    # Paired t-test for two measures
    t_test_result <- t.test(welfare_matrix[,1], welfare_matrix[,2], paired = TRUE)
    comparison_stats$t_test_p_value <- c(t_test_result$p.value, NA)
  }
  
  results <- list(
    correlation_matrix = correlation_matrix,
    comparison_stats = comparison_stats,
    welfare_matrix = welfare_matrix,
    mean_correlation = mean(correlation_matrix[upper.tri(correlation_matrix)]),
    highly_correlated = all(correlation_matrix[upper.tri(correlation_matrix)] > 0.7)
  )
  
  return(results)
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Helper function for safe division
#' @param x Numerator
#' @param y Denominator
#' @param replace_inf Value to replace infinite results
safe_divide <- function(x, y, replace_inf = NA) {
  result <- x / y
  result[is.infinite(result) | is.nan(result)] <- replace_inf
  return(result)
}

#' Helper function for null coalescing
#' @param x First value
#' @param y Second value (used if x is NULL)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Print confirmation when sourced
cat("Welfare loss calculation functions loaded successfully.\n")
cat("Available functions:\n")
cat("- calculate_theoretical_welfare_loss(): Core ΔW ≥ κE² implementation\n")
cat("- estimate_kappa_parameter(): Estimate structural parameter κ\n")
cat("- calculate_price_deviation_welfare_loss(): Market microstructure losses\n")
cat("- calculate_spread_welfare_loss(): Bid-ask spread based losses\n")
cat("- aggregate_welfare_losses(): Multi-market aggregation\n")
cat("- validate_welfare_measures(): Economic theory validation\n")
cat("Ready for welfare analysis.\n")