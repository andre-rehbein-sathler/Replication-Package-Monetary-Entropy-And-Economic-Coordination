# ============================================================================
# ENTROPY CALCULATION FUNCTIONS
# "Monetary Entropy and Economic Coordination: An Impossibility Theorem"
# Author: Anonymous Author
# ============================================================================

# Required packages
require(tidyverse)
require(zoo)
require(rugarch)

# ============================================================================
# CORE ENTROPY FUNCTIONS
# ============================================================================

#' Calculate baseline entropy from time series data
#' 
#' @param data Data frame with volatility column
#' @param baseline_period Number of observations for baseline calculation
#' @return Scalar baseline entropy value
calculate_entropy_baseline <- function(data, baseline_period = 252) {
  baseline_vol <- data %>%
    filter(!is.na(volatility)) %>%
    slice_head(n = baseline_period) %>%
    pull(volatility) %>%
    var(na.rm = TRUE)
  
  return(0.5 * log(1 + baseline_vol))
}

#' Calculate monetary entropy using theoretical formula
#' 
#' @param volatility Vector of volatility measures
#' @param baseline_var Baseline variance for normalization
#' @return Vector of entropy values
calculate_monetary_entropy <- function(volatility, baseline_var = NULL) {
  if (is.null(baseline_var)) {
    baseline_var <- var(volatility, na.rm = TRUE)
  }
  
  # Theoretical formula: E = 0.5 * log(1 + σ²/σ²_baseline)
  entropy <- 0.5 * log(1 + volatility^2 / baseline_var)
  
  return(entropy)
}

#' Calculate Shannon entropy for discrete probability distribution
#' 
#' @param probs Vector of probabilities (must sum to 1)
#' @return Scalar Shannon entropy
shannon_entropy <- function(probs) {
  # Remove zero probabilities to avoid log(0)
  probs <- probs[probs > 0]
  probs <- probs / sum(probs)  # Normalize
  
  return(-sum(probs * log(probs)))
}

#' Calculate Rényi entropy of order α
#' 
#' @param probs Vector of probabilities
#' @param alpha Order parameter (default = 2)
#' @return Scalar Rényi entropy
renyi_entropy <- function(probs, alpha = 2) {
  if (alpha == 1) {
    return(shannon_entropy(probs))
  }
  
  probs <- probs[probs > 0]
  probs <- probs / sum(probs)
  
  return((1 / (1 - alpha)) * log(sum(probs^alpha)))
}

#' Calculate Tsallis entropy of order q
#' 
#' @param probs Vector of probabilities  
#' @param q Order parameter (default = 2)
#' @return Scalar Tsallis entropy
tsallis_entropy <- function(probs, q = 2) {
  if (q == 1) {
    return(shannon_entropy(probs))
  }
  
  probs <- probs[probs > 0]
  probs <- probs / sum(probs)
  
  return((sum(probs^q) - 1) / (q - 1))
}

# ============================================================================
# GARCH-BASED ENTROPY CALCULATION
# ============================================================================

#' Estimate GARCH(1,1) model and extract conditional volatility
#' 
#' @param returns Vector of return series
#' @param spec GARCH specification (default = GARCH(1,1))
#' @return List containing fitted model and conditional volatility
estimate_garch_volatility <- function(returns, spec = NULL) {
  
  if (is.null(spec)) {
    spec <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
      distribution.model = "norm"
    )
  }
  
  # Handle potential convergence issues
  tryCatch({
    fit <- ugarchfit(spec = spec, data = returns, solver = "hybrid")
    
    if (convergence(fit) != 0) {
      warning("GARCH model did not converge properly")
      # Calculate rolling volatility
    data <- data %>%
      arrange(date) %>%
      mutate(
        volatility = zoo::rollapply(returns, width = window, FUN = sd, 
                                   align = "right", fill = NA, na.rm = TRUE)
      ) %>%
      filter(!is.na(volatility))
    
    # Calculate baseline variance
    baseline_var <- var(data$volatility, na.rm = TRUE)
    
    # Calculate entropy
    data <- data %>%
      mutate(
        entropy = 0.5 * log(1 + volatility^2 / baseline_var),
        weighted_entropy = entropy * weight
      )
    
    return(data %>% select(date, entropy, weighted_entropy))
  })
  
  # Combine all markets into MEI
  mei_data <- market_entropies %>%
    reduce(function(x, y) {
      full_join(x, y, by = "date", suffix = c("", "_y"))
    }) %>%
    rowwise() %>%
    mutate(
      mei = sum(c_across(contains("weighted_entropy")), na.rm = TRUE)
    ) %>%
    select(date, mei) %>%
    arrange(date)
  
  return(mei_data)
}

#' Apply market-specific weights based on theoretical framework
#' 
#' @return Named vector of theoretically-motivated market weights
get_theoretical_weights <- function() {
  weights <- c(
    equity = 0.35,    # Corporate information and growth expectations
    bonds = 0.25,     # Monetary policy and sovereign risk information
    credit = 0.25,    # Systemic risk and financial stability information
    fx = 0.15         # International trade and capital flow information
  )
  
  return(weights)
}

# ============================================================================
# ENTROPY VALIDATION AND DIAGNOSTIC FUNCTIONS
# ============================================================================

#' Validate entropy measures against theoretical bounds
#' 
#' @param entropy_values Vector of calculated entropy values
#' @param distribution Distribution assumption ("gaussian", "uniform", etc.)
#' @return List with validation results
validate_entropy_bounds <- function(entropy_values, distribution = "gaussian") {
  
  # Remove missing values
  entropy_clean <- entropy_values[!is.na(entropy_values)]
  
  # Theoretical bounds depend on distribution
  if (distribution == "gaussian") {
    theoretical_max <- 0.5 * log(2 * pi * exp(1))  # Maximum entropy for Gaussian
    theoretical_min <- 0  # Minimum (perfect information)
  } else if (distribution == "uniform") {
    theoretical_max <- log(length(entropy_clean))
    theoretical_min <- 0
  } else {
    warning("Unknown distribution, using generic bounds")
    theoretical_max <- Inf
    theoretical_min <- 0
  }
  
  # Check bounds
  min_entropy <- min(entropy_clean)
  max_entropy <- max(entropy_clean)
  mean_entropy <- mean(entropy_clean)
  
  # Validation flags
  within_bounds <- (min_entropy >= theoretical_min) && (max_entropy <= theoretical_max)
  has_variation <- (max_entropy - min_entropy) > 1e-6
  
  return(list(
    min_entropy = min_entropy,
    max_entropy = max_entropy,
    mean_entropy = mean_entropy,
    theoretical_min = theoretical_min,
    theoretical_max = theoretical_max,
    within_bounds = within_bounds,
    has_variation = has_variation,
    n_observations = length(entropy_clean)
  ))
}

#' Test correlation between different entropy measures
#' 
#' @param data Data frame with multiple entropy columns
#' @param entropy_cols Vector of column names containing entropy measures
#' @return Correlation matrix and summary statistics
test_entropy_correlations <- function(data, entropy_cols) {
  
  # Extract entropy measures
  entropy_matrix <- data %>%
    select(all_of(entropy_cols)) %>%
    na.omit()
  
  if (nrow(entropy_matrix) == 0) {
    stop("No complete observations for entropy correlation test")
  }
  
  # Calculate correlation matrix
  cor_matrix <- cor(entropy_matrix)
  
  # Summary statistics
  upper_tri <- cor_matrix[upper.tri(cor_matrix)]
  
  results <- list(
    correlation_matrix = cor_matrix,
    mean_correlation = mean(upper_tri),
    min_correlation = min(upper_tri),
    max_correlation = max(upper_tri),
    all_positive = all(upper_tri > 0),
    high_correlation = all(upper_tri > 0.5),
    n_measures = length(entropy_cols),
    n_observations = nrow(entropy_matrix)
  )
  
  return(results)
}

# ============================================================================
# WELFARE LOSS CALCULATION FUNCTIONS
# ============================================================================

#' Calculate welfare loss from entropy using theoretical framework
#' 
#' @param entropy Vector of entropy values
#' @param kappa Structural parameter (default estimated from literature)
#' @param baseline_welfare Baseline welfare level for scaling
#' @return Vector of welfare loss estimates
calculate_welfare_loss <- function(entropy, kappa = 0.5, baseline_welfare = 1) {
  
  # Theoretical relationship: ΔW ≥ κ × E²
  welfare_loss <- kappa * entropy^2 * baseline_welfare
  
  return(welfare_loss)
}

#' Calculate welfare loss from price deviations (empirical proxy)
#' 
#' @param prices Vector of observed prices
#' @param efficient_prices Vector of efficient/benchmark prices
#' @param method Method for calculating loss ("absolute", "squared", "relative")
#' @return Vector of welfare loss proxies
calculate_empirical_welfare_loss <- function(prices, efficient_prices, method = "relative") {
  
  if (length(prices) != length(efficient_prices)) {
    stop("Price vectors must have same length")
  }
  
  if (method == "absolute") {
    loss <- abs(prices - efficient_prices)
  } else if (method == "squared") {
    loss <- (prices - efficient_prices)^2
  } else if (method == "relative") {
    loss <- abs(prices - efficient_prices) / efficient_prices
  } else {
    stop("Unknown method. Use 'absolute', 'squared', or 'relative'")
  }
  
  return(loss)
}

#' Estimate structural parameter κ from data
#' 
#' @param entropy Vector of entropy values
#' @param welfare_loss Vector of observed welfare losses
#' @param method Estimation method ("ols", "robust")
#' @return List with estimated κ and diagnostics
estimate_kappa <- function(entropy, welfare_loss, method = "ols") {
  
  # Remove missing values
  complete_data <- data.frame(entropy = entropy, welfare = welfare_loss) %>%
    na.omit()
  
  if (nrow(complete_data) < 10) {
    stop("Insufficient data for κ estimation")
  }
  
  # Estimate using specified method
  if (method == "ols") {
    # Linear regression: W = κ * E²
    model <- lm(welfare ~ I(entropy^2) - 1, data = complete_data)
    kappa_est <- coef(model)[1]
    se <- summary(model)$coefficients[1, 2]
    r_squared <- summary(model)$r.squared
  } else if (method == "robust") {
    # Robust regression
    require(MASS)
    model <- rlm(welfare ~ I(entropy^2) - 1, data = complete_data)
    kappa_est <- coef(model)[1]
    se <- summary(model)$coefficients[1, 2]
    r_squared <- NA  # R² not directly available for robust regression
  } else {
    stop("Unknown method. Use 'ols' or 'robust'")
  }
  
  results <- list(
    kappa = kappa_est,
    standard_error = se,
    r_squared = r_squared,
    n_observations = nrow(complete_data),
    method = method,
    model = model
  )
  
  return(results)
}

# ============================================================================
# TIME SERIES UTILITIES FOR ENTROPY ANALYSIS
# ============================================================================

#' Calculate rolling entropy with specified window
#' 
#' @param data Vector of time series data
#' @param window Rolling window size
#' @param min_obs Minimum observations required for calculation
#' @return Vector of rolling entropy values
rolling_entropy <- function(data, window = 20, min_obs = 10) {
  
  n <- length(data)
  rolling_ent <- rep(NA, n)
  
  for (i in window:n) {
    window_data <- data[(i - window + 1):i]
    window_data <- window_data[!is.na(window_data)]
    
    if (length(window_data) >= min_obs) {
      # Calculate volatility for this window
      vol <- sd(window_data, na.rm = TRUE)
      # Calculate entropy
      rolling_ent[i] <- 0.5 * log(1 + vol^2)
    }
  }
  
  return(rolling_ent)
}

#' Detect structural breaks in entropy time series
#' 
#' @param data Data frame with 'date' and 'entropy' columns
#' @param method Method for break detection ("bcp", "strucchange")
#' @param min_segment_length Minimum length for each segment
#' @return List with break dates and diagnostics
detect_entropy_breaks <- function(data, method = "bcp", min_segment_length = 50) {
  
  # Remove missing values
  clean_data <- data %>%
    filter(!is.na(entropy)) %>%
    arrange(date)
  
  if (nrow(clean_data) < 2 * min_segment_length) {
    stop("Insufficient data for break detection")
  }
  
  if (method == "bcp") {
    require(bcp)
    
    # Bayesian change point detection
    bcp_result <- bcp(clean_data$entropy, mcmc = 1000, burnin = 100)
    
    # Find probable change points
    prob_threshold <- 0.5
    change_points <- which(bcp_result$prob.mean > prob_threshold)
    
    break_dates <- clean_data$date[change_points]
    
    results <- list(
      method = "bcp",
      break_dates = break_dates,
      probabilities = bcp_result$prob.mean[change_points],
      n_breaks = length(break_dates),
      bcp_object = bcp_result
    )
    
  } else if (method == "strucchange") {
    require(strucchange)
    
    # Structural change detection
    ts_data <- ts(clean_data$entropy)
    bp_test <- breakpoints(ts_data ~ 1, h = min_segment_length / nrow(clean_data))
    
    if (length(bp_test$breakpoints) > 0) {
      break_indices <- bp_test$breakpoints
      break_dates <- clean_data$date[break_indices]
    } else {
      break_indices <- numeric(0)
      break_dates <- as.Date(character(0))
    }
    
    results <- list(
      method = "strucchange",
      break_dates = break_dates,
      break_indices = break_indices,
      n_breaks = length(break_dates),
      bp_object = bp_test
    )
    
  } else {
    stop("Unknown method. Use 'bcp' or 'strucchange'")
  }
  
  return(results)
}

# ============================================================================
# ENTROPY DECOMPOSITION FUNCTIONS
# ============================================================================

#' Decompose total entropy into systematic and idiosyncratic components
#' 
#' @param returns_matrix Matrix where columns are different assets/markets
#' @param method Decomposition method ("pca", "factor")
#' @return List with systematic and idiosyncratic entropy measures
decompose_entropy <- function(returns_matrix, method = "pca") {
  
  # Remove rows with any missing values
  complete_returns <- na.omit(returns_matrix)
  
  if (nrow(complete_returns) < 20) {
    stop("Insufficient complete observations for decomposition")
  }
  
  if (method == "pca") {
    # Principal component decomposition
    pca_result <- prcomp(complete_returns, center = TRUE, scale. = TRUE)
    
    # First PC represents systematic component
    systematic_factor <- pca_result$x[, 1]
    
    # Calculate systematic entropy
    systematic_entropy <- 0.5 * log(1 + var(systematic_factor))
    
    # Idiosyncratic components
    n_assets <- ncol(complete_returns)
    idiosyncratic_entropy <- numeric(n_assets)
    
    for (i in 1:n_assets) {
      residuals <- complete_returns[, i] - 
        (pca_result$rotation[i, 1] * systematic_factor)
      idiosyncratic_entropy[i] <- 0.5 * log(1 + var(residuals))
    }
    
    results <- list(
      method = "pca",
      systematic_entropy = systematic_entropy,
      idiosyncratic_entropy = idiosyncratic_entropy,
      total_entropy = systematic_entropy + mean(idiosyncratic_entropy),
      explained_variance = summary(pca_result)$importance[2, 1],
      pca_object = pca_result
    )
    
  } else if (method == "factor") {
    # Single-factor model decomposition
    require(psych)
    
    # Extract single factor
    factor_result <- fa(complete_returns, nfactors = 1, rotate = "none")
    
    # Calculate systematic entropy from factor scores
    systematic_factor <- as.numeric(factor_result$scores)
    systematic_entropy <- 0.5 * log(1 + var(systematic_factor))
    
    # Calculate idiosyncratic entropy from uniquenesses
    idiosyncratic_entropy <- 0.5 * log(1 + factor_result$uniquenesses)
    
    results <- list(
      method = "factor",
      systematic_entropy = systematic_entropy,
      idiosyncratic_entropy = idiosyncratic_entropy,
      total_entropy = systematic_entropy + mean(idiosyncratic_entropy),
      communalities = factor_result$communality,
      factor_object = factor_result
    )
    
  } else {
    stop("Unknown method. Use 'pca' or 'factor'")
  }
  
  return(results)
}

# ============================================================================
# HELPER UTILITIES
# ============================================================================

#' Convert volatility to entropy using different specifications
#' 
#' @param volatility Vector of volatility measures
#' @param specification Entropy specification ("baseline", "normalized", "log-normal")
#' @param baseline_vol Baseline volatility for normalization
#' @return Vector of entropy values
volatility_to_entropy <- function(volatility, specification = "baseline", baseline_vol = NULL) {
  
  if (specification == "baseline") {
    # Standard specification: E = 0.5 * log(1 + σ²)
    entropy <- 0.5 * log(1 + volatility^2)
    
  } else if (specification == "normalized") {
    # Normalized by baseline: E = 0.5 * log(1 + σ²/σ₀²)
    if (is.null(baseline_vol)) {
      baseline_vol <- mean(volatility, na.rm = TRUE)
    }
    entropy <- 0.5 * log(1 + volatility^2 / baseline_vol^2)
    
  } else if (specification == "log-normal") {
    # Log-normal specification: E = log(σ) + constant
    entropy <- log(volatility) + 0.5 * log(2 * pi * exp(1))
    
  } else {
    stop("Unknown specification. Use 'baseline', 'normalized', or 'log-normal'")
  }
  
  return(entropy)
}

#' Create summary statistics for entropy analysis
#' 
#' @param data Data frame with entropy and related variables
#' @param group_var Optional grouping variable
#' @return Data frame with summary statistics
entropy_summary_stats <- function(data, group_var = NULL) {
  
  if (is.null(group_var)) {
    # Overall summary
    summary_stats <- data %>%
      summarise(
        n_obs = n(),
        n_complete = sum(!is.na(entropy)),
        mean_entropy = mean(entropy, na.rm = TRUE),
        sd_entropy = sd(entropy, na.rm = TRUE),
        min_entropy = min(entropy, na.rm = TRUE),
        max_entropy = max(entropy, na.rm = TRUE),
        median_entropy = median(entropy, na.rm = TRUE),
        q25_entropy = quantile(entropy, 0.25, na.rm = TRUE),
        q75_entropy = quantile(entropy, 0.75, na.rm = TRUE),
        skewness = moments::skewness(entropy, na.rm = TRUE),
        kurtosis = moments::kurtosis(entropy, na.rm = TRUE)
      )
  } else {
    # Grouped summary
    summary_stats <- data %>%
      group_by(!!sym(group_var)) %>%
      summarise(
        n_obs = n(),
        n_complete = sum(!is.na(entropy)),
        mean_entropy = mean(entropy, na.rm = TRUE),
        sd_entropy = sd(entropy, na.rm = TRUE),
        min_entropy = min(entropy, na.rm = TRUE),
        max_entropy = max(entropy, na.rm = TRUE),
        median_entropy = median(entropy, na.rm = TRUE),
        q25_entropy = quantile(entropy, 0.25, na.rm = TRUE),
        q75_entropy = quantile(entropy, 0.75, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  return(summary_stats)
} Fallback to rolling window volatility
      vol <- zoo::rollapply(returns, width = 20, FUN = sd, 
                           align = "right", fill = NA, na.rm = TRUE)
      return(list(model = NULL, volatility = vol, converged = FALSE))
    }
    
    vol <- as.numeric(sigma(fit))
    return(list(model = fit, volatility = vol, converged = TRUE))
    
  }, error = function(e) {
    warning(paste("GARCH estimation failed:", e$message))
    # Fallback to rolling window volatility
    vol <- zoo::rollapply(returns, width = 20, FUN = sd, 
                         align = "right", fill = NA, na.rm = TRUE)
    return(list(model = NULL, volatility = vol, converged = FALSE))
  })
}

#' Calculate entropy from GARCH conditional volatility
#' 
#' @param garch_fit Fitted GARCH model from estimate_garch_volatility
#' @param baseline_var Baseline variance for normalization
#' @return Vector of entropy values
garch_entropy <- function(garch_fit, baseline_var = NULL) {
  
  if (!garch_fit$converged) {
    vol <- garch_fit$volatility
  } else {
    vol <- as.numeric(sigma(garch_fit$model))
  }
  
  if (is.null(baseline_var)) {
    baseline_var <- mean(vol^2, na.rm = TRUE)
  }
  
  # Calculate entropy using conditional variance
  entropy <- 0.5 * log(1 + vol^2 / baseline_var)
  
  return(entropy)
}

# ============================================================================
# MULTI-MARKET ENTROPY INDEX CONSTRUCTION
# ============================================================================

#' Construct Monetary Entropy Index (MEI) from multiple markets
#' 
#' @param market_data List of data frames, each with 'date' and 'returns' columns
#' @param weights Named vector of market weights (must sum to 1)
#' @param window Rolling window for volatility calculation
#' @return Data frame with date and MEI
construct_mei <- function(market_data, weights, window = 20) {
  
  # Validate inputs
  if (abs(sum(weights) - 1) > 1e-6) {
    stop("Market weights must sum to 1")
  }
  
  if (length(market_data) != length(weights)) {
    stop("Number of markets must match number of weights")
  }
  
  # Calculate entropy for each market
  market_entropies <- map2(market_data, weights, function(data, weight) {
    
    #