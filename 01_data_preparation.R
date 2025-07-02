# ============================================================================
# DATA PREPARATION AND CLEANING
# "Monetary Entropy and Economic Coordination: An Impossibility Theorem"
# Author: Anonymous Author
# Journal of Political Economy - Replicat("Experimental results summary:\n")
print(experiment_summary)

# ============================================================================
# 5. PROCESS CROSS-STOCK COMPARISON DATA
# ============================================================================

cat("\nStep 5: Processing cross-stock comparison data\n")
cat("==============================================\n")

# Clean and validate cross-stock data
stocks_clean <- stocks_comparison %>%
  # Standardize symbol names
  mutate(symbol = str_trim(str_to_upper(symbol))) %>%
  # Validate entropy calculations
  mutate(
    entropy_theoretical = 0.5 * log(1 + volatility^2),
    entropy_error = abs(entropy - entropy_theoretical),
    entropy_valid = entropy_error < 1e-6
  ) %>%
  # Add rankings
  arrange(desc(entropy)) %>%
  mutate(
    entropy_rank = row_number(),
    volatility_rank = rank(desc(volatility))
  )

# Check entropy calculation accuracy
entropy_errors <- stocks_clean %>%
  filter(!entropy_valid) %>%
  nrow()

if (entropy_errors > 0) {
  cat("Warning:", entropy_errors, "stocks have entropy calculation errors\n")
} else {
  cat("✓ All entropy calculations match theoretical formula\n")
}

# Calculate cross-stock correlations
if (nrow(stocks_clean) > 2) {
  volatility_entropy_cor <- cor(stocks_clean$volatility, stocks_clean$entropy)
  cat("Volatility-entropy correlation:", round(volatility_entropy_cor, 4), "\n")
  
  if (volatility_entropy_cor > 0.95) {
    cat("✓ Strong correlation confirms theoretical relationship\n")
  } else {
    cat("⚠ Correlation lower than expected\n")
  }
}

cat("Cross-stock data summary:\n")
print(stocks_clean %>% select(symbol, volatility, entropy, entropy_rank))

# ============================================================================
# 6. CREATE SYNTHETIC EURO AND FLASH CRASH DATA
# ============================================================================

cat("\nStep 6: Creating synthetic time series for experiments\n")
cat("=====================================================\n")

# Create synthetic Euro introduction data based on summary results
euro_results <- summary_clean %>% filter(Experiment == "Euro Introduction")

if (nrow(euro_results) > 0) {
  # Create monthly data from 1995-2010
  euro_dates <- seq(from = as.Date("1995-01-01"), 
                   to = as.Date("2010-12-31"), 
                   by = "month")
  
  euro_treatment_date <- as.Date("1999-01-01")
  
  # Countries
  eurozone_countries <- c("Germany", "France", "Italy", "Spain", "Netherlands", 
                         "Belgium", "Austria", "Portugal", "Finland", "Ireland", 
                         "Luxembourg", "Greece")
  control_countries <- c("United Kingdom", "Sweden", "Denmark")
  
  # Create synthetic panel data
  euro_data <- expand_grid(
    date = euro_dates,
    country = c(eurozone_countries, control_countries)
  ) %>%
    mutate(
      post_euro = date >= euro_treatment_date,
      emu_member = country %in% eurozone_countries,
      treatment = post_euro & emu_member
    )
  
  # Add synthetic interest rate dispersion and entropy based on summary results
  std_pre <- euro_results$Pre_Treatment[euro_results$Variable == "std"]
  std_post <- euro_results$Post_Treatment[euro_results$Variable == "std"] 
  entropy_pre <- euro_results$Pre_Treatment[euro_results$Variable == "entropy"]
  entropy_post <- euro_results$Post_Treatment[euro_results$Variable == "entropy"]
  
  euro_data <- euro_data %>%
    group_by(date) %>%
    mutate(
      # Synthetic interest rate dispersion
      rate_dispersion = case_when(
        !any(treatment) ~ rnorm(1, std_pre, std_pre * 0.1),
        any(treatment) ~ rnorm(1, std_post, std_post * 0.1),
        TRUE ~ rnorm(1, std_pre, std_pre * 0.1)
      ),
      # Synthetic monetary entropy
      monetary_entropy = case_when(
        !any(treatment) ~ rnorm(1, entropy_pre, entropy_pre * 0.1),
        any(treatment) ~ rnorm(1, entropy_post, entropy_post * 0.1),
        TRUE ~ rnorm(1, entropy_pre, entropy_pre * 0.1)
      )
    ) %>%
    ungroup()
  
  cat("✓ Euro introduction synthetic data created:", nrow(euro_data), "observations\n")
} else {
  cat("⚠ No Euro introduction data found in summary results\n")
  euro_data <- data.frame()
}

# Create synthetic Flash Crash data
flash_results <- summary_clean %>% filter(Experiment == "Flash Crash")

if (nrow(flash_results) > 0) {
  # Create minute-level data for May 6, 2010
  flash_date <- as.Date("2010-05-06")
  flash_times <- seq(from = as.POSIXct(paste(flash_date, "09:30:00")),
                    to = as.POSIXct(paste(flash_date, "16:00:00")),
                    by = "1 min")
  
  # Define crash period (2:42-3:07 PM ET)
  crash_start <- as.POSIXct(paste(flash_date, "14:42:00"))
  crash_end <- as.POSIXct(paste(flash_date, "15:07:00"))
  
  vol_pre <- flash_results$Pre_Treatment[flash_results$Variable == "rolling_vol_15min"]
  vol_post <- flash_results$Post_Treatment[flash_results$Variable == "rolling_vol_15min"]
  welfare_pre <- flash_results$Pre_Treatment[flash_results$Variable == "welfare_loss"]
  welfare_post <- flash_results$Post_Treatment[flash_results$Variable == "welfare_loss"]
  
  flash_data <- data.frame(
    datetime = flash_times
  ) %>%
    mutate(
      crash_period = datetime >= crash_start & datetime <= crash_end,
      # Synthetic rolling volatility
      rolling_vol_15min = ifelse(crash_period, 
                                rnorm(n(), vol_post, vol_post * 0.1),
                                rnorm(n(), vol_pre, vol_pre * 0.1)),
      # Synthetic welfare loss
      welfare_loss = ifelse(crash_period,
                           rnorm(n(), welfare_post, welfare_post * 0.1), 
                           rnorm(n(), welfare_pre, welfare_pre * 0.1)),
      # Calculate entropy from volatility
      entropy = 0.5 * log(1 + rolling_vol_15min^2)
    )
  
  cat("✓ Flash crash synthetic data created:", nrow(flash_data), "observations\n")
} else {
  cat("⚠ No Flash Crash data found in summary results\n")
  flash_data <- data.frame()
}

# ============================================================================
# 7. SAVE PROCESSED DATA
# ============================================================================

cat("\nStep 7: Saving processed data files\n")
cat("===================================\n")

# Create processed data directory
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# Save cleaned AAPL data
write_csv(aapl_clean, "data/processed/aapl_clean.csv")
cat("✓ Saved: data/processed/aapl_clean.csv\n")

# Save cleaned experimental results
write_csv(summary_clean, "data/processed/summary_results_clean.csv")
cat("✓ Saved: data/processed/summary_results_clean.csv\n")

# Save cleaned cross-stock data
write_csv(stocks_clean, "data/processed/stocks_comparison_clean.csv") 
cat("✓ Saved: data/processed/stocks_comparison_clean.csv\n")

# Save synthetic Euro data
if (nrow(euro_data) > 0) {
  write_csv(euro_data, "data/processed/euro_synthetic.csv")
  cat("✓ Saved: data/processed/euro_synthetic.csv\n")
}

# Save synthetic Flash Crash data  
if (nrow(flash_data) > 0) {
  write_csv(flash_data, "data/processed/flash_crash_synthetic.csv")
  cat("✓ Saved: data/processed/flash_crash_synthetic.csv\n")
}

# ============================================================================
# 8. DATA QUALITY REPORT
# ============================================================================

cat("\nStep 8: Generating data quality report\n")
cat("======================================\n")

# Create comprehensive data quality report
data_quality_report <- list(
  # Summary statistics
  aapl_summary = aapl_clean %>%
    summarise(
      n_obs = n(),
      date_range = paste(range(date, na.rm = TRUE), collapse = " to "),
      returns_complete = sum(!is.na(returns)),
      volatility_complete = sum(!is.na(volatility)),
      entropy_complete = sum(!is.na(entropy)),
      returns_mean = mean(returns, na.rm = TRUE),
      returns_sd = sd(returns, na.rm = TRUE),
      volatility_mean = mean(volatility, na.rm = TRUE),
      entropy_mean = mean(entropy, na.rm = TRUE),
      entropy_valid = entropy_validation$valid
    ),
  
  # Experimental results summary
  experiments_summary = summary_clean %>%
    group_by(Experiment) %>%
    summarise(
      n_variables = n(),
      all_significant = all(P_Value < 0.05),
      mean_effect_size = mean(abs(Pct_Change), na.rm = TRUE),
      .groups = "drop"
    ),
  
  # Cross-stock summary
  stocks_summary = stocks_clean %>%
    summarise(
      n_stocks = n(),
      volatility_range = paste(range(volatility), collapse = " to "),
      entropy_range = paste(range(entropy), collapse = " to "),
      vol_entropy_correlation = cor(volatility, entropy),
      entropy_calculations_valid = all(entropy_valid)
    ),
  
  # Data completeness
  completeness = list(
    aapl_completeness = 1 - sum(is.na(aapl_clean)) / (nrow(aapl_clean) * ncol(aapl_clean)),
    summary_completeness = 1 - sum(is.na(summary_clean)) / (nrow(summary_clean) * ncol(summary_clean)),
    stocks_completeness = 1 - sum(is.na(stocks_clean)) / (nrow(stocks_clean) * ncol(stocks_clean))
  )
)

# Save data quality report
saveRDS(data_quality_report, "data/processed/data_quality_report.rds")

# Print summary
cat("Data Quality Summary:\n")
cat("====================\n")
cat("AAPL Data:\n")
cat("- Observations:", data_quality_report$aapl_summary$n_obs, "\n")
cat("- Date range:", data_quality_report$aapl_summary$date_range, "\n")
cat("- Completeness:", round(data_quality_report$completeness$aapl_completeness * 100, 1), "%\n")
cat("- Entropy validation:", ifelse(data_quality_report$aapl_summary$entropy_valid, "PASSED", "FAILED"), "\n\n")

cat("Experimental Results:\n")
print(data_quality_report$experiments_summary)
cat("\n")

cat("Cross-Stock Analysis:\n")
cat("- Number of stocks:", data_quality_report$stocks_summary$n_stocks, "\n")
cat("- Volatility-entropy correlation:", round(data_quality_report$stocks_summary$vol_entropy_correlation, 4), "\n")
cat("- Entropy calculations valid:", ifelse(data_quality_report$stocks_summary$entropy_calculations_valid, "YES", "NO"), "\n\n")

# ============================================================================
# 9. VALIDATION AND FINAL CHECKS
# ============================================================================

cat("Step 9: Final validation checks\n")
cat("===============================\n")

# Check that all required outputs exist
required_outputs <- c(
  "data/processed/aapl_clean.csv",
  "data/processed/summary_results_clean.csv", 
  "data/processed/stocks_comparison_clean.csv"
)

all_outputs_exist <- all(file.exists(required_outputs))

if (all_outputs_exist) {
  cat("✓ All required output files created successfully\n")
} else {
  missing_outputs <- required_outputs[!file.exists(required_outputs)]
  cat("✗ Missing output files:", paste(missing_outputs, collapse = ", "), "\n")
}

# Final validation summary
validation_summary <- list(
  data_loaded = TRUE,
  aapl_cleaned = nrow(aapl_clean) > 0,
  experiments_processed = nrow(summary_clean) > 0,
  stocks_processed = nrow(stocks_clean) > 0,
  entropy_validated = entropy_validation$valid,
  outputs_created = all_outputs_exist,
  overall_success = all_outputs_exist && entropy_validation$valid
)

cat("\nValidation Summary:\n")
cat("==================\n")
cat("Data loading:", ifelse(validation_summary$data_loaded, "✓", "✗"), "\n")
cat("AAPL cleaning:", ifelse(validation_summary$aapl_cleaned, "✓", "✗"), "\n")
cat("Experiments processing:", ifelse(validation_summary$experiments_processed, "✓", "✗"), "\n")
cat("Stocks processing:", ifelse(validation_summary$stocks_processed, "✓", "✗"), "\n")
cat("Entropy validation:", ifelse(validation_summary$entropy_validated, "✓", "✗"), "\n")
cat("Output files:", ifelse(validation_summary$outputs_created, "✓", "✗"), "\n")
cat("Overall success:", ifelse(validation_summary$overall_success, "✓", "✗"), "\n")

if (validation_summary$overall_success) {
  cat("\n🎉 DATA PREPARATION COMPLETED SUCCESSFULLY!\n")
  cat("Ready to proceed with entropy calculations and analysis.\n")
} else {
  cat("\n❌ DATA PREPARATION ENCOUNTERED ISSUES\n")
  cat("Please review validation summary above.\n")
}

# Save validation results
saveRDS(validation_summary, "data/processed/validation_summary.rds")

cat("\nNext steps:\n")
cat("- Run 02_entropy_calculation.R to build Monetary Entropy Index\n")
cat("- Check data/processed/ folder for cleaned datasets\n")
cat("- Review data_quality_report.rds for detailed statistics\n")

cat("\n=== DATA PREPARATION SCRIPT COMPLETED ===\n")ion Package
# ============================================================================

# This script cleans and prepares all raw data for analysis
# Input: Raw CSV files in data/raw/
# Output: Cleaned data files in data/processed/

# Load required packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(zoo)
  library(data.table)
  library(janitor)
})

# Source utility functions
source("code/functions/entropy_functions.R")

cat("=== DATA PREPARATION AND CLEANING ===\n")
cat("Step 1: Loading and validating raw data\n")
cat("======================================\n")

# ============================================================================
# 1. LOAD RAW DATA FILES
# ============================================================================

# Check if raw data files exist
required_files <- c(
  "data/raw/aapl_real_data.csv",
  "data/raw/stocks_comparison.csv", 
  "data/raw/summary_results.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing required data files: ", paste(missing_files, collapse = ", "))
}

# Load AAPL data
cat("Loading AAPL stock data...\n")
aapl_raw <- read_csv("data/raw/aapl_real_data.csv", 
                     col_types = cols(
                       timestamp = col_integer(),
                       open = col_double(),
                       high = col_double(),
                       low = col_double(),
                       close = col_double(),
                       volume = col_integer(),
                       date = col_character(),
                       returns = col_double(),
                       volatility = col_double(),
                       spread_proxy = col_double()
                     ),
                     show_col_types = FALSE)

# Load stock comparison data
cat("Loading stock comparison data...\n")
stocks_comparison <- read_csv("data/raw/stocks_comparison.csv",
                             col_types = cols(
                               symbol = col_character(),
                               volatility = col_double(),
                               entropy = col_double()
                             ),
                             show_col_types = FALSE)

# Load summary results
cat("Loading experimental results data...\n")
summary_results <- read_csv("data/raw/summary_results.csv",
                           col_types = cols(
                             Experiment = col_character(),
                             Variable = col_character(),
                             Pre_Treatment = col_double(),
                             Post_Treatment = col_double(),
                             Difference = col_double(),
                             P_Value = col_double()
                           ),
                           show_col_types = FALSE)

cat("Raw data loaded successfully:\n")
cat("- AAPL observations:", nrow(aapl_raw), "\n")
cat("- Stock comparisons:", nrow(stocks_comparison), "\n") 
cat("- Experimental results:", nrow(summary_results), "\n\n")

# ============================================================================
# 2. DATA VALIDATION AND QUALITY CHECKS
# ============================================================================

cat("Step 2: Data validation and quality checks\n")
cat("==========================================\n")

# Function to check for missing values
check_missing <- function(data, name) {
  missing_counts <- data %>%
    summarise_all(~sum(is.na(.))) %>%
    gather(variable, missing_count) %>%
    filter(missing_count > 0)
  
  if (nrow(missing_counts) > 0) {
    cat("Missing values in", name, ":\n")
    print(missing_counts)
  } else {
    cat("No missing values in", name, "\n")
  }
  return(missing_counts)
}

# Check AAPL data
aapl_missing <- check_missing(aapl_raw, "AAPL data")

# Check stocks comparison
stocks_missing <- check_missing(stocks_comparison, "stocks comparison")

# Check summary results  
summary_missing <- check_missing(summary_results, "summary results")

# Validate AAPL price data consistency
cat("\nValidating AAPL price data consistency...\n")
price_issues <- aapl_raw %>%
  filter(!is.na(open) & !is.na(high) & !is.na(low) & !is.na(close)) %>%
  mutate(
    high_valid = high >= pmax(open, close),
    low_valid = low <= pmin(open, close),
    positive_prices = open > 0 & high > 0 & low > 0 & close > 0
  ) %>%
  summarise(
    invalid_high = sum(!high_valid),
    invalid_low = sum(!low_valid), 
    non_positive = sum(!positive_prices)
  )

if (any(price_issues > 0)) {
  cat("Price data issues found:\n")
  print(price_issues)
} else {
  cat("✓ All price data passes consistency checks\n")
}

# ============================================================================
# 3. CLEAN AND PROCESS AAPL DATA
# ============================================================================

cat("\nStep 3: Cleaning and processing AAPL data\n")
cat("=========================================\n")

# Clean AAPL data
aapl_clean <- aapl_raw %>%
  # Convert date column to proper datetime
  mutate(
    date_parsed = case_when(
      !is.na(timestamp) ~ as_datetime(timestamp),
      !is.na(date) ~ ymd_hms(date),
      TRUE ~ NA_POSIXct_
    )
  ) %>%
  # Sort by date
  arrange(date_parsed) %>%
  # Calculate returns if missing
  mutate(
    returns_calc = log(close / lag(close)),
    returns = coalesce(returns, returns_calc)
  ) %>%
  # Calculate volatility if missing (20-day rolling window)
  mutate(
    volatility_calc = zoo::rollapply(returns, width = 20, FUN = sd, 
                                   align = "right", fill = NA, na.rm = TRUE),
    volatility = coalesce(volatility, volatility_calc)
  ) %>%
  # Calculate spread proxy if missing
  mutate(
    spread_proxy_calc = (high - low) / close,
    spread_proxy = coalesce(spread_proxy, spread_proxy_calc)
  ) %>%
  # Remove extreme outliers (>5 standard deviations in returns)
  filter(is.na(returns) | abs(scale(returns)[,1]) < 5) %>%
  # Calculate entropy using theoretical formula
  mutate(
    entropy = case_when(
      !is.na(volatility) & volatility > 0 ~ 0.5 * log(1 + volatility^2),
      TRUE ~ NA_real_
    )
  ) %>%
  # Select final columns
  select(
    timestamp, date = date_parsed, open, high, low, close, volume,
    returns, volatility, spread_proxy, entropy
  ) %>%
  # Remove rows with all missing key variables
  filter(!(is.na(returns) & is.na(volatility) & is.na(entropy)))

cat("AAPL data cleaning completed:\n")
cat("- Original observations:", nrow(aapl_raw), "\n")
cat("- After cleaning:", nrow(aapl_clean), "\n")
cat("- Returns calculated:", sum(!is.na(aapl_clean$returns)), "\n")
cat("- Volatility calculated:", sum(!is.na(aapl_clean$volatility)), "\n") 
cat("- Entropy calculated:", sum(!is.na(aapl_clean$entropy)), "\n")

# Validate entropy calculations
entropy_validation <- validate_entropy_bounds(aapl_clean$entropy)
if (entropy_validation$valid) {
  cat("✓ Entropy calculations pass theoretical bounds validation\n")
} else {
  cat("✗ Entropy validation issues detected\n")
  print(entropy_validation)
}

# ============================================================================
# 4. PROCESS EXPERIMENTAL RESULTS DATA
# ============================================================================

cat("\nStep 4: Processing experimental results\n")
cat("=======================================\n")

# Clean and validate experimental results
summary_clean <- summary_results %>%
  # Standardize experiment names
  mutate(
    Experiment = str_trim(Experiment),
    Variable = str_trim(Variable)
  ) %>%
  # Add derived variables
  mutate(
    Pct_Change = (Difference / Pre_Treatment) * 100,
    Effect_Size = abs(Difference) / sqrt((Pre_Treatment^2 + Post_Treatment^2) / 2),
    Significant = P_Value < 0.05,
    Highly_Significant = P_Value < 0.001
  ) %>%
  # Validate that differences match
  mutate(
    Difference_Check = Post_Treatment - Pre_Treatment,
    Difference_Valid = abs(Difference - Difference_Check) < 1e-10
  )

# Check for calculation errors
calc_errors <- summary_clean %>%
  filter(!Difference_Valid) %>%
  nrow()

if (calc_errors > 0) {
  cat("Warning:", calc_errors, "rows have inconsistent difference calculations\n")
} else {
  cat("✓ All difference calculations are consistent\n")
}

# Summary of experiments
experiment_summary <- summary_clean %>%
  group_by(Experiment) %>%
  summarise(
    n_variables = n(),
    all_significant = all(Significant),
    mean_effect_size = mean(Effect_Size, na.rm = TRUE),
    .groups = "drop"
  )

cat