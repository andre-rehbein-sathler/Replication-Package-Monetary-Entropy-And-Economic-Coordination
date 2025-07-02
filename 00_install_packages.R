# ============================================================================
# PACKAGE INSTALLATION AND LOADING
# "Monetary Entropy and Economic Coordination: An Impossibility Theorem"
# Author: Anonymous Author
# Journal of Political Economy - Replication Package
# ============================================================================

# This script installs and loads all required packages for the replication
# Run this script first before executing any analysis code

cat("Installing and loading required packages...\n")
cat("This may take several minutes on first run.\n\n")

# ============================================================================
# CRAN PACKAGES
# ============================================================================

# Core data manipulation and analysis packages
cran_packages <- c(
  # Data manipulation
  "tidyverse",        # >= 2.0.0 (dplyr, ggplot2, readr, etc.)
  "data.table",       # >= 1.14.0 (fast data operations)
  "lubridate",        # >= 1.9.0 (date/time handling)
  "zoo",              # >= 1.8.0 (time series operations)
  "xts",              # >= 0.12.0 (extensible time series)
  
  # Econometric analysis
  "fixest",           # >= 0.11.0 (fast fixed effects)
  "sandwich",         # >= 3.0.0 (robust standard errors)
  "lmtest",           # >= 0.9.0 (linear model testing)
  "bcp",              # >= 4.0.0 (Bayesian change point)
  "strucchange",      # >= 1.5.0 (structural break detection)
  "urca",             # >= 1.3.0 (unit root tests)
  
  # Time series analysis
  "tseries",          # >= 0.10.0 (time series tests)
  "forecast",         # >= 8.20.0 (forecasting methods)
  "rugarch",          # >= 1.4.0 (GARCH models)
  "vars",             # >= 1.5.0 (vector autoregression)
  
  # Statistical testing and robust methods
  "car",              # >= 3.1.0 (companion to applied regression)
  "MASS",             # >= 7.3.0 (robust statistics)
  "moments",          # >= 0.14 (statistical moments)
  "nortest",          # >= 1.0.0 (normality tests)
  
  # Visualization
  "ggplot2",          # >= 3.4.0 (grammar of graphics)
  "gridExtra",        # >= 2.3.0 (grid arrangements)
  "RColorBrewer",     # >= 1.1.0 (color palettes)
  "scales",           # >= 1.2.0 (scale functions for ggplot2)
  "cowplot",          # >= 1.1.0 (publication-ready plots)
  "viridis",          # >= 0.6.0 (color scales)
  
  # Table generation
  "stargazer",        # >= 5.2.0 (regression tables)
  "xtable",           # >= 1.8.0 (LaTeX tables)
  "kableExtra",       # >= 1.3.0 (enhanced tables)
  "gt",               # >= 0.8.0 (grammar of tables)
  
  # Parallel computing
  "parallel",         # Base R (parallel processing)
  "doParallel",       # >= 1.0.0 (parallel backend)
  "foreach",          # >= 1.5.0 (parallel loops)
  
  # Utility packages
  "here",             # >= 1.0.0 (file paths)
  "janitor",          # >= 2.1.0 (data cleaning)
  "skimr",            # >= 2.1.0 (data summary)
  "readxl",           # >= 1.4.0 (Excel files)
  "writexl",          # >= 1.4.0 (Excel output)
  
  # Factor analysis and PCA
  "psych",            # >= 2.2.0 (psychological research tools)
  "GPArotation",      # >= 2014.11-1 (factor rotation)
  
  # Information theory
  "entropy",          # >= 1.3.0 (entropy estimation)
  "infotheo",         # >= 1.2.0 (information theory measures)
  
  # Additional econometric tools
  "plm",              # >= 2.6.0 (panel linear models)
  "systemfit",        # >= 1.1.0 (simultaneous equations)
  "AER"               # >= 1.2.0 (applied econometrics)
)

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("Installing package:", pkg, "\n")
      tryCatch({
        install.packages(pkg, dependencies = TRUE, repos = "https://cran.r-project.org/")
        library(pkg, character.only = TRUE)
        cat("Successfully installed and loaded:", pkg, "\n")
      }, error = function(e) {
        cat("ERROR installing", pkg, ":", e$message, "\n")
        cat("Please install manually using: install.packages('", pkg, "')\n")
      })
    } else {
      cat("Package", pkg, "already installed and loaded.\n")
    }
  }
}

# Install CRAN packages
cat("Installing CRAN packages...\n")
install_if_missing(cran_packages)

# ============================================================================
# BIOCONDUCTOR PACKAGES (if needed)
# ============================================================================

# Uncomment if Bioconductor packages are needed
# if (!require("BiocManager", quietly = TRUE)) {
#   install.packages("BiocManager")
# }
# 
# bioc_packages <- c(
#   # Add Bioconductor packages here if needed
# )
# 
# if (length(bioc_packages) > 0) {
#   cat("Installing Bioconductor packages...\n")
#   BiocManager::install(bioc_packages, update = FALSE)
# }

# ============================================================================
# GITHUB PACKAGES (if needed)
# ============================================================================

# Install devtools for GitHub packages
if (!require("devtools", quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
}

# GitHub packages (uncomment if needed)
# github_packages <- c(
#   # "username/packagename"
# )
# 
# if (length(github_packages) > 0) {
#   cat("Installing GitHub packages...\n")
#   for (pkg in github_packages) {
#     tryCatch({
#       devtools::install_github(pkg)
#       cat("Successfully installed from GitHub:", pkg, "\n")
#     }, error = function(e) {
#       cat("ERROR installing GitHub package", pkg, ":", e$message, "\n")
#     })
#   }
# }

# ============================================================================
# PACKAGE VERSION CHECKING
# ============================================================================

cat("\n" , "="*60, "\n")
cat("PACKAGE VERSION INFORMATION\n")
cat("="*60, "\n")

# Critical packages for replication
critical_packages <- c("tidyverse", "fixest", "rugarch", "ggplot2", "stargazer")

for (pkg in critical_packages) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    version <- packageVersion(pkg)
    cat(sprintf("%-15s: %s\n", pkg, version))
  } else {
    cat(sprintf("%-15s: NOT INSTALLED\n", pkg))
  }
}

# ============================================================================
# LOAD ALL REQUIRED PACKAGES
# ============================================================================

cat("\nLoading all required packages for analysis...\n")

# Core packages for every script
required_packages <- c(
  "tidyverse", "data.table", "lubridate", "zoo",
  "fixest", "sandwich", "lmtest",
  "ggplot2", "gridExtra", "scales",
  "stargazer", "kableExtra"
)

for (pkg in required_packages) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# ============================================================================
# SYSTEM INFORMATION
# ============================================================================

cat("\n" , "="*60, "\n")
cat("SYSTEM INFORMATION\n")
cat("="*60, "\n")

cat("R version:", R.version.string, "\n")
cat("Platform:", R.version$platform, "\n")
cat("OS:", Sys.info()["sysname"], Sys.info()["release"], "\n")
cat("Locale:", Sys.getlocale("LC_CTYPE"), "\n")

# Memory information
if (Sys.info()["sysname"] == "Windows") {
  tryCatch({
    memory_info <- memory.size()
    cat("Available memory:", round(memory_info, 1), "MB\n")
  }, error = function(e) {
    cat("Memory information not available\n")
  })
} else {
  cat("Memory information: Use system tools for non-Windows systems\n")
}

# CPU cores
cat("CPU cores available:", parallel::detectCores(), "\n")

# ============================================================================
# CONFIGURATION CHECKS
# ============================================================================

cat("\n" , "="*60, "\n")
cat("CONFIGURATION CHECKS\n")
cat("="*60, "\n")

# Check for LaTeX (needed for some table output)
latex_available <- tryCatch({
  system("pdflatex --version", intern = TRUE, ignore.stderr = TRUE)
  TRUE
}, error = function(e) FALSE)

cat("LaTeX available:", ifelse(latex_available, "YES", "NO"), "\n")
if (!latex_available) {
  cat("  Warning: LaTeX not found. Some table outputs may not work.\n")
  cat("  Install LaTeX or use HTML/CSV output instead.\n")
}

# Check JAVA (needed for some packages)
java_available <- tryCatch({
  system("java -version", intern = TRUE, ignore.stderr = TRUE)
  TRUE
}, error = function(e) FALSE)

cat("Java available:", ifelse(java_available, "YES", "NO"), "\n")

# Check internet connection (needed for data downloads)
internet_available <- tryCatch({
  readLines("https://www.google.com", n = 1, warn = FALSE)
  TRUE
}, error = function(e) FALSE)

cat("Internet connection:", ifelse(internet_available, "YES", "NO"), "\n")
if (!internet_available) {
  cat("  Warning: No internet connection. Use offline data only.\n")
}

# ============================================================================
# TROUBLESHOOTING INFORMATION
# ============================================================================

cat("\n" , "="*60, "\n")
cat("TROUBLESHOOTING\n")
cat("="*60, "\n")

cat("If you encounter package installation errors:\n\n")

cat("1. For compilation errors on macOS:\n")
cat("   - Install Xcode Command Line Tools: xcode-select --install\n")
cat("   - Install gfortran: https://gcc.gnu.org/wiki/GFortranBinaries\n\n")

cat("2. For compilation errors on Linux:\n")
cat("   - Install build essentials: sudo apt-get install build-essential\n")
cat("   - Install development libraries: sudo apt-get install r-base-dev\n\n")

cat("3. For GARCH model convergence issues:\n")
cat("   - The code includes automatic fallbacks to simpler methods\n")
cat("   - Check rugarch package documentation for solver options\n\n")

cat("4. For memory issues with large datasets:\n")
cat("   - Increase R memory limit: memory.limit(size = 8000)  # Windows\n")
cat("   - Use data.table for large data operations\n")
cat("   - Consider running analysis in smaller chunks\n\n")

cat("5. For parallel processing issues:\n")
cat("   - Reduce number of cores: options(mc.cores = 2)\n")
cat("   - Disable parallel processing if necessary\n\n")

# ============================================================================
# SAVE SESSION INFO
# ============================================================================

# Save session information for reproducibility
session_info <- sessionInfo()
saveRDS(session_info, "output/logs/session_info.rds")

# Create a text version as well
sink("output/logs/session_info.txt")
print(session_info)
sink()

# ============================================================================
# FINAL VALIDATION
# ============================================================================

cat("="*60, "\n")
cat("PACKAGE INSTALLATION COMPLETE\n")
cat("="*60, "\n")

# Check that all critical packages are loaded
all_loaded <- all(sapply(critical_packages, function(x) x %in% (.packages())))

if (all_loaded) {
  cat("✓ All critical packages successfully loaded\n")
  cat("✓ Ready to run replication analysis\n")
  cat("\nNext steps:\n")
  cat("1. Run: source('code/run_all.R') for complete replication\n")
  cat("2. Or run individual analysis scripts in code/ directory\n")
} else {
  missing_packages <- critical_packages[!sapply(critical_packages, function(x) x %in% (.packages()))]
  cat("✗ Some critical packages failed to load:\n")
  for (pkg in missing_packages) {
    cat("  -", pkg, "\n")
  }
  cat("\nPlease resolve package installation issues before proceeding.\n")
  cat("Check the troubleshooting section above for guidance.\n")
}

cat("\nSession information saved to output/logs/session_info.rds\n")
cat("Installation log available in this console output\n")

# Set global options for analysis
options(
  digits = 6,           # Precision for numerical output
  scipen = 999,         # Avoid scientific notation
  warn = 1,             # Show warnings immediately
  stringsAsFactors = FALSE,  # Modern R behavior
  mc.cores = parallel::detectCores() - 1  # Parallel processing
)

cat("\nGlobal options configured for analysis\n")
cat("Ready to proceed with replication!\n")