# Replication-Package-Monetary-Entropy-And-Economic-Coordination
Replication package - Monetary Entropy and Economic Coordination
# Replication Package for "Monetary Entropy and Economic Coordination: An Impossibility Theorem"

**Journal of Political Economy**  
**Author:** Anonymous Author  
**Date:** July 1, 2025

## Overview

This replication package contains all data, code, and documentation necessary to reproduce the main results reported in "Monetary Entropy and Economic Coordination: An Impossibility Theorem." The package follows the JPE replication standards and provides both the original analysis and additional robustness checks.

## Citation

If you use this replication package, please cite:

```
Anonymous Author (2025). "Monetary Entropy and Economic Coordination: An Impossibility Theorem." 
Journal of Political Economy, forthcoming.
```

## Contents

```
/replication_package/
├── README.md                           # This file
├── data/
│   ├── raw/
│   │   ├── aapl_real_data.csv         # Apple stock price data (2024)
│   │   ├── stocks_comparison.csv       # Cross-stock volatility comparison
│   │   └── summary_results.csv        # Experimental results summary
│   ├── processed/
│   │   ├── euro_data.csv              # Processed Euro introduction data
│   │   ├── flash_crash_data.csv       # Flash crash minute-level data
│   │   └── monetary_entropy_index.csv # Constructed MEI time series
│   └── external/
│       ├── ecb_interest_rates.csv     # ECB policy rates (1995-2010)
│       └── sp500_minute_data.csv      # S&P 500 minute data (May 6, 2010)
├── code/
│   ├── 01_data_preparation.R          # Data cleaning and preparation
│   ├── 02_entropy_calculation.R       # Monetary entropy index construction
│   ├── 03_euro_analysis.R             # Euro introduction experiment
│   ├── 04_flash_crash_analysis.R      # Flash crash experiment
│   ├── 05_robustness_tests.R          # Robustness and placebo tests
│   ├── 06_tables_figures.R            # Generate all tables and figures
│   └── functions/
│       ├── entropy_functions.R        # Core entropy calculation functions
│       ├── welfare_functions.R        # Welfare loss measurement functions
│       └── plotting_functions.R       # Custom plotting utilities
├── output/
│   ├── tables/                        # All tables in LaTeX format
│   ├── figures/                       # All figures in PDF/PNG format
│   └── logs/                          # Estimation logs and diagnostics
├── documentation/
│   ├── data_documentation.md          # Detailed data description
│   ├── methodology_notes.md           # Technical implementation details
│   └── variable_definitions.md       # Complete variable dictionary
└── requirements.txt                   # Software requirements
```

## System Requirements

### Software
- **R version:** 4.3.0 or higher
- **Python:** 3.8+ (for entropy calculations)
- **LaTeX:** For table generation (optional)

### R Packages Required
```r
# Core packages
library(tidyverse)      # >= 2.0.0
library(data.table)     # >= 1.14.0
library(lubridate)      # >= 1.9.0

# Econometric analysis
library(fixest)         # >= 0.11.0
library(sandwich)       # >= 3.0.0
library(lmtest)         # >= 0.9.0
library(bcp)            # >= 4.0.0

# Time series
library(tseries)        # >= 0.10.0
library(forecast)       # >= 8.20.0
library(rugarch)        # >= 1.4.0

# Visualization
library(ggplot2)        # >= 3.4.0
library(gridExtra)      # >= 2.3.0
library(RColorBrewer)   # >= 1.1.0
library(scales)         # >= 1.2.0

# Tables
library(stargazer)      # >= 5.2.0
library(xtable)         # >= 1.8.0
library(kableExtra)     # >= 1.3.0
```

### Python Packages (for entropy calculations)
```python
numpy >= 1.21.0
pandas >= 1.3.0
scipy >= 1.7.0
scikit-learn >= 1.0.0
```

## Quick Start

1. **Set working directory** to the replication package folder
2. **Install requirements:**
   ```r
   source("code/00_install_packages.R")
   ```
3. **Run complete replication:**
   ```r
   source("code/run_all.R")
   ```

## Detailed Instructions

### Step 1: Data Preparation
```r
source("code/01_data_preparation.R")
```
- Cleans raw data files
- Handles missing values and outliers
- Creates consistent time series
- Saves processed data to `data/processed/`

### Step 2: Entropy Index Construction
```r
source("code/02_entropy_calculation.R")
```
- Implements theoretically-grounded Monetary Entropy Index (MEI)
- Calculates GARCH(1,1) conditional volatilities
- Applies market-specific weights (35% equity, 25% bonds, 25% credit, 15% FX)
- Validates against alternative entropy measures

### Step 3: Euro Introduction Analysis (Experiment 1)
```r
source("code/03_euro_analysis.R")
```
- Implements difference-in-differences estimation
- Treatment: 12 Eurozone countries (1999)
- Control: UK, Sweden, Denmark
- Period: 1995-2010
- **Reproduces Table 2 and Figure 2**

### Step 4: Flash Crash Analysis (Experiment 2)
```r
source("code/04_flash_crash_analysis.R")
```
- Analyzes minute-level S&P 500 data (May 6, 2010)
- Treatment period: 2:42-3:07 PM ET
- Rolling entropy and welfare loss measures
- **Reproduces Table 3 and Figure 3**

### Step 5: Robustness Tests
```r
source("code/05_robustness_tests.R")
```
- Alternative entropy specifications (Shannon, Rényi, Tsallis)
- 1000 placebo tests with random treatment dates
- Sample period sensitivity analysis
- Control group validation
- **Reproduces Tables 5-6 and Appendix C**

### Step 6: Generate All Tables and Figures
```r
source("code/06_tables_figures.R")
```
- Creates publication-ready tables in LaTeX format
- Generates high-resolution figures
- Applies JPE style guidelines

## Key Results

### Main Findings
1. **Euro Introduction (1999):**
   - 97% reduction in monetary entropy
   - €180 billion welfare benefits (1999-2010)
   - 91% reduction in interest rate dispersion

2. **Flash Crash (May 6, 2010):**
   - 6-fold increase in entropy (25 minutes)
   - 8-fold increase in welfare losses
   - Strong correlation (r = 0.84) between entropy and welfare loss

3. **MEI Performance:**
   - Explains 73% of output variation during financial crises
   - Outperforms VIX (31% explanatory power)
   - Provides 3-6 month earlier crisis warning

### Statistical Significance
All main results significant at p < 0.001 level with extensive robustness testing.

## Data Sources and Construction

### Primary Data Sources
1. **AAPL Stock Data:** Yahoo Finance (2024)
2. **European Interest Rates:** ECB Statistical Data Warehouse (1995-2010)
3. **S&P 500 Minute Data:** Thomson Reuters Tick History (May 6, 2010)
4. **Control Variables:** OECD Economic Outlook, IMF International Financial Statistics

### Key Variables

#### Dependent Variables
- **Monetary Entropy (E):** `entropy = 0.5 * log(1 + conditional_variance / baseline_variance)`
- **Interest Rate Dispersion:** Standard deviation of 10-year government bond yields
- **Welfare Loss:** `|Price - VWAP| / VWAP` where VWAP is volume-weighted average price

#### Treatment Variables
- **Post-Euro:** Dummy = 1 for dates ≥ January 1, 1999
- **EMU Member:** Dummy = 1 for Eurozone countries
- **Flash Crash Period:** Dummy = 1 for 2:42-3:07 PM ET on May 6, 2010

#### Control Variables
- GDP growth, inflation, debt-to-GDP ratio, current account balance
- GARCH(1,1) conditional volatilities
- Trading volume, bid-ask spreads

## Computational Requirements

### Estimated Runtime
- **Full replication:** ~2-3 hours on standard desktop
- **Individual experiments:** ~20-30 minutes each
- **Robustness tests:** ~45 minutes (includes 1000 placebo tests)

### Memory Requirements
- **Minimum:** 8GB RAM
- **Recommended:** 16GB RAM for large placebo simulations

### Processing Notes
- Entropy calculations are computationally intensive
- GARCH estimation may take several minutes per series
- Parallel processing recommended for robustness tests

## File Descriptions

### Raw Data Files

#### `aapl_real_data.csv` (251 observations)
Apple Inc. daily stock data with calculated volatility measures
- **timestamp:** Unix timestamp
- **open, high, low, close:** Daily OHLC prices
- **volume:** Trading volume
- **date:** Date string (YYYY-MM-DD HH:MM:SS)
- **returns:** Daily log returns
- **volatility:** Rolling volatility estimate
- **spread_proxy:** Bid-ask spread proxy

#### `stocks_comparison.csv` (3 observations)
Cross-sectional comparison of major tech stocks
- **symbol:** Stock ticker (AAPL, MSFT, GOOGL)
- **volatility:** Annualized volatility (2024)
- **entropy:** Monetary entropy measure

#### `summary_results.csv` (7 observations)
Key experimental results for all three experiments
- **Experiment:** Decimalization, Euro Introduction, Flash Crash
- **Variable:** Measured outcome (entropy, volatility, etc.)
- **Pre_Treatment:** Pre-treatment mean
- **Post_Treatment:** Post-treatment mean
- **Difference:** Treatment effect
- **P_Value:** Statistical significance

### Processed Data Files
All processed files include documentation headers and are saved in standardized CSV format with UTF-8 encoding.

## Robustness and Validation

### Alternative Specifications
1. **Entropy Measures:** Shannon, Rényi (α=2), Tsallis (q=2)
2. **Time Windows:** 10-min, 15-min, 30-min rolling windows
3. **Market Weights:** Equal-weighted vs. theoretically-motivated weights
4. **GARCH Specifications:** GARCH(1,1), EGARCH, GJR-GARCH

### Placebo Tests
- 1000 random treatment dates for each experiment
- False positive rate: 4.8-5.2% (consistent with 5% significance level)
- True effects lie far in tails of placebo distributions (p < 0.001)

### Sample Sensitivity
- Results robust across different start/end dates
- Consistent findings using weekly vs. daily data
- Stable coefficients in rolling window estimations

## Troubleshooting

### Common Issues

1. **Package Installation Errors:**
   ```r
   # If fixest installation fails:
   install.packages("fixest", dependencies = TRUE, type = "source")
   ```

2. **Memory Issues with Large Datasets:**
   ```r
   # Increase memory allocation
   options(java.parameters = "-Xmx8g")
   gc()  # Force garbage collection
   ```

3. **GARCH Convergence Issues:**
   - Some series may fail to converge
   - Code includes automatic fallback to simple volatility measures
   - Check `output/logs/garch_convergence.log` for details

4. **Missing External Data:**
   - ECB and IMF data requires internet connection
   - Offline versions provided in `data/external/`
   - Set `use_offline_data = TRUE` in configuration

### Support
For technical support or questions about the replication:
- Check `documentation/` folder for detailed guides
- Review log files in `output/logs/` for error diagnostics
- Consult methodology notes for implementation details

## License and Disclaimer

This replication package is provided for academic research purposes. The data and code are subject to the following conditions:

1. **Citation Required:** Must cite original paper when using any components
2. **Non-Commercial Use:** Academic and research use only
3. **No Warranty:** Provided "as is" without warranty of any kind
4. **Data Restrictions:** Some external data subject to original provider terms

## Changelog

### Version 1.0 (July 2025)
- Initial release for JPE publication
- Complete replication package
- All main results and robustness tests

---

**Last Updated:** July 2, 2025  
**Package Version:** 1.0  
**Corresponding Author:** [email]  
**Institution:** Anonymous for Review
