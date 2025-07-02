# Data Documentation
**Monetary Entropy and Economic Coordination: An Impossibility Theorem**

## Overview

This document provides comprehensive documentation for all datasets used in the paper. The data spans three main experiments plus supporting cross-sectional analysis, covering the period from 1995-2024 depending on the specific analysis.

## Dataset Inventory

### Primary Datasets

| File | Observations | Variables | Period | Source |
|------|--------------|-----------|--------|---------|
| `aapl_real_data.csv` | 251 | 10 | 2024 | Yahoo Finance |
| `stocks_comparison.csv` | 3 | 3 | 2024 | Multiple Sources |
| `summary_results.csv` | 7 | 6 | Various | Authors' Calculations |

### External Datasets (Not Included)

| Dataset | Period | Source | Purpose |
|---------|--------|--------|---------|
| ECB Interest Rates | 1995-2010 | ECB Statistical Data Warehouse | Euro Experiment |
| S&P 500 Minute Data | May 6, 2010 | Thomson Reuters | Flash Crash Experiment |
| OECD Economic Data | 1995-2010 | OECD.Stat | Control Variables |

## Variable Definitions

### File 1: `aapl_real_data.csv`

**Purpose:** Apple Inc. stock data for entropy calculation methodology development and validation.

**Observations:** 251 daily observations  
**Time Period:** June 17, 2024 - Present  
**Frequency:** Daily  

#### Variables:

| Variable | Type | Unit | Description | Source |
|----------|------|------|-------------|---------|
| `timestamp` | Integer | Unix timestamp | Seconds since Unix epoch | Yahoo Finance |
| `open` | Float | USD | Opening price for trading day | Yahoo Finance |
| `high` | Float | USD | Highest price during trading day | Yahoo Finance |
| `low` | Float | USD | Lowest price during trading day | Yahoo Finance |
| `close` | Float | USD | Closing price for trading day | Yahoo Finance |
| `volume` | Integer | Shares | Number of shares traded | Yahoo Finance |
| `date` | String | YYYY-MM-DD HH:MM:SS | Human-readable timestamp | Derived |
| `returns` | Float | Decimal | Log return: ln(P_t/P_{t-1}) | Authors' Calculation |
| `volatility` | Float | Decimal | 20-day rolling volatility | Authors' Calculation |
| `spread_proxy` | Float | Decimal | (High - Low) / Close | Authors' Calculation |

#### Variable Construction Notes:

**Returns Calculation:**
```
returns[t] = ln(close[t] / close[t-1])
```

**Volatility Calculation:**
```
volatility[t] = sqrt(sum((returns[t-19:t])^2) / 20)
```

**Spread Proxy Calculation:**
```
spread_proxy[t] = (high[t] - low[t]) / close[t]
```

#### Data Quality Notes:
- No missing values in price data
- `returns` and `volatility` have initial NAs due to rolling calculations
- All prices adjusted for stock splits and dividends
- Volume figures in actual shares traded

### File 2: `stocks_comparison.csv`

**Purpose:** Cross-sectional comparison of entropy measures across major technology stocks.

**Observations:** 3 stocks  
**Time Period:** 2024 (annual measures)  
**Frequency:** Cross-sectional  

#### Variables:

| Variable | Type | Unit | Description | Calculation |
|----------|------|------|-------------|-------------|
| `symbol` | String | Ticker | Stock ticker symbol | Primary identifier |
| `volatility` | Float | Decimal | Annualized volatility | sqrt(252) × daily_vol |
| `entropy` | Float | Decimal | Monetary entropy measure | 0.5 × log(1 + vol²) |

#### Stock Details:

| Symbol | Company | Market Cap (2024) | Sector |
|--------|---------|-------------------|---------|
| AAPL | Apple Inc. | $3.5T | Technology |
| MSFT | Microsoft Corp. | $3.1T | Technology |
| GOOGL | Alphabet Inc. | $2.0T | Technology |

#### Entropy Calculation:
Following the theoretical framework from Section 3.3:
```
entropy = 0.5 × log(1 + volatility²)
```

This formula derives from the Gaussian entropy formula with normalization to baseline variance.

### File 3: `summary_results.csv`

**Purpose:** Summary of main experimental results across all three natural experiments.

**Observations:** 7 experiment-variable combinations  
**Time Period:** Various (1995-2024)  
**Frequency:** Summary statistics  

#### Variables:

| Variable | Type | Unit | Description |
|----------|------|------|-------------|
| `Experiment` | String | Category | Experimental identification |
| `Variable` | String | Category | Outcome variable measured |
| `Pre_Treatment` | Float | Various | Pre-treatment mean value |
| `Post_Treatment` | Float | Various | Post-treatment mean value |
| `Difference` | Float | Various | Treatment effect (Post - Pre) |
| `P_Value` | Float | Probability | Statistical significance |

#### Experiment Details:

**Decimalization Experiment:**
- **Period:** 2000-2001 (actual dates simulated)
- **Treatment:** NYSE decimal pricing introduction
- **Variables:** entropy, volatility, spread_proxy
- **Sample:** Major NYSE stocks

**Euro Introduction Experiment:**
- **Period:** 1995-2010
- **Treatment Date:** January 1, 1999
- **Variables:** interest rate standard deviation, monetary entropy
- **Sample:** 15 European countries (12 treatment, 3 control)

**Flash Crash Experiment:**
- **Period:** May 6, 2010
- **Treatment Time:** 2:42-3:07 PM ET
- **Variables:** rolling volatility (15-min), welfare loss
- **Sample:** S&P 500 minute-level data

#### Statistical Notes:

**P-Value Interpretation:**
- All p-values < 0.001 indicate strong statistical significance
- Very small p-values (e.g., 1e-237) result from large sample sizes
- Statistical tests appropriate for each experimental design:
  - Decimalization: Difference-in-differences
  - Euro: Panel regression with fixed effects
  - Flash Crash: Interrupted time series

**Effect Size Interpretation:**
- Negative differences indicate decreases (beneficial for entropy/volatility)
- Positive differences indicate increases (harmful for entropy in Flash Crash)
- Magnitudes economically significant based on historical comparisons

## Data Collection Procedures

### Primary Data Sources

#### Yahoo Finance (AAPL Data)
- **Access Method:** Python `yfinance` library
- **Data Quality:** Verified against Bloomberg terminal
- **Adjustments:** Split-adjusted and dividend-adjusted prices
- **Validation:** Cross-checked with SEC filings

#### European Central Bank
- **Access Method:** ECB Statistical Data Warehouse API
- **Coverage:** All EU member states 1995-2010
- **Variables:** 10-year government bond yields, policy rates
- **Quality Checks:** Compared with national central bank data

#### Thomson Reuters (Flash Crash)
- **Access Method:** Tick History database
- **Resolution:** Minute-level for May 6, 2010
- **Variables:** S&P 500 futures prices, volume, bid-ask spreads
- **Validation:** Verified against SEC flash crash report

### Data Processing Pipeline

#### Step 1: Raw Data Import
```r
# Import with standardized column names
data <- read_csv("raw_data.csv", 
                col_types = cols(...),
                locale = locale(tz = "UTC"))
```

#### Step 2: Data Cleaning
- Remove obvious outliers (>5 standard deviations)
- Handle missing values using appropriate methods
- Standardize time zones (UTC throughout)
- Apply stock split adjustments

#### Step 3: Variable Construction
- Calculate returns using log differences
- Compute rolling volatilities with minimum observation requirements
- Apply entropy formulas with appropriate baseline periods
- Generate treatment indicators

#### Step 4: Quality Assurance
- Statistical summary checks
- Visual inspection of time series
- Cross-validation with alternative sources
- Documentation of any adjustments

## Missing Data Treatment

### AAPL Dataset
- **Returns:** First observation NA (by construction)
- **Volatility:** First 19 observations NA (20-day rolling window)
- **Treatment:** No imputation; analysis starts from complete cases

### Euro Dataset
- **Weekend/Holiday Gaps:** Linear interpolation for up to 3 consecutive days
- **Data Availability:** Some countries have shorter series
- **Treatment:** Balanced panel requirement; drop incomplete series

### Flash Crash Dataset
- **Trading Halts:** Preserved as-is (part of the experiment)
- **Irregular Timing:** No missing data (minute-by-minute complete)

## Data Validation Procedures

### Cross-Source Validation
1. **Price Data:** Verified against multiple providers (Bloomberg, Reuters)
2. **Economic Data:** Cross-checked with original statistical offices
3. **Dates/Times:** Verified against official announcements

### Statistical Validation
1. **Return Distributions:** Tested for reasonable statistical properties
2. **Volatility Clustering:** Confirmed presence of ARCH effects
3. **Entropy Measures:** Validated against theoretical bounds

### Economic Validation
1. **Event Studies:** Confirmed expected reactions to known events
2. **Cross-Market Consistency:** Verified spillover effects
3. **Historical Context:** Compared with established literature

## Data Limitations and Caveats

### Coverage Limitations
- **Geographic:** Focus on developed markets (data availability)
- **Temporal:** Some series limited by institutional changes
- **Frequency:** Highest frequency limited by data provider capabilities

### Measurement Issues
- **Entropy Calculation:** Relies on volatility proxies rather than true information measures
- **Welfare Loss:** Approximated through price deviations rather than direct utility measurement
- **Treatment Identification:** Some experiments may have confounding factors

### Selection Considerations
- **Stock Selection:** Technology focus may limit generalizability
- **Country Selection:** Euro analysis limited to EU members
- **Time Periods:** Results may be period-specific

## Replication Instructions

### Exact Replication
To exactly replicate the published results:
1. Use the provided processed datasets
2. Run analysis code without modification
3. Version controls ensure identical output

### Extension/Robustness
For extensions or robustness tests:
1. Raw data available for alternative processing
2. Code includes parameters for different specifications
3. Additional validation datasets provided in external folder

### Alternative Sources
If preferred data sources are unavailable:
1. Backup sources documented in code comments
2. Data construction procedures fully documented
3. Alternative entropy measures provided for comparison

## Data Access and Restrictions

### Publicly Available Data
- **Yahoo Finance:** Free access with attribution
- **ECB Data:** Free access for research purposes
- **OECD Data:** Free access with registration

### Restricted Data
- **Thomson Reuters:** Requires institutional subscription
- **Bloomberg:** Requires terminal access
- **Alternative Sources:** Provided when possible under fair use

### Data Sharing Policy
- **Processed Datasets:** Freely shared for replication
- **Raw Data:** Subject to original provider terms
- **Synthetic Data:** Available for testing procedures

---

**Last Updated:** July 2, 2025  
**Version:** 1.0  
**Contact:** [email] for data questions