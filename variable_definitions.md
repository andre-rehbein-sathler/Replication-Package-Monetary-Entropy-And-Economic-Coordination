# Variable Definitions
**Monetary Entropy and Economic Coordination: An Impossibility Theorem**

## Complete Variable Dictionary

This document provides comprehensive definitions for all variables used in the analysis, including construction methods, data sources, and theoretical justifications.

## Primary Variables

### Dependent Variables

#### `entropy`
- **Definition:** Monetary entropy measure capturing information uncertainty in price signals
- **Formula:** `entropy = 0.5 * log(1 + volatility²/baseline_variance)`
- **Unit:** Nats (natural logarithm base)
- **Range:** [0, ∞) with typical values [0, 2]
- **Theoretical Basis:** Derived from Shannon's information theory and Gaussian entropy formula
- **Construction:** 
  - Calculate rolling or GARCH conditional volatility
  - Normalize by baseline variance (first year of data)
  - Apply logarithmic transformation
- **Interpretation:** Higher values indicate greater information uncertainty and coordination difficulties

#### `welfare_loss`
- **Definition:** Empirical proxy for economic welfare loss from coordination failures
- **Formula:** `welfare_loss = |price - efficient_price| / efficient_price`
- **Unit:** Decimal proportion
- **Range:** [0, ∞) with typical values [0, 0.1]
- **Construction:**
  - `efficient_price` = Volume-weighted average price (VWAP) over 20-minute window
  - Calculate absolute deviation from efficient price
  - Normalize by efficient price level
- **Interpretation:** Percentage welfare loss relative to efficient allocation

#### `interest_rate_dispersion`
- **Definition:** Cross-country dispersion in government bond yields
- **Formula:** `dispersion = sd(bond_yields_across_countries)`
- **Unit:** Percentage points
- **Construction:**
  - Collect 10-year government bond yields for all countries in sample
  - Calculate standard deviation across countries for each time period
  - Handle missing values using linear interpolation (max 3 days)
- **Interpretation:** Higher dispersion indicates less monetary coordination

### Key Independent Variables

#### `volatility`
- **Definition:** Measure of price variability over specified time window
- **Primary Method:** GARCH(1,1) conditional volatility
- **Fallback Method:** Rolling window standard deviation (20 periods)
- **Formula (GARCH):** `σ²_t = ω + α*ε²_{t-1} + β*σ²_{t-1}`
- **Formula (Rolling):** `volatility = sqrt(Σ(r_i - r̄)² / (n-1))`
- **Unit:** Decimal (daily) or percentage (annualized)
- **Annualization:** Multiply daily volatility by sqrt(252)

#### `returns`
- **Definition:** Logarithmic price changes over one period
- **Formula:** `returns = log(price_t / price_{t-1})`
- **Unit:** Decimal (daily returns)
- **Range:** Typically [-0.1, 0.1] for daily equity returns
- **Construction:**
  - Use closing prices for daily returns
  - Handle stock splits and dividends through adjusted prices
  - Remove extreme outliers (>5 standard deviations)

### Treatment Variables

#### `post_euro`
- **Definition:** Time period indicator for Euro introduction
- **Formula:** `post_euro = 1 if date >= "1999-01-01", 0 otherwise`
- **Data Type:** Binary indicator
- **Coverage:** All countries in European sample
- **Source:** Official Euro adoption dates from ECB

#### `emu_member`
- **Definition:** Country indicator for Eurozone membership
- **Formula:** `emu_member = 1 if country in eurozone, 0 otherwise`
- **Data Type:** Binary indicator
- **Treatment Countries:** Germany, France, Italy, Spain, Netherlands, Belgium, Austria, Portugal, Finland, Ireland, Luxembourg (Greece from 2001)
- **Control Countries:** United Kingdom, Sweden, Denmark

#### `flash_crash_period`
- **Definition:** Time period indicator for May 6, 2010 flash crash
- **Formula:** `flash_crash = 1 if time in [14:42, 15:07] ET, 0 otherwise`
- **Data Type:** Binary indicator
- **Precision:** Minute-level timing based on SEC official report
- **Source:** SEC report on flash crash events

### Control Variables

#### Economic Fundamentals

##### `gdp_growth`
- **Definition:** Quarterly real GDP growth rate (annualized)
- **Formula:** `gdp_growth = (GDP_t/GDP_{t-1})^4 - 1`
- **Unit:** Decimal (e.g., 0.03 = 3% annual growth)
- **Source:** OECD Economic Outlook, national statistical offices
- **Frequency:** Quarterly, interpolated to monthly for panel regressions

##### `inflation`
- **Definition:** Consumer Price Index inflation rate (year-over-year)
- **Formula:** `inflation = (CPI_t/CPI_{t-12}) - 1`
- **Unit:** Decimal annual rate
- **Source:** OECD Main Economic Indicators
- **Treatment:** Seasonally adjusted where available

##### `debt_gdp_ratio`
- **Definition:** Government debt as percentage of GDP
- **Formula:** `debt_gdp = (Government_Debt / GDP) * 100`
- **Unit:** Percentage
- **Source:** OECD Economic Outlook, Eurostat
- **Frequency:** Annual, carried forward for higher frequency analysis

##### `current_account`
- **Definition:** Current account balance as percentage of GDP
- **Formula:** `current_account = (Current_Account_Balance / GDP) * 100`
- **Unit:** Percentage (positive = surplus, negative = deficit)
- **Source:** IMF International Financial Statistics

#### Financial Market Variables

##### `volume`
- **Definition:** Number of shares traded per period
- **Unit:** Shares (integers)
- **Transformation:** Often log-transformed for regression analysis
- **Source:** Exchange data, Bloomberg, Thomson Reuters

##### `spread_proxy`
- **Definition:** Bid-ask spread approximation using high-low prices
- **Formula:** `spread_proxy = (high - low) / close`
- **Unit:** Decimal proportion
- **Justification:** Correlated with true bid-ask spreads when tick data unavailable
- **Range:** Typically [0, 0.1] for liquid stocks

##### `market_cap`
- **Definition:** Market capitalization (shares outstanding × price)
- **Unit:** Currency units (USD, EUR, etc.)
- **Transformation:** Log-transformed for size controls
- **Source:** CRSP, Datastream, Bloomberg

### Constructed Indices

#### `mei` (Monetary Entropy Index)
- **Definition:** Weighted average of entropy across multiple financial markets
- **Formula:** `MEI = Σ w_j × entropy_j`
- **Weights:**
  - Equity: 35%
  - Bonds: 25% 
  - Credit: 25%
  - Foreign Exchange: 15%
- **Theoretical Basis:** Information content and price discovery importance
- **Construction:**
  1. Calculate entropy for each market separately
  2. Normalize each entropy measure by its baseline
  3. Apply theoretical weights
  4. Sum to create composite index

#### `mei_normalized`
- **Definition:** MEI scaled to [0,1] range for interpretability
- **Formula:** `mei_norm = (MEI - min(MEI)) / (max(MEI) - min(MEI))`
- **Unit:** Scale [0,1]
- **Purpose:** Easier interpretation and cross-period comparison

### Time and Identifier Variables

#### `timestamp`
- **Definition:** Unix timestamp (seconds since 1970-01-01)
- **Data Type:** Integer
- **Purpose:** Precise time identification for high-frequency data
- **Conversion:** `as.POSIXct(timestamp, origin="1970-01-01")`

#### `date`
- **Definition:** Calendar date in standardized format
- **Format:** "YYYY-MM-DD HH:MM:SS" or "YYYY-MM-DD"
- **Time Zone:** UTC for consistency across markets
- **Data Type:** Date or POSIXct class in R

#### `symbol` / `country`
- **Definition:** Asset or country identifier
- **Data Type:** Character string
- **Examples:** "AAPL", "MSFT", "Germany", "France"
- **Purpose:** Panel data identification

### Alternative Entropy Measures

#### `shannon_entropy`
- **Definition:** Shannon entropy calculated from discretized volatility distribution
- **Formula:** `H = -Σ p_i * log(p_i)`
- **Construction:**
  1. Discretize volatility into bins
  2. Calculate probability mass for each bin
  3. Apply Shannon formula
- **Purpose:** Robustness check for main entropy measure

#### `renyi_entropy`
- **Definition:** Rényi entropy of order α=2
- **Formula:** `H_α = (1/(1-α)) * log(Σ p_i^α)`
- **Parameter:** α = 2 (most common alternative to Shannon)
- **Purpose:** Alternative information measure for robustness

#### `tsallis_entropy`
- **Definition:** Tsallis entropy of order q=2
- **Formula:** `S_q = (Σ p_i^q - 1) / (q-1)`
- **Parameter:** q = 2
- **Purpose:** Non-extensive entropy measure for robustness

### Diagnostic and Validation Variables

#### `garch_converged`
- **Definition:** Indicator for GARCH model convergence
- **Data Type:** Binary (TRUE/FALSE)
- **Purpose:** Track estimation quality and automatic fallbacks

#### `outlier_flag`
- **Definition:** Indicator for observations flagged as outliers
- **Criteria:** |standardized_residual| > 5
- **Data Type:** Binary
- **Treatment:** Outliers excluded from main analysis but sensitivity tested

#### `missing_data_flag`
- **Definition:** Indicator for imputed or interpolated values
- **Data Type:** Binary
- **Purpose:** Transparency about data quality and robustness testing

### Placebo and Robustness Variables

#### `placebo_treatment`
- **Definition:** Randomly assigned treatment date for placebo testing
- **Construction:** Sample from available dates excluding true treatment periods
- **Purpose:** Validate that true treatment effects are not spurious

#### `rolling_coefficient`
- **Definition:** Time-varying parameter estimates from rolling window regressions
- **Window:** 3 years (750 daily observations)
- **Purpose:** Test parameter stability over time

## Variable Transformations

### Standard Transformations

#### Log Transformation
- **Applied to:** Prices, market cap, volume
- **Formula:** `log_var = log(var + 1)` (add 1 if zeros present)
- **Purpose:** Reduce skewness, interpret coefficients as elasticities

#### Standardization
- **Applied to:** Control variables in some specifications
- **Formula:** `std_var = (var - mean(var)) / sd(var)`
- **Purpose:** Compare coefficient magnitudes across variables

#### Winsorization
- **Applied to:** Returns, volatility for outlier treatment
- **Method:** Set extreme values to 1st/99th percentiles
- **Purpose:** Robust estimation while preserving sample size

### Lag Structures

#### `L1_variable`, `L2_variable`, etc.
- **Definition:** Lagged values of variables
- **Purpose:** Control for persistence, test dynamic effects
- **Construction:** `L1_var = lag(var, 1)` in time series context

#### `F1_variable` (Forward-looking)
- **Definition:** Future values for testing anticipation effects
- **Purpose:** Validate no pre-treatment effects in natural experiments

## Data Quality Indicators

### Completeness Measures

#### `data_availability_rate`
- **Definition:** Proportion of non-missing observations
- **Formula:** `availability = (n_complete / n_total)`
- **Threshold:** Minimum 80% for inclusion in main analysis

#### `interpolation_rate`
- **Definition:** Proportion of interpolated values
- **Threshold:** Maximum 5% interpolated values for series inclusion

### Reliability Measures

#### `measurement_error_variance`
- **Definition:** Estimated variance of measurement error
- **Method:** Difference between repeated measurements when available
- **Purpose:** Adjust for attenuation bias in coefficients

## Special Cases and Exceptions

### Holiday and Weekend Treatment
- **Method:** Forward-fill last available value
- **Maximum:** 3 consecutive missing periods
- **Alternative:** Drop periods from analysis if preferred

### Market Closure Handling
- **Approach:** Exclude closed-market periods from analysis
- **Exception:** Include in event studies if closure is part of treatment

### Currency Conversion
- **Method:** Convert to common currency using daily exchange rates
- **Timing:** Use end-of-period rates for consistency
- **Source:** Federal Reserve H.10 release, ECB reference rates

### Frequency Alignment
- **Daily to Monthly:** Use end-of-month values
- **Intraday to Daily:** Use closing values
- **Quarterly to Monthly:** Linear interpolation between quarters

## Variable Naming Conventions

### Prefixes
- `L#_` : Lagged variables (# = number of lags)
- `D_` : First differences
- `log_` : Log-transformed variables
- `std_` : Standardized variables

### Suffixes
- `_pct` : Percentage terms (multiplied by 100)
- `_bp` : Basis points (multiplied by 10000)
- `_norm` : Normalized to [0,1] scale
- `_flag` : Binary indicator variables

---

**Variable Dictionary Version:** 1.0  
**Last Updated:** July 2, 2025  
**Coverage:** All variables used in main analysis and robustness tests