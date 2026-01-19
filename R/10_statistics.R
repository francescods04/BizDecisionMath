#' Recall a Statistics/Regression Concept
#'
#' This function prints a requested statistics or regression concept to the console.
#'
#' @param name A string specifying the name of the concept.
#'             Matches are performed using loose keywords.
#'             Run `show_stats()` without arguments to see the list of available topics.
#'
#' @return Prints the concept text to the console.
#' @export
#'
#' @examples
#' show_stats("ols")
#' show_stats("r squared")
show_stats <- function(name) {
  stats_list <- list(

    # ========== REGRESSION FUNDAMENTALS ==========
    linear_model = "
**LINEAR REGRESSION MODEL**

Theoretical equation:
**Y = beta_0 + beta_1*x_1 + ... + beta_p*x_p + epsilon**

Where:
- Y = Dependent (response) variable (random)
- x_1, ..., x_p = Explanatory (independent) variables
- beta_0 = Intercept
- beta_j = Coefficient of x_j (effect on E[Y] per unit change)
- epsilon = Error term (random)

Equivalent formulation:
**E[Y] = beta_0 + beta_1*x_1 + ... + beta_p*x_p**

Simple regression (p=1): Y = beta_0 + beta_1*x + epsilon
",

    assumptions = "
**MODEL ASSUMPTIONS**

1. **E[epsilon_i] = 0** for all i
   (Errors have zero mean -> Linearity assumption)

2. **Var[epsilon_i] = sigma^2** for all i
   (Homoscedasticity: constant error variance)

3. **Cov[epsilon_k, epsilon_i] = 0** for k != i
   (No autocorrelation between errors)

4. **epsilon_i ~ N(0, sigma^2)** for all i
   (Normality of errors)

5. **No perfect multicollinearity**
   (No perfect linear relationship among x_j's)
",

    # ========== STATISTICAL THEORY (NEW) ==========
    hypothesis_testing = "
**WHAT IS A HYPOTHESIS TEST?**

**Definition:**
A statistical procedure to decide between two competing claims
about a population parameter based on sample data.

**Structure:**
- **H_0 (Null Hypothesis)**: The 'default' claim (e.g., no effect, beta = 0)
- **H_1 (Alternative Hypothesis)**: What we want to show (e.g., beta != 0)

**Procedure:**
1. State H_0 and H_1
2. Choose significance level alpha (typically 0.01, 0.05, or 0.10)
3. Compute test statistic from sample data
4. Compare to critical value OR compute p-value
5. Make decision: Reject H_0 or Fail to reject H_0

**Key Principle:**
We assume H_0 is true and ask: 'How likely is our observed data?'
If very unlikely (p-value < alpha), we reject H_0.

**IMPORTANT:** Failing to reject H_0 does NOT prove H_0 is true!
",

    type_errors = "
**TYPE I AND TYPE II ERRORS**

|                    | H_0 True          | H_0 False         |
|--------------------|-------------------|-------------------|
| Reject H_0         | TYPE I ERROR (α)  | CORRECT           |
| Fail to reject H_0 | CORRECT           | TYPE II ERROR (β) |

**Type I Error (False Positive):**
- Rejecting H_0 when it is actually true
- Probability = alpha (significance level)
- Example: Concluding x has effect when it doesn't

**Type II Error (False Negative):**
- Failing to reject H_0 when it is actually false
- Probability = beta
- Example: Missing a real effect

**Power of a Test:**
**Power = 1 - beta = P(Reject H_0 | H_0 is false)**
- Probability of correctly detecting a real effect
- Higher power = better at finding true effects
- Increased by: larger n, larger effect size, higher alpha
",

    p_value = "
**P-VALUE: DEEP UNDERSTANDING**

**Definition:**
The probability of observing a test statistic as extreme as (or more
extreme than) the one computed, ASSUMING H_0 is true.

**Formal:**
**p-value = P(Test Statistic >= observed | H_0 is true)**

**Interpretation:**
- Small p-value: Data is unlikely under H_0 -> Evidence against H_0
- Large p-value: Data is plausible under H_0 -> No strong evidence

**Decision Rule:**
- If p-value < alpha: Reject H_0
- If p-value >= alpha: Fail to reject H_0

**COMMON MISCONCEPTIONS:**
- p-value is NOT P(H_0 is true)
- p-value is NOT the probability of making an error
- p-value does NOT measure effect size or practical importance

**Typical Thresholds:**
- p < 0.001: Very strong evidence against H_0 (***)
- p < 0.01: Strong evidence (**)
- p < 0.05: Moderate evidence (*)
- p < 0.10: Weak evidence (.)
- p >= 0.10: Insufficient evidence
",

    distributions = "
**STATISTICAL DISTRIBUTIONS IN REGRESSION**

**1. NORMAL DISTRIBUTION N(mu, sigma^2)**
- Symmetric, bell-shaped
- Used for: errors (epsilon), large-sample test statistics
- Key: Z = (X - mu)/sigma ~ N(0,1)

**2. t-DISTRIBUTION (t_{df})**
- Similar to normal but heavier tails
- df = degrees of freedom (more df -> closer to normal)
- Used for: coefficient tests when sigma unknown
- In regression: df = n - p - 1
- As df -> infinity, t -> N(0,1)

**3. F-DISTRIBUTION (F_{df1, df2})**
- Right-skewed, non-negative
- Ratio of two chi-squares divided by their df
- Used for: comparing variances, ANOVA, F-tests
- df1 = numerator df (restrictions tested)
- df2 = denominator df (n - p - 1)

**4. CHI-SQUARE DISTRIBUTION (chi^2_{df})**
- Sum of squared standard normals
- Right-skewed, non-negative
- Used for: logit model tests, goodness of fit

**Degrees of Freedom (df):**
= Sample size - Number of parameters estimated
= n - p - 1 in regression (p variables + intercept)
",

    estimator_properties = "
**PROPERTIES OF ESTIMATORS**

**1. UNBIASEDNESS**
E[b] = beta (expected value equals true parameter)
- OLS estimates are unbiased under Assumptions 1-5

**2. CONSISTENCY**
b -> beta as n -> infinity (converges to true value)
- Larger samples = more accurate estimates

**3. EFFICIENCY**
Among unbiased estimators, the one with smallest variance
- OLS is efficient under homoscedasticity

**4. BLUE (Best Linear Unbiased Estimator)**
Gauss-Markov Theorem:
Under Assumptions 1-3 (linearity, homoscedasticity, no autocorrelation):
**OLS is BLUE** = Best Linear Unbiased Estimator

'Best' means minimum variance among all linear unbiased estimators.

**5. MINIMUM VARIANCE (with normality)**
Under all 5 assumptions (including normality):
OLS has minimum variance among ALL unbiased estimators
(not just linear ones).
",

    heteroskedasticity = "
**HETEROSKEDASTICITY**

**Definition:**
Violation of the constant variance assumption.
**Var[epsilon_i] != sigma^2** (variance changes across observations)

**Visual Detection:**
Residual plot shows 'funnel shape' or systematic pattern in spread.

**Consequences:**
1. OLS estimates still UNBIASED and CONSISTENT
2. BUT: Standard errors are WRONG (usually underestimated)
3. t-tests and F-tests become UNRELIABLE
4. Confidence intervals are INVALID

**Formal Test: BREUSCH-PAGAN TEST**
- H_0: Homoscedasticity (Var[epsilon] = sigma^2)
- H_1: Heteroskedasticity (Var[epsilon] depends on x)
- Test statistic: BP ~ chi^2_{p} under H_0
- Reject H_0 if p-value < alpha

**R Code:**
library(lmtest)
bptest(model)  # Breusch-Pagan test

**Fixes:**
1. Use ROBUST (heteroskedasticity-consistent) standard errors:
   coeftest(model, vcov = vcovHC(model, type = 'HC1'))
2. Transform dependent variable: log(Y)
3. Weighted Least Squares (WLS)
",

    autocorrelation = "
**AUTOCORRELATION (Serial Correlation)**

**Definition:**
Violation of the independence assumption.
**Cov[epsilon_i, epsilon_j] != 0** for i != j
(Errors at different observations are correlated)

**Common in:**
- Time series data (today's error related to yesterday's)
- Spatial data (nearby locations have similar errors)

**Consequences:**
1. OLS estimates still UNBIASED
2. BUT: Standard errors are WRONG
3. t-tests and F-tests become UNRELIABLE
4. Efficiency is lost (OLS no longer BLUE)

**Detection: DURBIN-WATSON TEST**
- H_0: No first-order autocorrelation (rho = 0)
- H_1: First-order autocorrelation (rho != 0)
- Test statistic: DW ~ 2 under H_0
- DW near 0: Positive autocorrelation
- DW near 4: Negative autocorrelation
- DW near 2: No autocorrelation

**R Code:**
library(lmtest)
dwtest(model)  # Durbin-Watson test

**Fixes:**
1. Include lagged variables
2. Use HAC (Newey-West) standard errors
3. Use time series models (ARIMA)
4. First differencing
",

    normality_test = "
**NORMALITY OF ERRORS**

**Assumption:**
epsilon_i ~ N(0, sigma^2)
Errors are normally distributed.

**Why It Matters:**
- Required for EXACT validity of t-tests and F-tests
- Required for valid confidence intervals
- Less critical with large samples (Central Limit Theorem)

**Visual Detection: Q-Q PLOT**
- If points lie on diagonal line: Normality OK
- S-shape: Heavy tails
- Curved: Skewness

**Formal Test: SHAPIRO-WILK TEST**
- H_0: Residuals are normally distributed
- H_1: Residuals are NOT normally distributed
- Most powerful test for normality
- Reject H_0 if p-value < alpha

**R Code:**
shapiro.test(residuals(model))

**Alternative: JARQUE-BERA TEST**
- Based on skewness and kurtosis
- H_0: Skewness = 0 and Kurtosis = 3 (normal)
- JB ~ chi^2_2 under H_0

**Fixes:**
1. Transform Y: log(Y), sqrt(Y), Box-Cox
2. Remove outliers
3. With large n, rely on asymptotic results
",

    aic_bic = "
**AIC AND BIC: MODEL SELECTION CRITERIA**

**AIC (Akaike Information Criterion):**
**AIC = -2*log(L) + 2*k**
= -2*(log-likelihood) + 2*(number of parameters)

**BIC (Bayesian Information Criterion):**
**BIC = -2*log(L) + k*log(n)**
= -2*(log-likelihood) + k*log(sample size)

Where:
- L = Maximized likelihood of the model
- k = Number of estimated parameters (p + 1 for intercept)
- n = Sample size

**Interpretation:**
- LOWER values = BETTER model
- Both penalize complexity (more parameters = worse)
- BIC penalizes complexity MORE heavily than AIC
- BIC tends to select SIMPLER models

**Usage:**
- For non-nested models: Compare AIC or BIC directly
- For nested models: Use partial F-test (better)
- AIC good for prediction
- BIC good for finding 'true' model

**R Code:**
AIC(model1, model2, model3)
BIC(model1, model2, model3)
",

    blue = "
**GAUSS-MARKOV THEOREM & BLUE**

**Theorem Statement:**
Under Assumptions 1-3:
1. E[epsilon] = 0 (zero mean errors)
2. Var[epsilon] = sigma^2 (homoscedasticity)
3. Cov[epsilon_i, epsilon_j] = 0 (no autocorrelation)

**OLS is BLUE:**
- **B**est = Minimum variance
- **L**inear = Linear function of Y
- **U**nbiased = E[b] = beta
- **E**stimator

**What This Means:**
Among ALL estimators that are:
  (a) Linear in Y
  (b) Unbiased

OLS has the SMALLEST variance (most precise).

**Implications:**
- If assumptions hold: OLS is optimal
- If heteroskedasticity: OLS still unbiased, but NOT best
- If autocorrelation: OLS still unbiased, but NOT best
- Violations -> Use robust standard errors or different estimators
",

    stationarity_tests = "
**STATIONARITY TESTS**

**Why Test for Stationarity?**
- ARIMA models require stationary series
- Non-stationary series have spurious regression risk
- Forecasting assumes stationarity

**AUGMENTED DICKEY-FULLER (ADF) TEST:**
- H_0: Series has a UNIT ROOT (non-stationary)
- H_1: Series is STATIONARY
- Reject H_0 = Evidence of stationarity

R Code:
library(tseries)
adf.test(y)

**LJUNG-BOX TEST:**
- H_0: All autocorrelations up to lag k = 0
- H_1: At least one autocorrelation != 0
- Test statistic: Q ~ chi^2_k under H_0
- Used for residual diagnostics

R Code:
Box.test(residuals, lag = k, type = 'Ljung-Box')

**KPSS TEST:**
- H_0: Series is STATIONARY
- H_1: Series is NON-STATIONARY
- Opposite of ADF!

R Code:
library(tseries)
kpss.test(y)
",

    ols = "
**ORDINARY LEAST SQUARES (OLS)**

Estimation method: Minimize the sum of squared residuals

**min Sum[(y_i - (beta_0 + beta_1*x_i1 + ... + beta_p*x_ip))^2]**

The estimates b_0, b_1, ..., b_p are called OLS estimates.

Estimated equation:
**y_hat = b_0 + b_1*x_1 + ... + b_p*x_p**

Interpretation of b_j:
- Estimated average change in Y for a one-unit increase in x_j,
  holding all other variables constant.
",

    # ========== INFERENCE ==========
    confidence_interval = "
**CONFIDENCE INTERVAL FOR beta_j**

Structure:
**(b_j - t_{alpha/2, n-p-1} * s_{b_j}, b_j + t_{alpha/2, n-p-1} * s_{b_j})**

Where:
- b_j = Estimate of beta_j
- s_{b_j} = Standard error of b_j
- t_{alpha/2, n-p-1} = Critical value from t-distribution
- n = Sample size, p = Number of explanatory variables

Margin of Error (ME):
**ME = t_{alpha/2, n-p-1} * s_{b_j}**

Interpretation: With confidence (1 - alpha), the true effect of x_j
on E[Y] lies within the interval.
",

    t_test = "
**T-TEST FOR INDIVIDUAL COEFFICIENT**

Hypotheses:
- H_0: beta_j = 0 (x_j has no effect)
- H_1: beta_j != 0 (x_j has an effect)

Test statistic:
**t_obs = b_j / s_{b_j}**

Decision rule (at level alpha):
- Reject H_0 if |t_obs| >= t_{alpha/2, n-p-1}
- Equivalently: Reject H_0 if p-value < alpha

Where: **p-value = 2 * P(T > |t_obs| | H_0)**

Rejecting H_0 means x_j is **significant** in the model.
",

    f_test_partial = "
**PARTIAL F-TEST (Comparing Nested Models)**

Compares:
- Reduced model: q variables (x_1, ..., x_q)
- Complete model: p variables (x_1, ..., x_q, x_{q+1}, ..., x_p)

Hypotheses:
- H_0: beta_{q+1} = beta_{q+2} = ... = beta_p = 0
- H_1: At least one coefficient != 0

Test statistic:
**F = [(RES.DEV_red - RES.DEV_comp) / (p - q)] / [RES.DEV_comp / (n - p - 1)]**

Under H_0: F ~ F_{p-q, n-p-1}

Reject H_0 if F_obs >= F_{p-q, n-p-1, alpha}
",

    f_test_global = "
**GLOBAL F-TEST (Model Significance)**

Tests if the entire model is significant.

Hypotheses:
- H_0: beta_1 = beta_2 = ... = beta_p = 0
- H_1: At least one coefficient != 0

Test statistic:
**F = [EXP.DEV / p] / [RES.DEV / (n - p - 1)]**

Where: EXP.DEV = TOT.DEV - RES.DEV

Under H_0: F ~ F_{p, n-p-1}

Rejecting H_0 means the model is **globally significant**.
",

    # ========== DEVIANCE & R-SQUARED ==========
    deviance = "
**DECOMPOSITION OF TOTAL DEVIANCE**

**Sum(y_i - y_bar)^2 = Sum(y_i - y_hat_i)^2 + Sum(y_hat_i - y_bar)^2**
       TOTAL DEV.    =    RESIDUAL DEV.    +   EXPLAINED DEV.

- **Total Deviance**: Overall variability of Y
- **Residual Deviance**: Unexplained variability (due to error)
- **Explained Deviance**: Variability explained by the model

Also interpretable as prediction error:
- Total Dev. = Prediction error of null model (using y_bar)
- Residual Dev. = Prediction error of the fitted model
- Explained Dev. = Reduction in prediction error
",

    r_squared = "
**COEFFICIENT OF DETERMINATION (R^2)**

**R^2 = Explained Deviance / Total Deviance**
**R^2 = 1 - (Residual Deviance / Total Deviance)**

Properties:
- R^2 in [0, 1]
- R^2 = 1: Perfect fit (all variability explained)
- R^2 = 0: No explanatory power

Interpretation:
If R^2 = 0.4, the model explains 40% of Y's variability.

**Adjusted R^2:**
**R^2_adj = 1 - (1 - R^2) * (n - 1) / (n - p - 1)**

- Penalizes model complexity
- Use for comparing models with different numbers of variables
- For nested models, use partial F-test instead
",

    # ========== DUMMY VARIABLES ==========
    dummy = "
**DUMMY (DICHOTOMOUS) VARIABLES**

Binary variable coded as 0 or 1.
- 1 = Presence of characteristic
- 0 = Absence of characteristic

Effect interpretation:
b_j = Estimated difference in E[Y] between:
  - Category coded as 1
  - Category coded as 0
  (holding other variables constant)

For k-category variable: Use k-1 dummy variables.
The excluded category is the **reference category**.

Example: Zone A, B, C -> Create x_B and x_C
- y_hat = b_0 + b_age*age + b_B*x_B + b_C*x_C
- b_B = Difference between Zone B and Zone A (mean Y)
- b_C = Difference between Zone C and Zone A (mean Y)
",

    # ========== TRANSFORMATIONS & INTERACTIONS ==========
    transformations = "
**VARIABLE TRANSFORMATIONS**

To handle non-linear effects, transform x:
- log(x), x^2, x^3, ...

**Logarithmic transformation:**
- y_hat = b_0 + b_1*log(x)
- Effect of 1% increase in x: ~0.01 * b_1 change in E[Y]

**Log-log model:**
- log(Y) = beta_0 + beta_1*log(x) + epsilon
- b_1 = Elasticity (% change in Y per 1% change in x)

**Semi-log model (log-lin):**
- log(Y) = beta_0 + beta_1*x + epsilon
- Useful for correcting heteroscedasticity

Rules:
- Log requires x > 0
- If including x^2, must also include x
",

    interactions = "
**INTERACTIONS BETWEEN VARIABLES**

Interaction term: x_1 * x_2

Model with interaction:
**Y = beta_0 + beta_1*x_1 + beta_2*x_2 + beta_int*x_1*x_2 + epsilon**

Effect: The impact of x_1 on Y depends on the level of x_2.

Example (x_1 quantitative, x_2 dummy):
- For x_2 = 0: y_hat = b_0 + b_1*x_1
- For x_2 = 1: y_hat = (b_0 + b_2) + (b_1 + b_int)*x_1

The two groups have different intercepts AND different slopes.
",

    # ========== DIAGNOSTICS ==========
    multicollinearity = "
**MULTICOLLINEARITY**

Definition: High linear correlation among explanatory variables.

Effects:
1. Inflated standard errors of coefficient estimates
2. Low t-statistics, high p-values
3. Variables appear non-significant individually,
   but model may be globally significant

Detection: **Variance Inflation Factor (VIF)**
**VIF_j = 1 / (1 - R^2_j)**

Where R^2_j = R^2 from regressing x_j on all other x's.

Rule of thumb: VIF > 10 indicates high multicollinearity.

Solutions:
- Remove redundant variables
- Increase sample size
- Use PCA/Factor Analysis
",

    outliers = "
**OUTLIERS, LEVERAGE POINTS, INFLUENTIAL OBSERVATIONS**

**Outlier**: Observation with high residual |e_i|
- Rule: |standardized residual| > 3 (or 2) -> Outlier

**Leverage Point**: Observation with unusual x values
- Leverage measures distance from mean of x's
- Rule: leverage > 2(p+1)/n or 3(p+1)/n -> High leverage

**Influential Observation**: Changes regression results significantly
- dfbeta: Change in coefficient when observation removed
  Rule: |dfbeta| >= 2/sqrt(n) -> Influential
- Cook's Distance: Global measure
  Rule: Cook's D >= 1 -> Influential
",

    residual_plots = "
**RESIDUAL DIAGNOSTICS**

Residual: e_i = y_i - y_hat_i

**Plot 1: Residuals vs Fitted (y_hat)**
- Check linearity: Residuals randomly scattered around 0
- Check homoscedasticity: No funnel/pattern in spread
- Violation: Systematic pattern or changing variance

**Plot 2: Q-Q Plot**
- Check normality: Points should lie on a straight line
- Violation: Strong deviations indicate non-normality

**Plot 3: Scale-Location**
- Check homoscedasticity: Horizontal band of points
- Violation: Increasing spread = heteroscedasticity
",

    # ========== LOGISTIC REGRESSION ==========
    logit_model = "
**LOGISTIC REGRESSION (LOGIT MODEL)**

For binary dependent variable Y in {0, 1}.

Theoretical equation:
**log(p / (1-p)) = beta_0 + beta_1*x_1 + ... + beta_p*x_p**

Where p = P(Y = 1) = E[Y]

Equivalent forms:
- p = exp(linear) / (1 + exp(linear))
- Odds = p / (1-p)
- log(Odds) = linear predictor

Parameters estimated via **Maximum Likelihood**.
",

    odds_ratio = "
**ODDS RATIO INTERPRETATION**

For quantitative x_j:
**exp(b_j) = Odds Ratio**

Interpretation:
- Odds of Y=1 when x_j increases by 1 are multiplied by exp(b_j)
- If b_j > 0: exp(b_j) > 1 -> Odds increase
- If b_j < 0: exp(b_j) < 1 -> Odds decrease

Example:
- b_1 = 0.2 -> exp(0.2) = 1.22
- Each unit increase in x_1 increases odds by 22%

For dummy x_j:
- exp(b_j) = Ratio of odds for x_j=1 vs x_j=0
",

    logit_tests = "
**LOGIT MODEL: TESTS**

**Individual coefficient test (Z-test):**
- H_0: beta_j = 0
- z_obs = b_j / SE(b_j)
- Under H_0: z ~ N(0,1)
- Reject if |z_obs| > z_{alpha/2} or p-value < alpha

**Chi-Square Test (Partial):**
- Compares nested logit models
- chi^2 = RES.DEV_reduced - RES.DEV_complete
- df = p - q (difference in number of variables)

**Chi-Square Test (Global):**
- chi^2 = RES.DEV_null - RES.DEV_model
- df = p
- Tests overall model significance

**AIC (Akaike Information Criterion):**
AIC = -2*log(L) + 2*(p + 1)
Lower AIC = Better model
",

    # ========== PANEL DATA ==========
    panel_data = "
**PANEL DATA MODELS**

Data structure: n units observed over T time periods
Obs: (x_{it}, y_{it}) for i = 1,...,n and t = 1,...,T

**Pooling Model** (naive):
Ignore panel structure, treat as nT observations.
Usually inappropriate due to correlated errors.

**Fixed Effects Model (FEM):**
Y_{it} = alpha + beta*x_{it} + mu_i + epsilon_{it}
- mu_i = Fixed (deterministic) individual effect
- Captures unobserved heterogeneity
- mu_i estimated as parameters

**Random Effects Model (REM):**
Y_{it} = alpha + beta*x_{it} + mu_i + epsilon_{it}
- mu_i = Random individual effect
- mu_i ~ (0, sigma^2_mu)
- More efficient if assumptions hold
",

    hausman_test = "
**HAUSMAN TEST (FEM vs REM)**

Purpose: Choose between Fixed and Random Effects models.

Hypotheses:
- H_0: REM assumptions hold (consistent & efficient)
- H_1: REM assumptions violated (estimates inconsistent)

Decision:
- Reject H_0 -> Choose FEM
- Fail to reject H_0 -> Choose REM (more efficient)

R syntax:
phtest(model_fe, model_re)

Note: FEM requires option model = 'within'
",

    # ========== TIME SERIES ==========
    time_series = "
**TIME SERIES BASICS**

Univariate time series: y_t0, y_{t0+1}, ..., y_T

**Key Components:**
1. Trend (T_t): Long-term tendency
2. Seasonality (S_t): Regular periodic fluctuations
3. Error (E_t): Irregular/random component

**Decomposition Models:**
- Multiplicative: y_t = T_t * S_t * E_t
- Additive: y_t = T_t + S_t + E_t

**Stationarity (weak):**
- E[Y_t] = mu (constant mean)
- Var[Y_t] = sigma^2 (constant variance)
- Corr[Y_t, Y_{t-k}] = rho_k (depends only on lag k)

R: decompose(ts, type='mult' or 'additive')
",

    acf_pacf = "
**AUTOCORRELATION FUNCTIONS**

**ACF (Autocorrelation Function):**
rho_k = Corr(Y_t, Y_{t-k})
- Measures correlation at lag k
- Includes indirect correlations through intermediate lags

**PACF (Partial Autocorrelation Function):**
- Correlation between Y_t and Y_{t-k}
- After removing effect of Y_{t-1}, ..., Y_{t-k+1}

**Uses for Model Identification:**
- AR(p): ACF decays, PACF cuts off after lag p
- MA(q): ACF cuts off after lag q, PACF decays
- ARMA: Both decay

R: acf(y), pacf(y)
",

    arima = "
**ARIMA MODELS**

**AR(p) - Autoregressive:**
Y_t = phi_0 + phi_1*Y_{t-1} + ... + phi_p*Y_{t-p} + epsilon_t
- Stationary if |phi_1| < 1 (for AR(1))

**MA(q) - Moving Average:**
Y_t = epsilon_t + theta_1*epsilon_{t-1} + ... + theta_q*epsilon_{t-q}
- Always stationary

**ARMA(p,q) - Combined:**
Both AR and MA components.

**ARIMA(p,d,q) - Integrated:**
- d = Order of differencing
- Apply difference operator d times to achieve stationarity
- ARIMA(1,1,0) = Random walk with drift if phi_0 != 0

R: arima(y, order=c(p,d,q))
   auto.arima(y)  # Automatic selection
",

    white_noise = "
**WHITE NOISE & RANDOM WALK**

**White Noise:**
- E[Y_t] = 0
- Var[Y_t] = sigma^2
- Corr[Y_t, Y_{t-k}] = 0 for k != 0
- Unpredictable, completely random

**Random Walk:**
Y_t = Y_{t-1} + epsilon_t
- Non-stationary (variance grows with t)
- Var[Y_t] = t * sigma^2
- High persistence: shocks never fade

**Random Walk with Drift:**
Y_t = c + Y_{t-1} + epsilon_t
- Deterministic linear trend (slope = c)
- Still non-stationary
",

    forecasting = "
**FORECASTING METHODS**

**Naive:** y_hat_{T+h} = y_T (last observed value)

**Simple Mean:** y_hat_{T+h} = mean(y_0, ..., y_T)

**Simple Exponential Smoothing (SES):**
y_hat_{T+h} = alpha*y_T + alpha*(1-alpha)*y_{T-1} + ...
- Weights decay exponentially
- alpha in (0,1]: higher = more weight on recent

**Holt's Method:** Adds linear trend component
- Level: l_t = alpha*y_t + (1-alpha)*(l_{t-1} + b_{t-1})
- Trend: b_t = beta*(l_t - l_{t-1}) + (1-beta)*b_{t-1}
- Forecast: y_hat_{T+h} = l_T + h*b_T

**Holt-Winters:** Adds seasonality (additive or multiplicative)

R: HoltWinters(ts, seasonal='mult')
   forecast(arima_model, h=5)
",

    model_selection = "
**MODEL SELECTION CRITERIA**

**AIC (Akaike Information Criterion):**
AIC = -2*log(L) + 2*(p + 1)
- Lower is better
- Balances fit and complexity

**Adjusted R^2:**
R^2_adj = 1 - (1 - R^2) * (n-1)/(n-p-1)
- Use for non-nested models only
- For nested: use partial F-test

**ARIMA Selection:**
1. Determine d (differencing order) for stationarity
2. Examine ACF/PACF for p and q
3. Use auto.arima() for automatic AIC minimization

**Residual Diagnostics:**
- Should resemble white noise
- Check with ADF test, Ljung-Box test
"

  )

  if (missing(name)) {
    cat("Please provide the name of the topic. Available topics:\n\n")
    cat("=== STATISTICAL FOUNDATIONS (NEW!) ===\n")
    cat("1.  hypothesis testing (what is a test?)\n")
    cat("2.  type errors (Type I, Type II, power)\n")
    cat("3.  p value (interpretation, misconceptions)\n")
    cat("4.  distributions (t, F, chi-square, normal)\n")
    cat("5.  estimator properties (bias, consistency, efficiency)\n")
    cat("6.  blue (Gauss-Markov theorem)\n")
    cat("\n=== ASSUMPTION VIOLATIONS (NEW!) ===\n")
    cat("7.  heteroskedasticity (Breusch-Pagan test)\n")
    cat("8.  autocorrelation (Durbin-Watson test)\n")
    cat("9.  normality test (Shapiro-Wilk, Q-Q plot)\n")
    cat("10. stationarity tests (ADF, Ljung-Box, KPSS)\n")
    cat("11. aic bic (model selection theory)\n")
    cat("\n=== REGRESSION FUNDAMENTALS ===\n")
    cat("12. linear model\n")
    cat("13. assumptions\n")
    cat("14. ols (least squares)\n")
    cat("\n=== INFERENCE ===\n")
    cat("15. confidence interval\n")
    cat("16. t test (individual coefficient)\n")
    cat("17. f test partial (nested models)\n")
    cat("18. f test global (model significance)\n")
    cat("\n=== DEVIANCE & R-SQUARED ===\n")
    cat("19. deviance (decomposition)\n")
    cat("20. r squared\n")
    cat("\n=== CATEGORICAL VARIABLES ===\n")
    cat("21. dummy variables\n")
    cat("\n=== TRANSFORMATIONS & INTERACTIONS ===\n")
    cat("22. transformations (log, polynomial)\n")
    cat("23. interactions\n")
    cat("\n=== DIAGNOSTICS ===\n")
    cat("24. multicollinearity (VIF)\n")
    cat("25. outliers (leverage, influential)\n")
    cat("26. residual plots\n")
    cat("\n=== LOGISTIC REGRESSION ===\n")
    cat("27. logit model\n")
    cat("28. odds ratio\n")
    cat("29. logit tests (z-test, chi-square)\n")
    cat("\n=== PANEL DATA ===\n")
    cat("30. panel data (FEM, REM)\n")
    cat("31. hausman test\n")
    cat("\n=== TIME SERIES ===\n")
    cat("32. time series (basics, decomposition)\n")
    cat("33. acf pacf\n")
    cat("34. arima\n")
    cat("35. white noise (random walk)\n")
    cat("36. forecasting (SES, Holt, Holt-Winters)\n")
    cat("37. model selection\n")
    return(invisible(NULL))
  }

  name_clean <- tolower(trimws(name))
  key <- NULL

  # Matching logic - NEW THEORY TOPICS FIRST
  if (grepl("hypothesis", name_clean) && grepl("test", name_clean)) {
    key <- "hypothesis_testing"
  } else if (grepl("type", name_clean) && grepl("error", name_clean)) {
    key <- "type_errors"
  } else if (grepl("type.?i", name_clean) || grepl("type.?ii", name_clean)) {
    key <- "type_errors"
  } else if (grepl("power", name_clean) && grepl("test", name_clean)) {
    key <- "type_errors"
  } else if (grepl("p.?value", name_clean) || grepl("pvalue", name_clean)) {
    key <- "p_value"
  } else if (grepl("distribution", name_clean)) {
    key <- "distributions"
  } else if (grepl("estimator", name_clean) || grepl("unbias", name_clean) || grepl("consisten", name_clean) || grepl("efficien", name_clean)) {
    key <- "estimator_properties"
  } else if (grepl("\\bblue\\b", name_clean) || grepl("gauss.?markov", name_clean)) {
    key <- "blue"
  } else if (grepl("heterosked", name_clean) || grepl("heteroscedas", name_clean) || grepl("breusch", name_clean)) {
    key <- "heteroskedasticity"
  } else if (grepl("autocorrel", name_clean) && !grepl("function", name_clean)) {
    key <- "autocorrelation"
  } else if (grepl("durbin", name_clean) || grepl("serial correl", name_clean)) {
    key <- "autocorrelation"
  } else if (grepl("normality", name_clean) || grepl("shapiro", name_clean) || grepl("jarque", name_clean)) {
    key <- "normality_test"
  } else if (grepl("\\baic\\b", name_clean) && grepl("\\bbic\\b", name_clean)) {
    key <- "aic_bic"
  } else if (grepl("\\bbic\\b", name_clean)) {
    key <- "aic_bic"
  } else if (grepl("stationarity", name_clean) || grepl("unit.?root", name_clean) || grepl("\\badf\\b", name_clean) || grepl("dickey", name_clean) || grepl("ljung", name_clean) || grepl("\\bkpss\\b", name_clean)) {
    key <- "stationarity_tests"
  # ORIGINAL TOPICS
  } else if (grepl("linear", name_clean) && grepl("model", name_clean)) {
    key <- "linear_model"
  } else if (grepl("assumption", name_clean)) {
    key <- "assumptions"
  } else if (grepl("ols", name_clean) || grepl("least square", name_clean)) {
    key <- "ols"
  } else if (grepl("confidence", name_clean) || grepl("\\bci\\b", name_clean)) {
    key <- "confidence_interval"
  } else if (grepl("\\bt.?test", name_clean) || grepl("individual", name_clean) && grepl("test", name_clean)) {
    key <- "t_test"
  } else if (grepl("partial", name_clean) && grepl("f", name_clean)) {
    key <- "f_test_partial"
  } else if (grepl("global", name_clean) && grepl("f", name_clean)) {
    key <- "f_test_global"
  } else if (grepl("f.?test", name_clean) && grepl("nested", name_clean)) {
    key <- "f_test_partial"
  } else if (grepl("f.?test", name_clean)) {
    key <- "f_test_partial"
  } else if (grepl("deviance", name_clean) || grepl("decomposition", name_clean)) {
    key <- "deviance"
  } else if (grepl("r.?squared", name_clean) || grepl("r2", name_clean) || grepl("r\\^2", name_clean)) {
    key <- "r_squared"
  } else if (grepl("adjusted", name_clean)) {
    key <- "r_squared"
  } else if (grepl("dummy", name_clean) || grepl("categorical", name_clean)) {
    key <- "dummy"
  } else if (grepl("transform", name_clean) || grepl("log", name_clean) && !grepl("logit", name_clean)) {
    key <- "transformations"
  } else if (grepl("interaction", name_clean)) {
    key <- "interactions"
  } else if (grepl("multicollinearity", name_clean) || grepl("vif", name_clean)) {
    key <- "multicollinearity"
  } else if (grepl("outlier", name_clean) || grepl("leverage", name_clean) || grepl("influential", name_clean) || grepl("cook", name_clean)) {
    key <- "outliers"
  } else if (grepl("residual", name_clean) && grepl("plot", name_clean)) {
    key <- "residual_plots"
  } else if (grepl("residual", name_clean) && grepl("diagnostic", name_clean)) {
    key <- "residual_plots"
  } else if (grepl("qq", name_clean) || grepl("q-q", name_clean)) {
    key <- "residual_plots"
  } else if (grepl("logit", name_clean) || grepl("logistic", name_clean)) {
    key <- "logit_model"
  } else if (grepl("odds", name_clean)) {
    key <- "odds_ratio"
  } else if (grepl("chi.?square", name_clean) || grepl("z.?test", name_clean) && grepl("logit", name_clean)) {
    key <- "logit_tests"
  } else if (grepl("panel", name_clean) || grepl("fem", name_clean) || grepl("rem", name_clean)) {
    key <- "panel_data"
  } else if (grepl("hausman", name_clean)) {
    key <- "hausman_test"
  } else if (grepl("time.?series", name_clean) || grepl("seasonality", name_clean) || grepl("trend", name_clean)) {
    key <- "time_series"
  } else if (grepl("acf", name_clean) || grepl("pacf", name_clean)) {
    key <- "acf_pacf"
  } else if (grepl("arima", name_clean) || grepl("arma", name_clean) || grepl("\\bar\\b", name_clean) || grepl("\\bma\\b", name_clean)) {
    key <- "arima"
  } else if (grepl("white.?noise", name_clean) || grepl("random.?walk", name_clean)) {
    key <- "white_noise"
  } else if (grepl("forecast", name_clean) || grepl("holt", name_clean) || grepl("exponential.?smooth", name_clean)) {
    key <- "forecasting"
  } else if (grepl("selection", name_clean) || grepl("\\baic\\b", name_clean)) {
    key <- "model_selection"
  }

  if (is.null(key)) {
    cat("Topic not found for: '", name, "'\n", sep = "")
    cat("Try keywords like 'ols', 'r squared', 'logit', 'arima', 'panel', etc.\n")
    return(invisible(NULL))
  }

  cat(stats_list[[key]])
}
