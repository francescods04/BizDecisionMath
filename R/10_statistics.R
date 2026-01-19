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
    cat("=== REGRESSION FUNDAMENTALS ===\n")
    cat("1.  linear model\n")
    cat("2.  assumptions\n")
    cat("3.  ols (least squares)\n")
    cat("\n=== INFERENCE ===\n")
    cat("4.  confidence interval\n")
    cat("5.  t test (individual coefficient)\n")
    cat("6.  f test partial (nested models)\n")
    cat("7.  f test global (model significance)\n")
    cat("\n=== DEVIANCE & R-SQUARED ===\n")
    cat("8.  deviance (decomposition)\n")
    cat("9.  r squared\n")
    cat("\n=== CATEGORICAL VARIABLES ===\n")
    cat("10. dummy variables\n")
    cat("\n=== TRANSFORMATIONS & INTERACTIONS ===\n")
    cat("11. transformations (log, polynomial)\n")
    cat("12. interactions\n")
    cat("\n=== DIAGNOSTICS ===\n")
    cat("13. multicollinearity (VIF)\n")
    cat("14. outliers (leverage, influential)\n")
    cat("15. residual plots\n")
    cat("\n=== LOGISTIC REGRESSION ===\n")
    cat("16. logit model\n")
    cat("17. odds ratio\n")
    cat("18. logit tests (z-test, chi-square)\n")
    cat("\n=== PANEL DATA ===\n")
    cat("19. panel data (FEM, REM)\n")
    cat("20. hausman test\n")
    cat("\n=== TIME SERIES ===\n")
    cat("21. time series (basics, decomposition)\n")
    cat("22. acf pacf\n")
    cat("23. arima\n")
    cat("24. white noise (random walk)\n")
    cat("25. forecasting (SES, Holt, Holt-Winters)\n")
    cat("26. model selection (AIC)\n")
    return(invisible(NULL))
  }

  name_clean <- tolower(trimws(name))
  key <- NULL

  # Matching logic
  if (grepl("linear", name_clean) && grepl("model", name_clean)) {
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
  } else if (grepl("acf", name_clean) || grepl("pacf", name_clean) || grepl("autocorrel", name_clean)) {
    key <- "acf_pacf"
  } else if (grepl("arima", name_clean) || grepl("arma", name_clean) || grepl("\\bar\\b", name_clean) || grepl("\\bma\\b", name_clean)) {
    key <- "arima"
  } else if (grepl("white.?noise", name_clean) || grepl("random.?walk", name_clean)) {
    key <- "white_noise"
  } else if (grepl("forecast", name_clean) || grepl("holt", name_clean) || grepl("exponential.?smooth", name_clean)) {
    key <- "forecasting"
  } else if (grepl("selection", name_clean) || grepl("aic", name_clean)) {
    key <- "model_selection"
  }

  if (is.null(key)) {
    cat("Topic not found for: '", name, "'\n", sep = "")
    cat("Try keywords like 'ols', 'r squared', 'logit', 'arima', 'panel', etc.\n")
    return(invisible(NULL))
  }

  cat(stats_list[[key]])
}
