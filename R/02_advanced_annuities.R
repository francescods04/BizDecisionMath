#' @title Present Value of Geometric (Growing) Annuity
#' 
#' @description
#' Calculates the present value of an annuity where payments grow at a constant rate g
#' each period. Also known as a growing annuity or escalating payment annuity.
#'
#' @details
#' **Mathematical Formulas:**
#'
#' Ordinary Growing Annuity (i ≠ g):
#' \deqn{PV = \frac{R}{i-g} \times \left[1 - \left(\frac{1+g}{1+i}\right)^n\right]}
#'
#' Due Growing Annuity (i ≠ g):
#' \deqn{PV = \frac{R \times (1+i)}{i-g} \times \left[1 - \left(\frac{1+g}{1+i}\right)^n\right]}
#'
#' Special Case (i = g):
#' \deqn{PV = \frac{n \times R}{1+i}}
#'
#' **Payment Pattern:**
#' - Period 1: R
#' - Period 2: R(1+g)
#' - Period 3: R(1+g)²
#' - Period n: R(1+g)^(n-1)
#'
#' **Use Cases:**
#' - Salary-indexed pension payments
#' - Inflation-adjusted income streams
#' - Leases with built-in rent increases
#'
#' @param R Numeric. The first payment amount.
#' @param g Numeric. The growth rate of payments per period (decimal).
#' @param i Numeric. The interest/discount rate per period (decimal).
#' @param n Integer. The number of payment periods.
#' @param type Character. `"ordinary"` (default) or `"due"`.
#' @param deferral Integer. Number of periods to defer the annuity (default 0).
#' @param verbose Logical. If TRUE, displays exam-style step-by-step solution.
#'
#' @return Numeric. The present value of the growing annuity.
#'
#' @seealso 
#' \code{\link{annuity_pv}} for constant payment annuity
#' \code{\link{perpetuity_pv}} for infinite growing payments (Gordon Growth)
#' \code{\link{show_formula}("growing annuity")} for theoretical formula
#' \code{\link{show_proof}("growing annuity")} for mathematical proof
#'
#' @examples
#' # Basic usage: 10,000€ first payment, growing 2%/year for 10 years at 5%
#' geom_annuity_pv(R = 10000, g = 0.02, i = 0.05, n = 10)
#'
#' # Exam problem: Pension valuation
#' # An employee will receive a pension starting at 30,000€/year,
#' # increasing 3% annually for 20 years. Discount rate is 6%.
#' # What is the present value of this pension?
#' geom_annuity_pv(R = 30000, g = 0.03, i = 0.06, n = 20, verbose = TRUE)
#'
#' # Deferred growing annuity: pension starts in 5 years
#' geom_annuity_pv(R = 30000, g = 0.03, i = 0.06, n = 20, deferral = 5)
#'
#' @export
geom_annuity_pv <- function(R, g, i, n, type = "ordinary", deferral = 0, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  if (i == g) {
    # Special case where growth rate equals interest rate
    if (type == "ordinary") {
      base_pv <- n * R / (1 + i)
    } else {
      base_pv <- n * R
    }
    warning("Special case: growth rate equals interest rate. Using limit formula: PV = n*R/(1+i)")
  } else {
    ratio <- (1 + g) / (1 + i)
    ratio_power <- ratio^n
    bracket_term <- 1 - ratio_power
    base_term <- (R / (i - g)) * bracket_term
    
    if (type == "due") {
      base_pv <- base_term * (1 + i)
    } else {
      base_pv <- base_term
    }
  }
  
  # Apply deferral discount
  if (deferral > 0) {
    pv_before_deferral <- base_pv
    base_pv <- base_pv * (1 + i)^(-deferral)
  }
  
  result <- base_pv
  
  # Verbose output
  if (verbose && i != g) {
    payment_adj <- ifelse(type == "due", R * (1 + i), R)
    
    format_result(
      formula_name = sprintf("Geometric Annuity Present Value (%s)", 
                             ifelse(type == "due", "Due", "Ordinary")),
      formula_text = ifelse(type == "ordinary",
                           "PV = [R / (i - g)] × [1 - ((1+g)/(1+i))^n]",
                           "PV = [R×(1+i) / (i - g)] × [1 - ((1+g)/(1+i))^n]"),
      inputs = list(
        "R (First payment)" = R,
        "g (Growth rate)" = g,
        "i (Interest rate)" = i,
        "n (Number of payments)" = n,
        "Type" = type,
        "Deferral" = deferral
      ),
      additional_info = list(
        "(1+g)/(1+i) ratio" = ratio,
        "Ratio^n" = ratio_power,
        "[1 - ratio^n]" = bracket_term,
        "Base term" = base_term,
        if (deferral > 0) c("PV before deferral" = pv_before_deferral,
                           "Discount factor" = (1+i)^(-deferral))
      ),
      result = result,
      interpretation = sprintf(
        "Present value of %d payments starting at %.2f, growing at %.2f%%, discounted at %.2f%%: %.2f",
        n, R, g * 100, i * 100, result
      )
    )
  }
  
  return(result)
}

#' @title Present Value of Perpetuity
#' 
#' @description
#' Calculates the present value of an infinite stream of payments.
#' Supports both constant perpetuities and growing perpetuities (Gordon Growth Model).
#'
#' @details
#' **Mathematical Formulas:**
#'
#' Constant Perpetuity (g = 0):
#' \deqn{PV = \frac{R}{i}}
#'
#' Growing Perpetuity / Gordon Growth Model (g > 0):
#' \deqn{PV = \frac{R}{i - g}}
#'
#' **Convergence Condition:**
#' For the series to converge, we MUST have g < i.
#' If g ≥ i, the perpetuity has infinite present value.
#'
#' **Gordon Growth Model Context:**
#' When used for stock valuation:
#' - R = Next year's dividend (D₁)
#' - i = Required rate of return
#' - g = Dividend growth rate
#' - PV = Stock price (P₀)
#'
#' **Use Cases:**
#' - Stock valuation (Gordon Growth)
#' - Preferred stock pricing
#' - Endowment fund sizing
#' - Consol bond pricing
#'
#' @param R Numeric. The payment amount (first payment for growing perpetuity).
#' @param i Numeric. The interest/discount rate (must be > g).
#' @param g Numeric. The growth rate (default 0 for constant perpetuity).
#' @param type Character. `"ordinary"` (default) or `"due"`.
#' @param deferral Integer. Number of periods to defer start (default 0).
#' @param verbose Logical. If TRUE, displays exam-style solution.
#'
#' @return Numeric. The present value of the perpetuity.
#'
#' @seealso 
#' \code{\link{geom_annuity_pv}} for finite growing annuity
#' \code{\link{gordon_model}} for stock valuation
#' \code{\link{show_formula}("perpetuity")} for theoretical formula
#'
#' @examples
#' # Constant perpetuity: 1000€/year forever at 5%
#' perpetuity_pv(R = 1000, i = 0.05)
#' # Returns: 20,000
#'
#' # Growing perpetuity: 1000€/year growing 2% at 5%
#' perpetuity_pv(R = 1000, i = 0.05, g = 0.02)
#' # Returns: 33,333
#'
#' # Exam problem: Stock valuation using Gordon Growth
#' # A stock pays a dividend of 2€ next year, growing 3% annually.
#' # If required return is 8%, what should the stock price be?
#' perpetuity_pv(R = 2, i = 0.08, g = 0.03, verbose = TRUE)
#'
#' # Deferred perpetuity: Endowment paying 50,000€/year starting in 10 years
#' perpetuity_pv(R = 50000, i = 0.04, deferral = 10)
#'
#' @export
perpetuity_pv <- function(R, i, g = 0, type = "ordinary", deferral = 0, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  if (g >= i) stop("Growth rate g must be strictly less than interest rate i for a perpetuity.")
  
  base_pv <- R / (i - g)
  
  if (type == "due") {
    base_pv <- base_pv * (1 + i)
  }
  
  # Apply deferral
  if (deferral > 0) {
    pv_before_deferral <- base_pv
    base_pv <- base_pv * (1 + i)^(-deferral)
  }
  
  result <- base_pv
  
  # Verbose output
  if (verbose) {
    formula_text <- if (g == 0) {
      if (type == "ordinary") "PV = R / i" else "PV = (R / i) × (1 + i)"
    } else {
      if (type == "ordinary") "PV = R / (i - g)" else "PV = (R / (i - g)) × (1 + i)"
    }
    
    model_name <- if (g > 0) "Growing Perpetuity (Gordon Growth Model)" else "Constant Perpetuity"
    
    format_result(
      formula_name = sprintf("%s (%s)", model_name,
                             ifelse(type == "due", "Due", "Ordinary")),
      formula_text = formula_text,
      inputs = list(
        "R (Payment)" = R,
        "i (Interest rate)" = i,
        if (g > 0) c("g (Growth rate)" = g),
        "Type" = type,
        "Deferral (periods)" = deferral
      ),
      additional_info = list(
        if (deferral > 0) c("PV at deferral point" = pv_before_deferral,
                           "Discount factor" = (1+i)^(-deferral),
                           "Deferral (years)" = deferral)
      ),
      result = result,
      interpretation = if (g == 0) {
        sprintf("Perpetuity of %.2f per period at %.2f%% interest: %.2f", 
                R, i * 100, result)
      } else {
        sprintf("Growing perpetuity starting at %.2f, growing at %.2f%%, discounted at %.2f%%: %.2f",
                R, g * 100, i * 100, result)
      }
    )
  }
  
  return(result)
}

#' @title Calculate Forward Rate from Spot Rates
#' 
#' @description
#' Calculates the implied forward interest rate between two future dates
#' using the term structure of spot rates. Based on the no-arbitrage principle.
#'
#' @details
#' **Mathematical Formula:**
#' The no-arbitrage condition implies:
#' \deqn{(1 + r_{0,s})^s \times (1 + r_{s,k})^{k-s} = (1 + r_{0,k})^k}
#'
#' Solving for the forward rate r(s,k):
#' \deqn{r_{s,k} = \left[\frac{(1 + r_{0,k})^k}{(1 + r_{0,s})^s}\right]^{\frac{1}{k-s}} - 1}
#'
#' **Interpretation:**
#' - r(0,s) = Spot rate from today to time s
#' - r(0,k) = Spot rate from today to time k
#' - r(s,k) = Forward rate agreed today for borrowing/lending from s to k
#'
#' **No-Arbitrage Principle:**
#' The forward rate is the rate that makes investing for k years directly
#' equivalent to investing for s years then reinvesting at the forward rate.
#'
#' **Use Cases:**
#' - Pricing forward rate agreements (FRAs)
#' - Extracting market expectations of future rates
#' - Bond trading strategies
#'
#' @param spot_rate_s Numeric. Spot rate r(0,s) to time s.
#' @param spot_rate_k Numeric. Spot rate r(0,k) to time k.
#' @param t_s Numeric. Start time s (in years).
#' @param t_k Numeric. End time k (in years). Must be > t_s.
#' @param verbose Logical. If TRUE, displays exam-style solution.
#'
#' @return Numeric. The forward interest rate r(s,k).
#'
#' @seealso 
#' \code{\link{show_proof}("spot forward")} for the no-arbitrage proof
#' \code{\link{show_formula}("forward rate")} for theoretical formula
#'
#' @examples
#' # Basic usage: Given 1-year spot = 3% and 2-year spot = 4%,
#' # find the 1-year forward rate starting in 1 year
#' spot_to_forward(spot_rate_s = 0.03, spot_rate_k = 0.04, t_s = 1, t_k = 2)
#' # Returns: ~5.01%
#'
#' # Exam problem: Term structure analysis
#' # The current term structure shows:
#' # - 1-year spot rate: 2.5%
#' # - 3-year spot rate: 3.5%
#' # What is the implied forward rate from year 1 to year 3?
#' spot_to_forward(0.025, 0.035, 1, 3, verbose = TRUE)
#'
#' # Verify no-arbitrage: Both paths should give same accumulation
#' # Direct: (1 + 0.04)^2 = 1.0816
#' # Indirect: (1 + 0.03)^1 * (1 + 0.0501)^1 ≈ 1.0816
#'
#' @export
spot_to_forward <- function(spot_rate_s, spot_rate_k, t_s, t_k, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  if (t_k <= t_s) stop("Time k must be greater than time s.")
  
  numerator <- (1 + spot_rate_k)^t_k
  denominator <- (1 + spot_rate_s)^t_s
  power <- 1 / (t_k - t_s)
  
  forward_rate <- (numerator / denominator)^power - 1
  
  if (verbose) {
    format_result(
      formula_name = "Forward Rate",
      formula_text = "r(s,k) = [(1+r(0,k))^k / (1+r(0,s))^s]^(1/(k-s)) - 1",
      inputs = list(
        "r(0,s) (Spot rate to time s)" = spot_rate_s,
        "r(0,k) (Spot rate to time k)" = spot_rate_k,
        "s (Start time)" = t_s,
        "k (End time)" = t_k
      ),
      additional_info = list(
        "(1+r(0,k))^k" = numerator,
        "(1+r(0,s))^s" = denominator,
        "Ratio" = numerator / denominator,
        "Time period (k-s)" = t_k - t_s
      ),
      result = forward_rate,
      interpretation = sprintf(
        "Forward rate from year %.1f to %.1f: %.2f%%",
        t_s, t_k, forward_rate * 100
      )
    )
  }
  
  return(forward_rate)
}
