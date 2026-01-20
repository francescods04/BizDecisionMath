#' @title Simple Interest Accumulation
#' 
#' @description
#' Calculates the final value (M) of a principal invested at simple interest.
#' Simple interest means interest is calculated only on the original principal,
#' not on accumulated interest.
#'
#' @details
#' **Mathematical Formula:**
#' \deqn{M = C \times (1 + i \times t)}
#'
#' **Key Properties of Simple Interest:**
#' - Interest grows linearly with time
#' - NOT decomposable (cannot split time periods)
#' - Used for short-term investments (< 1 year)
#'
#' **Notation:**
#' - M = Final (accumulated) value
#' - C = Principal (initial capital)
#' - i = Simple annual interest rate
#' - t = Time in years
#'
#' @param C Numeric. The principal amount (initial capital at t=0).
#' @param i Numeric. The simple annual interest rate (as decimal, e.g., 0.05 for 5%).
#' @param t Numeric. The time in years (can be fractional, e.g., 0.5 for 6 months).
#' @param verbose Logical. If TRUE, displays exam-style step-by-step calculation.
#'   Default uses global option `BizDecisionMath.verbose`.
#'
#' @return Numeric. The final accumulated value M.
#'
#' @seealso 
#' \code{\link{calc_simple_discount}} for present value with simple interest
#' \code{\link{calc_compound_accumulation}} for compound interest
#' \code{\link{show_formula}("simple interest")} for theoretical formula
#'
#' @examples
#' # Basic usage: 1000€ at 5% for 2 years
#' calc_simple_accumulation(C = 1000, i = 0.05, t = 2)
#' # Returns: 1100
#'
#' # With verbose output for exam preparation
#' calc_simple_accumulation(C = 1000, i = 0.05, t = 2, verbose = TRUE)
#'
#' # Exam problem: Short-term deposit
#' # A bank offers 3% simple interest on a 6-month deposit.
#' # How much will 5000€ grow to?
#' calc_simple_accumulation(C = 5000, i = 0.03, t = 0.5)
#'
#' @export
calc_simple_accumulation <- function(C, i, t, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  if (any(c(C, i, t) < 0)) warning("Inputs typically should be non-negative.")
  result <- C * (1 + i * t)
  
  if (verbose) {
    format_result(
      formula_name = "Simple Interest Accumulation",
      formula_text = "M = C × (1 + i × t)",
      inputs = list("C (Principal)" = C, "i (Interest rate)" = i, "t (Time in years)" = t),
      result = result,
      interpretation = sprintf("A principal of %.2f at %.2f%% simple interest for %.2f years grows to %.2f",
                              C, i * 100, t, result)
    )
  }
  
  return(result)
}

#' @title Simple Discount (Present Value)
#' 
#' @description
#' Calculates the present value (A) of a future amount using simple discount.
#' This is the inverse operation of simple accumulation.
#'
#' @details
#' **Mathematical Formula:**
#' \deqn{A = \frac{S}{1 + i \times t}}
#'
#' **Use Cases:**
#' - Discounting short-term receivables
#' - Calculating the price of Treasury bills
#' - Present value of amounts due within one year
#'
#' **Relationship to Accumulation:**
#' If M = C × (1 + it), then C = M / (1 + it)
#'
#' @param S Numeric. The nominal/future value to be discounted.
#' @param i Numeric. The simple annual interest rate (decimal).
#' @param t Numeric. The time in years until the amount is received.
#' @param verbose Logical. If TRUE, displays step-by-step calculation.
#'
#' @return Numeric. The present value A.
#'
#' @seealso 
#' \code{\link{calc_simple_accumulation}} for the inverse operation
#' \code{\link{calc_compound_discount}} for compound discounting
#'
#' @examples
#' # Basic usage: Present value of 1100€ due in 2 years at 5%
#' calc_simple_discount(S = 1100, i = 0.05, t = 2)
#' # Returns: 1000
#'
#' # Exam problem: Treasury bill pricing
#' # A T-bill with face value 10,000€ matures in 90 days.
#' # Market rate is 4%. What is its price?
#' calc_simple_discount(S = 10000, i = 0.04, t = 90/365)
#'
#' @export
calc_simple_discount <- function(S, i, t, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  denominator <- 1 + i * t
  result <- S / denominator
  
  if (verbose) {
    format_result(
      formula_name = "Simple Discount (Present Value)",
      formula_text = "A = S / (1 + i×t)",
      inputs = list(
        "S (Future/Nominal value)" = S,
        "i (Interest rate)" = i,
        "t (Time periods)" = t
      ),
      additional_info = list(
        "1 + i×t" = denominator
      ),
      result = result,
      interpretation = sprintf(
        "Present value of %.2f at %.2f%% simple interest for %.1f periods: %.2f",
        S, i * 100, t, result
      )
    )
  }
  
  return(result)
}

#' @title Compound Interest Accumulation
#' 
#' @description
#' Calculates the final value (M) of a principal invested at compound interest.
#' Compound interest means interest is earned on both principal AND accumulated interest.
#'
#' @details
#' **Mathematical Formula:**
#' \deqn{M = C \times (1 + i)^t}
#'
#' **Key Properties of Compound Interest:**
#' - Interest grows exponentially with time
#' - Decomposable: can split time periods
#' - Standard for medium/long-term investments
#' - (1 + i)^t is the accumulation factor
#'
#' **Relationship to Present Value:**
#' M = C × (1+i)^t ⟺ C = M × (1+i)^(-t)
#'
#' @param C Numeric. The principal amount (initial capital).
#' @param i Numeric. The annual interest rate (as decimal).
#' @param t Numeric. The time in years.
#' @param verbose Logical. If TRUE, displays exam-style calculation steps.
#'
#' @return Numeric. The final accumulated value M.
#'
#' @seealso 
#' \code{\link{calc_compound_discount}} for present value
#' \code{\link{calc_simple_accumulation}} for simple interest comparison
#' \code{\link{calc_equivalent_rate}} for rate conversions
#'
#' @examples
#' # Basic usage: 1000€ at 5% for 10 years
#' calc_compound_accumulation(C = 1000, i = 0.05, t = 10)
#' # Returns: 1628.89
#'
#' # Compare with simple interest
#' calc_compound_accumulation(C = 1000, i = 0.05, t = 10)  # 1628.89
#' calc_simple_accumulation(C = 1000, i = 0.05, t = 10)    # 1500.00
#'
#' # Exam problem: Long-term investment
#' # An investor deposits 50,000€ at 4% compound interest for 5 years.
#' # What is the final value?
#' calc_compound_accumulation(C = 50000, i = 0.04, t = 5, verbose = TRUE)
#'
#' @export
calc_compound_accumulation <- function(C, i, t, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  result <- C * (1 + i)^t
  
  if (verbose) {
    format_result(
      formula_name = "Compound Interest Accumulation",
      formula_text = "M = C × (1 + i)^t",
      inputs = list("C (Principal)" = C, "i (Interest rate)" = i, "t (Time in years)" = t),
      result = result,
      interpretation = sprintf("A principal of %.2f at %.2f%% compound interest for %.2f years grows to %.2f",
                              C, i * 100, t, result)
    )
  }
  
  return(result)
}

#' @title Compound Discount (Present Value)
#' 
#' @description
#' Calculates the present value (A) of a future amount using compound discount.
#' This is the inverse of compound accumulation.
#'
#' @details
#' **Mathematical Formula:**
#' \deqn{A = S \times (1 + i)^{-t} = \frac{S}{(1 + i)^t}}
#'
#' **Discount Factor:**
#' - v = (1 + i)^(-1) is the one-period discount factor
#' - v^t = (1 + i)^(-t) is the t-period discount factor
#'
#' **Use Cases:**
#' - Valuing future cash flows (NPV analysis)
#' - Bond pricing
#' - Pension fund calculations
#'
#' @param S Numeric. The nominal/future value.
#' @param i Numeric. The annual interest rate (decimal).
#' @param t Numeric. The time in years.
#' @param verbose Logical. If TRUE, displays calculation steps.
#'
#' @return Numeric. The present value A.
#'
#' @seealso 
#' \code{\link{calc_compound_accumulation}} for the inverse operation
#' \code{\link{calculate_npv}} for discounting multiple cash flows
#' \code{\link{annuity_pv}} for discounting annuity payments
#'
#' @examples
#' # Basic usage: Present value of 1628.89€ due in 10 years at 5%
#' calc_compound_discount(S = 1628.89, i = 0.05, t = 10)
#' # Returns: ~1000
#'
#' # Exam problem: Zero-coupon bond pricing
#' # A zero-coupon bond pays 100,000€ at maturity in 5 years.
#' # Market interest rate is 6%. What is the bond's price?
#' calc_compound_discount(S = 100000, i = 0.06, t = 5, verbose = TRUE)
#'
#' @export
calc_compound_discount <- function(S, i, t, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  discount_factor <- (1 + i)^(-t)
  result <- S * discount_factor
  
  if (verbose) {
    format_result(
      formula_name = "Compound Discount (Present Value)",
      formula_text = "A = S / (1+i)^t = S × (1+i)^(-t)",
      inputs = list(
        "S (Future/Nominal value)" = S,
        "i (Interest rate)" = i,
        "t (Time periods)" = t
      ),
      additional_info = list(
        "(1+i)^t" = (1+i)^t,
        "Discount factor (1+i)^(-t)" = discount_factor
      ),
      result = result,
      interpretation = sprintf(
        "Present value of %.2f at %.2f%% compound interest for %.1f periods: %.2f",
        S, i * 100, t, result
      )
    )
  }
  
  return(result)
}

#' @title Convert Between Annual and Periodic Interest Rates
#' 
#' @description
#' Converts between an effective annual interest rate (i) and a periodic rate (i_m)
#' that compounds m times per year. Essential for matching payment frequency with rate frequency.
#'
#' @details
#' **Mathematical Formulas:**
#' 
#' Annual to Periodic (given annual rate i, find periodic rate i_m):
#' \deqn{i_m = (1 + i)^{1/m} - 1}
#'
#' Periodic to Annual (given periodic rate i_m, find annual rate i):
#' \deqn{i = (1 + i_m)^m - 1}
#'
#' **Common Frequencies (m):**
#' - m = 12: Monthly
#' - m = 4: Quarterly
#' - m = 2: Semi-annual
#' - m = 365: Daily
#'
#' **When to Use:**
#' Use `annual_to_periodic` when you have an annual rate but payments are monthly/quarterly.
#' Use `periodic_to_annual` when you have a monthly rate and need the effective annual rate.
#'
#' @param rate Numeric. The known interest rate (annual or periodic).
#' @param m Integer. Frequency of compounding per year.
#' @param type Character. Either `"annual_to_periodic"` (default) or `"periodic_to_annual"`.
#' @param verbose Logical. If TRUE, displays the formula and calculation steps.
#'
#' @return Numeric. The equivalent interest rate.
#'
#' @seealso 
#' \code{\link{annuity_pv}} which requires matching rate and payment frequencies
#' \code{\link{show_formula}("equivalent rate")} for theoretical background
#' \code{\link{show_proof}("annual compounded")} for the proof
#'
#' @examples
#' # Convert 6% annual rate to monthly rate
#' calc_equivalent_rate(rate = 0.06, m = 12, type = "annual_to_periodic")
#' # Returns: 0.004868 (0.4868% per month)
#'
#' # Convert 0.5% monthly rate to annual rate
#' calc_equivalent_rate(rate = 0.005, m = 12, type = "periodic_to_annual")
#' # Returns: 0.0617 (6.17% effective annual rate)
#'
#' # Exam problem: Mortgage rate conversion
#' # A mortgage advertises 6% annual rate with monthly payments.
#' # What is the monthly rate to use in annuity calculations?
#' calc_equivalent_rate(rate = 0.06, m = 12, verbose = TRUE)
#'
#' @export
calc_equivalent_rate <- function(rate, m, type = "annual_to_periodic", verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  if (type == "annual_to_periodic") {
    result <- (1 + rate)^(1/m) - 1
    formula_text <- "i_m = (1 + i)^(1/m) - 1"
  } else if (type == "periodic_to_annual") {
    result <- (1 + rate)^m - 1
    formula_text <- "i = (1 + i_m)^m - 1"
  } else {
    stop("Invalid type. Use 'annual_to_periodic' or 'periodic_to_annual'.")
  }
  
  if (verbose) {
    format_result(
      formula_name = ifelse(type == "annual_to_periodic", 
                           "Annual to Periodic Rate Conversion",
                           "Periodic to Annual Rate Conversion"),
      formula_text = formula_text,
      inputs = list("Rate" = rate, "m (Frequency)" = m),
      result = result,
      interpretation = sprintf("The equivalent %s rate is %.6f (%.4f%%)",
                              ifelse(type == "annual_to_periodic", "periodic", "annual"),
                              result, result * 100)
    )
  }
  
  return(result)
}

#' @title Present Value of an Annuity
#' 
#' @description
#' Calculates the present value of a series of equal periodic payments.
#' Supports ordinary (end of period), due (beginning of period), and deferred annuities.
#'
#' @details
#' **Mathematical Formulas:**
#'
#' Ordinary Annuity (payments at END of each period):
#' \deqn{PV = R \times \frac{1 - (1+i)^{-n}}{i} = R \times a_{\overline{n}|i}}
#'
#' Due Annuity (payments at START of each period):
#' \deqn{PV = R \times \frac{1 - (1+i)^{-n}}{i} \times (1+i) = R \times \ddot{a}_{\overline{n}|i}}
#'
#' Deferred Annuity (starts after k periods):
#' \deqn{PV = R \times a_{\overline{n}|i} \times (1+i)^{-k}}
#'
#' **Key Concepts:**
#' - Ordinary: First payment at t=1 (e.g., loan payments)
#' - Due: First payment at t=0 (e.g., rent, insurance premiums)
#' - Deferred: First payment at t=k+1
#'
#' @param R Numeric. The periodic payment amount.
#' @param i Numeric. The interest rate per period (match payment frequency!).
#' @param n Integer. The number of payment periods.
#' @param type Character. `"ordinary"` (default) or `"due"`.
#' @param deferral Integer. Number of periods to defer start (default 0).
#' @param verbose Logical. If TRUE, displays exam-style solution.
#'
#' @return Numeric. The present value of the annuity.
#'
#' @seealso 
#' \code{\link{annuity_fv}} for future value of annuity
#' \code{\link{geom_annuity_pv}} for growing payment annuity
#' \code{\link{perpetuity_pv}} for infinite payment stream
#' \code{\link{calc_equivalent_rate}} to match rate with payment frequency
#' \code{\link{show_formula}("annuity")} for theoretical formulas
#'
#' @examples
#' # Basic ordinary annuity: 100€/month for 5 years at 0.5% monthly
#' annuity_pv(R = 100, i = 0.005, n = 60)
#' # Returns: ~5172
#'
#' # Due annuity (payments at beginning): same parameters
#' annuity_pv(R = 100, i = 0.005, n = 60, type = "due")
#' # Returns: ~5198 (higher because payments received earlier)
#'
#' # Deferred annuity: starts after 2 years (24 months)
#' annuity_pv(R = 100, i = 0.005, n = 60, deferral = 24)
#'
#' # Exam problem: Loan valuation
#' # A loan requires 36 monthly payments of 500€ at 6% annual rate.
#' # What is the loan principal?
#' monthly_rate <- calc_equivalent_rate(0.06, 12)
#' annuity_pv(R = 500, i = monthly_rate, n = 36, verbose = TRUE)
#'
#' @export
annuity_pv <- function(R, i, n, type = "ordinary", deferral = 0, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  # Base PV for ordinary annuity of 1: a_n_i = (1 - (1+i)^-n) / i
  if (i == 0) {
    base_calc <- n # If interest is 0, just sum of payments
  } else {
    base_calc <- (1 - (1 + i)^(-n)) / i
  }
  
  pv <- R * base_calc
  
  # Adjust for due annuity
  if (type == "due") {
    pv <- pv * (1 + i)
  }
  
  # Adjust for deferral
  if (deferral > 0) {
    pv <- pv * (1 + i)^(-deferral)
  }
  
  if (verbose) {
    formula_name <- paste0("Present Value of ", 
                          ifelse(type == "due", "Due", "Ordinary"),
                          " Annuity",
                          ifelse(deferral > 0, paste0(" (Deferred ", deferral, " periods)"), ""))
    formula_text <- ifelse(i == 0,
                          "PV = R × n",
                          paste0("PV = R × [(1 - (1+i)^(-n)) / i]",
                                ifelse(type == "due", " × (1+i)", ""),
                                ifelse(deferral > 0, paste0(" × (1+i)^(-", deferral, ")"), "")))
    format_result(
      formula_name = formula_name,
      formula_text = formula_text,
      inputs = list(
        "R (Payment)" = R,
        "i (Rate per period)" = i,
        "n (Periods)" = n,
        "Type" = type,
        "Deferral" = deferral
      ),
      result = pv,
      additional_info = list("Base factor" = base_calc),
      interpretation = sprintf("The present value of %d payments of %.2f at %.2f%% interest is %.2f",
                              n, R, i * 100, pv)
    )
  }
  
  return(pv)
}

#' @title Future Value of an Annuity
#' 
#' @description
#' Calculates the accumulated (future) value of a series of equal periodic payments
#' at the time of the last payment.
#'
#' @details
#' **Mathematical Formulas:**
#'
#' Ordinary Annuity:
#' \deqn{FV = R \times \frac{(1+i)^n - 1}{i} = R \times s_{\overline{n}|i}}
#'
#' Due Annuity:
#' \deqn{FV = R \times \frac{(1+i)^n - 1}{i} \times (1+i) = R \times \ddot{s}_{\overline{n}|i}}
#'
#' **Relationship to Present Value:**
#' FV = PV × (1+i)^n
#'
#' **Use Cases:**
#' - Retirement savings planning
#' - Sinking fund calculations
#' - Education fund accumulation
#'
#' @param R Numeric. The periodic payment amount.
#' @param i Numeric. The interest rate per period.
#' @param n Integer. The number of payment periods.
#' @param type Character. `"ordinary"` (default) or `"due"`.
#' @param verbose Logical. If TRUE, displays exam-style solution.
#'
#' @return Numeric. The future value of the annuity.
#'
#' @seealso 
#' \code{\link{annuity_pv}} for present value
#' \code{\link{calc_compound_accumulation}} for single sum
#'
#' @examples
#' # Basic savings plan: 500€/month for 10 years at 0.4% monthly
#' annuity_fv(R = 500, i = 0.004, n = 120)
#'
#' # Exam problem: Retirement savings
#' # An employee saves 1000€/month for 30 years at 5% annual rate.
#' # How much will they have at retirement?
#' monthly_rate <- calc_equivalent_rate(0.05, 12)
#' annuity_fv(R = 1000, i = monthly_rate, n = 360, verbose = TRUE)
#'
#' # Compare ordinary vs due annuity
#' annuity_fv(R = 100, i = 0.01, n = 12, type = "ordinary")  
#' annuity_fv(R = 100, i = 0.01, n = 12, type = "due")  # Higher
#'
#' @export
annuity_fv <- function(R, i, n, type = "ordinary", verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  # Base FV for ordinary annuity of 1: s_n_i = ((1+i)^n - 1) / i
  if (i == 0) {
    base_calc <- n
  } else {
    base_calc <- ((1 + i)^n - 1) / i
  }
  
  fv <- R * base_calc
  
  # Adjust for due annuity
  if (type == "due") {
    fv <- fv * (1 + i)
  }
  
  if (verbose) {
    formula_name <- paste0("Future Value of ", ifelse(type == "due", "Due", "Ordinary"), " Annuity")
    formula_text <- ifelse(i == 0,
                          "FV = R × n",
                          paste0("FV = R × [((1+i)^n - 1) / i]",
                                ifelse(type == "due", " × (1+i)", "")))
    format_result(
      formula_name = formula_name,
      formula_text = formula_text,
      inputs = list(
        "R (Payment)" = R,
        "i (Rate per period)" = i,
        "n (Periods)" = n,
        "Type" = type
      ),
      result = fv,
      additional_info = list("Base factor" = base_calc),
      interpretation = sprintf("After %d payments of %.2f at %.2f%% interest, the accumulated value is %.2f",
                              n, R, i * 100, fv)
    )
  }
  
  return(fv)
}
