#' Calculate Simple Interest (Accumulation)
#' 
#' Calculates the final value M of a principal C invested at a simple annual interest rate i for a time t.
#' Formula: M = C * (1 + i * t)
#'
#' @param C Numeric. The principal amount (wealth at t=0).
#' @param i Numeric. The simple annual interest rate (decimal, e.g., 0.03 for 3%).
#' @param t Numeric. The time in years.
#' @param verbose Logical. If TRUE, displays formula, inputs, and result. Default FALSE.
#' @return Numeric. The final value M.
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

#' Calculate Simple Discount (Present Value)
#' 
#' Calculates the present value A of a nominal value S available at a future date t, using simple discount.
#' Formula: A = S / (1 + i * t)
#'
#' @param S Numeric. The nominal value (future amount).
#' @param i Numeric. The simple annual interest rate.
#' @param t Numeric. The time in years.
#' @param verbose Logical. If TRUE, displays calculation steps.
#' @return Numeric. The present value A.
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

#' Calculate Compound Interest (Accumulation)
#' 
#' Calculates the final value M in compound interest.
#' Formula: M = C * (1 + i)^t
#'
#' @param C Numeric. The principal.
#' @param i Numeric. The annual interest rate.
#' @param t Numeric. The time in years.
#' @param verbose Logical. If TRUE, displays formula, inputs, and result. Default FALSE.
#' @return Numeric. The final value M.
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

#' Calculate Compound Discount (Present Value)
#' 
#' Calculates the present value A in compound interest.
#' Formula: A = S * (1 + i)^(-t)
#'
#' @param S Numeric. The nominal value.
#' @param i Numeric. The annual interest rate.
#' @param t Numeric. The time in years.
#' @param verbose Logical. If TRUE, displays calculation steps.
#' @return Numeric. The present value A.
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

#' Calculate Equivalent Interest Rate
#' 
#' Converts between annual interest rate i and periodic interest rate i_m.
#' Formula i -> i_m: i_m = (1 + i)^(1/m) - 1
#' Formula i_m -> i: i = (1 + i_m)^m - 1
#'
#' @param rate Numeric. The known rate.
#' @param m Integer. Frequency of capitalization per year (e.g., 12 for monthly, 2 for semi-annual).
#' @param type Character. "annual_to_periodic" (default) or "periodic_to_annual".
#' @param verbose Logical. If TRUE, displays formula, inputs, and result. Default FALSE.
#' @return Numeric. The equivalent rate.
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

#' Present Value of an Annuity
#' 
#' Calculates the present value of an annuity.
#' Supports ordinary (post-icipated) and due (pre-icipated) annuities, with optional deferral.
#' 
#' @param R Numeric. The periodic payment amount.
#' @param i Numeric. The interest rate consistent with the period (e.g., use monthly rate for monthly payments).
#' @param n Integer. The number of periods.
#' @param type Character. "ordinary" (end of period) or "due" (beginning of period). Default "ordinary".
#' @param deferral Integer. Number of periods to defer the start of the annuity (default 0).
#' @param verbose Logical. If TRUE, displays formula, inputs, and result. Default FALSE.
#' @return Numeric. The present value.
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

#' Final Value of an Annuity
#' 
#' Calculates the final value of an annuity at time n.
#' 
#' @param R Numeric. The periodic payment.
#' @param i Numeric. The interest rate per period.
#' @param n Integer. The number of periods.
#' @param type Character. "ordinary" or "due". Default "ordinary".
#' @param verbose Logical. If TRUE, displays formula, inputs, and result. Default FALSE.
#' @return Numeric. The final value.
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
