#' Present Value of Geometric Annuity
#' 
#' Calculates PV of an annuity with geometrically increasing payments.
#' Formulas (Ordinary): PV = (R / (i - g)) * (1 - ((1 + g)/(1 + i))^n) * (1 + i)^-deferral
#' Formula (Due): PV = (R * (1 + i)) / (i - g) * (1 - ((1 + g)/(1 + i))^n) * (1 + i)^-deferral
#'
#' @param R Numeric. The first payment amount.
#' @param g Numeric. The growth rate of payments.
#' @param i Numeric. The interest rate.
#' @param n Integer. The number of periods.
#' @param type Character. "ordinary" or "due". Default "ordinary".
#' @param deferral Integer. Number of periods to defer the start of the annuity (default 0).
#' @return Numeric. The present value.
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

#' Present Value of Perpetuity
#' 
#' Calculates PV of a perpetuity, optionally with constant growth (geometric).
#' Formula (Ordinary): PV = R / (i - g)
#' Formula (Due): PV = (R / (i - g)) * (1 + i)
#'
#' @param R Numeric. The payment amount (first payment for geometric).
#' @param i Numeric. The interest rate.
#' @param g Numeric. The growth rate (default 0 for constant perpetuity). Must be less than i.
#' @param type Character. "ordinary" or "due". Default "ordinary".
#' @param deferral Integer. Number of periods to defer start (default 0).
#' @return Numeric. The present value.
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

#' Calculate Forward Rates from Spot Rates
#' 
#' Calculates the implied forward rate r(s, k) between time s and k given spot rates r(0, s) and r(0, k).
#' Formula: (1 + r(0,s))^s * (1 + r(s,k))^(k-s) = (1 + r(0,k))^k
#'
#' @param spot_rate_s Numeric. Spot rate for time s.
#' @param spot_rate_k Numeric. Spot rate for time k.
#' @param t_s Numeric. Time s (in years).
#' @param t_k Numeric. Time k (in years). Must be greater than t_s.
#' @return Numeric. The forward interest rate r(s, k).
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
