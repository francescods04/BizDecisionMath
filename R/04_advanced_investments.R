#' Generalized Net Present Value (GNPV)
#' 
#' Calculates NPV using a term structure of interest rates (spot rates) instead of a constant rate.
#' Formula: GNPV = Sum(CashFlow_t / (1 + r_t)^t)
#'
#' @param cash_flows Numeric vector. The cash flows (including initial at t=0).
#' @param times Numeric vector. The times corresponding to cash flows.
#' @param spot_rates Numeric vector. The spot rates r(0, t) matching each time point. 
#'                   For t=0, the rate is irrelevant (usually 0).
#' @return Numeric. The Generalized NPV.
#' @export
generalized_npv <- function(cash_flows, times, spot_rates) {
  if (length(cash_flows) != length(times) || length(times) != length(spot_rates)) {
    stop("Length of cash_flows, times, and spot_rates must match.")
  }
  
  discount_factors <- (1 + spot_rates)^(-times)
  gnpv <- sum(cash_flows * discount_factors)
  
  return(gnpv)
}

#' Profitability Index
#' 
#' Calculates the Profitability Index (PI).
#' Formula: PI = PV(Future Cash Flows) / Absolute(Initial Investment)
#'          PI = (NPV + |C0|) / |C0|
#'
#' @param cash_flows Numeric vector. The cash flows.
#' @param times Numeric vector. The times (default 0:(n-1)).
#' @param i Numeric. The discount rate.
#' @param verbose Logical. If TRUE, prints detailed calculation steps and interpretation.
#' @return Numeric. The Profitability Index.
#' @export
profitability_index <- function(cash_flows, times = NULL, i, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  if (is.null(times)) {
    times <- seq_along(cash_flows) - 1
  }
  
  # Identify initial investment (assumed at time 0 and negative)
  initial_idx <- which(times == 0)
  if (length(initial_idx) == 0) stop("No cash flow at time 0 found.")
  
  initial_investment <- cash_flows[initial_idx]
  if (initial_investment >= 0) warning("Initial investment is usually negative. Calculating using absolute value.")
  
  abs_initial <- abs(initial_investment)
  
  # Placeholder for calculate_npv function, assuming it's defined elsewhere
  # For this example, we'll define a simple NPV calculation.
  calculate_npv <- function(cf, t, rate) {
    sum(cf / (1 + rate)^t)
  }
  
  # Calculate PV of all cash flows (NPV)
  npv <- calculate_npv(cash_flows, times, i)
  
  # PI = (NPV + |C0|) / |C0|
  pi <- (npv + abs_initial) / abs_initial
  
  return(pi)
}

#' Put-Call Parity
#' 
#' Uses Put-Call Parity to find the missing value among Call, Put, Stock Price, or Strike Price.
#' Formula: c - p = S - K * (1 + r)^-T
#'
#' @param c Numeric. Call price (set as NA to calculate).
#' @param p Numeric. Put price (set as NA to calculate).
#' @param S Numeric. Stock price (set as NA to calculate).
#' @param K Numeric. Strike price (set as NA to calculate).
#' @param r Numeric. Risk-free interest rate.
#' @param T Numeric. Time to maturity (years).
#' @return Numeric. The missing value.
#' @export
put_call_parity <- function(c = NA, p = NA, S = NA, K = NA, r, T) {
  discount_factor <- (1 + r)^(-T)
  
  # Check which one is missing
  if (is.na(c)) {
    # c = p + S - K * discount
    return(p + S - K * discount_factor)
  }
  if (is.na(p)) {
    # p = c - S + K * discount
    return(c - S + K * discount_factor)
  }
  if (is.na(S)) {
    # S = c - p + K * discount
    return(c - p + K * discount_factor)
  }
  if (is.na(K)) {
    # K * discount = p + S - c  => K = (p + S - c) / discount
    return((p + S - c) / discount_factor)
  }
  
  stop("Exactly one of c, p, S, K, must be NA (missing) to compute.")
}
