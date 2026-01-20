#' @title Generalized Net Present Value (GNPV)
#' 
#' @description
#' Calculates the Net Present Value (NPV) using a term structure of interest rates
#' (dependent on time) instead of a constant discount rate.
#' This is more accurate when the yield curve is not flat.
#'
#' @details
#' **Mathematical Formula:**
#' \deqn{GNPV = \sum_{t=0}^{n} \frac{CF_t}{(1 + r_{0,t})^t}}
#'
#' **Components:**
#' - \eqn{CF_t}: Cash flow at time t
#' - \eqn{r_{0,t}}: Spot rate for maturity t (the rate for a zero-coupon bond maturing at t)
#' - \eqn{(1 + r_{0,t})^{-t}}: The discount factor for time t (also denoted v(t) or B(0,t))
#'
#' **Assumptions:**
#' 1. Markets are efficient (no arbitrage)
#' 2. Spot rates are known for all relevant maturities
#'
#' **Use Cases:**
#' - Valuing long-term projects where interest rate risk is significant
#' - Bond pricing using the zero-coupon yield curve
#' - Liability driven investment (LDI) analysis
#'
#' @param cash_flows Numeric vector. The cash flows (including initial at t=0).
#' @param times Numeric vector. The times corresponding to cash flows (e.g., 0, 1, 2...).
#' @param spot_rates Numeric vector. The spot rates r(0, t) matching each time point. 
#'                   The rate corresponding to t=0 is typically irrelevant (usually 0).
#' @return Numeric. The Generalized NPV.
#'
#' @seealso 
#' \code{\link{calculate_npv}} for constant rate NPV
#' \code{\link{spot_to_forward}} for deriving forward rates from spot rates
#' \code{\link{show_formula}("gnpv")} for theoretical formula
#'
#' @examples
#' # Basic usage: Cash flows at t=0, 1, 2 with spot rates 0%, 3%, 4%
#' generalized_npv(cash_flows = c(-1000, 500, 600), 
#'                 times = c(0, 1, 2), 
#'                 spot_rates = c(0, 0.03, 0.04))
#' # Returns: ~33.88
#'
#' # Exam problem: Pricing a bond using spot rates
#' # A 2-year bond pays 5% annual coupon and face value 100 at maturity.
#' # Spot rates: 1-year = 2%, 2-year = 3.5%. Calculate price.
#' # Cash flows: t=1: 5, t=2: 105
#' generalized_npv(cash_flows = c(5, 105), 
#'                 times = c(1, 2), 
#'                 spot_rates = c(0.02, 0.035))
#'
#' @export
generalized_npv <- function(cash_flows, times, spot_rates) {
  if (length(cash_flows) != length(times) || length(times) != length(spot_rates)) {
    stop("Length of cash_flows, times, and spot_rates must match.")
  }
  
  discount_factors <- (1 + spot_rates)^(-times)
  gnpv <- sum(cash_flows * discount_factors)
  
  return(gnpv)
}

#' @title Profitability Index (PI)
#' 
#' @description
#' Calculates the Profitability Index, also known as the Benefit-Cost Ratio.
#' Measures the value created per unit of investment.
#'
#' @details
#' **Mathematical Formula:**
#' \deqn{PI = \frac{PV(\text{Future Inflows})}{\text{Initial Investment}}}
#' \deqn{PI = \frac{NPV + |C_0|}{|C_0|} = 1 + \frac{NPV}{|C_0|}}
#'
#' **Decision Rule:**
#' - PI > 1: Accept project (equivalent to NPV > 0)
#' - PI < 1: Reject project
#' - PI = 1: Indifferent
#'
#' **Use Cases:**
#' - Capital rationing: When budget is limited, rank projects by PI
#' - Comparing projects of different sizes
#'
#' **Interpretation:**
#' A PI of 1.15 means for every 1€ invested, the project returns 1.15€ in present value terms.
#'
#' @param cash_flows Numeric vector. The cash flows (must include initial investment at t=0).
#' @param times Numeric vector. The times (default 0:(n-1)).
#' @param i Numeric. The discount rate.
#' @param verbose Logical. If TRUE, prints detailed calculation steps and interpretation.
#' @return Numeric. The Profitability Index.
#'
#' @seealso 
#' \code{\link{calculate_npv}} for absolute value measure
#' \code{\link{calculate_irr}} for rate of return measure
#'
#' @examples
#' # Project: Invest 1000, receive 1200 in 1 year at 10% rate
#' # PV(Inflow) = 1200/1.1 = 1090.9
#' # PI = 1090.9 / 1000 = 1.09
#' profitability_index(c(-1000, 1200), i = 0.10)
#'
#' # Capital rationing exam problem:
#' # Choose between Project A (Cost 100, NPV 20) and Project B (Cost 50, NPV 15).
#' # PI_A = (120)/100 = 1.20
#' # PI_B = (65)/50 = 1.30
#' # Conclusion: Project B is more efficient per dollar invested.
#' profitability_index(c(-100, 132), i = 0.10) # A approx
#'
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
  
  # Calculate PV of all cash flows (NPV)
  calculate_npv_internal <- function(cf, t, rate) {
    sum(cf / (1 + rate)^t)
  }
  
  npv <- calculate_npv_internal(cash_flows, times, i)
  
  # PI = (NPV + |C0|) / |C0|
  # If NPV includes C0, then NPV+|C0| is just PV of future flows
  pi <- (npv + abs_initial) / abs_initial
  
  if (verbose) {
    format_result(
      formula_name = "Profitability Index (PI)",
      formula_text = "PI = PV(Inflows) / Initial Investment = 1 + NPV/|C0|",
      inputs = list(
        "Initial Investment (|C0|)" = abs_initial,
        "NPV" = npv,
        "PV of Inflows" = npv + abs_initial
      ),
      result = pi,
      interpretation = sprintf("PI = %.2f. %s", 
                              pi,
                              ifelse(pi > 1, "Accept (value created per $ > 1)", "Reject"))
    )
  }
  
  return(pi)
}

#' @title Put-Call Parity
#' 
#' @description
#' Uses the fundamental no-arbitrage relationship to find the missing price of European options.
#' Put-Call Parity relates the prices of puts and calls with the same strike and maturity.
#'
#' @details
#' **Mathematical Formula:**
#' \deqn{c + K \cdot e^{-rT} = p + S}
#' Or rearranged:
#' \deqn{c - p = S - K \cdot e^{-rT}}
#'
#' **Components:**
#' - c: European Call price
#' - p: European Put price
#' - S: Current Stock price
#' - K: Strike price
#' - \eqn{e^{-rT}}: Continuous discount factor (PV of 1)
#'
#' **Strategy Interpretation:**
#' - Left side (c + K*e^-rT): Buy Call + Invest PV(K) in risk-free bond (Fiduciary Call)
#' - Right side (p + S): Buy Put + Buy Stock (Protective Put)
#' Both strategies guarantee having value max(S_T, K) at maturity T.
#'
#' **Assumptions:**
#' - No dividends (basic formula)
#' - European options only
#' - No transaction costs
#' - No arbitrage opportunities exist
#'
#' @param c Numeric. Call price (set as NA to calculate).
#' @param p Numeric. Put price (set as NA to calculate).
#' @param S Numeric. Stock price (set as NA to calculate).
#' @param K Numeric. Strike price (set as NA to calculate).
#' @param r Numeric. Risk-free interest rate (continuous).
#' @param T Numeric. Time to maturity (years).
#' @return Numeric. The missing value calculated from parity.
#'
#' @seealso 
#' \code{\link{binomial_option_price}} for pricing options
#' \code{\link{show_proof}("put call parity")} for the no-arbitrage proof
#'
#' @examples
#' # Find Call Price: S=100, K=100, r=5%, T=1, Put=5
#' # c = p + S - K*e^-rT
#' put_call_parity(p=5, S=100, K=100, r=0.05, T=1)
#' # Returns: ~9.88
#'
#' # Arbitrage check: If market Call is 11, is there arbitrage?
#' # Theoretical 9.88 < Market 11 => Sell Call, Buy synthetic (Put+Stock-Bond)
#'
#' # Find Implied Stock Price given options
#' put_call_parity(c=10, p=5, K=100, r=0.05, T=1)
#'
#' @export
put_call_parity <- function(c = NA, p = NA, S = NA, K = NA, r, T) {
  discount_factor <- exp(-r * T)
  
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
