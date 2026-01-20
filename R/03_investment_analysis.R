#' @title Gordon Growth Model (Dividend Discount Model)
#' 
#' @description
#' Calculates the intrinsic value of a stock based on future dividend payments.
#' Also known as the Dividend Discount Model (DDM) or Gordon-Shapiro Model.
#'
#' @details
#' **Mathematical Formulas:**
#'
#' Infinite Horizon (standard Gordon Growth):
#' \deqn{P_0 = \frac{D_1}{i - g}}
#'
#' Finite Horizon (with terminal value):
#' \deqn{P_0 = \frac{D_1}{i-g} \times \left[1 - \left(\frac{1+g}{1+i}\right)^n\right] + \frac{P_n}{(1+i)^n}}
#'
#' **Key Assumptions:**
#' 1. Dividends grow at a constant rate g forever
#' 2. Required return i > growth rate g
#' 3. Company pays dividends
#'
#' **Relationship to Perpetuity:**
#' This is mathematically equivalent to a growing perpetuity.
#'
#' **Limitations:**
#' - Not suitable for non-dividend paying stocks
#' - Assumes constant growth (may not hold)
#' - Sensitive to the g and i estimates
#'
#' @param d Numeric. The next year's dividend (D₁).
#' @param i Numeric. The required rate of return (cost of equity).
#' @param g Numeric. The constant dividend growth rate.
#' @param P_future Numeric. Terminal stock price at year n (for finite horizon).
#' @param n Numeric. Number of years (Inf for infinite horizon).
#' @param verbose Logical. If TRUE, displays exam-style solution.
#'
#' @return Numeric. The intrinsic stock value P₀.
#'
#' @seealso 
#' \code{\link{perpetuity_pv}} for the underlying perpetuity calculation
#' \code{\link{geom_annuity_pv}} for finite growing cash flows
#' \code{\link{show_formula}("gordon")} for theoretical formula
#'
#' @examples
#' # Basic usage: D₁ = 2€, required return 10%, growth 3%
#' gordon_model(d = 2, i = 0.10, g = 0.03)
#' # Returns: 28.57
#'
#' # Exam problem: Stock valuation
#' # A company just paid a dividend of 1.50€. Dividends grow 4% annually.
#' # If required return is 12%, what is the stock worth?
#' # Note: d should be NEXT year's dividend = 1.50 * 1.04
#' gordon_model(d = 1.50 * 1.04, i = 0.12, g = 0.04, verbose = TRUE)
#'
#' # Finite horizon: Hold for 5 years, then sell at P₅ = 50€
#' gordon_model(d = 2, i = 0.10, g = 0.03, P_future = 50, n = 5)
#'
#' @export
gordon_model <- function(d, i, g, P_future = 0, n = Inf, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  if (i <= g && is.infinite(n)) stop("For infinite horizon, interest rate i must be greater than growth rate g.")
  
  if (is.infinite(n)) {
    result <- d / (i - g)
  } else {
    # Finite horizon: sum of discounted dividends + discounted price
    # V0 = d/(i-g) * (1 - ((1+g)/(1+i))^n) + P/(1+i)^n
    term1 <- (d / (i - g)) * (1 - ((1 + g) / (1 + i))^n)
    term2 <- P_future / (1 + i)^n
    result <- term1 + term2
  }
  
  if (verbose) {
    formula_name <- ifelse(is.infinite(n), "Gordon Growth Model (Infinite Horizon)", 
                          paste0("Gordon Growth Model (Finite Horizon, n=", n, ")"))
    formula_text <- ifelse(is.infinite(n),
                          "P = d₁ / (i - g)",
                          "P = [d/(i-g)] × [1 - ((1+g)/(1+i))^n] + P_future/(1+i)^n")
    format_result(
      formula_name = formula_name,
      formula_text = formula_text,
      inputs = list(
        "d (Next dividend)" = d,
        "i (Required return)" = i,
        "g (Growth rate)" = g,
        "P_future" = if(!is.infinite(n)) P_future else "N/A",
        "n (Years)" = if(!is.infinite(n)) n else "Infinite"
      ),
      result = result,
      interpretation = sprintf("Stock value: %.2f (discount rate %.2f%%, growth rate %.2f%%)",
                              result, i * 100, g * 100)
    )
  }
  
  return(result)
}

#' @title Net Present Value (NPV)
#' 
#' @description
#' Calculates the Net Present Value of a series of cash flows at a given discount rate.
#' NPV is the fundamental metric for evaluating investment projects.
#'
#' @details
#' **Mathematical Formula:**
#' \deqn{NPV = \sum_{t=0}^{n} \frac{CF_t}{(1+i)^t}}
#'
#' **Decision Rule:**
#' - NPV > 0: Accept the project (creates value)
#' - NPV < 0: Reject the project (destroys value)
#' - NPV = 0: Indifferent (earns exactly the required return)
#'
#' **Key Properties:**
#' - Accounts for time value of money
#' - Considers all cash flows
#' - Provides absolute value creation measure
#' - Additive: NPV(A+B) = NPV(A) + NPV(B)
#'
#' **Relationship to IRR:**
#' NPV(IRR) = 0 by definition
#'
#' @param cash_flows Numeric vector. Cash flows including initial investment at t=0.
#'   Typically: c(-initial_investment, CF1, CF2, ..., CFn)
#' @param times Numeric vector. Times corresponding to each cash flow.
#'   Default: 0, 1, 2, ... (assumes annual cash flows starting at t=0).
#' @param i Numeric. The discount rate (opportunity cost of capital).
#' @param verbose Logical. If TRUE, displays exam-style solution.
#'
#' @return Numeric. The Net Present Value.
#'
#' @seealso 
#' \code{\link{calculate_irr}} for Internal Rate of Return
#' \code{\link{profitability_index}} for PI calculation
#' \code{\link{payback_period}} for payback analysis
#' \code{\link{generalized_npv}} for NPV with term structure
#' \code{\link{show_proof}("npv irr")} for NPV-IRR equivalence proof
#'
#' @examples
#' # Basic usage: Initial investment -1000, returns 300, 400, 500 over 3 years at 10%
#' calculate_npv(cash_flows = c(-1000, 300, 400, 500), i = 0.10)
#'
#' # With verbose output for exam
#' calculate_npv(cash_flows = c(-1000, 300, 400, 500), i = 0.10, verbose = TRUE)
#'
#' # Exam problem: Project evaluation
#' # A machine costs 50,000€ and generates cash flows of:
#' # Year 1: 15,000€, Year 2: 20,000€, Year 3: 25,000€, Year 4: 10,000€
#' # Cost of capital is 8%. Should the company invest?
#' calculate_npv(c(-50000, 15000, 20000, 25000, 10000), i = 0.08, verbose = TRUE)
#'
#' # Non-standard timing: Cash flow at t=0.5 (6 months)
#' calculate_npv(c(-1000, 500, 600), times = c(0, 0.5, 1.5), i = 0.10)
#'
#' @export
calculate_npv <- function(cash_flows, times = NULL, i, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  if (is.null(times)) {
    times <- seq_along(cash_flows) - 1 # Assume 0, 1, 2...
  }
  
  if (length(cash_flows) != length(times)) stop("Length of cash_flows and times must match.")
  
  discount_factors <- (1 + i)^(-times)
  npv <- sum(cash_flows * discount_factors)
  
  if (verbose) {
    format_result(
      formula_name = "Net Present Value (NPV)",
      formula_text = "NPV = ∑ [CF_t / (1+i)^t]",
      inputs = list(
        "Cash flows" = paste(sprintf("%.2f", cash_flows), collapse = ", "),
        "Times" = paste(times, collapse = ", "),
        "i (Discount rate)" = i
      ),
      result = npv,
      additional_info = list(
        "Number of periods" = length(cash_flows),
        "Total inflows" = sum(cash_flows[cash_flows > 0]),
        "Total outflows" = sum(cash_flows[cash_flows < 0])
      ),
      interpretation = sprintf("NPV = %.2f %s at discount rate %.2f%%",
                              abs(npv),
                              ifelse(npv >= 0, "(Accept project)", "(Reject project)"),
                              i * 100)
    )
  }
  
  return(npv)
}

#' @title Internal Rate of Return (IRR)
#' 
#' @description
#' Calculates the Internal Rate of Return - the discount rate that makes NPV = 0.
#' IRR represents the project's expected rate of return.
#'
#' @details
#' **Mathematical Definition:**
#' IRR is the rate r that solves:
#' \deqn{0 = \sum_{t=0}^{n} \frac{CF_t}{(1+r)^t}}
#'
#' **Decision Rule:**
#' - IRR > cost of capital: Accept the project
#' - IRR < cost of capital: Reject the project
#' - IRR = cost of capital: Indifferent
#'
#' **Key Properties:**
#' - Provides a percentage return measure
#' - Independent of the discount rate
#' - Multiple IRRs possible with non-conventional cash flows
#'
#' **Limitations:**
#' - Not suitable for mutually exclusive projects of different scales
#' - May give multiple or no solutions
#' - Can conflict with NPV for ranking projects
#'
#' **Relationship to NPV:**
#' NPV > 0 ⟺ IRR > cost of capital (for conventional cash flows)
#'
#' @param cash_flows Numeric vector. The series of cash flows.
#' @param times Numeric vector. Times corresponding to cash flows.
#' @param guess Numeric. Initial guess for numerical solver (default 0.1).
#' @param verbose Logical. If TRUE, displays exam-style solution.
#'
#' @return Numeric. The IRR as a decimal (e.g., 0.15 for 15%). Returns NA if no solution.
#'
#' @seealso 
#' \code{\link{calculate_npv}} for NPV calculation
#' \code{\link{show_proof}("npv irr")} for NPV-IRR equivalence proof
#'
#' @examples
#' # Basic usage: Investment of 1000, returns of 400, 400, 400
#' calculate_irr(c(-1000, 400, 400, 400))
#' # Returns: ~0.0966 (9.66%)
#'
#' # Exam problem: Project evaluation
#' # A project requires 100,000€ investment and returns:
#' # Year 1: 30,000€, Year 2: 40,000€, Year 3: 50,000€
#' # What is the IRR? Should we accept if cost of capital is 10%?
#' irr <- calculate_irr(c(-100000, 30000, 40000, 50000), verbose = TRUE)
#' # Compare: irr > 0.10 means accept
#'
#' # Verify IRR: NPV at IRR should be ~0
#' irr <- calculate_irr(c(-1000, 400, 400, 400))
#' calculate_npv(c(-1000, 400, 400, 400), i = irr)  # Should be ~0
#'
#' @export
calculate_irr <- function(cash_flows, times = NULL, guess = 0.1, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  # Define NPV function for uniroot
  npv_func <- function(r) {
    calculate_npv(cash_flows, times, r)
  }
  
  # Try to find root
  # We search in range [-0.99, 100] (from -99% to 10000% return) to cover extreme cases
  tryCatch({
    root <- uniroot(npv_func, interval = c(-0.99, 100), tol = 1e-6)$root
    
    if (verbose) {
      format_result(
        formula_name = "Internal Rate of Return (IRR)",
        formula_text = "IRR is the rate r where NPV(r) = 0",
        inputs = list(
          "Cash flows" = paste(sprintf("%.2f", cash_flows), collapse = ", "),
          "Times" = paste(if(is.null(times)) seq_along(cash_flows) - 1 else times, collapse = ", ")
        ),
        result = root,
        interpretation = sprintf("IRR = %.4f (%.2f%%). %s",
                                root, root * 100,
                                ifelse(root > 0, "Project is profitable.", "Project is unprofitable."))
      )
    }
    
    return(root)
  }, error = function(e) {
    warning("Could not converge to an IRR. Check if cash flows change sign exactly once.")
    return(NA)
  })
}

#' @title Payback Period
#' 
#' @description
#' Calculates the time required to recover the initial investment.
#' Supports both simple (undiscounted) and discounted payback period.
#'
#' @details
#' **Definition:**
#' The payback period is the time at which cumulative cash flows equal zero.
#'
#' **Types:**
#' - Simple Payback (i = 0): Ignores time value of money
#' - Discounted Payback (i > 0): Uses discounted cash flows
#'
#' **Calculation:**
#' 1. Calculate cumulative (discounted) cash flows
#' 2. Find when cumulative total crosses zero
#' 3. Interpolate for exact crossing point
#'
#' **Decision Rule:**
#' Accept if payback < maximum acceptable payback period
#'
#' **Limitations:**
#' - Ignores cash flows after payback
#' - Simple payback ignores time value
#' - Doesn't measure profitability
#'
#' **Use Cases:**
#' - Liquidity-constrained firms
#' - Projects with high uncertainty beyond near term
#' - Quick screening of projects
#'
#' @param cash_flows Numeric vector. Cash flows including initial investment.
#' @param times Numeric vector. Times corresponding to cash flows.
#' @param i Numeric. Discount rate (0 for simple payback, >0 for discounted).
#'
#' @return Numeric. The payback period (interpolated). Returns NA if never pays back.
#'
#' @seealso 
#' \code{\link{calculate_npv}} for comprehensive project evaluation
#' \code{\link{calculate_irr}} for rate of return
#'
#' @examples
#' # Simple payback: Investment 1000, annual returns 300
#' payback_period(c(-1000, 300, 300, 300, 300, 300), i = 0)
#' # Returns: 3.33 years
#'
#' # Discounted payback at 10%
#' payback_period(c(-1000, 300, 300, 300, 300, 300), i = 0.10)
#' # Returns: longer than simple payback
#'
#' # Exam problem: Project payback
#' # Investment: 50,000€. Annual cash flows: 15,000€ for 5 years.
#' # Calculate simple and discounted (8%) payback periods.
#' payback_period(c(-50000, rep(15000, 5)), i = 0)     # Simple
#' payback_period(c(-50000, rep(15000, 5)), i = 0.08)  # Discounted
#'
#' @export
payback_period <- function(cash_flows, times = NULL, i = 0) {
  if (is.null(times)) {
    times <- seq_along(cash_flows) - 1
  }
  
  # Calculate DCF (Discounted Cash Flows)
  dcf <- cash_flows / (1 + i)^times
  
  # Calculate Cumulative DCF
  cum_dcf <- cumsum(dcf)
  
  # Find first positive cumulative value
  pos_index <- which(cum_dcf >= 0)[1]
  
  if (is.na(pos_index)) return(NA) # Never pays back
  if (pos_index == 1) return(times[1]) # Pays back immediately (if initial is positive, weird)
  
  # Interpolation
  t_curr <- times[pos_index]
  t_prev <- times[pos_index - 1]
  cum_curr <- cum_dcf[pos_index]
  cum_prev <- cum_dcf[pos_index - 1]
  
  # Linear interpolation for the crossing of zero
  fraction <- (0 - cum_prev) / (cum_curr - cum_prev)
  time_period <- t_prev + fraction * (t_curr - t_prev)
  
  return(time_period)
}
