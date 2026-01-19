#' Gordon Growth Model
#' 
#' Calculates the value of a stock based on future dividends.
#' Formula: P = d / (i - g)
#' 
#' @param d Numeric. The dividend payment (usually next year's dividend d1).
#' @param i Numeric. The required rate of return (must be > g).
#' @param g Numeric. The constant growth rate of dividends.
#' @param P_future Numeric. Optional future selling price at year n (if model is finite).
#' @param n Integer. Number of years if finite horizon. If infinite (default), n is ignored.
#' @param verbose Logical. If TRUE, displays formula, inputs, and result. Default FALSE.
#' @return Numeric. The stock value.
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

#' Calculate Net Present Value (NPV)
#' 
#' Calculates the NPV of a series of cash flows.
#' Formula: NPV = Sum(CashFlow_t / (1 + i)^t)
#'
#' @param cash_flows Numeric vector. The cash flows (must include initial investment at t=0).
#' @param times Numeric vector. The times corresponding to cash flows (default 0:(n-1)).
#' @param i Numeric. The annual interest rate (opportunity cost of capital).
#' @param verbose Logical. If TRUE, displays formula, inputs, and result. Default FALSE.
#' @return Numeric. The Net Present Value.
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

#' Calculate Internal Rate of Return (IRR)
#' 
#' Calculates the IRR for a series of cash flows.
#' Finds r such that NPV(r) = 0.
#'
#' @param cash_flows Numeric vector. The series of cash flows.
#' @param times Numeric vector. The times corresponding to cash flows.
#' @param guess Numeric. Initial guess for root finding (default 0.1).
#' @param verbose Logical. If TRUE, displays formula, inputs, and result. Default FALSE.
#' @return Numeric. The IRR. Returns NA if not found.
#' @export
calculate_irr <- function(cash_flows, times = NULL, guess = 0.1, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  # Define NPV function for uniroot
  npv_func <- function(r) {
    calculate_npv(cash_flows, times, r)
  }
  
  # Try to find root
  # Try to find the root (IRR)
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

#' Calculate Payback Period
#' 
#' Calculates the (simple and discounted) payback period.
#'
#' @param cash_flows Numeric vector. The series of cash flows.
#' @param times Numeric vector. The times corresponding to cash flows.
#' @param i Numeric. Discount rate (set 0 for simple payback, >0 for discounted).
#' @return Numeric. The payback period (interpolated).
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
  # t* = t_last_neg + |cum_dcf_last_neg| / (dcf_current) * (t_current - t_last_neg)
  # Actually simpler: t* = t_prev + (0 - cum_prev) / (cum_curr - cum_prev) * (t_curr - t_prev)
  
  t_curr <- times[pos_index]
  t_prev <- times[pos_index - 1]
  cum_curr <- cum_dcf[pos_index]
  cum_prev <- cum_dcf[pos_index - 1]
  
  # Linear interpolation for the crossing of zero
  fraction <- (0 - cum_prev) / (cum_curr - cum_prev)
  time_period <- t_prev + fraction * (t_curr - t_prev)
  
  return(time_period)
}
