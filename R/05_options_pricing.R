#' @title Cox-Ross-Rubinstein (CRR) Calibration
#' 
#' @description
#' Calibrates the parameters (u, d, p) for a Binomial Tree model based on volatility.
#' The CRR method ensures the binomial model converges to Black-Scholes as n -> infinity.
#'
#' @details
#' **Formulas:**
#' \deqn{u = e^{\sigma \sqrt{\Delta t}}}
#' \deqn{d = e^{-\sigma \sqrt{\Delta t}} = \frac{1}{u}}
#' \deqn{p = \frac{e^{r \Delta t} - d}{u - d}}
#'
#' **Components:**
#' - \eqn{\sigma}: Annual volatility (standard deviation of log returns)
#' - \eqn{\Delta t}: Time step length (T/n)
#' - \eqn{r}: Risk-free rate (continuous)
#'
#' **Key Property:**
#' Recombining tree: u * d = 1. This drastically reduces computational complexity.
#'
#' @param sigma Numeric. Volatility of the underlying asset (e.g., 0.20 for 20%).
#' @param T Numeric. Time to maturity in years.
#' @param n Integer. Number of time steps.
#' @param r Numeric. Risk-free annual interest rate (continuous).
#' @return List containing:
#'   \item{u}{Up factor}
#'   \item{d}{Down factor}
#'   \item{p}{Risk-neutral probability of up move}
#'   \item{delta_t}{Length of each time step}
#'
#' @seealso 
#' \code{\link{binomial_option_price}} which uses these parameters
#' \code{\link{show_formula}("crr")} for theory
#'
#' @examples
#' # Calibrate for: sigma=20%, T=1 year, n=2 steps, r=5%
#' params <- crr_calibration(sigma = 0.20, T = 1, n = 2, r = 0.05)
#' params$u  # ~1.15
#' params$d  # ~0.87
#'
#' @export
crr_calibration <- function(sigma, T, n, r) {
  delta_t <- T / n
  u <- exp(sigma * sqrt(delta_t))
  d <- exp(-sigma * sqrt(delta_t))
  
  # Growth factor per step (Risk-neutral)
  growth <- exp(r * delta_t)
  p <- (growth - d) / (u - d)
  
  return(list(u = u, d = d, p = p, delta_t = delta_t))
}

#' @title Binomial Option Pricing (European)
#' 
#' @description
#' Prices European options using a multi-step binomial tree.
#' Supports both custom parameters (u, d) and CRR calibration from volatility.
#'
#' @details
#' **Methodology:**
#' 1. **Tree Construction:** Simulate stock price paths forward from S0 to T.
#' 2. **Terminal Payoffs:** Calculate max(S_T - K, 0) or max(K - S_T, 0) at all final nodes.
#' 3. **Backward Induction:** Discount values back to t=0 using risk-neutral probability p.
#'    \deqn{V_t = e^{-r \Delta t} [p V_{t+1}^{up} + (1-p) V_{t+1}^{down}]}
#'
#' **Verbose Output:**
#' Creates a detailed, exam-ready solution showing:
#' - Parameter derivation
#' - Full stock price tree
#' - Terminal option values
#' - Step-by-step backward induction
#'
#' **Assumptions:**
#' - European options (exercise only at maturity)
#' - No dividends (unless delta specified)
#' - Frictionless market
#'
#' @param S Numeric. Current stock price (S0).
#' @param K Numeric. Strike price.
#' @param r Numeric. Risk-free interest rate (continuous).
#' @param T Numeric. Time to maturity (years).
#' @param type Character. "call" or "put".
#' @param n Integer. Number of steps (default 1).
#' @param sigma Numeric. Volatility (optional, for CRR).
#' @param u Numeric. Up factor (optional, if not using CRR).
#' @param d Numeric. Down factor (optional, if not using CRR).
#' @param delta Numeric. Continuous dividend yield (default 0).
#' @param verbose Logical. If TRUE, generates a full exam-style walk-through.
#'
#' @return Numeric. The calculated option price.
#'
#' @seealso 
#' \code{\link{crr_calibration}} for parameter calculation
#' \code{\link{put_call_parity}} for checking results
#' \code{\link{show_formula}("binomial")} for theory
#'
#' @examples
#' # Single step example (classic exam problem)
#' # S=100, K=100, r=5%, T=1, u=1.1, d=0.9
#' binomial_option_price(S=100, K=100, r=0.05, T=1, u=1.1, d=0.9, n=1)
#'
#' # Full CRR Exam Solution:
#' # S=50, K=50, r=10%, sigma=40%, T=5 months (5/12)
#' # Use 5 steps (monthly)
#' binomial_option_price(S=50, K=50, r=0.10, sigma=0.40, T=5/12, n=5, verbose=TRUE)
#'
#' @export
binomial_option_price <- function(S, K, r, T, type = "call", n = 1, sigma = NULL, u = NULL, d = NULL, delta = 0, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  
  # Track if we used CRR for verbose output
  used_crr <- FALSE
  
  # 1. Determine Model Parameters
  if (!is.null(sigma)) {
    # Use CRR
    used_crr <- TRUE
    params <- crr_calibration(sigma, T, n, r)
    u <- params$u
    d <- params$d
    dt <- params$delta_t
    growth <- exp((r - delta) * dt)
    p <- (growth - d) / (u - d)
    
  } else if (!is.null(u) && !is.null(d)) {
    # Use provided u, d
    dt <- T / n
    growth <- exp((r - delta) * dt)
    p <- (growth - d) / (u - d)
    
  } else {
    stop("Must provide either sigma (for CRR) or u and d.")
  }
  
  # 2. Build Price Tree for final nodes
  final_prices <- numeric(n + 1)
  for (i in 0:n) {
    # i up moves, (n-i) down moves
    final_prices[i + 1] <- S * (u^i) * (d^(n - i))
  }
  
  # 3. Calculate Payoffs at Maturity
  option_payoffs <- numeric(n + 1)
  for (i in seq_along(final_prices)) {
    if (type == "call") {
      option_payoffs[i] <- max(final_prices[i] - K, 0)
    } else {
      option_payoffs[i] <- max(K - final_prices[i], 0)
    }
  }
  
  # Store intermediate prices for verbose output
  prices_t1 <- NULL
  if (verbose && n >= 2) {
    prices_t1 <- numeric(n)
  }
  
  # 4. Backward Induction
  discount_factor <- exp(-r * dt)
  option_values <- option_payoffs
  
  if (n > 0) {
    for (step in n:1) { 
      current_values <- numeric(step)
      for (i in 1:step) {
        val_up <- option_values[i + 1]
        val_down <- option_values[i]
        current_values[i] <- (p * val_up + (1 - p) * val_down) * discount_factor
      }
      
      # Store t=1 prices for verbose (BEFORE updating option_values)
      if (verbose && step == n && n >= 2) {
        prices_t1 <- current_values
      }
      
      option_values <- current_values
    }
  }
  
  result_price <- option_values[1]
  
  # 5. Verbose Output (Exam-Style Format with ALL intermediate calculations)
  if (verbose) {
    cat("\n")
    cat("=== BINOMIAL OPTION PRICING - EXAM SOLUTION FORMAT ===\n\n")
    
    # Show CRR calibration if used
    if (used_crr) {
      cat("CRR Model Calibration (Cox-Ross-Rubinstein):\n\n")
      cat(sprintf("   h = T/n = %.4f/%d = %.4f years\n", T, n, dt))
      cat(sprintf("   σ (volatility) = %.4f = %.2f%%\n\n", sigma, sigma * 100))
      
      cat("   u = e^(σ√h)\n")
      cat(sprintf("     = e^(%.4f × √%.4f)\n", sigma, dt))
      cat(sprintf("     = e^(%.4f × %.4f)\n", sigma, sqrt(dt)))
      cat(sprintf("     = e^%.4f\n", sigma * sqrt(dt)))
      cat(sprintf("     = %.4f\n\n", u))
      
      cat("   d = e^(-σ√h)\n")
      cat(sprintf("     = e^(-%.4f × √%.4f)\n", sigma, dt))
      cat(sprintf("     = e^(-%.4f × %.4f)\n", sigma, sqrt(dt)))
      cat(sprintf("     = e^%.4f\n", -sigma * sqrt(dt)))
      cat(sprintf("     = %.4f\n\n", d))
    } else {
      cat(sprintf("(a) We have h = T/n = %.4f/%d = %.4f ", T, n, dt))
      if (dt < 1) {
        cat(sprintf("(i.e., %.0f/12 years", dt * 12))
      }
      cat(").\n\n")
      cat(sprintf("Given: u = %.3f, d = %.3f\n\n", u, d))
    }
    
    # Risk-neutral probabilities
    cat("The risk-neutral probabilities are:\n\n")
    
    # Risk-neutral probability with FULL arithmetic
    cat("   π = [e^((ρ-δ)×h) - d] / (u - d)\n")
    cat(sprintf("     = [e^((%.4f-%.4f)×%.4f) - %.3f] / (%.3f - %.3f)\n", 
                r, delta, dt, d, u, d))
    
    # Show the exponential calculation
    exp_val <- exp((r - delta) * dt)
    cat(sprintf("     = [%.4f - %.3f] / %.3f\n", exp_val, d, u - d))
    cat(sprintf("     = %.4f / %.3f\n", exp_val - d, u - d))
    cat(sprintf("     ≈ %.4f", p))
    cat(sprintf("  ⇒  1 - π ≈ %.4f\n\n", 1 - p))
    
    # Underlying asset prices with EXPLICIT calculations
    cat("The underlying are:\n\n")
    for (i in seq_along(final_prices)) {
      num_up <- i - 1
      num_down <- n - num_up
      
      if (num_up == n && num_down == 0) {
        # All ups: u^n × S
        cat(sprintf("   u^%d × S = %.3f^%d · %.0f ≈ %.2f", 
                    n, u, n, S, final_prices[i]))
      } else if (num_up == 0 && num_down == n) {
        # All downs: d^n × S
        cat(sprintf("   d^%d × S = %.3f^%d · %.0f ≈ %.2f", 
                    n, d, n, S, final_prices[i]))
      } else {
        # Mixed: u^i × d^j × S
        u_part <- u^num_up
        d_part <- d^num_down
        cat(sprintf("   u^%d × d^%d × S = %.3f · %.3f · %.0f ≈ %.2f", 
                    num_up, num_down, u_part, d_part, S, final_prices[i]))
      }
      
      # Add semicolon separator except for last one
      if (i < length(final_prices)) {
        cat(" ; ")
      } else {
        cat("\n\n")
      }
    }
    
    # Option payoffs
    cat(sprintf("Therefore the European %s with K = %.0f and maturity T = %.0f months has the following final payoffs:\n\n", 
                type, K, T * 12))
    
    for (i in seq_along(option_payoffs)) {
      node_time <- n
      
      if (type == "call") {
        cat(sprintf("   c%d%s = max(0; %.2f - %.0f) = %.2f", 
                    node_time, 
                    c("⁰", "¹", "²", "³", "⁴")[i],
                    final_prices[i], K, option_payoffs[i]))
      } else {
        cat(sprintf("   p%d%s = max(0; %.0f - %.2f) = %.2f", 
                    node_time,
                    c("⁰", "¹", "²", "³", "⁴")[i],
                    K, final_prices[i], option_payoffs[i]))
      }
      
      if (i < length(option_payoffs)) {
        cat(" ; ")
      } else {
        cat("\n\n")
      }
    }
    
    # Prices at t=1 (if n >= 2) with FULL arithmetic
    if (n >= 2 && !is.null(prices_t1)) {
      symbol <- ifelse(type == "call", "c", "p")
      cat(sprintf("The possible no-arbitrage prices of the European %s at time 1 are:\n\n", type))
      
      for (i in seq_along(prices_t1)) {
        val_up <- option_payoffs[i + 1]
        val_down <- option_payoffs[i]
        
        # Calculate the intermediate value for display
        weighted_sum <- p * val_up + (1-p) * val_down
        intermediate_price <- weighted_sum * discount_factor
        
        cat(sprintf("   %s%d%s = (%.4f · %.2f + %.4f · %.2f) × e^(-%.4f×%.4f)\n", 
                    symbol, 1, c("⁰", "¹", "²", "³")[i],
                    p, val_up, 1-p, val_down, r, dt))
        
        # Show intermediate calculation
        cat(sprintf("      = %.4f × e^(-%.4f)\n", weighted_sum, r * dt))
        cat(sprintf("      = %.4f × %.4f\n", weighted_sum, discount_factor))
        cat(sprintf("      ≈ %.3f\n\n", intermediate_price))
      }
    }
    
    # Final price calculation
    if (n == 1) {
      symbol <- ifelse(type == "call", "c", "p")
      val_up <- option_payoffs[2]
      val_down <- option_payoffs[1]
      
      cat("This drives us to the initial price:\n\n")
      cat(sprintf("   %s₀ = (%.4f · %.2f + %.4f · %.2f) × e^(-%.4f×%.4f)\n", 
                  symbol, p, val_up, 1-p, val_down, r, dt))
      
      weighted_sum <- p * val_up + (1-p) * val_down
      cat(sprintf("      = %.4f × %.4f\n", weighted_sum, discount_factor))
      cat(sprintf("      ≈ %.3f\n\n", result_price))
      
    } else if (n >= 2) {
      symbol <- ifelse(type == "call", "c", "p")
      val_up <- prices_t1[length(prices_t1)]
      val_down <- prices_t1[1]
      
      cat("This drives us to the initial price:\n\n")
      cat(sprintf("   %s₀ = (%.4f · %.3f + %.4f · %.3f) × e^(-%.4f×%.4f)\n", 
                  symbol, p, val_up, 1-p, val_down, r, dt))
      
      weighted_sum <- p * val_up + (1-p) * val_down
      cat(sprintf("      = %.4f × %.4f\n", weighted_sum, discount_factor))
      cat(sprintf("      ≈ %.3f\n", result_price))
    }
    
    cat("\n")
  }
  
  return(result_price)
}

#' @title Put-Call Parity (Verification & Calculation)
#' 
#' @description
#' Verifies if Put-Call Parity holds (Arbitrage Check) or calculates a missing 
#' value (European Option Pricing) if one inputs is NULL.
#'
#' @details
#' **Parity Relationship:**
#' \deqn{C - P = S - K e^{-rT}}
#'
#' **Arbitrage Conditions:**
#' - If \eqn{C - P > S - Ke^{-rT}}: Call is overvalued. Sell C, Buy P+S, Borrow Ke^{-rT}
#' - If \eqn{C - P < S - Ke^{-rT}}: Put is overvalued. Sell P, Buy C, Short S, Lend Ke^{-rT}
#'
#' **Use Cases:**
#' - Pricing synthetic options
#' - Identifying arbitrage opportunities
#' - Validating option market data
#'
#' @param S Numeric. Stock price.
#' @param K Numeric. Strike price.
#' @param r Numeric. Risk-free rate.
#' @param T Numeric. Time to maturity.
#' @param call_price Numeric. Call price (optional).
#' @param put_price Numeric. Put price (optional).
#' @param verbose Logical. If TRUE, displays detailed analysis.
#'
#' @return 
#' - If one price missing: Reimputed price (Numeric)
#' - If both prices present: List with arbitrage check logic
#'
#' @seealso 
#' \code{\link{binomial_option_price}}
#' \code{\link{put_call_parity_solver}} simplified version
#'
#' @examples
#' # Calculate Put Price
#' put_call_parity(S=100, K=100, r=0.05, T=1, call_price=10)
#'
#' # Check for Arbitrage
#' put_call_parity(S=100, K=100, r=0.05, T=1, call_price=10, put_price=3, verbose=TRUE)
#'
#' @export
put_call_parity <- function(S, K, r, T, call_price = NULL, put_price = NULL, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  
  discount_K <- K * exp(-r * T)
  parity_diff <- S - discount_K
  
  if (!is.null(call_price) && !is.null(put_price)) {
    # Check parity
    actual_diff <- call_price - put_price
    arbitrage <- abs(actual_diff - parity_diff) > 1e-6
    
    if (verbose) {
      format_result(
        formula_name = "Put-Call Parity (Verification)",
        formula_text = "C - P = S - K × e^(-r×T)",
        inputs = list(
          "S (Stock price)" = S,
          "K (Strike)" = K,
          "r (Risk-free rate)" = r,
          "T (Time to maturity)" = T,
          "C (Call price)" = call_price,
          "P (Put price)" = put_price
        ),
        result = actual_diff,
        additional_info = list(
          "Expected (C - P)" = parity_diff,
          "Actual (C - P)" = actual_diff,
          "Arbitrage opportunity?" = ifelse(arbitrage, "YES", "NO")
        ),
        interpretation = ifelse(arbitrage,
                                "Put-call parity is VIOLATED. Arbitrage opportunity exists!",
                                "Put-call parity holds. No arbitrage.")
      )
    }
    
    return(list(arbitrage = arbitrage, expected_diff = parity_diff, actual_diff = actual_diff))
    
  } else if (!is.null(call_price)) {
    # Calculate put price: P = C - (S - Ke^-rT)
    put_price_calc <- call_price - parity_diff
    
    if (verbose) {
      format_result(
        formula_name = "Put-Call Parity (Calculate Put Price)",
        formula_text = "P = C - S + K × e^(-r×T)",
        inputs = list(
          "C (Call price)" = call_price,
          "S (Stock price)" = S,
          "K (Strike)" = K,
          "r (Risk-free rate)" = r,
          "T (Time to maturity)" = T
        ),
        result = put_price_calc,
        interpretation = sprintf("Given call price %.4f, the put price must be %.4f", call_price, put_price_calc)
      )
    }
    
    return(put_price_calc)
    
  } else if (!is.null(put_price)) {
    # Calculate call price: C = P + (S - Ke^-rT)
    call_price_calc <- put_price + parity_diff
    
    if (verbose) {
      format_result(
        formula_name = "Put-Call Parity (Calculate Call Price)",
        formula_text = "C = P + S - K × e^(-r×T)",
        inputs = list(
          "P (Put price)" = put_price,
          "S (Stock price)" = S,
          "K (Strike)" = K,
          "r (Risk-free rate)" = r,
          "T (Time to maturity)" = T
        ),
        result = call_price_calc,
        interpretation = sprintf("Given put price %.4f, the call price must be %.4f", put_price, call_price_calc)
      )
    }
    
    return(call_price_calc)
    
  } else {
    stop("Must provide at least one of call_price or put_price. If finding S or K, use solver.")
  }
}
