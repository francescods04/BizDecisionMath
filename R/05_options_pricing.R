#' CRR Calibration
#' 
#' Calibrates the Cox-Ross-Rubinstein binomial model parameters.
#' u = exp(sigma * sqrt(delta_t))
#' d = exp(-sigma * sqrt(delta_t))
#' 
#' @param sigma Numeric. Volatility of the underlying asset.
#' @param T Numeric. Time to maturity (years).
#' @param n Integer. Number of steps.
#' @param r Numeric. Risk-free annual interest rate.
#' @return List containing u, d, p (probability up), and delta_t.
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

#' Binomial Option Pricing
#' 
#' Calculates Option Price using Binomial Tree (European).
#' 
#' @param S Numeric. Current stock price.
#' @param K Numeric. Strike price.
#' @param r Numeric. Risk-free interest rate (continuous, per annum).
#' @param T Numeric. Time to maturity (in years or consistent with r).
#' @param type Character. "call" or "put".
#' @param n Integer. Number of steps (default 1).
#' @param sigma Numeric. Volatility (optional, for CRR calibration).
#' @param u Numeric. Up factor (optional, can be provided directly).
#' @param d Numeric. Down factor (optional, can be provided directly).
#' @param delta Numeric. Continuous dividend yield (default 0).
#' @param verbose Logical. If TRUE, displays exam-style step-by-step solution. Default FALSE.
#' @return Numeric option price.
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
      # When step==n-1, we're calculating prices at t=1 (moving from t=n to t=n-1)
      # For n=2: when step==2, we're calculating t=1 from t=2
      # We need to store BEFORE assigning to option_values
      if (verbose && step == n && n >= 2) {
        # This is the first backward step: from t=n to t=n-1
        # Store these as t=(n-1) prices
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
      # Use superscript notation: c₂⁰, c₂¹, c₂²
      node_time <- n
      node_position <- i - 1
      
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
      # prices_t1 has length n after backward induction from n to 1
      # For n=2: prices_t1 has indices 1 and 2 representing V_0^(1) and V_1^(1)
      val_up <- prices_t1[length(prices_t1)]  # Last element (upper node at t=1)
      val_down <- prices_t1[1]  # First element (lower node at t=1)
      
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

#' Put-Call Parity
#' 
#' Verifies or calculates prices using put-call parity.
#' C - P = S - K × e^(-r×T)
#' 
#' @param S Numeric. Current stock price.
#' @param K Numeric. Strike price.
#' @param r Numeric. Risk-free rate.
#' @param T Numeric. Time to maturity.
#' @param call_price Numeric. Call option price (if known).
#' @param put_price Numeric. Put option price (if known).
#' @param verbose Logical. If TRUE, displays formula and calculation. Default FALSE.
#' @return Numeric or List. If both call and put prices are provided, returns the arbitrage check. Otherwise returns the missing price.
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
    # Calculate put price
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
    # Calculate call price
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
    stop("Must provide at least one of call_price or put_price")
  }
}
