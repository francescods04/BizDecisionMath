#' Recall a Formula
#'
#' This function prints a requested formula or concept to the console.
#'
#' @param name A string specifying the name of the formula.
#'             Matches are performed using loose keywords.
#'             Run `show_formula()` without arguments to see the list of available formulas.
#'
#' @return Prints the formula text to the console.
#' @export
#'
#' @examples
#' show_formula("compound interest")
#' show_formula("gordon")
show_formula <- function(name) {
  formulas_list <- list(

    # ========== INTEREST & ACCUMULATION ==========
    simple_interest = "
**SIMPLE INTEREST**

Final value at time t:
**M = C * (1 + i*t)**

Where:
- C = Principal (initial amount)
- i = Annual interest rate
- t = Time in years
- M = Final value (maturity value)

Interest earned:
**I = M - C = C * i * t**

Inverse formulas:
- C = M / (1 + i*t)
- i = (1/t) * (M/C - 1)
- t = (1/i) * (M/C - 1)
",

    compound_interest = "
**COMPOUND INTEREST**

Final value at time t:
**M = C * (1 + i)^t**

Where:
- C = Principal
- i = Annual interest rate
- t = Time in years
- M = Final value

Interest earned:
**I = M - C = C * [(1 + i)^t - 1]**

Inverse formulas:
- C = M * (1 + i)^(-t)
- i = (M/C)^(1/t) - 1
- t = ln(M/C) / ln(1 + i)
",

    continuous_interest = "
**CONTINUOUS INTEREST**

Final value:
**M = C * e^(delta * t)**

Present value:
**A = S * e^(-delta * t)**

Where:
- delta = ln(1 + i) = continuous interest rate
- e^delta = 1 + i (relationship to annual rate)

Conversion:
- delta = ln(1 + i)
- i = e^delta - 1
",

    simple_discount = "
**SIMPLE DISCOUNT**

Present value:
**A = S / (1 + i*t) = S * (1 + i*t)^(-1)**

Discount amount:
**D = S - A = S * [1 - (1 + i*t)^(-1)]**

Where:
- S = Nominal (future) value
- A = Present value
- i = Interest rate
- t = Time
",

    compound_discount = "
**COMPOUND DISCOUNT**

Present value:
**A = S * (1 + i)^(-t)**

Discount amount:
**D = S - A = S * [1 - (1 + i)^(-t)]**

Where:
- phi(t) = (1 + i)^(-t) is the discount factor
",

    # ========== RATE CONVERSIONS ==========
    equivalent_rates = "
**EQUIVALENT INTEREST RATES**

Financial equivalence between annual rate i and periodic rate i_m:
**(1 + i)^t = (1 + i_m)^(t*m)**

Conversion formulas:
- **i = (1 + i_m)^m - 1** (periodic to annual)
- **i_m = (1 + i)^(1/m) - 1** (annual to periodic)

Common periods:
- m = 2: semi-annual
- m = 4: quarterly
- m = 12: monthly
- m = 365: daily
",

    nominal_rate = "
**NOMINAL INTEREST RATE**

The annual nominal rate j_m:
**j_m = m * i_m**

Relationship to effective annual rate:
**(1 + j_m/m)^m = 1 + i**

Note: j_m <= i always (nominal is always less than or equal to effective)
",

    # ========== ANNUITIES ==========
    ordinary_annuity_pv = "
**PRESENT VALUE OF ORDINARY ANNUITY**

**V_0 = R * [1 - (1 + i)^(-n)] / i**

Or using financial symbol:
**V_0 = R * a_n|i**

Where:
- R = Constant periodic payment
- i = Interest rate per period
- n = Number of periods
- a_n|i = [1 - (1 + i)^(-n)] / i
",

    ordinary_annuity_fv = "
**FUTURE VALUE OF ORDINARY ANNUITY**

**V_n = R * [(1 + i)^n - 1] / i**

Or using financial symbol:
**V_n = R * s_n|i**

Where:
- s_n|i = [(1 + i)^n - 1] / i

Relationship:
V_n = V_0 * (1 + i)^n
",

    due_annuity = "
**DUE ANNUITY (Annuity in Advance)**

Payments at BEGINNING of each period.

Present value:
**V_0 = R * [1 - (1 + i)^(-n)] / i * (1 + i)**
**V_0 = R * a_n|i * (1 + i)**

Future value:
**V_n = R * [(1 + i)^n - 1] / i * (1 + i)**
**V_n = R * s_n|i * (1 + i)**
",

    deferred_annuity = "
**DEFERRED ANNUITY**

Annuity deferred by j periods.

Present value:
**V_0 = R * a_n|i * (1 + i)^(-j)**

The factor (1 + i)^(-j) discounts the annuity back j periods.
",

    growing_annuity = "
**GROWING ANNUITY (Geometric Progression)**

Cash flows: R_k = R * (1 + g)^(k-1)

Present value (i > g):
**V_0 = [R / (i - g)] * [1 - ((1 + g)/(1 + i))^n]**

Future value:
**V_n = [R / (i - g)] * [(1 + i)^n - (1 + g)^n]**

Where g = growth rate of payments.
",

    # ========== PERPETUITIES ==========
    perpetuity = "
**PERPETUITY (Ordinary)**

Infinite stream of constant payments.

Present value:
**V_0 = R / i**

Deferred perpetuity (first payment at time j+1):
**V_0 = (R / i) * (1 + i)^(-j)**
",

    growing_perpetuity = "
**GROWING PERPETUITY (Gordon Growth Model)**

Infinite stream of payments growing at rate g.

Present value (i > g):
**V_0 = R / (i - g)**

This is also the Gordon-Shapiro formula for stock valuation:
**P_0 = D_1 / (r - g)**

Where D_1 = next dividend, r = required return, g = growth rate.
",

    # ========== TERM STRUCTURE ==========
    spot_rates = "
**SPOT TERM STRUCTURE**

The sequence {r(0,k)} for k = 1, 2, ..., n represents spot rates.

- r(0, k) = annual rate for investment from 0 to k

Present value with term structure:
**V_0 = Sum of [R_k / (1 + r(0,k))^k]**
",

    forward_rates = "
**FORWARD RATES**

No-arbitrage condition:
**(1 + r(0,s))^s * (1 + r(s,k))^(k-s) = (1 + r(0,k))^k**

Forward rate formula:
**r(s,k) = [(1 + r(0,k))^k / (1 + r(0,s))^s]^(1/(k-s)) - 1**

If term structure is flat (all r(0,k) = i):
All forward rates = i
",

    # ========== INVESTMENT ANALYSIS ==========
    npv = "
**NET PRESENT VALUE (NPV)**

**NPV = Sum of [a_k / (1 + i)^(t_k)]** for k = 0 to n

Decision rule:
- NPV > 0: Accept (creates value)
- NPV = 0: Indifferent
- NPV < 0: Reject

Where a_k = cash flow at time t_k, i = discount rate.
",

    irr = "
**INTERNAL RATE OF RETURN (IRR)**

IRR is the rate x* such that NPV(x*) = 0:
**Sum of [a_k / (1 + x*)^(t_k)] = 0**

Decision rule:
- IRR > cost of capital: Accept
- IRR < cost of capital: Reject

Relationship: NPV(i) > 0 <==> i < IRR
",

    profitability_index = "
**PROFITABILITY INDEX (PI)**

**PI = [Sum of a_k * (1+i)^(-k) for k=1 to n] / |a_0|**

Or equivalently:
**PI = (NPV + |a_0|) / |a_0| = NPV/|a_0| + 1**

Decision rule:
- PI > 1: Accept
- PI = 1: Indifferent
- PI < 1: Reject
",

    # ========== OPTIONS ==========
    call_payoff = "
**CALL OPTION PAYOFF**

At maturity T:
**c_T = max(S_T - K, 0)**

- If S_T >= K: Exercise, payoff = S_T - K
- If S_T < K: Don't exercise, payoff = 0

Where S_T = stock price at T, K = strike price.
",

    put_payoff = "
**PUT OPTION PAYOFF**

At maturity T:
**p_T = max(K - S_T, 0)**

- If S_T < K: Exercise, payoff = K - S_T
- If S_T >= K: Don't exercise, payoff = 0
",

    put_call_parity = "
**PUT-CALL PARITY**

For European options on non-dividend paying stock:
**c_0 - p_0 = S_0 - K * (1 + r)^(-T)**

With continuous compounding:
**c_0 - p_0 = S_0 - K * e^(-r*T)**

Derived formulas:
- c_0 = p_0 + S_0 - K/(1+r)^T
- p_0 = c_0 - S_0 + K/(1+r)^T
",

    binomial_option = "
**BINOMIAL OPTION PRICING (1-Period)**

Risk-neutral probability:
**pi = (1 + r - d) / (u - d)**

Call price:
**c_0 = [c_u * pi + c_d * (1 - pi)] / (1 + r)**

Put price:
**p_0 = [p_u * pi + p_d * (1 - pi)] / (1 + r)**

No-arbitrage condition: d < 1 + r < u
",

    replicating_portfolio = "
**REPLICATING PORTFOLIO**

Portfolio (alpha, beta) replicates option payoff.

**beta = (c_u - c_d) / (S_u - S_d)**    (Delta)

**alpha = (c_d * S_u - c_u * S_d) / [(1 + r) * (S_u - S_d)]**

Option price:
**c_0 = alpha + beta * S_0**
",

    crr_model = "
**CRR CALIBRATION MODEL**

Cox-Ross-Rubinstein parameters:
**u = e^(sigma * sqrt(dt))**
**d = e^(-sigma * sqrt(dt))**

Where:
- sigma = volatility
- dt = T/n (time step)
- Note: u * d = 1

CRR synthetic probability:
**pi = (e^(delta*dt) - d) / (u - d)**
",

    # ========== DECOMPOSABILITY ==========
    decomposability = "
**DECOMPOSABILITY PROPERTY**

An accumulation factor f is decomposable if:
**f(s) * f(t - s) = f(t)** for all t, s >= 0

Key result:
- Compound interest IS decomposable
- Simple interest is NOT decomposable

This means: only in compound interest does the intermediate investment date not matter.
"
  )

  if (missing(name)) {
    cat("Please provide the name of the formula. Available formulas:\n\n")
    cat("=== INTEREST & TIME VALUE ===\n")
    cat("1.  simple interest\n")
    cat("2.  compound interest\n")
    cat("3.  continuous interest\n")
    cat("4.  simple discount\n")
    cat("5.  compound discount\n")
    cat("6.  equivalent rates\n")
    cat("7.  nominal rate\n")
    cat("\n=== ANNUITIES ===\n")
    cat("8.  ordinary annuity pv\n")
    cat("9.  ordinary annuity fv\n")
    cat("10. due annuity\n")
    cat("11. deferred annuity\n")
    cat("12. growing annuity\n")
    cat("\n=== PERPETUITIES ===\n")
    cat("13. perpetuity\n")
    cat("14. growing perpetuity (gordon)\n")
    cat("\n=== TERM STRUCTURE ===\n")
    cat("15. spot rates\n")
    cat("16. forward rates\n")
    cat("\n=== INVESTMENT ANALYSIS ===\n")
    cat("17. npv\n")
    cat("18. irr\n")
    cat("19. profitability index\n")
    cat("\n=== OPTIONS ===\n")
    cat("20. call payoff\n")
    cat("21. put payoff\n")
    cat("22. put call parity\n")
    cat("23. binomial option\n")
    cat("24. replicating portfolio\n")
    cat("25. crr model\n")
    cat("\n=== OTHER ===\n")
    cat("26. decomposability\n")
    return(invisible(NULL))
  }

  name_clean <- tolower(trimws(name))
  key <- NULL

  # Matching logic
  if (grepl("simple", name_clean) && grepl("interest", name_clean)) {
    key <- "simple_interest"
  } else if (grepl("compound", name_clean) && grepl("interest", name_clean)) {
    key <- "compound_interest"
  } else if (grepl("continuous", name_clean)) {
    key <- "continuous_interest"
  } else if (grepl("simple", name_clean) && grepl("discount", name_clean)) {
    key <- "simple_discount"
  } else if (grepl("compound", name_clean) && grepl("discount", name_clean)) {
    key <- "compound_discount"
  } else if (grepl("equivalent", name_clean) || (grepl("periodic", name_clean) && grepl("rate", name_clean))) {
    key <- "equivalent_rates"
  } else if (grepl("nominal", name_clean)) {
    key <- "nominal_rate"
  } else if (grepl("ordinary", name_clean) && grepl("pv", name_clean)) {
    key <- "ordinary_annuity_pv"
  } else if (grepl("ordinary", name_clean) && grepl("fv", name_clean)) {
    key <- "ordinary_annuity_fv"
  } else if (grepl("due", name_clean) && grepl("annuity", name_clean)) {
    key <- "due_annuity"
  } else if (grepl("deferred", name_clean)) {
    key <- "deferred_annuity"
  } else if (grepl("growing", name_clean) && grepl("annuity", name_clean)) {
    key <- "growing_annuity"
  } else if (grepl("perpetuity", name_clean) && grepl("growing", name_clean)) {
    key <- "growing_perpetuity"
  } else if (grepl("gordon", name_clean)) {
    key <- "growing_perpetuity"
  } else if (grepl("perpetuity", name_clean)) {
    key <- "perpetuity"
  } else if (grepl("spot", name_clean)) {
    key <- "spot_rates"
  } else if (grepl("forward", name_clean)) {
    key <- "forward_rates"
  } else if (grepl("npv", name_clean) || grepl("net present", name_clean)) {
    key <- "npv"
  } else if (grepl("irr", name_clean) || grepl("internal rate", name_clean)) {
    key <- "irr"
  } else if (grepl("profitability", name_clean) || grepl("\\bpi\\b", name_clean)) {
    key <- "profitability_index"
  } else if (grepl("call", name_clean) && grepl("payoff", name_clean)) {
    key <- "call_payoff"
  } else if (grepl("put", name_clean) && grepl("payoff", name_clean)) {
    key <- "put_payoff"
  } else if (grepl("put", name_clean) && grepl("call", name_clean) && grepl("parity", name_clean)) {
    key <- "put_call_parity"
  } else if (grepl("parity", name_clean)) {
    key <- "put_call_parity"
  } else if (grepl("binomial", name_clean)) {
    key <- "binomial_option"
  } else if (grepl("replicating", name_clean) || grepl("delta", name_clean)) {
    key <- "replicating_portfolio"
  } else if (grepl("crr", name_clean) || grepl("calibration", name_clean)) {
    key <- "crr_model"
  } else if (grepl("decompos", name_clean)) {
    key <- "decomposability"
  }

  if (is.null(key)) {
    cat("Formula not found for: '", name, "'\n", sep = "")
    cat("Try keywords like 'compound interest', 'npv', 'gordon', 'binomial', etc.\n")
    return(invisible(NULL))
  }

  cat(formulas_list[[key]])
}
