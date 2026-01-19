#' Recall a Proof
#'
#' This function prints a requested proof or formula derivation to the console.
#'
#' @param name A string specifying the name of the proof. 
#'             Matches are performed using loose keywords (e.g., "annual rate", "put call").
#'             Run `show_proof()` without arguments to see the list of available proofs.
#'
#' @return Prints the proof text to the console.
#' @export
#'
#' @examples
#' show_proof("put call")
show_proof <- function(name) {
  # Define the proofs list inside the function or in the package environment
  # For simplicity, defining it here to ensure it's self-contained
  proofs_list <- list(
    annual_compounded = "
**RELATIONSHIP BETWEEN ANNUAL RATE AND RATE COMPOUNDED K TIMES A YEAR**

**Definition:**
Interest rates i (annual) and i_m (periodic, m times a year) are financially equivalent in compound interest when they produce the same final value for the same time t.

**(1 + i)^t = (1 + i_m)^(t*m)**

Where:
- i is the effective annual interest rate
- i_m is the periodic interest rate (e.g., monthly if m=12)
- m is the frequency of compounding per year

**Derivation:**
Taking the t-th root of both sides:
1 + i = (1 + i_m)^m

From this, we get the conversion formulas:
1. **i = (1 + i_m)^m - 1**
2. **i_m = (1 + i)^(1/m) - 1**

**Nominal Rate:**
The annual nominal interest rate j_m is defined as:
j_m = m * i_m

Substituting i_m = j_m / m into the equivalence relation:
**(1 + i) = (1 + j_m / m)^m**
",

    spot_forward = "
**SPOT RATE, FORWARD RATES AND THEIR RELATIONSHIP**

**Assumption:** No Arbitrage.

Let:
- **r(0, k)** be the spot rate for maturity k (investing from now until time k).
- **r(s, k)** be the forward rate for the period between s and k (future loan/investment).

**No-Arbitrage Condition:**
An investor must be indifferent between:
A) Investing directly for k years at the spot rate r(0, k).
B) Investing for s years at r(0, s), and then reinvesting the proceeds for the remaining (k-s) years at the forward rate r(s, k).

**Equation:**
**(1 + r(0, k))^k = (1 + r(0, s))^s * (1 + r(s, k))^(k - s)**

**Proof:**
If the left side > right side, everyone would borrow short-term + forward to invest long-term (arbitrage).
If the left side < right side, everyone would borrow long-term to invest short-term + forward (arbitrage).
Thus, they must be equal.

**Forward Rate Formula:**
Solving for r(s, k):
**(1 + r(s, k))^(k - s) = (1 + r(0, k))^k / (1 + r(0, s))^s**

**r(s, k) = [ (1 + r(0, k))^k / (1 + r(0, s))^s ]^(1 / (k - s)) - 1**
",

    growing_annuity = "
**PRESENT VALUE OF AN ORDINARY ANNUITY WITH INSTALMENTS GROWING AT RATE g**

**Problem:**
Calculate the Present Value (V_0) of an annuity where cash flows grow geometrically:
R_k = R * (1 + g)^(k-1) for k = 1 to n.
Interest rate i, growth rate g.
**Assumption:** i > g.

**Formula:**
**V_0 = [ R / (i - g) ] * [ 1 - ((1 + g) / (1 + i))^n ]**

**Proof:**
The definition of PV is the sum of discounted cash flows:
V_0 = Sum_{k=1 to n} [ R_k * (1 + i)^(-k) ]

Substitute R_k = R * (1 + g)^(k-1):
V_0 = Sum_{k=1 to n} [ R * (1 + g)^(k-1) * (1 + i)^(-k) ]

Factor out R / (1 + i):
V_0 = [ R / (1 + i) ] * Sum_{k=1 to n} [ (1 + g)^(k-1) * (1 + i)^(-(k-1)) ]
V_0 = [ R / (1 + i) ] * Sum_{j=0 to n-1} [ ((1 + g) / (1 + i))^j ]

This is the sum of a geometric series Sum_{j=0 to N-1} q^j = (1 - q^N) / (1 - q), where:
- q = (1 + g) / (1 + i)
- N = n

V_0 = [ R / (1 + i) ] * [ (1 - q^n) / (1 - q) ]

Simplify the denominator (1 - q):
1 - (1 + g)/(1 + i) = [ (1 + i) - (1 + g) ] / (1 + i) = (i - g) / (1 + i)

Substitute back into V_0:
V_0 = [ R / (1 + i) ] * [ (1 - q^n) / ( (i - g) / (1 + i) ) ]

The term (1 + i) cancels out:
**V_0 = [ R / (i - g) ] * [ 1 - ((1 + g) / (1 + i))^n ]**
Q.E.D.
",

    npv_irr = "
**EQUIVALENCE BETWEEN NPV AND IRR RULES**

**Proposition:**
For a standard investment project (initial outflow, subsequent inflows), the NPV rule and IRR rule are equivalent.
Specifically: **NPV(i) > 0  <==>  i < IRR**
(assuming profitable investment)

**Definitions:**
- **NPV(x):** Net Present Value calculated at rate x.
- **IRR:** The rate x* such that NPV(x*) = 0.
- **i:** The opportunity cost of capital (market interest rate).

**Proof:**

1. **(=>) Assume i < IRR.**
   Since the project is standard (costs first, then benefits), the function NPV(x) is **strictly decreasing** with respect to x.
   We know NPV(IRR) = 0.
   Because i < IRR and the function is decreasing, it must be that:
   NPV(i) > NPV(IRR)
   **NPV(i) > 0**
   
2. **(<=) Assume NPV(i) > 0.**
   We know NPV(IRR) = 0.
   Since NPV(i) > 0, the point i acts on the curve 'higher' than zero.
   Since the curve is strictly decreasing, to trigger a positive value, the rate i must be to the left of the root (IRR).
   Therefore:
   **i < IRR**

**Conclusion:**
The condition for accepting a project using NPV (NPV > 0) is mathematically identical to the condition using IRR (IRR > i).
",

    put_call = "
**THE PUT-CALL PARITY**

**Proposition:**
For European options on a non-dividend paying stock:
**c_0 - p_0 = S_0 - K * (1 + r)^(-T)**

Where:
- c_0, p_0: Prices of Call and Put at t=0
- S_0: Stock price at t=0
- K: Strike price
- T: Maturity
- r: Risk-free rate

**Proof (No-Arbitrage Argument):**

Construct two portfolios at t=0:

**Portfolio A:** Buy 1 Call + Sell 1 Put
- Value at t=0: c_0 - p_0
- Payoff at T:
  - If S_T >= K: (S_T - K) - 0 = S_T - K
  - If S_T < K:  0 - (K - S_T) = S_T - K
- **Result:** Payoff is always **S_T - K**.

**Portfolio B:** Buy 1 Stock + Borrow PV(K)
- Buy Stock S_0.
- Borrow K * (1 + r)^(-T) (Sell zero-coupon bond).
- Value at t=0: S_0 - K * (1 + r)^(-T)
- Payoff at T:
  - Stock is worth S_T.
  - Repay loan of K (face value).
- **Result:** Payoff is **S_T - K**.

**Conclusion:**
Since Portfolio A and Portfolio B have the exact same payoff in all future states, they **must** have the same value today (Law of One Price).

**c_0 - p_0 = S_0 - K * (1 + r)^(-T)**
Q.E.D.
",

    replicating_portfolio = "
**REPLICATING PORTFOLIO OF A CALL OPTION IN A BINOMIAL MARKET**

**Goal:** Determine the composition of a portfolio (alpha, beta) that perfectly replicates the call option payoff at t=1.

**Model (1-Period):**
- Stock price: S (today) -> S_u (up) or S_d (down)
- Call payoff: c_u (if up) or c_d (if down)
- Bond: Grows by factor (1+r)

**Portfolio:**
- **alpha:** Amount invested in risk-free bonds.
- **beta:** Number of shares of stock.

**System of Equations (at t=1):**
1. alpha * (1 + r) + beta * S_u = c_u  (Up state)
2. alpha * (1 + r) + beta * S_d = c_d  (Down state)

**Solving for beta:**
Subtract (2) from (1):
beta * S_u - beta * S_d = c_u - c_d
beta * (S_u - S_d) = c_u - c_d

**beta = (c_u - c_d) / (S_u - S_d)**
(This is the Delta of the option).

**Solving for alpha:**
From (2): alpha * (1 + r) = c_d - beta * S_d
alpha = (c_d - beta * S_d) / (1 + r)
Substitute beta:
alpha = [ c_d - ((c_u - c_d)/(S_u - S_d)) * S_d ] / (1 + r)

Simplify numerator:
(c_d(S_u - S_d) - S_d(c_u - c_d)) / (S_u - S_d)
= (c_d*S_u - c_d*S_d - c_u*S_d + c_d*S_d) / (S_u - S_d)
= (c_d*S_u - c_u*S_d) / (S_u - S_d)

**alpha = (c_d * S_u - c_u * S_d) / [ (1 + r) * (S_u - S_d) ]**

**Option Price:**
By No-Arbitrage, Call Price c_0 = Cost of Portfolio:
**c_0 = alpha + beta * S_0**
",

    risk_neutral = "
**RISK NEUTRAL PROBABILITY**

**Definition:**
The synthetic probability 'pi' under which the expected return of the risky asset equals the risk-free rate r.
This allows us to price options as the discounted expected payoff under expectations E_pi.

**Derivation:**
We require that the expected stock price at t=1 equals the future value of the stock price at t=0 growing at rate r.

**E_pi[S_1] = S_0 * (1 + r)**

In a binomial model (Up/Down):
- Up probability: pi
- Down probability: 1 - pi
- S_u = u * S_0
- S_d = d * S_0

Substitute into Expectation:
pi * S_u + (1 - pi) * S_d = S_0 * (1 + r)

Substitute u/d factors:
pi * (u * S_0) + (1 - pi) * (d * S_0) = S_0 * (1 + r)

Divide everything by S_0 (assuming S_0 > 0):
pi * u + d - pi * d = 1 + r

Group terms with pi:
pi * (u - d) + d = 1 + r
pi * (u - d) = 1 + r - d

**pi = (1 + r - d) / (u - d)**

This 'pi' is the risk-neutral probability. 
Note: For no arbitrage, d < 1+r < u, which ensures 0 < pi < 1.
"
  )

  if (missing(name)) {
    cat("Please provide the name of the proof. Available proofs:\n\n")
    cat("1. RELATIONSHIP BETWEEN ANNUAL RATE AND RATE COMPOUNDED K TIMES A YEAR\n")
    cat("2. SPOT RATE, FORWARD RATES AND THEIR RELATIONSHIP\n")
    cat("3. PRESENT VALUE OF AN ORDINARY ANNUITY WITH INSTALMENTS GROWING AT RATE g\n")
    cat("4. EQUIVALENCE BETWEEN NPV AND IRR RULES\n")
    cat("5. THE PUT-CALL PARITY\n")
    cat("6. REPLICATING PORTFOLIO OF A CALL OPTION IN A BINOMIAL MARKET\n")
    cat("7. RISK NEUTRAL PROBABILITY\n")
    return(invisible(NULL))
  }

  name_clean <- tolower(trimws(name))
  
  key <- NULL
  
  # Loose matching logic
  if (grepl("annual", name_clean) && grepl("compounded", name_clean)) {
    key <- "annual_compounded"
  } else if (grepl("spot", name_clean) && grepl("forward", name_clean)) {
    key <- "spot_forward"
  } else if (grepl("annuity", name_clean) && grepl("growing", name_clean)) {
    key <- "growing_annuity"
  } else if (grepl("npv", name_clean) && grepl("irr", name_clean)) {
    key <- "npv_irr"
  } else if (grepl("put", name_clean) && grepl("call", name_clean)) {
    key <- "put_call"
  } else if (grepl("replicating", name_clean) || (grepl("binomial", name_clean) && grepl("portfolio", name_clean))) {
    key <- "replicating_portfolio"
  } else if (grepl("risk", name_clean) && grepl("neutral", name_clean)) {
    key <- "risk_neutral"
  }

  if (is.null(key)) {
    cat("Proof not found for: '", name, "'\n", sep = "")
    cat("Please try using keywords like 'put call', 'irr', 'annuity', etc.\n")
    return(invisible(NULL))
  }

  # Print the found proof
  cat(proofs_list[[key]])
}
