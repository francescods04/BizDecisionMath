# Solution Script for Mock Exam Jan 16 2023
# Using BizDecisionMath package

# Load package
sapply(list.files("R", full.names = TRUE), source)

cat("--- EXERCISE 1: ANNUITIES ---\n")
# (a) Theoretical proof (not code). Formula implemented in annuity_pv.
cat("(a) Formula Proof: covered by logic in annuity_pv.\n")

# (b) Formula for PV ordinary annuity, quarterly, n years.
# Need equivalent quarterly rate i4.
# Example usage:
cat("(b) Convert annual to quarterly rate using calc_equivalent_rate(i, 4, 'annual_to_periodic'). Then use annuity_pv(..., n=n*4).\n")

# (c) Geometric Annuity Formula.
cat("(c) Geometric Annuity: covered by geom_annuity_pv.\n")


cat("\n--- EXERCISE 2: CAPITAL PROJECT EVALUATION ---\n")
# Investment A
cf_A <- c(-3000, 750, 800, 850, 950)
t_A <- 0:4

# Investment B
cf_B <- c(-4000, 1100, 1400, 2000) # Only up to year 3? Table says Year 4 is "-"
t_B <- 0:3

# (a) Rate i = 4%
i_a <- 0.04
npv_A_a <- calculate_npv(cf_A, t_A, i_a)
npv_B_a <- calculate_npv(cf_B, t_B, i_a)
cat(sprintf("(a) r=4%%: NPV A = %.4f, NPV B = %.4f\n", npv_A_a, npv_B_a))
if (npv_B_a > npv_A_a) cat("Investment B is more convenient.\n")

# (b) Rate i = 15%
i_b <- 0.15
npv_A_b <- calculate_npv(cf_A, t_A, i_b)
npv_B_b <- calculate_npv(cf_B, t_B, i_b)
cat(sprintf("(b) r=15%%: NPV A = %.4f, NPV B = %.4f\n", npv_A_b, npv_B_b))
if (npv_A_b < 0 && npv_B_b < 0) cat("Neither investment is convenient (NPV < 0).\n")


cat("\n--- EXERCISE 3: OPTION PRICING ---\n")
# Call Option
# S0 = 40, K = 40
# T = 16 months (16/12 years)
# r = 2% (instantaneous -> continuous)
# u = 1.2, d = 0.8
# n = 2 periods (8 months each)

S <- 40; K <- 40; r <- 0.02; T_yrs <- 16/12
u <- 1.2; d <- 0.8
n <- 2

# (a) Price Tree & Risk-Neutral Prob
# (b) Call Option Tree
# Using detailed=TRUE
res <- binomial_option_price(S=S, K=K, r=r, T=T_yrs, type="call", n=n, u=u, d=d, detailed=TRUE)

cat(sprintf("(a) Risk-Neutral Probability p: %.4f\n", res$probability))
cat("(a) Stock Price Tree:\n")
print(res$stock_tree)

cat("(b) Call Option Tree:\n")
print(res$option_tree)
cat(sprintf("Call Price at t=0: %.4f\n", res$price))

# (c) Put Calculation via Parity
# c - p = S - K*exp(-rT) => p = c - S + K*exp(-rT)
# Package function handles this logic.
put_price <- put_call_parity(c=res$price, S=S, K=K, r=r, T=T_yrs, p=NA)
cat(sprintf("(c) Put Price (Parity): %.4f\n", put_price))
