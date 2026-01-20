#' @title CAPM Expected Return
#' 
#' @description
#' Calculates the expected return of an asset/portfolio using the Capital Asset Pricing Model (CAPM).
#'
#' @details
#' **Mathematical Formula:**
#' \deqn{E(R_i) = R_f + \beta_i (E(R_m) - R_f)}
#' or
#' \deqn{\mu_i = r + \beta_i (\mu_M - r)}
#'
#' **Components:**
#' - \eqn{R_f}: Risk-free rate (time value of money)
#' - \eqn{\beta_i}: Systematic risk of asset i
#' - \eqn{R_m - R_f}: Market Risk Premium (price of risk)
#'
#' **Interpretation:**
#' An investor should be compensated for time (risk-free rate) and systematic risk (beta times premium).
#' Idiosyncratic risk is not compensated because it can be diversified away.
#'
#' **Use Cases:**
#' - Estimating cost of equity (Ke) for valuation
#' - Evaluating portfolio performance (Jensen's Alpha)
#' - Setting required hurdle rates for projects
#"
#' @param beta Numeric. The beta coefficient (systematic risk).
#' @param r_m Numeric. The expected market return.
#' @param r_f Numeric. The risk-free rate.
#' @param verbose Logical. If TRUE, displays exam-style solution.
#'
#' @return Numeric. The expected return.
#'
#' @seealso 
#' \code{\link{capm_beta}} for calculating beta
#' \code{\link{show_formula}("capm")} for theoretical formula
#'
#' @examples
#' # Calculate required return for a stock with Beta = 1.2
#' # Risk-free rate = 3%, Market Return = 8%
#' capm_expected_return(beta = 1.2, r_m = 0.08, r_f = 0.03)
#' # Returns: 0.03 + 1.2 * (0.08 - 0.03) = 0.09 (9%)
#'
#' # Exam problem: Cost of Equity
#' # Beta is 1.5, risk-free rate is 4%, market risk premium is 6%.
#' # Note: If "Market Risk Premium" is given directly, use r_m = r_f + premium
#' capm_expected_return(beta = 1.5, r_m = 0.10, r_f = 0.04, verbose = TRUE)
#'
#' @export
capm_expected_return <- function(beta, r_m, r_f, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  market_premium <- r_m - r_f
  beta_times_premium <- beta * market_premium
  result <- r_f + beta_times_premium
  
  if (verbose) {
    format_result(
      formula_name = "CAPM Expected Return",
      formula_text = "μ = r + β(μ_M - r)",
      inputs = list(
        "r (Risk-free rate)" = r_f,
        "β (Beta coefficient)" = beta,
        "μ_M (Market return)" = r_m
      ),
      additional_info = list(
        "Market premium (μ_M - r)" = market_premium,
        "β × (μ_M - r)" = beta_times_premium
      ),
      result = result,
      interpretation = sprintf(
        "Expected return = %.2f%% (risk-free: %.2f%% + risk premium: %.2f%%)",
        result * 100, r_f * 100, beta_times_premium * 100
      )
    )
  }
 
  return(result)
}

#' @title CAPM Beta Coefficient
#' 
#' @description
#' Calculates the Beta of an asset, which measures its sensitivity to market movements.
#' Rearranges the CAPM formula to solve for Beta.
#'
#' @details
#' **Mathematical Formula:**
#' \deqn{\beta_i = \frac{E(R_i) - R_f}{E(R_m) - R_f}}
#'
#' **Alternative Definition (Covariance):**
#' \deqn{\beta_i = \frac{Cov(R_i, R_m)}{Var(R_m)} = \rho_{i,m} \frac{\sigma_i}{\sigma_m}}
#'
#' **Interpretation:**
#' - \eqn{\beta = 1}: Asset moves in sync with market
#' - \eqn{\beta > 1}: Aggressive/Volatile (amplifies market moves)
#' - \eqn{\beta < 1}: Defensive (less volatile than market)
#' - \eqn{\beta = 0}: Uncorrelated with market (e.g., risk-free asset)
#'
#' @param mu Numeric. The expected return of the asset/portfolio.
#' @param r_m Numeric. The expected market return.
#' @param r_f Numeric. The risk-free rate.
#' @param verbose Logical. If TRUE, displays exam-style solution.
#'
#' @return Numeric. The beta coefficient.
#'
#' @seealso 
#' \code{\link{capm_expected_return}} for using beta
#' \code{\link{show_formula}("beta")} for theory
#'
#' @examples
#' # Find Beta: Stock return 12%, Market 10%, Risk-free 2%
#' capm_beta(mu = 0.12, r_m = 0.10, r_f = 0.02)
#' # Returns: (0.12-0.02)/(0.10-0.02) = 0.10/0.08 = 1.25
#'
#' # Exam problem: Portfolio Beta
#' # A portfolio returns 15% when the market returns 11%. Risk-free is 3%.
#' # What is the portfolio's systematic risk (beta)?
#' capm_beta(mu = 0.15, r_m = 0.11, r_f = 0.03, verbose = TRUE)
#'
#' @export
capm_beta <- function(mu, r_m, r_f, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  numerator <- mu - r_f
  denominator <- r_m - r_f
  beta <- numerator / denominator
  
  if (verbose) {
    format_result(
      formula_name = "CAPM Beta Coefficient",
      formula_text = "β = (μ - r) / (μ_M - r)",
      inputs = list(
        "μ (Asset/Portfolio return)" = mu,
        "μ_M (Market return)" = r_m,
        "r (Risk-free rate)" = r_f
      ),
      additional_info = list(
        "μ - r" = numerator,
        "μ_M - r" = denominator
      ),
      result = beta,
      interpretation = sprintf(
        "Beta β = %.4f. Asset has %s systematic risk than market.",
        beta,
        ifelse(beta > 1, "higher", ifelse(beta < 1, "lower", "same"))
      )
    )
  }
  
  return(beta)
}

#' @title Two Asset Portfolio Return
#' 
#' @description
#' Calculates the expected return of a portfolio consisting of two assets.
#' Returns are simply the weighted average of individual asset returns.
#'
#' @details
#' **Mathematical Formula:**
#' \deqn{\mu_P = w \mu_1 + (1-w) \mu_2}
#'
#' **Components:**
#' - w: Weight invested in Asset 1
#' - 1-w: Weight invested in Asset 2
#'
#' **Key Property:**
#' Linearity: The return of a portfolio is always the weighted average,
#' regardless of the correlation between assets.
#'
#' @param w Numeric. Weight of asset 1 (0 to 1).
#' @param mu1 Numeric. Expected return of asset 1.
#' @param mu2 Numeric. Expected return of asset 2.
#' @param verbose Logical. If TRUE, displays calculation steps.
#'
#' @return Numeric. Portfolio expected return.
#'
#' @seealso 
#' \code{\link{portfolio_risk_two_assets}} for calculating risk
#'
#' @examples
#' # 60% in Stock A (10% return), 40% in Stock B (6% return)
#' portfolio_return_two_assets(w = 0.60, mu1 = 0.10, mu2 = 0.06)
#' # Returns: 0.6*0.1 + 0.4*0.06 = 0.084 (8.4%)
#'
#' @export
portfolio_return_two_assets <- function(w, mu1, mu2, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  w1_contribution <- w * mu1
  w2_contribution <- (1 - w) * mu2
  mu_p <- w1_contribution + w2_contribution
  
  if (verbose) {
    format_result(
      formula_name = "Two-Asset Portfolio Expected Return",
      formula_text = "μ_P = w×μ₁ + (1-w)×μ₂",
      inputs = list(
        "w (Weight of asset 1)" = w,
        "μ₁ (Return of asset 1)" = mu1,
        "μ₂ (Return of asset 2)" = mu2,
        "1-w (Weight of asset 2)" = 1 - w
      ),
      additional_info = list(
        "w × μ₁" = w1_contribution,
        "(1-w) × μ₂" = w2_contribution
      ),
      result = mu_p,
      interpretation = sprintf(
        "Portfolio return: %.2f%% (%.0f%% asset 1 + %.0f%% asset 2)",
        mu_p * 100, w * 100, (1-w) * 100
      )
    )
  }
  
  return(mu_p)
}

#' @title Two Asset Portfolio Risk (Standard Deviation)
#' 
#' @description
#' Calculates the risk (standard deviation) of a two-asset portfolio.
#' This demonstrates the effect of diversification.
#'
#' @details
#' **Mathematical Formula:**
#' \deqn{\sigma_P = \sqrt{w^2 \sigma_1^2 + (1-w)^2 \sigma_2^2 + 2 w (1-w) \sigma_1 \sigma_2 \rho_{1,2}}}
#'
#' **Diversification Effect:**
#' The lower the correlation \eqn{\rho}, the lower the portfolio risk.
#' - \eqn{\rho = 1}: No diversification benefits (linear risk)
#' - \eqn{\rho < 1}: Risk is LESS than weighted average (Diversification)
#' - \eqn{\rho = -1}: Maximum diversification (risk can be eliminated)
#'
#' @param w Numeric. Weight of asset 1.
#' @param sigma1 Numeric. Volatility (std dev) of asset 1.
#' @param sigma2 Numeric. Volatility (std dev) of asset 2.
#' @param rho Numeric. Correlation coefficient (-1 to 1).
#' @param verbose Logical. If TRUE, displays calculation steps.
#'
#' @return Numeric. Portfolio volatility (sigma).
#'
#' @seealso 
#' \code{\link{portfolio_return_two_assets}}
#' \code{\link{show_proof}("portfolio risk")} for derivation
#'
#' @examples
#' # 50/50 Portfolio, both assets 20% vol, correlation 0 (uncorrelated)
#' portfolio_risk_two_assets(w=0.5, sigma1=0.2, sigma2=0.2, rho=0)
#' # Returns: sqrt(0.25*0.04 + 0.25*0.04) = sqrt(0.02) ≈ 14.14%
#' # Note: 14.14% < 20% (Diversification benefit!)
#'
#' # Exam problem: Minimum Variance
#' # Asset A: 20% vol, Asset B: 30% vol, Rho = -0.5, Weights 60/40
#' portfolio_risk_two_assets(w=0.6, sigma1=0.2, sigma2=0.3, rho=-0.5, verbose=TRUE)
#'
#' @export
portfolio_risk_two_assets <- function(w, sigma1, sigma2, rho, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  w1_var <- w^2 * sigma1^2
  w2_var <- (1 - w)^2 * sigma2^2
  cov_term <- 2 * w * (1 - w) * sigma1 * sigma2 * rho
  var_p <- w1_var + w2_var + cov_term
  sigma_p <- sqrt(var_p)
  
  if (verbose) {
    format_result(
      formula_name = "Two-Asset Portfolio Risk (Standard Deviation)",
      formula_text = "σ_P = √[w²σ₁² + (1-w)²σ₂² + 2w(1-w)σ₁σ₂ρ]",
      inputs = list(
        "w (Weight of asset 1)" = w,
        "σ₁ (Volatility of asset 1)" = sigma1,
        "σ₂ (Volatility of asset 2)" = sigma2,
        "ρ (Correlation)" = rho
      ),
      additional_info = list(
        "w²σ₁²" = w1_var,
        "(1-w)²σ₂²" = w2_var,
        "2w(1-w)σ₁σ₂ρ" = cov_term,
        "Variance σ²_P" = var_p
      ),
      result = sigma_p,
      interpretation = sprintf(
        "Portfolio risk: %.2f%% (correlation ρ=%.2f %s diversification)",
        sigma_p * 100, rho,
        ifelse(rho < 0, "increases", ifelse(rho > 0.5, "reduces", "moderately affects"))
      )
    )
  }
  
  return(sigma_p)
}

#' @title Efficient Frontier Risk (Hyperbola)
#' 
#' @description
#' Calculates portfolio risk (sigma) for a target return (mu) using the 
#' Efficient Frontier equation (Markovitz hyperbola).
#'
#' @details
#' **Mathematical Formula:**
#' \deqn{\sigma(\mu) = \sqrt{A \mu^2 + B \mu + C}}
#'
#' This equation defines the efficient frontier in \eqn{(\mu, \sigma)} space.
#' A, B, and C are constants derived from the inverse covariance matrix.
#'
#' **Properties:**
#' - The Minimum Variance Portfolio (MVP) is the vertex of the hyperbola.
#' - The upper half is the Efficient Frontier.
#' - The lower half is inefficient.
#'
#' **Exam Tip:**
#' In exams, the coefficients A, B, C are usually given or derived from 3 representative portfolios.
#'
#' @param mu Numeric. The target expected return.
#' @param A Numeric. Coefficient of mu^2 (related to C).
#' @param B Numeric. Coefficient of mu (related to B).
#' @param C Numeric. Constant term (related to A).
#' @param verbose Logical. If TRUE, displays calculation steps.
#'
#' @return Numeric. The portfolio risk (standard deviation).
#'
#' @seealso 
#' \code{\link{solve_market_portfolio_return}} using frontier parameters
#'
#' @examples
#' # Given frontier equation: sigma = sqrt(40*mu^2 - 4*mu + 0.2)
#' # Find risk for 10% return
#' efficient_frontier_risk(mu=0.10, A=40, B=-4, C=0.2)
#'
#' @export
efficient_frontier_risk <- function(mu, A, B, C, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  # Calculate intermediate terms
  a_term <- A * mu^2
  b_term <- B * mu
  variance <- a_term + b_term + C
  
  if (variance < 0) warning("Variance calculation resulted in negative value. Check parameters.")
  
  sigma <- sqrt(variance)
  
  if (verbose) {
    format_result(
      formula_name = "Portfolio Risk (Efficient Frontier)",
      formula_text = "σ = √(A×μ² + B×μ + C)",
      inputs = list(
        "μ (Expected return)" = mu,
        "A (Coefficient)" = A,
        "B (Coefficient)" = B,
        "C (Constant)" = C
      ),
      additional_info = list(
        "A × μ²" = a_term,
        "B × μ" = b_term,
        "C (constant)" = C,
        "Variance (A×μ² + B×μ + C)" = variance
      ),
      result = sigma,
      interpretation = sprintf(
        "Portfolio with expected return %.2f%% has risk (std dev) of %.2f%%",
        mu * 100, sigma * 100
      )
    )
  }
  
  return(sigma)
}

#' @title Market Portfolio Return (Tangency Portfolio)
#' 
#' @description
#' Calculates the expected return of the Market Portfolio (\eqn{\mu_M}) given the 
#' efficient frontier parameters and the risk-free rate.
#' The Market Portfolio is the point where the Capital Market Line (CML) is tangent to the Efficient Frontier.
#'
#' @details
#' **Mathematical Formula:**
#' Derived from maximizing the Sharpe Ratio (tangency condition):
#' \deqn{\mu_M = \frac{-2C + B r}{B + 2A r}}
#'
#' **Note:** The parameter names A, B, C follow the convention:
#' \eqn{\sigma^2 = A \mu^2 + B \mu + C}
#'
#' **Significance:**
#' This portfolio maximizes the Sharpe Ratio: \eqn{\frac{\mu - r}{\sigma}}.
#' It is the unique portfolio of risky assets held by all rational investors in the CAPM world.
#'
#' @param r Numeric. Risk-free rate.
#' @param A Numeric. Frontier coefficient for mu^2.
#' @param B Numeric. Frontier coefficient for mu.
#' @param C Numeric. Frontier coefficient (constant).
#' @param verbose Logical. If TRUE, displays calculation steps.
#'
#' @return Numeric. The expected return of the market portfolio.
#'
#' @seealso 
#' \code{\link{efficient_frontier_risk}}
#'
#' @examples
#' # Frontier: sigma = sqrt(40*mu^2 - 4*mu + 0.2)
#' # Risk-free rate: 2%
#' solve_market_portfolio_return(r=0.02, A=40, B=-4, C=0.2, verbose=TRUE)
#'
#' @export
solve_market_portfolio_return <- function(r, A, B, C, verbose = getOption("BizDecisionMath.verbose", FALSE)) {
  # Standard formula from exam solutions: mu_M = (-2C + B*r) / (B + 2*A*r)
  numerator <- -2 * C + B * r
  denominator <- B + 2 * A * r
  mu_m <- numerator / denominator
  
  if (verbose) {
    format_result(
      formula_name = "Market Portfolio Expected Return",
      formula_text = "μ_M = (-2C + B×r) / (B + 2A×r)",
      inputs = list(
        "r (Risk-free rate)" = r,
        "A (Frontier coefficient)" = A,
        "B (Frontier coefficient)" = B,
        "C (Frontier coefficient)" = C
      ),
      additional_info = list(
        "-2C" = -2 * C,
        "B × r" = B * r,
        "Numerator (-2C + B×r)" = numerator,
        "2A × r" = 2 * A * r,
        "Denominator (B + 2A×r)" = denominator
      ),
      result = mu_m,
      interpretation = sprintf(
        "Market portfolio has expected return of %.2f%% (risk-free rate: %.2f%%)",
        mu_m * 100, r * 100
      )
    )
  }
  
  return(mu_m)
}
