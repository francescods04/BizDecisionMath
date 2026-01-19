#' CAPM Expected Return
#' 
#' Calculates the expected return of an asset/portfolio using CAPM.
#' Formula: E(R) = R_f + beta * (E(R_m) - R_f)
#'
#' @param beta Numeric. The beta coefficient.
#' @param r_m Numeric. The expected market return.
#' @param r_f Numeric. The risk-free rate.
#' @return Numeric. The expected return.
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

#' CAPM Beta
#' 
#' Calculates Beta given expected return and market parameters.
#' Formula: beta = (E(R) - R_f) / (E(R_m) - R_f)
#'
#' @param mu Numeric. The expected return of the asset/portfolio.
#' @param r_m Numeric. The expected market return.
#' @param r_f Numeric. The risk-free rate.
#' @return Numeric. The beta coefficient.
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

#' Two Asset Portfolio Return
#' 
#' Calculates expected return of a portfolio of two assets.
#' Formula: mu_p = w * mu1 + (1-w) * mu2
#'
#' @param w Numeric. Weight of asset 1.
#' @param mu1 Numeric. Expected return of asset 1.
#' @param mu2 Numeric. Expected return of asset 2.
#' @return Numeric. Portfolio expected return.
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

#' Two Asset Portfolio Risk (Volatility)
#' 
#' Calculates standard deviation of a portfolio of two assets.
#' Formula: sigma_p = sqrt(w^2*sigma1^2 + (1-w)^2*sigma2^2 + 2*w*(1-w)*sigma1*sigma2*rho)
#'
#' @param w Numeric. Weight of asset 1.
#' @param sigma1 Numeric. Volatility of asset 1.
#' @param sigma2 Numeric. Volatility of asset 2.
#' @param rho Numeric. Correlation coefficient (-1 to 1).
#' @param verbose Logical. If TRUE, displays calculation steps.
#' @return Numeric. Portfolio volatility.
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

#' Efficient Frontier Risk (Hyperbola)
#' 
#' Calculates portfolio risk (sigma) given expected return (mu) using the explicit efficient frontier equation often found in exams.
#' Equation: sigma = sqrt(A*mu^2 + B*mu + C)
#'
#' @param mu Numeric. The target expected return.
#' @param A Numeric. Coefficient of mu^2.
#' @param B Numeric. Coefficient of mu.
#' @param C Numeric. Constant term.
#' @return Numeric. The portfolio risk (sigma).
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

#' Market Portfolio Return (from Frontier)
#' 
#' Calculates the Market Portfolio expected return (mu_M) given the efficient frontier coefficients and risk-free rate.
#' The market portfolio is the tangency portfolio.
#' Formula derived from CML tangency: mu_M = (-2C + B*r) / (B + 2*A*r)
#' 
#' @param r Numeric. Risk-free rate.
#' @param A Numeric. Frontier coefficient.
#' @param B Numeric. Frontier coefficient.
#' @param C Numeric. Frontier coefficient.
#' @param verbose Logical. If TRUE, displays calculation steps. Default FALSE.
#' @return Numeric. The expected return of the market portfolio.
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
