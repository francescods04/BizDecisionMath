#' Classify Question to Determine Relevant Context
#' 
#' Analyzes a user question and determines the problem type, relevant slides,
#' R functions, and example exam solutions to include in context.
#' 
#' @param question Character. The student's question.
#' @return List with type, slides, r_functions, and example_exams.
#' @export
classify_question <- function(question) {
  q_lower <- tolower(question)
  
  # Priority order: Most specific topics first
  
  # --- STATISTICS & REGRESSION TOPICS (Highest Priority) ---
  
  # Regression Analysis (Linear, Multiple)
  if (grepl("regression|linear model|least squares|ols|r-squared|coefficient of determination", q_lower)) {
    return(list(
      type = "regression",
      slides = c("Math_Sections/01_Introduction_To_Regression.md", 
                 "Math_Sections/09_Multiple_Regression.md",
                 "Math_Sections/07_Coefficient_Of_Determination.md"),
      r_functions = NULL,
      example_exams = NULL
    ))
  }

  # Confidence Intervals
  # Note: "beta" is often used here too, so check this before Portfolio
  if (grepl("confidence interval|margin of error|student t|quantile", q_lower)) {
    return(list(
      type = "confidence_interval",
      slides = c("Math_Sections/03_Confidence_Interval.md"),
      r_functions = NULL,
      example_exams = NULL
    ))
  }

  # Hypothesis Testing
  if (grepl("hypothesis|null hypothesis|p-value|significance level|reject|test statistic", q_lower)) {
    return(list(
      type = "hypothesis_testing",
      slides = c("Math_Sections/04_Hypothesis_Testing.md"),
      r_functions = NULL,
      example_exams = NULL
    ))
  }

  # Prediction
  if (grepl("prediction|forecast|estimate y|mean response", q_lower)) {
    return(list(
      type = "prediction",
      slides = c("Math_Sections/05_Prediction.md"),
      r_functions = NULL,
      example_exams = NULL
    ))
  }
  
  # --- FINANCE TOPICS ---
  
  # Options Pricing (binomial model, put/call, derivatives)
  if (grepl("binomial|option|call|put|strike|payoff|exercise|derivative|replicat", q_lower)) {
    return(list(
      type = "options",
      slides = c("Slide_Set_3.md", "Slide_Set_4.md"),
      r_functions = c("binomial_option_price", "put_call_parity", "crr_calibration"),
      example_exams = c("Exam_August_30th2018_Solution.md", "Exam_January_15th_2015_Group_A_-_Solution.md")
    ))
  }
  
  # Portfolio Selection (CAPM, efficient frontier, beta)
  if (grepl("portfolio|beta|capm|frontier|sharpe|efficient|systematic risk|market portfolio", q_lower)) {
    return(list(
      type = "portfolio",
      slides = c("Slide_Set_5.md"),  # Note: May need fallback if not available
      r_functions = c("capm_expected_return", "capm_beta", "efficient_frontier_risk", 
                     "portfolio_return_two_assets", "portfolio_risk_two_assets"),
      example_exams = c("Exam_January_15th_2015_Group_A_-_Solution.md")
    ))
  }
  
  # Perpetuities (before general annuities to be more specific)
  if (grepl("perpetuity|perpetual|infinite horizon|forever", q_lower)) {
    return(list(
      type = "perpetuity",
      slides = c("Slide_Set_1.md"),
      r_functions = c("perpetuity_pv", "geom_annuity_pv"),
      example_exams = c("Exam_January_15th_2015_Group_A_-_Solution.md")
    ))
  }
  
  # Geometric/Growing Annuities
  if (grepl("geometric|growing|growth rate|g =|increasing.*rate", q_lower)) {
    return(list(
      type = "geometric_annuity",
      slides = c("Slide_Set_1.md"),
      r_functions = c("geom_annuity_pv", "perpetuity_pv"),
      example_exams = c("Exam_January_15th_2015_Group_A_-_Solution.md")
    ))
  }
  
  # General Annuities (ordinary, due, deferred)
  if (grepl("annuity|annuities|periodic payment|installment|deferred|ordinary|due", q_lower)) {
    return(list(
      type = "annuities",
      slides = c("Slide_Set_0.md", "Slide_Set_1.md"),
      r_functions = c("annuity_pv", "annuity_fv"),
      example_exams = c("Exam_August_30th2018_Solution.md")
    ))
  }
  
  # Investment Analysis (NPV, IRR, Gordon model)
  if (grepl("npv|irr|net present value|internal rate|investment|profitability index|gordon|dividend|stock valuation", q_lower)) {
    return(list(
      type = "valuation",
      slides = c("Slide_Set_2.md", "Slide_Set_3.md"),
      r_functions = c("calculate_npv", "calculate_irr", "gordon_model"),
      example_exams = c("Exam_January_15th_2015_Group_A_-_Solution.md")
    ))
  }
  
  # --- STATISTICS & REGRESSION TOPICS ---
  
  # Regression Analysis (Linear, Multiple)
  if (grepl("regression|linear model|least squares|ols|r-squared|coefficient of determination", q_lower)) {
    return(list(
      type = "regression",
      slides = c("Math_Sections/01_Introduction_To_Regression.md", 
                 "Math_Sections/09_Multiple_Regression.md",
                 "Math_Sections/07_Coefficient_Of_Determination.md"),
      r_functions = NULL,
      example_exams = NULL
    ))
  }

  # Confidence Intervals
  if (grepl("confidence interval|margin of error|student t|quantile", q_lower)) {
    return(list(
      type = "confidence_interval",
      slides = c("Math_Sections/03_Confidence_Interval.md"),
      r_functions = NULL,
      example_exams = NULL
    ))
  }

  # Hypothesis Testing
  if (grepl("hypothesis|null hypothesis|p-value|significance level|reject|test statistic", q_lower)) {
    return(list(
      type = "hypothesis_testing",
      slides = c("Math_Sections/04_Hypothesis_Testing.md"),
      r_functions = NULL,
      example_exams = NULL
    ))
  }

  # Prediction
  if (grepl("prediction|forecast|estimate y|mean response", q_lower)) {
    return(list(
      type = "prediction",
      slides = c("Math_Sections/05_Prediction.md"),
      r_functions = NULL,
      example_exams = NULL
    ))
  }

  # Basic Interest Calculations
  if (grepl("simple interest|compound interest|equivalent rate|accumulation|discount factor", q_lower)) {
    return(list(
      type = "basics",
      slides = c("Slide_Set_0.md"),
      r_functions = c("calc_simple_accumulation", "calc_compound_accumulation", 
                     "calc_equivalent_rate", "calc_simple_discount", "calc_compound_discount"),
      example_exams = NULL
    ))
  }
  
  # Default: General financial mathematics
  return(list(
    type = "general",
    slides = c("Slide_Set_0.md", "Math_Sections/01_Introduction_To_Regression.md"),
    r_functions = NULL,
    example_exams = NULL
  ))
}


#' Get All Slide Files Based on Context Type
#' 
#' Helper function to retrieve slide file names for manual context selection.
#' 
#' @param data_dir Character. Base directory containing slides.
#' @param context_type Character. One of: "slides", "exams", "all".
#' @return Character vector of file names.
#' @export
get_all_slides <- function(data_dir, context_type = "slides") {
  if (context_type == "slides") {
    basic_slides <- c("Slide_Set_0.md", "Slide_Set_1.md", "Slide_Set_2.md", 
             "Slide_Set_3.md", "Slide_Set_4.md")
    math_files <- list.files(file.path(data_dir, "Math_Sections"), pattern = "\\.md$", full.names = FALSE)
    math_paths <- file.path("Math_Sections", math_files)
    return(c(basic_slides, math_paths))
  } else if (context_type == "exams") {
    # Return all exam solution files
    all_files <- list.files(data_dir, pattern = "Solution\\.md$", full.names = FALSE)
    return(all_files)
  } else if (context_type == "all") {
    basic_slides <- c("Slide_Set_0.md", "Slide_Set_1.md", "Slide_Set_2.md", 
                "Slide_Set_3.md", "Slide_Set_4.md")
    math_files <- list.files(file.path(data_dir, "Math_Sections"), pattern = "\\.md$", full.names = FALSE)
    math_paths <- file.path("Math_Sections", math_files)
    exams <- list.files(data_dir, pattern = "Solution\\.md$", full.names = FALSE)
    return(c(basic_slides, math_paths, exams))
  }
  
  return(c("Slide_Set_0.md"))  # Default fallback
}
