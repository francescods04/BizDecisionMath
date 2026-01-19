#' Format Result with Verbose Formula Display
#' 
#' Internal helper function to display formula information when verbose = TRUE.
#' Prints structured output showing formula name, mathematical expression, inputs, 
#' result, and optional interpretation.
#' 
#' @param formula_name Character. Human-readable name of the formula.
#' @param formula_text Character. The mathematical formula (plain text or Unicode).
#' @param inputs Named list. Input parameters with their values.
#' @param result Numeric. The calculated result.
#' @param interpretation Character. Optional explanation of what the result means.
#' @param additional_info Named list. Optional additional computed values to display.
#' @return Invisible NULL (side effect: prints to console).
#' @keywords internal
format_result <- function(formula_name, 
                          formula_text, 
                          inputs, 
                          result, 
                          interpretation = NULL,
                          additional_info = NULL) {
  
  cat("\n")
  cat("=== FORMULA ===\n")
  cat(formula_name, "\n")
  cat(formula_text, "\n\n")
  
  cat("=== INPUTS ===\n")
  for (name in names(inputs)) {
    value <- inputs[[name]]
    # Format value nicely
    if (is.numeric(value) && length(value) == 1) {
      cat(sprintf("%s = %.6g\n", name, value))
    } else if (is.numeric(value)) {
      cat(sprintf("%s = [%s]\n", name, paste(value, collapse = ", ")))
    } else {
      cat(sprintf("%s = %s\n", name, as.character(value)))
    }
  }
  cat("\n")
  
  if (!is.null(additional_info) && length(additional_info) > 0) {
    cat("=== INTERMEDIATE VALUES ===\n")
    for (name in names(additional_info)) {
      value <- additional_info[[name]]
      if (is.numeric(value) && length(value) == 1) {
        cat(sprintf("%s = %.6g\n", name, value))
      } else {
        cat(sprintf("%s = %s\n", name, as.character(value)))
      }
    }
    cat("\n")
  }
  
  cat("=== RESULT ===\n")
  if (is.numeric(result) && length(result) == 1) {
    cat(sprintf("%.10g\n", result))
  } else if (is.numeric(result)) {
    cat(sprintf("[%s]\n", paste(sprintf("%.6g", result), collapse = ", ")))
  } else {
    cat(as.character(result), "\n")
  }
  
  if (!is.null(interpretation)) {
    cat("\n=== INTERPRETATION ===\n")
    cat(interpretation, "\n")
  }
  
  cat("\n")
  invisible(NULL)
}
