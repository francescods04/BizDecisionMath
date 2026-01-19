#' Ask Gemini Math Tutor (Enhanced)
#' 
#' Sends a question to Google Gemini API using targeted context from course materials.
#' Automatically classifies the question type and loads relevant slides, exam solutions,
#' and R function references to provide comprehensive, exam-style answers.
#' 
#' @param question Character. The question to ask.
#' @param context_type Character. "auto" (default, uses classifier), "slides", "exams", or "all".
#' @param show_r_code Logical. Whether to include R function references (default: TRUE).
#' @param detail_level Character. "exam" (default, full step-by-step) or "concise".
#' @param data_dir Character. Path to directory containing .md files.
#' @param api_key Character. Your Gemini API Key. Defaults to Sys.getenv("GEMINI_API_KEY").
#' @return Character. The answer from Gemini.
#' @import httr
#' @import jsonlite
#' @export
ask_gemini_tutor <- function(question, 
                              context_type = "auto", 
                              show_r_code = TRUE,
                              detail_level = "exam",
                              data_dir = "/Users/francescodelsesto/Downloads/Data", 
                              api_key = Sys.getenv("GEMINI_API_KEY")) {
  
  if (api_key == "") {
    stop("API Key is missing. Please provide 'api_key' argument or set GEMINI_API_KEY environment variable.")
  }
  
  # 1. Classify question to determine relevant context
  if (context_type == "auto") {
    classification <- classify_question(question)
    context_info <- classification
  } else {
    # Manual context selection
    context_info <- list(
      type = context_type,
      slides = get_all_slides(data_dir, context_type),
      r_functions = NULL,
      example_exams = NULL
    )
  }
  
  # 2. Load only relevant files based on classification
  selected_files <- c()
  
  # Add relevant slide files
  if (!is.null(context_info$slides) && length(context_info$slides) > 0) {
    for (slide_name in context_info$slides) {
      slide_path <- file.path(data_dir, slide_name)
      if (file.exists(slide_path)) {
        selected_files <- c(selected_files, slide_path)
      }
    }
  }
  
  # Add example exam solutions (max 2 to avoid token bloat)
  if (!is.null(context_info$example_exams) && length(context_info$example_exams) > 0) {
    for (exam_name in head(context_info$example_exams, 2)) {
      exam_path <- file.path(data_dir, exam_name)
      if (file.exists(exam_path)) {
        selected_files <- c(selected_files, exam_path)
      }
    }
  }
  
  if (length(selected_files) == 0) {
    stop(paste("No relevant files found for question type:", context_info$type, "in", data_dir))
  }
  
  # 3. Build context from selected files with type labels
  context_text <- ""
  for (f in selected_files) {
    content <- readLines(f, warn = FALSE)
    file_type <- if(grepl("Solution", basename(f))) "[EXAMPLE SOLUTION]" else "[THEORY]"
    context_text <- paste(context_text, "\n\n--- ", file_type, " ", basename(f), " ---\n", 
                          paste(content, collapse = "\n"))
  }
  
  # 4. Add R function references if requested
  r_code_reference <- ""
  if (show_r_code && !is.null(context_info$r_functions) && length(context_info$r_functions) > 0) {
    r_code_reference <- "\n\n=== AVAILABLE R FUNCTIONS ===\n"
    r_code_reference <- paste0(r_code_reference, 
      "The BizDecisionMath R package provides the following functions for this topic:\n")
    for (func_name in context_info$r_functions) {
      r_code_reference <- paste0(r_code_reference, "  - ", func_name, "()\n")
    }
    r_code_reference <- paste0(r_code_reference, 
      "\nNOTE: While these R functions provide correct numerical answers, ",
      "exam solutions require showing manual step-by-step calculations.\n",
      "You may mention the relevant R function for verification purposes.\n")
  }
  
  # 5. Construct Concise Exam-Style System Prompt
  system_prompt <- paste0(
    "You are writing EXAM SOLUTIONS in Financial Mathematics.\n\n",
    "=== STRICT FORMAT (MATCH EXACTLY) ===\n",
    "Each solution part should follow this CONCISE structure:\n\n",
    "1. **Formula**: State it directly (e.g., r₂ = (1 + r)^(1/2) - 1)\n",
    "2. **Substitution**: Show numerical values (e.g., = 1.1^0.5 - 1)\n",
    "3. **Result**: Final answer (e.g., = 0.0488)\n\n",
    "=== CRITICAL FORMATTING RULES ===\n",
    "- **NO LaTeX delimiters**: NEVER use $...$ or $$...$$\n",
    "- **Plain text only**: Write formulas as readable text\n",
    "- Use Unicode symbols directly: π, μ, σ, δ, ρ, β, α\n",
    "- Use Unicode subscripts: ₀ ₁ ₂ or underscore notation (S_0, p_1)\n",
    "- Use Unicode superscripts: ⁰ ¹ ² or caret notation (e^(rt), u^2)\n",
    "- Use arrows: → for 'implies' or 'leads to'\n",
    "- Clear spacing and line breaks for readability\n\n",
    "=== WRITING STYLE ===\n",
    "- BE EXTREMELY CONCISE - match the brevity of provided exam solutions\n",
    "- NO lengthy theoretical explanations\n",
    "- NO 'Theoretical Basis' or 'Background' sections\n",
    "- NO R package verification notes at the end\n",
    "- Use (a), (b), (c) labels for multi-part questions\n",
    "- Show formula → substitution → result pattern\n",
    "- Use 4 decimal places for final answers\n\n",
    "=== EXAMPLE OF CORRECT FORMAT ===\n",
    "GOOD (plain text, readable):\n",
    "(a) Semi-annual rate:\n",
    "   r₂ = (1 + r)^(1/2) - 1\n",
    "   = 1.1^0.5 - 1\n",
    "   = 0.0488\n\n",
    "BAD (LaTeX delimiters):\n",
    "$r_2 = (1 + r)^{\\frac{1}{2}} - 1$\n",
    "$= 1.1^{0.5} - 1$\n",
    "$= 0.0488$\n\n",
    if (show_r_code) {
      "If an R function exists, you may add ONE line: 'Verify: function_name()'\n\n"
    } else {
      ""
    },
    "Match the EXACT conciseness and PLAIN TEXT format of the provided example solutions.\n"
  )
  
  # 6. Assemble Full Prompt
  full_prompt <- paste(
    "=== COURSE MATERIALS ===", context_text,
    r_code_reference,
    "\n\n=== STUDENT QUESTION ===\n", question
  )
  
  # 4. Call Gemini API
  # Endpoint for Gemini 3 Pro Preview (generateContent)
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/gemini-3-pro-preview:generateContent?key=", api_key)
  
  body <- list(
    contents = list(
      list(
        parts = list(
          list(text = paste(system_prompt, "\n\n", full_prompt))
        )
      )
    )
  )
  
  response <- httr::POST(
    url,
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    httr::add_headers("Content-Type" = "application/json")
  )
  
  # 5. Parse Response
  if (httr::status_code(response) != 200) {
    stop(paste("API Request Failed:", httr::content(response, "text")))
  }
  
  resp_content <- httr::content(response, as = "parsed")
  answer <- resp_content$candidates[[1]]$content$parts[[1]]$text
  
  return(answer)
}
