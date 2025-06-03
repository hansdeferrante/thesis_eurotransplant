format_dates <- function(text) {
  library(stringr)
  library(lubridate)
  
  # Regular expression to match dates in DD-MM-YYYY format
  date_pattern <- "\\b(\\d{2})-(\\d{2})-(\\d{4})\\b"
  
  # Replace dates using str_replace_all and lubridate's parsing
  formatted_text <- str_replace_all(text, date_pattern, function(x) {
    date <- dmy(x)
    date_str <- format(date, "%B %d, %Y")  # Format as "January 01, 2019"
    str_replace(date_str, '\\s0', ' ')
  })
  
  return(formatted_text)
}

insert_footnote_manually <- function(.x, .text, .label='') {
  
  lines <- .x |>
    str_split('\\n') |>
    unlist()
  .label <- if_else(nchar(.label)>0, paste0('$^', .label, '$'), .label)
  
  insert_place <- which(str_detect(lines, fixed('\\end{table}'))) - 1
  lines <- append(
    lines,
    values = paste0('\\parbox{\\textwidth}{\\footnotesize \\smallskip ', .label, .text, '}'), 
    after = insert_place
  ) 
  
  .x2 <- lines |> paste0(collapse = '\n')
  attributes(.x2) <- attributes(.x)
  return(.x2)
}

# Function to escape LaTeX characters
escape_latex_inner <- function(s) {
  s |> 
    str_replace_all(fixed("%"), "\\%") |>
    str_replace_all(fixed("_"), "\\_") |>
    str_replace_all(fixed("$"), "\\$") |>
    str_replace_all(fixed("{"), "\\{") |>
    str_replace_all(fixed("}"), "\\}")
}

is_even <- function(x) {
  x %% 2 == 0
}

combine_parts_with_delimiters <- function(parts, delimiters) {
  
  if (length(parts) == 1) {
    return(parts[[1]])
  }
  # Initialize the result with the first part
  result <- parts[[1]]
  
  # Iterate through the remaining parts and delimiters
  for (i in 2:length(parts)) {
    result <- paste0(result, delimiters[i - 1], parts[[i]])
  }
  
  return(result)
}

escape_latex <- function(text) {
  # Split the text by parts that are math (wrapped in $...$ or $$...$$)
  parts <- str_split(text, "\\$\\$?", simplify = FALSE)[[1]]
  delimiters <- str_extract_all(text, '\\$+')[[1]]
  
  # Identify which parts are math expressions
  is_math <- str_detect(parts, "^\\$[^\\$]*\\$$|^\\$\\$[^\\$]*\\$\\$$")
  
  # Apply escaping only to non-math parts
  parts[!is_even(seq_along(parts))] <- lapply(parts[!is_even(seq_along(parts))], escape_latex_inner)
  
  # Recombine the parts back together
  combine_parts_with_delimiters(parts, delimiters)
}

return_standalone_tikz_code <- function(path, use_sans_serif = FALSE) {
  tikz_code <- read_lines(path)
  extra_lines <- c(
    "\\documentclass[tikz]{standalone}",
    "\\usepackage{tikz}",
    "\\usepackage{enumitem}",
    "\\pdfmapfile{=pdftex.map}",
    "\\pdfinclusioncopyfonts=1",
    "\\usetikzlibrary{shapes.geometric, arrows, fit}",
    "\\begin{document}",
    ifelse(use_sans_serif, '\\sffamily', '')
    ) |> paste0(collapse = '\n')
  
  tikz_code <- append(tikz_code, extra_lines, after=0)
  tikz_code <- append(tikz_code, '\\end{document}')
  tikz_code
}

process_tikz_fig <- function(tex_fig, final_fig) {
  tikz_code <- return_standalone_tikz_code(tex_fig, use_sans_serif = TRUE)
  writeLines(tikz_code, "tmp_ss.tex")
  system(paste("pdflatex tmp_ss.tex  -jobname ", final_fig, sep=' '))
  system2("wsl", glue("pdf2svg {NEEDED_FIGS[[1]]} {NEEDED_FIGS[[2]]}"))
  
  tikz_code <- return_standalone_tikz_code(tex_fig)
  writeLines(tikz_code, "tmp.tex")
  system(paste("pdflatex tmp.tex  -jobname ", final_fig, sep=' '))
}


add_p_value_footnote <- function(.x, symbol = '*', extra_text = '') {
  # Validate symbol input
  if (!symbol %in% c('*', 'dagger')) {
    stop("Invalid symbol. Please use '*' or 'dagger'.")
  }
  
  # Determine symbol for HTML and LaTeX formats
  html_symbol <- if (symbol == '*') '\u2217' else '&dagger;'  # \u2217 = Unicode for *
  latex_symbol <- if (symbol == '*') '*' else '\\dagger'
  
  # Check for HTML or LaTeX output
  if (!any(stringr::str_detect(.x, 'hspace'))) {
    # HTML output
    string <- force(
      glue::glue('<sup>{html_symbol}</sup> p < 0.05, <sup>{html_symbol}{html_symbol}</sup> p < 0.01, <sup>{html_symbol}{html_symbol}{html_symbol}</sup> p < 0.001{extra_text}')
    )
    add_footnote(
      .x,
      string,
      notation = 'none',
      escape = FALSE
    )
  } else {
    # LaTeX output
    string <- force(
      glue('$^{__latex_symbol$$}$ p < 0.05, $^{__latex_symbol$$__latex_symbol$$}$ p < 0.01, $^{__latex_symbol$$__latex_symbol$$__latex_symbol$$}$ p < 0.001__extra_text$$',
           .open = '__', .close = '$$')
    )
    add_footnote(
      .x,
      string,
      notation = 'none',
      escape = FALSE
    )
  }
}

wrap_table <- function(input_file, my_label = NULL, my_caption = NULL, scale_down = TRUE, scale_factor=1, pos='h') {
  # Read the .tex file content
  if (length(input_file)==1) {
    table_content <- readLines(input_file)
  } else {
    table_content <- input_file
  }
  
  label <- if (!is.null(my_label)) paste0("\\label{", my_label, "}") else NULL
  caption <- if (!is.null(my_caption)) paste0("\\caption{", my_caption, "}") else NULL # \
  
  # Create the table environment wrapper with optional resizebox
  if (scale_down) {
    table_wrapped <- c(
      paste0("\\begin{table}[", pos, "]"),
      caption,
      label,
      '\\centering',
      paste0(
        "\\resizebox{",
        scale_factor,
        "\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{%"
      ),
      table_content,
      "}",
      "\\end{table}"
    )
  } else {
    table_wrapped <- c(
      paste0("\\begin{table}[", pos, "]"),
      caption,
      label,
      table_content,
      "\\end{table}"
    )
  }
  
  # Collapse into a single string with newlines and output as asis
  table_wrapped <- paste0(table_wrapped, collapse = '\n')
  knitr::asis_output(table_wrapped)
}


make_first_low <- function(x) {
  substr(x, 1, 1) <- tolower(substr(x, 1, 1))
  x
}

firstlow <- function(x) {
  exceptions <- c('HB', 'NHB', 'CNS', 'CVA', "Austria", "Belgium", "Croatia", "Germany", "Hungary", "Netherlands", "Slovenia", 'SMD', 'ReMELD-Na', 'ETKAS', '(?<![A-Za-z])(A|B|AB|O)(?![A-Za-z])', 'AM', 'ESP', 'Eurotransplant', 'WL', 'HLA', 'HCC', 'MELD', 'UNOS', 'NAFLD', 'INR', 'ReMELD', 'LR', 'NSE', 'PLD')
  regex_exceptions <- paste0(
    '^(',
    glue('({exceptions})') |> paste0(collapse = '|'),
    ')'
  )
  if_else(
    str_detect(x, regex_exceptions),
    x,
    make_first_low(x)
  )
}

