source('R/R_functions.R')

files <- list.files(pattern = "\\.Rmd$", full.names = TRUE)
for (file in files) {
  text <- readLines(file)
  formatted_text <- format_dates(paste(text, collapse = "\n"))
  writeLines(strsplit(formatted_text, "\n")[[1]], file)
}
