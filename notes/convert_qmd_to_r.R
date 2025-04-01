convert_qmd_to_r <- function(qmd_file, r_file) {
  # Read all the lines from the qmd file
  lines <- readLines(qmd_file)
  
  # Initialize a vector for the output lines
  out_lines <- character()
  
  # Flag to indicate if currently inside an R code block
  in_code <- FALSE
  
  # Loop through each line
  for (line in lines) {
    # Detect the start of an R code block: pattern like ```{r ...}
    if (grepl("^```\\{r", line)) {
      in_code <- TRUE
      next  # Skip the code block start indicator line
    }
    # Detect the end of a code block: a line with only ``` (allowing trailing whitespace)
    if (grepl("^```\\s*$", line) && in_code) {
      in_code <- FALSE
      next  # Skip the code block end indicator line
    }
    
    # If inside a code block, add the line as is
    if (in_code) {
      out_lines <- c(out_lines, line)
    } else {
      # If in text section, convert the text line to an R comment
      out_lines <- c(out_lines, paste0("# ", line))
    }
  }
  
  # Write the converted content to a new .R file
  writeLines(out_lines, r_file)
}

# Example usage: set the working directory and convert the file
setwd("D:/AAnus/semester2/ST5209X Analysis Of Time Series Data/st5209-2025-master/notes")
convert_qmd_to_r("note_week8.qmd", "note_week8.R")