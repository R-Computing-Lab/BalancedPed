library("fs")
library("purrr")

# Specify the folder path and pattern to search
folder_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
pattern1 <- "setwd(dirname(rstudioapi::getActiveDocumentContext()$path))"
pattern2 <- "setwd(dirname(rstudioapi::getActiveDocumentContext()$path))"
pattern3 <- "setwd(dirname(rstudioapi::getActiveDocumentContext()$path))"
pattern4 <- "setwd(dirname(rstudioapi::getActiveDocumentContext()$path))"

# Function to comment out lines matching the pattern in a file
comment_matching_lines <- function(file_path, pattern) {
    lines <- readLines(file_path)
    matching_lines <- grep(pattern, lines)
    
    if (length(matching_lines) > 0) {
        lines[matching_lines] <- paste0("#", lines[matching_lines])
        writeLines(lines, file_path)
    }
}

# Find all .R files in the folder and subfolders
r_files <- dir_ls(folder_path, regexp = "\\.R$", recurse = TRUE, fail = FALSE)

# Comment out the lines matching the pattern in all .R files
walk(r_files, comment_matching_lines, pattern = pattern)
