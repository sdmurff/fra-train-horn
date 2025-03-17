# Required packages
if (!require("httr")) install.packages("httr")
if (!require("rvest")) install.packages("rvest")

library(httr)
library(rvest)

# Base URL
base_url <- "https://www2.census.gov/geo/tiger/TIGER2024/TABBLOCK20/"

# Create a directory to store downloaded files
download_dir <- "~/Downloads/State Census Files"

# Scrape the webpage to get all ZIP file links
page <- read_html(base_url)

# Extract ZIP file links
zip_links <- page %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  grep("tabblock20.zip$", ., value = TRUE)

# Full URLs
zip_urls <- paste0(base_url, zip_links)

# Download each file
for (url in zip_urls) {
  file_name <- basename(url)
  file_path <- file.path(download_dir, file_name)
  
  if (!file.exists(file_path)) {
    cat("Downloading:", file_name, "...\n")
    tryCatch({
      download.file(url, file_path, mode = "wb")
      cat("Downloaded:", file_name, "\n")
    }, error = function(e) {
      cat("Failed to download:", file_name, "\n")
    })
  } else {
    cat(file_name, "already exists, skipping...\n")
  }
}

cat("All downloads completed!\n")
