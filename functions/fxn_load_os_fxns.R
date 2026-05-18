# this loads all GC's os lame functions from github

fxn_load_os_fxns <- function() {
  # 1. Ensure required packages are present
  req_pkgs <- c("dplyr", "purrr", "stringr", "httr", "cli")
  missing <- req_pkgs[!(req_pkgs %in% installed.packages()[, "Package"])]
  
  if (length(missing) > 0) {
    stop(paste("Please install the following packages first:", paste(missing, collapse = ", ")))
  }
  
  library(dplyr)
  library(purrr)
  library(stringr)
  library(httr)
  
  cli::cli_h1("Loading OS Functions from GitHub")
  
  # 2. Define the API endpoint
  repo_url <- "https://api.github.com/repos/Dairy-Cow-Foot-Doc/os_functions/contents/"
  
  # 3. Tidy pipeline to fetch and source
  tryCatch({
    GET(repo_url) %>%
      content() %>%
      map_df(as_tibble) %>%
      filter(str_detect(name, "\\.[Rr]$")) %>%
      distinct(name, .keep_all = TRUE) %>%
      # download_url is the direct 'raw' path provided by GitHub API
      pull(download_url) %>%
      walk(~ {
        cli::cli_progress_step("Sourcing {.file {basename(.x)}}")
        source(.x)
      })
    
    cli::cli_alert_success("All functions loaded successfully!")
    
  }, error = function(e) {
    cli::cli_abort(c(
      "x" = "Failed to load functions from GitHub.",
      "i" = "Error detail: {e$message}",
      "!" = "Check your internet connection or GitHub API rate limits."
    ))
  })
}

# To use it, just run:
# fxn_load_os_functions()