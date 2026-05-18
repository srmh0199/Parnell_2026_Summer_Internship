log_msg <- function(msg) {
  cat(sprintf("[%s] %s\n", Sys.time(), msg))
  flush.console()
}

safe_write_rds <- function(object, path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_rds(object, path, ...)
}
safe_write_csv <- function(object, path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(object, path, ...)
}

safe_write_parquet <- function(object, path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(object, path, ...)
}
safe_write <- function(object, path, write_func, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_func(object, path, ...)
}

log_all_objects <- function(loop_label = "") {
  gc_result <- gc(reset = FALSE)
  total_mb <- sum(gc_result[, 2])
  
  message(sprintf("\n===== MEMORY SNAPSHOT %s =====", loop_label))
  message(sprintf("Total R memory usage: %.1f MB", total_mb))
  
  # Get all objects in global environment
  objs <- ls(envir = .GlobalEnv)
  if (length(objs) > 0) {
    sizes <- sapply(objs, function(x) {
      object.size(get(x, envir = .GlobalEnv))
    })
    # Sort descending
    sizes <- sort(sizes, decreasing = TRUE)
    
    message(sprintf("\n%-40s %10s", "Object", "Size (MB)"))
    message(paste(rep("-", 52), collapse = ""))
    for (i in seq_along(sizes)) {
      message(sprintf("%-40s %10.2f", names(sizes)[i], sizes[i] / 1024^2))
    }
    message(paste(rep("-", 52), collapse = ""))
    message(sprintf("%-40s %10.2f", "SUM of global objects", sum(sizes) / 1024^2))
  }
  
  message(sprintf("%-40s %10.2f", "Total R process memory (gc)", total_mb))
  message("===== END SNAPSHOT =====\n")
}