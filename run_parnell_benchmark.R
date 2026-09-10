# Launches ParnellBenchmark. Run from the project root (the .claude/launch.json
# preview config does). The port comes from the PORT env var when the Claude
# Code preview harness assigns one; 7789 is the standalone default.
port <- suppressWarnings(as.integer(Sys.getenv("PORT", "7789")))
if (is.na(port)) port <- 7789L
shiny::runApp("ParnellBenchmark", port = port, launch.browser = FALSE, host = "127.0.0.1")
