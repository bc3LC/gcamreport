devtools::load_all(".", reset = TRUE, quiet = TRUE)

t0 <- Sys.time()
result <- tryCatch({
  generate_report(
    db_path = "C:/Users/pjhan/Desktop/GCAM/gcam-v9.1-Windows-Release-Package/output",
    db_name = "database_basexdb",
    prj_name = "fork_v9.1_test_ALL.dat",
    scenarios = "Reference", final_year = 2050,
    desired_regions = "All", desired_variables = "All",
    GCAM_version = "v9.1", launch_ui = FALSE, save_output = TRUE
  )
  "SUCCESS"
}, error = function(e) {
  paste("ERROR:", conditionMessage(e))
})
t1 <- Sys.time()

cat("\n\n=== RESULT:", result, "===\n")
cat("=== TIME:", format(t1 - t0), "===\n")
