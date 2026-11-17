files <- c(
  list.files("config", pattern = "[.]R$", full.names = TRUE),
  list.files("R",      pattern = "[.]R$", full.names = TRUE),
  list.files("scripts",pattern = "[.]R$", full.names = TRUE),
  "run_all.R"
)
bad <- 0
for (f in files) {
  res <- tryCatch({ invisible(parse(f, encoding = "UTF-8")); "OK" },
                  error = function(e) paste("PARSE ERROR:", conditionMessage(e)))
  if (res != "OK") bad <- bad + 1
  cat(sprintf("%-45s %s\n", f, res))
}
cat("\nfiles:", length(files), " problems:", bad, "\n")
