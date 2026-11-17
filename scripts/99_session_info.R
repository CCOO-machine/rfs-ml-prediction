# =========================================================================
# scripts/99_session_info.R
# 记录运行环境，便于复现（投稿时可直接作为补充材料）
# 输出：outputs/tables/99_session_info.txt
# =========================================================================

out_file <- file.path(paths$tables, "99_session_info.txt")
con <- file(out_file, open = "wt", encoding = "UTF-8")

cat("Repository : rfs-ml-prediction\n", file = con)
cat("Run at     : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = con)
cat("R version  : ", R.version.string, "\n", file = con)
cat("Platform   : ", R.version$platform, "\n\n", file = con)

cat("Package versions\n", file = con)
cat("-----------------\n", file = con)
pkgs <- sort(unique(c(required_packages, "kernelshap", "shapviz")))
for (p in pkgs) {
  v <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) "not installed")
  cat(sprintf("  %-14s %s\n", p, v), file = con)
}

cat("\nSession info\n------------\n", file = con)
utils::capture.output(utils::sessionInfo(), file = con, append = TRUE)

close(con)
message("已写出：", out_file)
