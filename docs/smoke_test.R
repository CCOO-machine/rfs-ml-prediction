# =========================================================================
# docs/smoke_test.R
# 端到端冒烟测试：用合成数据把 01-08 全流程跑一遍（小样本、少折数）。
# 不接触真实数据，所有输入输出写入临时目录。
#
# 用法：Rscript docs/smoke_test.R
# =========================================================================

set.seed(42)

## ---- 临时工作区 ----
tmp <- file.path(tempdir(), "rfs_smoke")
unlink(tmp, recursive = TRUE)
dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
data_dir <- file.path(tmp, "data"); dir.create(data_dir)

## ---- 合成数据 ----
n <- 300
make_cohort <- function(n, seed) {
  set.seed(seed)
  age     <- round(rnorm(n, 62, 14)); age <- pmin(pmax(age, 18), 95)
  bmi     <- round(rnorm(n, 23, 4), 1)
  nrs     <- pmin(rpois(n, 3), 7)
  apache  <- round(rnorm(n, 18, 6))
  sofa    <- round(rnorm(n, 7, 3))
  p_pre   <- round(rnorm(n, 1.05, 0.28), 2)
  p_post  <- round(p_pre - rnorm(n, 0.15, 0.18), 2)
  alb_pre <- round(rnorm(n, 32, 5), 1)
  alb_post<- round(alb_pre - rnorm(n, 3, 2.5), 1)
  fast_h  <- round(runif(n, 0, 48))
  mv_h    <- round(runif(n, 0, 240))
  protein <- round(rnorm(n, 55, 18))
  cost    <- round(rlnorm(n, 10.5, 0.6))
  dx      <- sample(c("sepsis", "respiratory", "neurological", "other"), n, replace = TRUE)
  multi   <- factor(sample(c("yes", "no"), n, replace = TRUE))

  lp <- -4.2 + 0.05 * (age - 62) + 0.42 * nrs - 1.9 * (p_pre - 1.05) +
        0.07 * apache + 0.9 * (fast_h > 24)
  pr <- 1 / (1 + exp(-lp))
  y  <- factor(ifelse(runif(n) < pr, "是", "否"), levels = c("否", "是"))

  df <- data.frame(
    `序号` = seq_len(n), `患者所在医院` = sample(c("A", "B", "C"), n, replace = TRUE),
    `A2年龄.岁.` = age, `A6BMI` = bmi,
    `NRS2002.前.` = nrs, `APACHE.前.` = apache, `SOFA.前.` = sofa,
    `C5磷.前.` = p_pre, `D5磷.后.` = p_post,
    `C6白蛋白.前.` = alb_pre, `D6白蛋白.后.` = alb_post,
    `E1喂养前禁食小时数` = fast_h, `E6.机械通气小时数` = mv_h,
    `E9蛋白质` = protein, `F4住院费用` = cost,
    `A7主要诊断` = factor(dx), `多系统` = multi,
    `F1RFS` = y,
    check.names = FALSE
  )

  ## 制造缺失，触发 MICE
  for (v in c("C5磷.前.", "D6白蛋白.后.", "E6.机械通气小时数")) {
    idx <- sample(n, round(n * 0.12))
    df[[v]][idx] <- NA
  }
  df
}

train_df <- make_cohort(n, 1)
ext_df   <- make_cohort(round(n * 0.6), 2)

write.csv(train_df, file.path(data_dir, "Modeldata2.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(ext_df,   file.path(data_dir, "External_validation_data.csv"), row.names = FALSE, fileEncoding = "UTF-8")

## ---- 加载工程（含 config，之后再覆盖参数）----
source("config/config.R")
source("R/utils.R"); source("R/preprocess.R"); source("R/metrics.R"); source("R/models.R")

## ---- 缩小规模、重定向输出到临时目录 ----
paths$raw_data  <- file.path(data_dir, "Modeldata2.csv")
paths$ext_data  <- file.path(data_dir, "External_validation_data.csv")
paths$cache     <- file.path(tmp, "cache")
paths$models    <- file.path(tmp, "cache", "models")
paths$tables    <- file.path(tmp, "outputs", "tables")
paths$figures   <- file.path(tmp, "outputs", "figures")
paths$plot_data <- file.path(tmp, "outputs", "plot_data")
invisible(lapply(unique(unlist(paths[c("cache","models","tables","figures","plot_data")])),
                 dir.create, recursive = TRUE, showWarnings = FALSE))
unlink(list.files(paths$cache, full.names = TRUE), recursive = TRUE)

cv_folds <- 3
cv_repeats <- 1
m_imputations <- 2
maxit_mice <- 3
n_boot <- 50
shap_background_n <- 30
shap_explain_n <- 20
model_list <- c("LR", "RF", "XGBoost", "NB")
focus_model <- "LR"
importance_model <- "RF"
smote_ratio_cutoff <- 10    # 阈值调高即可关闭 SMOTE

## ---- 逐阶段执行 ----
stages <- c("scripts/01_data_prep.R", "scripts/02_impute_select.R",
            "scripts/03_train_models.R", "scripts/04_evaluate.R",
            "scripts/05_interpret_and_figures.R", "scripts/06_risk_stratify.R",
            "scripts/07_export_plot_data.R", "scripts/08_external_validation.R",
            "scripts/99_session_info.R")

for (.stage_file in stages) {
  cat("\n", strrep("=", 78), "\n>>> ", .stage_file, "\n", strrep("=", 78), "\n", sep = "")
  ok <- tryCatch({ sys.source(.stage_file, envir = globalenv()); TRUE },
                 error = function(e) { cat("\n!! 失败：", conditionMessage(e), "\n"); FALSE })
  if (!ok) { cat("\nSMOKE TEST FAILED at ", .stage_file, "\n", sep = ""); quit(status = 1) }
}

## ---- 校验产物 ----
tb <- list.files(paths$tables)
fg <- list.files(paths$figures)
cat("\n表格文件：", length(tb), "\n"); print(tb)
cat("\n图形文件：", length(fg), "\n"); print(fg)

## 可选：把产物复制到持久目录以便人工检查（Rscript 的 tempdir 会话结束即删）
out_env <- Sys.getenv("SMOKE_OUT")
if (nzchar(out_env)) {
  for (sub in c("tables", "figures", "plot_data")) {
    dir.create(file.path(out_env, sub), recursive = TRUE, showWarnings = FALSE)
  }
  copy_dir <- function(from, to) {
    fs <- list.files(from, full.names = TRUE)
    if (length(fs) > 0) file.copy(fs, to, overwrite = TRUE)
  }
  copy_dir(paths$tables,   file.path(out_env, "tables"))
  copy_dir(paths$figures,  file.path(out_env, "figures"))
  copy_dir(paths$plot_data, file.path(out_env, "plot_data"))
  cat("\n产物已复制到：", out_env, "\n")
}

cat("\nSMOKE TEST PASSED\n")
