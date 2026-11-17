# =========================================================================
# config/config.R
# 全部可修改设置集中在此文件。仓库内其它脚本不得出现硬编码路径或建模选择。
# All user-editable settings. No other script should hard-code paths or
# modelling choices.
# =========================================================================

## -------------------------------------------------------------------------
## 0. 工程根目录
## -------------------------------------------------------------------------
## 约定：始终在仓库根目录下运行（Rscript run_all.R 或 source("run_all.R")）。
PROJECT_ROOT <- normalizePath(".", mustWork = TRUE)

paths <- list(
  raw_data   = file.path(PROJECT_ROOT, "data", "Modeldata2.xlsx"),
  ext_data   = file.path(PROJECT_ROOT, "data", "External_validation_data.xlsx"),
  cache      = file.path(PROJECT_ROOT, "cache"),
  models     = file.path(PROJECT_ROOT, "cache", "models"),
  tables     = file.path(PROJECT_ROOT, "outputs", "tables"),
  figures    = file.path(PROJECT_ROOT, "outputs", "figures"),
  plot_data  = file.path(PROJECT_ROOT, "outputs", "plot_data")
)

invisible(lapply(
  unique(unlist(paths[c("cache", "models", "tables", "figures", "plot_data")])),
  dir.create, recursive = TRUE, showWarnings = FALSE
))

## 是否忽略已有缓存、强制全量重跑
reset_cache <- FALSE

## -------------------------------------------------------------------------
## 1. 全局参数
## -------------------------------------------------------------------------
seed_val            <- 123
outcome_var         <- "F1RFS"
positive_class      <- "是"
negative_class      <- "否"

train_fraction      <- 0.70          # 训练集比例
m_imputations       <- 5             # 多重插补套数
maxit_mice          <- 10            # MICE 迭代次数
cv_folds            <- 5             # 交叉验证折数
cv_repeats          <- 3             # 重复次数（5-fold x 3 = 15 次评估）
smote_ratio_cutoff  <- 2             # 类别不平衡 > 2:1 时启用 SMOTE

p_threshold         <- 0.05          # 单因素初筛阈值
p_threshold_fallback<- 0.10          # 无变量通过时的放宽阈值
lasso_consensus_frac<- 0.60          # LASSO 共识：变量需出现在 >=60% 插补集
miss_cutoff_pct     <- 20            # 缺失率剔除阈值（%）
vars_keep_high_miss <- c("SOFA.前.", "E6.机械通气小时数")  # 高于阈值但临床保留
vars_clinical_exclude <- c("多系统")  # 依据临床共线性判断剔除
drop_columns        <- c("序号", "患者所在医院")
winsorize_var       <- "F4住院费用"

n_boot              <- 1000          # Bootstrap 次数
dca_thresholds      <- seq(0, 0.99, by = 0.01)

## SHAP 计算规模（降低可加速，但会牺牲稳定性）
shap_background_n   <- 100
shap_explain_n      <- 200

## -------------------------------------------------------------------------
## 2. 需要强制转为数值的变量
## -------------------------------------------------------------------------
vars_to_numeric <- c(
  "A2年龄.岁.", "A3身高.cm.", "A4体重.Kg.", "A6BMI", "B8.1剂量", "B9.1剂量", "B10.1剂量",
  "NRS2002.前.", "APACHE.前.", "SOFA.前.", "C1钾.前.", "C2钙.前.", "C3镁.前.", "C4钠.前.",
  "C5磷.前.", "C6白蛋白.前.", "D1钾.后.", "D2钙.后.", "D3镁.后.", "D4钠.后.", "D5磷.后.",
  "D6白蛋白.后.", "E1喂养前禁食小时数", "E6.机械通气小时数", "E9蛋白质", "F4住院费用"
)

## 在 Table 1 之前再次强制转数值（防止 read_excel 误判）
vars_force_numeric_tab1 <- c(
  "A2年龄.岁.", "A6BMI", "B8.1剂量", "B9.1剂量", "B10.1剂量",
  "C1钾.前.", "D1钾.后.", "D2钙.后.", "D5磷.后."
)

## -------------------------------------------------------------------------
## 3. 模型清单
## -------------------------------------------------------------------------
## 建模模型（顺序即训练顺序）
model_list <- c("LR", "KNN", "RF", "GBM", "SVM", "NN", "XGBoost", "NB")

## 需要做 SHAP / 瀑布图 / 风险分层 / 部署导出的模型。
## 说明：本仓库不输出"唯一最优模型"，下游分析对下列模型逐个执行。
## 若希望覆盖全部模型，请改为 model_list。
focus_models <- c("LR", "RF", "XGBoost", "NB")

## 变量重要性 + 变量个数-AUC 曲线所用的模型（仅 R 包内可直接取 Gini 的模型适用）
importance_model <- "RF"

## -------------------------------------------------------------------------
## 4. 必需的 R 包
## -------------------------------------------------------------------------
required_packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "caret", "mice", "glmnet",
  "brglm2", "randomForest", "gbm", "xgboost", "kernlab", "nnet",
  "e1071", "pROC", "PRROC", "car", "kernelshap", "shapviz"
)

## -------------------------------------------------------------------------
## 5. 依赖检查（不自动联网安装；缺失时给出安装命令）
## -------------------------------------------------------------------------
check_packages <- function(pkgs = required_packages, strict = TRUE) {
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    msg <- paste0(
      "缺少以下 R 包：", paste(missing_pkgs, collapse = ", "), "\n",
      "请先安装后重试，例如：\n",
      '  install.packages(c(',
      paste0('"', missing_pkgs, '"', collapse = ", "), '))\n'
    )
    if (strict) stop(msg, call. = FALSE) else message(msg)
  }
  invisible(length(missing_pkgs) == 0)
}

check_packages(strict = FALSE)

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2)
  library(caret);  library(mice);  library(glmnet);  library(pROC)
  library(PRROC);  library(car);   library(randomForest)
  library(gbm);    library(xgboost); library(kernlab); library(nnet)
  library(e1071);  library(brglm2)
})
