# =========================================================================
# scripts/04_evaluate.R
# 判别力 / 校准 / 临床效用 / 过拟合诊断 / 模型间 DeLong 检验
# 输出：cache/stage04_eval.rds + outputs/tables/*
#
# 说明：所有阈值（Youden）均在训练集上确定，再固定应用到测试集，
#       避免用测试集调阈值造成的乐观偏倚（原脚本在测试集上取阈值）。
# =========================================================================

section("04 模型评估")

stage03 <- get_or_load("stage03", "stage03_models")
models_pooled  <- stage03$models_pooled
train_imp_list <- stage03$train_imp_list
test_imp_list  <- stage03$test_imp_list
primary_set    <- stage03$primary_set

true_label_train <- train_imp_list[[1]][[outcome_var]]
true_label_test  <- test_imp_list[[1]][[outcome_var]]
y_train <- encode_y(true_label_train)
y_test  <- encode_y(true_label_test)

## -------------------------------------------------------------------------
## 1. ROC + 全指标（含 Bootstrap 95% CI）
## -------------------------------------------------------------------------
roc_train_list <- list()
roc_test_list  <- list()
metric_rows    <- list()

for (m in names(models_pooled)) {
  log_step("评估 ", m)
  tr_prob <- models_pooled[[m]]$train_prob
  te_prob <- models_pooled[[m]]$pooled_prob
  if (is.null(tr_prob) || is.null(te_prob)) next

  roc_train_list[[m]] <- tryCatch(pROC::roc(true_label_train, tr_prob, quiet = TRUE,
                                            levels = c(negative_class, positive_class)),
                                  error = function(e) NULL)
  roc_test_list[[m]]  <- tryCatch(pROC::roc(true_label_test, te_prob, quiet = TRUE,
                                            levels = c(negative_class, positive_class)),
                                  error = function(e) NULL)

  ## 训练集上定阈值
  thr <- youden_threshold(tr_prob, true_label_train)

  ci_auc_tr    <- bootstrap_ci(tr_prob, true_label_train, calc_auc)
  ci_auc_te    <- bootstrap_ci(te_prob, true_label_test,  calc_auc)
  ci_auprc_tr  <- bootstrap_ci(tr_prob, true_label_train, calc_auprc)
  ci_auprc_te  <- bootstrap_ci(te_prob, true_label_test,  calc_auprc)
  ci_brier_tr  <- bootstrap_ci(tr_prob, true_label_train, calc_brier)
  ci_brier_te  <- bootstrap_ci(te_prob, true_label_test,  calc_brier)

  m_tr <- calc_class_metrics(tr_prob, true_label_train, thr)
  m_te <- calc_class_metrics(te_prob, true_label_test,  thr)

  metric_rows[[m]] <- data.frame(
    Model = m,
    Threshold = thr,
    Train_AUC   = ci_auc_tr[["est"]],   Train_AUC_lower   = ci_auc_tr[["lower"]],   Train_AUC_upper   = ci_auc_tr[["upper"]],
    Test_AUC    = ci_auc_te[["est"]],   Test_AUC_lower    = ci_auc_te[["lower"]],   Test_AUC_upper    = ci_auc_te[["upper"]],
    Train_AUPRC = ci_auprc_tr[["est"]], Train_AUPRC_lower = ci_auprc_tr[["lower"]], Train_AUPRC_upper = ci_auprc_tr[["upper"]],
    Test_AUPRC  = ci_auprc_te[["est"]], Test_AUPRC_lower  = ci_auprc_te[["lower"]], Test_AUPRC_upper  = ci_auprc_te[["upper"]],
    Train_Brier = ci_brier_tr[["est"]], Train_Brier_lower = ci_brier_tr[["lower"]], Train_Brier_upper = ci_brier_tr[["upper"]],
    Test_Brier  = ci_brier_te[["est"]], Test_Brier_lower  = ci_brier_te[["lower"]], Test_Brier_upper  = ci_brier_te[["upper"]],
    Train_Sensitivity = m_tr$Sensitivity, Train_Specificity = m_tr$Specificity,
    Train_PPV = m_tr$PPV, Train_NPV = m_tr$NPV, Train_F1 = m_tr$F1, Train_Accuracy = m_tr$Accuracy,
    Test_Sensitivity  = m_te$Sensitivity, Test_Specificity  = m_te$Specificity,
    Test_PPV = m_te$PPV, Test_NPV = m_te$NPV, Test_F1 = m_te$F1, Test_Accuracy = m_te$Accuracy,
    CV_AUC = models_pooled[[m]]$cv_auc,
    stringsAsFactors = FALSE
  )
}

metrics <- do.call(rbind, metric_rows)
rownames(metrics) <- NULL

## 可读版本（论文制表用）
metrics_pretty <- metrics
for (base in c("AUC", "AUPRC", "Brier")) {
  metrics_pretty[[paste0("Train_", base)]] <- fmt_ci(metrics[[paste0("Train_", base)]],
                                                     metrics[[paste0("Train_", base, "_lower")]],
                                                     metrics[[paste0("Train_", base, "_upper")]])
  metrics_pretty[[paste0("Test_", base)]]  <- fmt_ci(metrics[[paste0("Test_", base)]],
                                                     metrics[[paste0("Test_", base, "_lower")]],
                                                     metrics[[paste0("Test_", base, "_upper")]])
}
keep_cols <- c("Model", "Threshold",
               "Train_AUC", "Train_AUPRC", "Train_Brier",
               "Train_Sensitivity", "Train_Specificity", "Train_PPV", "Train_NPV",
               "Train_Accuracy", "Train_F1",
               "Test_AUC", "Test_AUPRC", "Test_Brier",
               "Test_Sensitivity", "Test_Specificity", "Test_PPV", "Test_NPV",
               "Test_Accuracy", "Test_F1", "CV_AUC")
write_table(metrics, "04_model_metrics.csv")
write_table(metrics_pretty[, keep_cols], "04_model_metrics_formatted.csv")

cat("\n===== 模型性能（测试集，阈值来自训练集）=====\n")
print(metrics_pretty[, c("Model", "Threshold", "Test_AUC", "Test_AUPRC", "Test_Brier",
                         "Test_Sensitivity", "Test_Specificity", "CV_AUC")],
      row.names = FALSE)

## -------------------------------------------------------------------------
## 2. 过拟合诊断
## -------------------------------------------------------------------------
overfit_check <- do.call(rbind, lapply(seq_len(nrow(metrics)), function(i) {
  r <- metrics[i, ]
  cbind(Model = r$Model, overfit_diagnosis(r$Train_AUC, r$Test_AUC, r$CV_AUC))
}))
write_table(overfit_check, "04_overfitting_diagnostics.csv")
cat("\n===== 过拟合诊断 =====\n")
print(overfit_check[, c("Model", "Train_AUC", "Test_AUC", "CV_AUC", "Train_Test_Gap", "Diagnosis")],
      row.names = FALSE)

## -------------------------------------------------------------------------
## 3. DeLong 检验（模型间 AUC 差异）
## -------------------------------------------------------------------------
delong_rows <- list()
roc_names <- names(roc_test_list)[!vapply(roc_test_list, is.null, logical(1))]
if (length(roc_names) >= 2) {
  for (i in seq_len(length(roc_names) - 1)) {
    for (j in (i + 1):length(roc_names)) {
      a <- roc_names[i]; b <- roc_names[j]
      p <- tryCatch(
        pROC::roc.test(roc_test_list[[a]], roc_test_list[[b]], method = "delong")$p.value,
        error = function(e) NA_real_
      )
      delong_rows[[length(delong_rows) + 1]] <- data.frame(
        Model_1 = a, Model_2 = b,
        P_value = round(p, 4),
        Significant = ifelse(!is.na(p) && p < 0.05, "Yes", "No"),
        stringsAsFactors = FALSE
      )
    }
  }
}
delong <- do.call(rbind, delong_rows)
if (!is.null(delong)) {
  write_table(delong, "04_delong_test.csv")
  cat("\n===== DeLong 检验 =====\n")
  print(delong, row.names = FALSE)
}

## -------------------------------------------------------------------------
## 4. 校准（各模型，训练集 + 测试集）
## -------------------------------------------------------------------------
cal_rows <- list()
cal_slope_rows <- list()

for (m in names(models_pooled)) {
  for (split in c("train", "test")) {
    prob <- if (split == "train") models_pooled[[m]]$train_prob else models_pooled[[m]]$pooled_prob
    y    <- if (split == "train") y_train else y_test
    if (is.null(prob)) next

    bins <- calibration_bins(prob, y)
    if (nrow(bins) > 0) {
      bins$Model <- m; bins$Split <- split
      cal_rows[[length(cal_rows) + 1]] <- bins
    }
    si <- calibration_slope_intercept(prob, y)
    cal_slope_rows[[length(cal_slope_rows) + 1]] <- data.frame(
      Model = m, Split = split, Intercept = si$intercept, Slope = si$slope,
      ECI = calibration_eci(bins), stringsAsFactors = FALSE
    )
  }
}

cal_bins_all  <- do.call(rbind, cal_rows)
cal_indices   <- do.call(rbind, cal_slope_rows)
write_table(cal_bins_all, "04_calibration_bins.csv")
write_table(cal_indices, "04_calibration_slope_intercept.csv")

cat("\n===== 校准指标 =====\n")
print(cal_indices[cal_indices$Split == "test", ], row.names = FALSE)

## -------------------------------------------------------------------------
## 5. DCA（训练集 + 测试集）
## -------------------------------------------------------------------------
dca_rows <- list()
for (m in names(models_pooled)) {
  for (split in c("train", "test")) {
    prob <- if (split == "train") models_pooled[[m]]$train_prob else models_pooled[[m]]$pooled_prob
    y    <- if (split == "train") y_train else y_test
    if (is.null(prob)) next
    d <- compute_dca(y, prob)
    d$Model <- m; d$Split <- split
    dca_rows[[length(dca_rows) + 1]] <- d
  }
}
dca_all <- do.call(rbind, dca_rows)

## 参考线（列名已统一为 Model / Split）
ref_test  <- dca_reference_lines(y_test)
ref_train <- dca_reference_lines(y_train)
ref_rows <- rbind(
  cbind(ref_train$none, Split = "train"), cbind(ref_train$all, Split = "train"),
  cbind(ref_test$none,  Split = "test"),  cbind(ref_test$all,  Split = "test")
)
write_table(rbind(dca_all, ref_rows), "04_dca_curves.csv")

## -------------------------------------------------------------------------
## 6. 保存
## -------------------------------------------------------------------------
stage04 <- list(
  metrics       = metrics,
  metrics_pretty = metrics_pretty,
  overfit       = overfit_check,
  delong        = delong,
  cal_bins      = cal_bins_all,
  cal_indices   = cal_indices,
  dca           = dca_all,
  roc_train     = roc_train_list,
  roc_test      = roc_test_list,
  y_train       = y_train,
  y_test        = y_test
)
save_stage(stage04, "stage04_eval")
log_step("已保存阶段缓存：stage04_eval")
