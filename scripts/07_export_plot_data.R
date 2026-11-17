# =========================================================================
# scripts/07_export_plot_data.R
# 导出绘图用数据（供 Python / R 重新作图），并汇总所有模型的预测概率
# 输出：outputs/plot_data/*
# =========================================================================

section("07 导出绘图数据")

stage03 <- get_or_load("stage03", "stage03_models")
stage04 <- get_or_load("stage04", "stage04_eval")

models_pooled <- stage03$models_pooled
primary_set   <- stage03$primary_set
y_train <- stage04$y_train
y_test  <- stage04$y_test

## -------------------------------------------------------------------------
## 1. 预测概率（训练集 / 测试集）
## -------------------------------------------------------------------------
pred_train <- data.frame(outcome = y_train, stringsAsFactors = FALSE)
pred_test  <- data.frame(outcome = y_test,  stringsAsFactors = FALSE)
for (m in names(models_pooled)) {
  pred_train[[m]] <- models_pooled[[m]]$train_prob
  pred_test[[m]]  <- models_pooled[[m]]$pooled_prob
}
write_plot_data(pred_train, "07_predictions_train.csv")
write_plot_data(pred_test,  "07_predictions_test.csv")

## -------------------------------------------------------------------------
## 2. ROC 曲线
## -------------------------------------------------------------------------
roc_export <- function(roc_list, split) {
  do.call(rbind, lapply(names(roc_list), function(m) {
    r <- roc_list[[m]]
    if (is.null(r)) return(NULL)
    data.frame(
      Model = m, Split = split,
      FPR = 1 - r$specificities, TPR = r$sensitivities,
      AUC = round(as.numeric(pROC::auc(r)), 4),
      stringsAsFactors = FALSE
    )
  }))
}
write_plot_data(roc_export(stage04$roc_train, "train"), "07_roc_train.csv")
write_plot_data(roc_export(stage04$roc_test,  "test"),  "07_roc_test.csv")

## -------------------------------------------------------------------------
## 3. 校准曲线（分箱）
## -------------------------------------------------------------------------
write_plot_data(stage04$cal_bins,    "07_calibration_bins.csv")
write_plot_data(stage04$cal_indices, "07_calibration_indices.csv")

## -------------------------------------------------------------------------
## 4. DCA
## -------------------------------------------------------------------------
write_plot_data(stage04$dca, "07_dca_curves.csv")

## -------------------------------------------------------------------------
## 5. 指标表
## -------------------------------------------------------------------------
write_plot_data(stage04$metrics,    "07_metrics_numeric.csv")
write_plot_data(stage04$overfit,    "07_overfitting.csv")
if (!is.null(stage04$delong)) write_plot_data(stage04$delong, "07_delong.csv")

## -------------------------------------------------------------------------
## 6. 变量清单
## -------------------------------------------------------------------------
write_plot_data(data.frame(variable = primary_set, stringsAsFactors = FALSE),
                "07_final_variables.csv")

log_step("绘图数据已导出到：", paths$plot_data)
