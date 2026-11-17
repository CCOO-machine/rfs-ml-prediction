# =========================================================================
# scripts/05_interpret_and_figures.R
# 图形输出 + 模型解释
#   - ROC / 校准 / DCA 汇总图
#   - 池化 logistic 的 OR 森林图
#   - 变量重要性（Gini）与"变量个数-AUC"曲线
#   - SHAP（对 config 中的 focus_model）
#   - 瀑布图（预测风险排序）
# 输出：outputs/figures/*, cache/stage05_interpret.rds
# =========================================================================

section("05 图形与解释")

stage03 <- get_or_load("stage03", "stage03_models")
stage04 <- get_or_load("stage04", "stage04_eval")

models_pooled  <- stage03$models_pooled
train_imp_list <- stage03$train_imp_list
test_imp_list  <- stage03$test_imp_list
primary_set    <- stage03$primary_set
metrics        <- stage04$metrics
y_test         <- stage04$y_test
y_train        <- stage04$y_train

## focus_model 必须存在
focus_model <- if (focus_model %in% names(models_pooled)) focus_model else names(models_pooled)[1]
log_step("下游解释所用模型：", focus_model)

## -------------------------------------------------------------------------
## 1. ROC 汇总图
## -------------------------------------------------------------------------
roc_df <- do.call(rbind, lapply(names(stage04$roc_test), function(m) {
  r <- stage04$roc_test[[m]]
  if (is.null(r)) return(NULL)
  data.frame(
    FPR = 1 - r$specificities, TPR = r$sensitivities, Model = m,
    AUC = round(as.numeric(pROC::auc(r)), 3), stringsAsFactors = FALSE
  )
}))
if (!is.null(roc_df)) {
  roc_df$Label <- sprintf("%s (AUC=%.3f)", roc_df$Model, roc_df$AUC)
  p_roc <- ggplot(roc_df, aes(x = FPR, y = TPR, colour = Label)) +
    geom_line(linewidth = 0.9) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
    labs(title = "Test set ROC curves", x = "1 - Specificity", y = "Sensitivity",
         colour = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  save_fig(p_roc, "05_ROC_all_models.png", width = 8, height = 7)
}

## -------------------------------------------------------------------------
## 2. 校准曲线（测试集，按模型分面）
## -------------------------------------------------------------------------
cal_test <- stage04$cal_bins[stage04$cal_bins$Split == "test", , drop = FALSE]
if (nrow(cal_test) > 0) {
  p_cal <- ggplot(cal_test, aes(x = mean_pred, y = mean_obs)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
    geom_point(aes(size = n), colour = "firebrick", alpha = 0.85) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.02,
                  colour = "firebrick", alpha = 0.6) +
    facet_wrap(~ Model) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(title = "Calibration (test set, deciles)",
         x = "Predicted probability", y = "Observed proportion", size = "n") +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom")
  save_fig(p_cal, "05_calibration_test.png", width = 9, height = 7)
}

## -------------------------------------------------------------------------
## 3. DCA
## -------------------------------------------------------------------------
dca_test <- stage04$dca[stage04$dca$Split == "test", , drop = FALSE]
ref_test <- dca_reference_lines(y_test)
p_dca <- ggplot(dca_test, aes(x = threshold, y = net_benefit, colour = Model)) +
  geom_line(linewidth = 0.9) +
  geom_line(data = ref_test$none, aes(x = threshold, y = net_benefit),
            linetype = "dotted", inherit.aes = FALSE) +
  geom_line(data = ref_test$all, aes(x = threshold, y = net_benefit),
            linetype = "dashed", inherit.aes = FALSE) +
  coord_cartesian(xlim = c(0, 0.8), ylim = c(-0.05, max(dca_test$net_benefit, na.rm = TRUE) + 0.05)) +
  labs(title = "Decision curve analysis (test set)",
       x = "Threshold probability", y = "Net benefit", colour = NULL) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")
save_fig(p_dca, "05_DCA_test.png", width = 8, height = 6)

## -------------------------------------------------------------------------
## 4. 池化 logistic 的 OR 森林图
## -------------------------------------------------------------------------
glm_fits <- lapply(train_imp_list, function(d) {
  stats::glm(stats::as.formula(paste(outcome_var, "~ .")), data = d, family = stats::binomial())
})
pooled <- summary(mice::pool(glm_fits))
coef_df <- pooled[pooled$term != "(Intercept)", , drop = FALSE]
coef_df$OR    <- exp(coef_df$estimate)
coef_df$Lower <- exp(coef_df$estimate - 1.96 * coef_df$std.error)
coef_df$Upper <- exp(coef_df$estimate + 1.96 * coef_df$std.error)
coef_df <- coef_df[is.finite(coef_df$OR) & is.finite(coef_df$Lower) & is.finite(coef_df$Upper), , drop = FALSE]

if (nrow(coef_df) > 0) {
  coef_df <- coef_df[order(coef_df$OR), , drop = FALSE]
  coef_df$Label <- factor(coef_df$term, levels = coef_df$term)
  p_forest <- ggplot(coef_df, aes(x = OR, y = Label)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_errorbar(aes(xmin = Lower, xmax = Upper), orientation = "y", width = 0.2, colour = "steelblue") +
    geom_point(shape = 18, size = 3, colour = "steelblue") +
    scale_x_log10() +
    geom_text(aes(label = sprintf("%.2f (%.2f-%.2f)", OR, Lower, Upper)),
              hjust = -0.1, size = 3, colour = "grey30") +
    labs(title = "Pooled logistic regression (OR, 95% CI)", x = "Odds ratio", y = NULL) +
    theme_minimal(base_size = 12)
  save_fig(p_forest, "05_forest_pooled_logistic.png",
           width = 9, height = max(4, nrow(coef_df) * 0.45), dpi = 300)
  write_table(coef_df[, c("term", "OR", "Lower", "Upper", "p.value")],
              "05_pooled_logistic_OR.csv")
}

## -------------------------------------------------------------------------
## 5. 变量重要性 + 变量个数-AUC 曲线（importance_model）
## -------------------------------------------------------------------------
var_imp <- NULL
imp_model <- if (importance_model %in% names(models_pooled)) importance_model else NA

if (!is.na(imp_model) && imp_model == "RF") {
  rf_imps <- lapply(models_pooled$RF$models, function(m) {
    if (is.null(m)) return(NULL)
    randomForest::importance(m$finalModel)[, "MeanDecreaseGini"]
  })
  rf_imps <- rf_imps[!vapply(rf_imps, is.null, logical(1))]

  if (length(rf_imps) > 0) {
    all_vars <- names(rf_imps[[1]])
    imp_mat  <- sapply(rf_imps, function(x) x[all_vars])
    rownames(imp_mat) <- all_vars
    avg_imp <- rowMeans(imp_mat, na.rm = TRUE)

    ## 因子哑变量并回原变量
    xlev <- models_pooled$RF$models[[1]]$xlevels
    orig_of <- vapply(all_vars, function(v) {
      for (var in names(xlev)) if (startsWith(v, var)) return(var)
      v
    }, character(1))
    var_imp <- sort(tapply(avg_imp, orig_of, sum), decreasing = TRUE)

    imp_df <- data.frame(variable = names(var_imp),
                         gini = as.numeric(var_imp), stringsAsFactors = FALSE)
    write_table(imp_df, "05_rf_variable_importance.csv")

    imp_df$variable <- factor(imp_df$variable, levels = rev(imp_df$variable))
    p_imp <- ggplot(imp_df, aes(x = variable, y = gini)) +
      geom_col(fill = "steelblue") + coord_flip() +
      labs(title = sprintf("Random forest variable importance (%s)", imp_model),
           x = NULL, y = "Mean decrease in Gini") +
      theme_minimal(base_size = 12)
    save_fig(p_imp, "05_rf_importance.png", width = 8, height = 6)
  }
}

## 变量个数-AUC 曲线（按重要性从高到低累加）
if (!is.null(var_imp) && length(var_imp) >= 3) {
  ranked_vars <- names(var_imp)
  auc_by_k <- rep(NA_real_, length(ranked_vars))
  rf_args  <- rf_extra_args(nrow(train_imp_list[[1]]))

  for (k in seq_along(ranked_vars)) {
    vars <- ranked_vars[1:k]
    tr_sub <- lapply(train_imp_list, function(d) d[, c(vars, outcome_var), drop = FALSE])
    te_sub <- lapply(test_imp_list,  function(d) d[, c(vars, outcome_var), drop = FALSE])
    p_k <- length(vars)
    grid_k <- data.frame(mtry = unique(pmin(pmax(
      round(seq(min(max(2, floor(sqrt(p_k) * 0.3)), p_k),
                min(max(3, floor(sqrt(p_k))), p_k), length.out = 5)), 1), p_k)))
    res <- train_across_imputations(
      "rf", tr_sub, te_sub, stage03$ctrl,
      tuneGrid = grid_k, ntree = rf_args$ntree, nodesize = rf_args$nodesize,
      maxnodes = rf_args$maxnodes, sampsize = rf_args$sampsize
    )
    auc_by_k[k] <- res$cv_auc
  }

  curve_df <- data.frame(
    n_variables = seq_along(ranked_vars),
    added_variable = ranked_vars,
    cv_auc = round(auc_by_k, 4),
    auc_gain = round(c(NA, diff(auc_by_k)), 4),
    stringsAsFactors = FALSE
  )
  write_table(curve_df, "05_variable_count_auc_curve.csv")

  p_curve <- ggplot(curve_df, aes(x = n_variables, y = cv_auc)) +
    geom_line(colour = "steelblue") + geom_point(size = 2, colour = "steelblue") +
    labs(title = "Variables retained vs. cross-validated AUC",
         x = "Number of variables (ranked by importance)",
         y = "Cross-validated AUC") +
    theme_minimal(base_size = 12)
  save_fig(p_curve, "05_variable_count_auc_curve.png", width = 8, height = 5)
}

## -------------------------------------------------------------------------
## 6. SHAP（focus_model）
## -------------------------------------------------------------------------
shap_result <- NULL

if (requireNamespace("kernelshap", quietly = TRUE)) {
  fit <- models_pooled[[focus_model]]$models[[1]]

  if (!is.null(fit)) {
    X_bg <- train_imp_list[[1]][, setdiff(names(train_imp_list[[1]]), outcome_var), drop = FALSE]
    X_ex <- test_imp_list[[1]][,  setdiff(names(test_imp_list[[1]]),  outcome_var), drop = FALSE]

    set.seed(seed_val)
    bg <- X_bg[sample.int(nrow(X_bg), min(shap_background_n, nrow(X_bg))), , drop = FALSE]
    Xe <- X_ex[sample.int(nrow(X_ex), min(shap_explain_n, nrow(X_ex))), , drop = FALSE]

    pred_fun <- if (models_pooled[[focus_model]]$type == "xgb") {
      function(object, newdata) {
        mm <- stats::model.matrix(~ . - 1, data = newdata)
        mm <- align_matrix_columns(mm, models_pooled[[focus_model]]$recipe$cols)
        as.numeric(stats::predict(object, newdata = xgboost::xgb.DMatrix(data = mm, missing = NA)))
      }
    } else {
      function(object, newdata) as.numeric(stats::predict(object, newdata = newdata, type = "prob")[, positive_class])
    }

    bg_mm <- if (models_pooled[[focus_model]]$type == "xgb") {
      align_matrix_columns(stats::model.matrix(~ . - 1, data = bg), models_pooled[[focus_model]]$recipe$cols)
    } else bg
    Xe_mm <- if (models_pooled[[focus_model]]$type == "xgb") {
      align_matrix_columns(stats::model.matrix(~ . - 1, data = Xe), models_pooled[[focus_model]]$recipe$cols)
    } else Xe

    set.seed(seed_val)
    shap <- tryCatch(
      kernelshap::kernelshap(object = fit, X = Xe_mm, bg_X = bg_mm,
                             pred_fun = pred_fun, parallel = FALSE),
      error = function(e) { log_step("SHAP 计算失败：", conditionMessage(e)); NULL }
    )

    if (!is.null(shap)) {
      S_df <- as.data.frame(shap$S)
      X_df <- as.data.frame(shap$X)
      mean_abs <- colMeans(abs(S_df), na.rm = TRUE)
      shap_imp <- data.frame(
        variable = names(mean_abs),
        mean_abs_shap = as.numeric(mean_abs),
        relative_pct = round(100 * as.numeric(mean_abs) / max(mean_abs), 1),
        stringsAsFactors = FALSE
      )
      shap_imp <- shap_imp[order(-shap_imp$mean_abs_shap), , drop = FALSE]
      rownames(shap_imp) <- NULL
      write_table(shap_imp, "05_shap_importance.csv")
      write_plot_data(S_df, "05_shap_values.csv")
      write_plot_data(X_df, "05_shap_feature_values.csv")

      ## 重要性条形图
      top <- shap_imp[1:min(15, nrow(shap_imp)), , drop = FALSE]
      top$variable <- factor(top$variable, levels = rev(top$variable))
      p_shap <- ggplot(top, aes(x = variable, y = mean_abs_shap)) +
        geom_col(fill = "#4575B4") + coord_flip() +
        labs(title = sprintf("SHAP importance (%s)", focus_model),
             x = NULL, y = "Mean |SHAP|") +
        theme_minimal(base_size = 12)
      save_fig(p_shap, "05_shap_importance.png", width = 8, height = 6)
      shap_result <- list(S_df = S_df, X_df = X_df, importance = shap_imp)
    }
  }
}

## -------------------------------------------------------------------------
## 7. 瀑布图：按预测风险排序（focus_model）
## -------------------------------------------------------------------------
wf <- data.frame(
  patient_id = seq_along(models_pooled[[focus_model]]$pooled_prob),
  risk = models_pooled[[focus_model]]$pooled_prob,
  outcome = ifelse(y_test == 1, "Event", "No event"),
  stringsAsFactors = FALSE
)
wf <- wf[order(-wf$risk), , drop = FALSE]
wf$rank <- seq_len(nrow(wf))

p_wf <- ggplot(wf, aes(x = rank, y = risk, fill = outcome)) +
  geom_col(width = 1, alpha = 0.85) +
  scale_fill_manual(values = c("Event" = "#D53E4F", "No event" = "#3288BD"), name = "Outcome") +
  labs(title = sprintf("Predicted risk per patient - %s (test set)", focus_model),
       x = "Patients (ranked by predicted risk)", y = "Predicted risk") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
save_fig(p_wf, "05_waterfall_predicted_risk.png", width = 10, height = 5)
write_plot_data(wf, "05_waterfall_data.csv")

## -------------------------------------------------------------------------
## 8. 保存
## -------------------------------------------------------------------------
stage05 <- list(
  focus_model  = focus_model,
  var_importance = var_imp,
  shap         = shap_result
)
save_stage(stage05, "stage05_interpret")
log_step("已保存阶段缓存：stage05_interpret")
