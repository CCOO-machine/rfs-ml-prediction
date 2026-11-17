# =========================================================================
# scripts/08_external_validation.R
# 外部数据验证：加载训练好的模型 -> 预处理对齐 -> 插补 -> 预测 -> 指标
#
# 与原始脚本的差异：
#   * 已按要求删除"模型再校准（最优模型）"整节，不再估计/施加校准系数；
#   * 不再依赖 best_model_name 字段，全部模型逐一验证；
#   * 路径、随机种子、插补套数全部来自 config/config.R。
# =========================================================================

section("08 外部验证")

## -------------------------------------------------------------------------
## 1. 加载训练阶段的产物
## -------------------------------------------------------------------------
var_info_file <- file.path(paths$models, "var_info.rds")
if (!file.exists(var_info_file)) {
  stop("找不到 ", var_info_file, "，请先完成 03_train_models.R。", call. = FALSE)
}

var_info        <- readRDS(var_info_file)
primary_set     <- var_info$primary_set
all_model_names <- var_info$all_model_names
factor_levels   <- var_info$factor_levels
factor_vars     <- var_info$factor_vars
train_ref       <- readRDS(file.path(paths$models, "train_ref.rds"))
xgb_recipe      <- tryCatch(readRDS(file.path(paths$models, "xgb_recipe.rds")),
                            error = function(e) NULL)

log_step("建模变量：", paste(primary_set, collapse = ", "))

models_external <- list()
for (m in all_model_names) {
  f <- file.path(paths$models, paste0("models_", m, ".rds"))
  if (file.exists(f)) models_external[[m]] <- readRDS(f)
}
if (length(models_external) == 0) stop("没有可用的模型文件。", call. = FALSE)
log_step("已加载模型：", paste(names(models_external), collapse = ", "))

## -------------------------------------------------------------------------
## 2. 读入外部数据
## -------------------------------------------------------------------------
ext_raw <- read_input_data(paths$ext_data) |>
  standardise_columns() |>
  coerce_base_types()

if (!outcome_var %in% names(ext_raw)) {
  stop("外部数据缺少结局变量 ", outcome_var, call. = FALSE)
}
log_step(sprintf("外部数据：%d 行 x %d 列", nrow(ext_raw), ncol(ext_raw)))
print(table(as_outcome_factor(ext_raw[[outcome_var]]), useNA = "ifany"))

## -------------------------------------------------------------------------
## 3. 预处理与变量对齐
## -------------------------------------------------------------------------
ext_clean <- preprocess_data(ext_raw)

missing_vars <- setdiff(primary_set, names(ext_clean))
for (v in missing_vars) {
  if (v %in% factor_vars) {
    ext_clean[[v]] <- factor(NA_character_, levels = factor_levels[[v]])
  } else {
    ext_clean[[v]] <- NA_real_
  }
  log_step("外部数据缺少变量，置为 NA 待插补：", v)
}

ext_model <- ext_clean[, primary_set, drop = FALSE]
for (v in intersect(factor_vars, names(ext_model))) {
  lv <- levels(train_ref[[v]])
  if (is.null(lv)) next
  chr <- as.character(ext_model[[v]])
  novel <- setdiff(unique(chr[!is.na(chr)]), lv)
  if (length(novel) > 0) {
    chr[chr %in% novel] <- NA_character_
    log_step(sprintf("%s：%d 个训练集中未出现的水平被置为 NA", v, length(novel)))
  }
  ext_model[[v]] <- factor(chr, levels = lv)
}

ext_model[[outcome_var]] <- as_outcome_factor(ext_clean[[outcome_var]])
y_ext <- encode_y(ext_model[[outcome_var]])

## -------------------------------------------------------------------------
## 4. 外部数据缺失处理：与训练阶段一致的 CART 多重插补
## -------------------------------------------------------------------------
na_before <- sum(is.na(ext_model[, primary_set, drop = FALSE]))
log_step("插补前缺失值总数：", na_before)

if (na_before > 0) {
  to_impute <- ext_model[, primary_set, drop = FALSE]
  vars_na <- names(which(colSums(is.na(to_impute)) > 0))
  methods <- stats::setNames(rep("", length(primary_set)), primary_set)
  methods[vars_na] <- "cart"

  set.seed(seed_val)
  mids_ext <- mice::mice(to_impute, m = m_imputations, maxit = maxit_mice,
                         method = methods, printFlag = FALSE, seed = seed_val)
  ext_imp_list <- lapply(seq_len(m_imputations), function(i) {
    d <- mice::complete(mids_ext, action = i)
    d[[outcome_var]] <- ext_model[[outcome_var]]
    d
  })
} else {
  ext_imp_list <- list(ext_model)
}
log_step("插补套数：", length(ext_imp_list))

## -------------------------------------------------------------------------
## 5. 逐模型预测
## -------------------------------------------------------------------------
ext_prob <- list()

for (m in names(models_external)) {
  log_step("预测：", m)
  model_obj <- models_external[[m]]
  probs <- matrix(NA_real_, nrow = nrow(ext_imp_list[[1]]), ncol = length(ext_imp_list))

  for (k in seq_along(ext_imp_list)) {
    d <- ext_imp_list[[k]]
    fit <- if (is.list(model_obj)) model_obj[[min(k, length(model_obj))]] else model_obj

    probs[, k] <- tryCatch({
      if (var_info$all_model_names[which(var_info$all_model_names == m)] == "XGBoost") {
        robust_predict(fit, d[, primary_set, drop = FALSE],
                       positive = positive_class, recipe = xgb_recipe)
      } else if (m == "NB") {
        ## NB 训练时因子被编码为数值，此处保持同样处理
        nd <- d[, primary_set, drop = FALSE]
        for (v in names(nd)) {
          if (is.factor(nd[[v]])) nd[[v]] <- as.numeric(nd[[v]])
          if (any(is.na(nd[[v]]))) nd[[v]][is.na(nd[[v]])] <- stats::median(nd[[v]], na.rm = TRUE)
        }
        robust_predict(fit, nd, positive = positive_class)
      } else {
        robust_predict(fit, d[, primary_set, drop = FALSE], positive = positive_class)
      }
    }, error = function(e) {
      log_step("  插补集 ", k, " 预测失败：", conditionMessage(e))
      rep(NA_real_, nrow(d))
    })
  }

  ext_prob[[m]] <- rowMeans(probs, na.rm = TRUE)
  if (all(!is.finite(ext_prob[[m]]))) {
    log_step("  该模型外部预测全部失败，跳过")
    ext_prob[[m]] <- NULL
  }
}

if (length(ext_prob) == 0) stop("所有模型外部预测失败，请检查数据格式。", call. = FALSE)

## -------------------------------------------------------------------------
## 6. 外部验证指标
## -------------------------------------------------------------------------
ext_rows   <- list()
ext_roc    <- list()
ext_cal    <- list()

for (m in names(ext_prob)) {
  p <- ext_prob[[m]]
  ## 阈值仍沿用训练集原则：在外部数据上按 Youden 取值仅用于展示，
  ## 论文中报告性能时以 AUC/AUPRC/Brier 为主。
  thr <- youden_threshold(p, ext_model[[outcome_var]])

  r <- tryCatch(pROC::roc(ext_model[[outcome_var]], p, quiet = TRUE,
                          levels = c(negative_class, positive_class)),
                error = function(e) NULL)
  if (!is.null(r)) ext_roc[[m]] <- r

  ci_auc   <- bootstrap_ci(p, ext_model[[outcome_var]], calc_auc)
  ci_auprc <- bootstrap_ci(p, ext_model[[outcome_var]], calc_auprc)
  ci_brier <- bootstrap_ci(p, ext_model[[outcome_var]], calc_brier)
  cm       <- calc_class_metrics(p, ext_model[[outcome_var]], thr)

  bins <- calibration_bins(p, y_ext)
  if (nrow(bins) > 0) { bins$Model <- m; ext_cal[[length(ext_cal) + 1]] <- bins }
  si <- calibration_slope_intercept(p, y_ext)

  ext_rows[[m]] <- data.frame(
    Model = m, Threshold = thr,
    AUC = ci_auc[["est"]], AUC_lower = ci_auc[["lower"]], AUC_upper = ci_auc[["upper"]],
    AUPRC = ci_auprc[["est"]], AUPRC_lower = ci_auprc[["lower"]], AUPRC_upper = ci_auprc[["upper"]],
    Brier = ci_brier[["est"]], Brier_lower = ci_brier[["lower"]], Brier_upper = ci_brier[["upper"]],
    Sensitivity = cm$Sensitivity, Specificity = cm$Specificity,
    PPV = cm$PPV, NPV = cm$NPV, F1 = cm$F1, Accuracy = cm$Accuracy,
    Cal_Intercept = si$intercept, Cal_Slope = si$slope,
    ECI = calibration_eci(bins),
    stringsAsFactors = FALSE
  )
}

ext_metrics <- do.call(rbind, ext_rows)
rownames(ext_metrics) <- NULL
write_table(ext_metrics, "08_external_validation_metrics.csv")

cat("\n===== 外部验证性能 =====\n")
print(ext_metrics[, c("Model", "AUC", "AUPRC", "Brier", "Sensitivity", "Specificity",
                      "Cal_Slope", "ECI")], row.names = FALSE)

## 格式化版本
ext_pretty <- ext_metrics
ext_pretty$AUC   <- fmt_ci(ext_metrics$AUC,   ext_metrics$AUC_lower,   ext_metrics$AUC_upper)
ext_pretty$AUPRC <- fmt_ci(ext_metrics$AUPRC, ext_metrics$AUPRC_lower, ext_metrics$AUPRC_upper)
ext_pretty$Brier <- fmt_ci(ext_metrics$Brier, ext_metrics$Brier_lower, ext_metrics$Brier_upper)
write_table(ext_pretty[, c("Model", "AUC", "AUPRC", "Brier", "Sensitivity", "Specificity",
                           "PPV", "NPV", "Accuracy", "Cal_Slope", "ECI")],
            "08_external_validation_metrics_formatted.csv")

## -------------------------------------------------------------------------
## 7. 外部 ROC / DCA / 校准
## -------------------------------------------------------------------------
if (length(ext_roc) > 0) {
  roc_df <- do.call(rbind, lapply(names(ext_roc), function(m) {
    r <- ext_roc[[m]]
    data.frame(Model = m, FPR = 1 - r$specificities, TPR = r$sensitivities,
               AUC = round(as.numeric(pROC::auc(r)), 3), stringsAsFactors = FALSE)
  }))
  roc_df$Label <- sprintf("%s (AUC=%.3f)", roc_df$Model, roc_df$AUC)
  p_roc <- ggplot(roc_df, aes(x = FPR, y = TPR, colour = Label)) +
    geom_line(linewidth = 0.9) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
    labs(title = "External validation - ROC", x = "1 - Specificity", y = "Sensitivity",
         colour = NULL) +
    theme_minimal(base_size = 12) + theme(legend.position = "bottom")
  save_fig(p_roc, "08_external_ROC.png", width = 8, height = 7)
}

dca_rows <- list()
for (m in names(ext_prob)) {
  d <- compute_dca(y_ext, ext_prob[[m]]); d$Model <- m
  dca_rows[[m]] <- d
}
dca_ext <- do.call(rbind, dca_rows)
ref <- dca_reference_lines(y_ext)

p_dca <- ggplot(dca_ext, aes(x = threshold, y = net_benefit, colour = Model)) +
  geom_line(linewidth = 0.9) +
  geom_line(data = ref$none, aes(x = threshold, y = net_benefit),
            linetype = "dotted", inherit.aes = FALSE) +
  geom_line(data = ref$all, aes(x = threshold, y = net_benefit),
            linetype = "dashed", inherit.aes = FALSE) +
  coord_cartesian(xlim = c(0, 0.8)) +
  labs(title = "External validation - decision curve analysis",
       x = "Threshold probability", y = "Net benefit", colour = NULL) +
  theme_bw(base_size = 12) + theme(legend.position = "bottom")
save_fig(p_dca, "08_external_DCA.png", width = 8, height = 6)
write_plot_data(rbind(dca_ext, ref$none, ref$all), "08_external_dca.csv")

if (length(ext_cal) > 0) {
  cal_df <- do.call(rbind, ext_cal)
  write_table(cal_df, "08_external_calibration_bins.csv")
  write_plot_data(cal_df, "08_external_calibration_bins.csv")

  p_cal <- ggplot(cal_df, aes(x = mean_pred, y = mean_obs)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
    geom_point(aes(size = n), colour = "firebrick", alpha = 0.85) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.02,
                  colour = "firebrick", alpha = 0.6) +
    facet_wrap(~ Model) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(title = "External validation - calibration", x = "Predicted probability",
         y = "Observed proportion", size = "n") +
    theme_bw(base_size = 12) + theme(legend.position = "bottom")
  save_fig(p_cal, "08_external_calibration.png", width = 9, height = 7)
}

## -------------------------------------------------------------------------
## 8. 内部 vs 外部对比（性能衰减）
## -------------------------------------------------------------------------
internal_file <- file.path(paths$tables, "04_model_metrics.csv")
if (file.exists(internal_file)) {
  internal <- utils::read.csv(internal_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  comp <- merge(
    internal[, c("Model", "Test_AUC", "Test_AUPRC", "Test_Brier")],
    ext_metrics[, c("Model", "AUC", "AUPRC", "Brier")],
    by = "Model"
  )
  names(comp) <- c("Model", "AUC_internal", "AUPRC_internal", "Brier_internal",
                   "AUC_external", "AUPRC_external", "Brier_external")
  comp$AUC_drop <- comp$AUC_internal - comp$AUC_external
  comp$Brier_increase <- comp$Brier_external - comp$Brier_internal
  write_table(comp, "08_internal_vs_external.csv")

  cat("\n===== 内部 vs 外部 =====\n")
  print(comp[, c("Model", "AUC_internal", "AUC_external", "AUC_drop",
                 "Brier_internal", "Brier_external")], row.names = FALSE)
}

## -------------------------------------------------------------------------
## 9. 保存
## -------------------------------------------------------------------------
stage08 <- list(
  ext_metrics = ext_metrics,
  ext_prob    = ext_prob,
  y_ext       = y_ext
)
save_stage(stage08, "stage08_external")
log_step("已保存阶段缓存：stage08_external")
