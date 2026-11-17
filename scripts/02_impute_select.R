# =========================================================================
# scripts/02_impute_select.R
# MICE 多重插补 -> 清洗 -> 单因素初筛 -> LASSO 共识 -> VIF -> 完美分离 -> 测试集对齐
# 输出：cache/stage02_select.rds
# =========================================================================

section("02 多重插补与变量筛选")

stage01 <- get_or_load("stage01", "stage01_split")
train_set <- stage01$train_set
test_set  <- stage01$test_set
n_events  <- stage01$n_events

## -------------------------------------------------------------------------
## 1. 高缺失变量剔除
## -------------------------------------------------------------------------
if (length(stage01$high_miss_vars) > 0) {
  keep <- setdiff(names(train_set), stage01$high_miss_vars)
  train_set <- train_set[, keep, drop = FALSE]
  test_set  <- test_set[, intersect(keep, names(test_set)), drop = FALSE]
  log_step("剔除高缺失变量后：", ncol(train_set), " 列")
}

## -------------------------------------------------------------------------
## 2. MICE 插补（统一 CART）
## -------------------------------------------------------------------------
pred_mat <- mice::quickpred(train_set, mincor = 0.1, minpuc = 0.05)
pred_mat[, outcome_var] <- 0

## 限制每个变量最多 20 个预测变量，避免参数爆炸
for (i in seq_len(nrow(pred_mat))) {
  sel <- which(pred_mat[i, ] == 1)
  if (length(sel) > 20) pred_mat[i, sample(sel, length(sel) - 20)] <- 0
}
log_step("预测矩阵非零元素：", sum(pred_mat))

vars_with_na <- setdiff(names(which(colSums(is.na(train_set)) > 0)), outcome_var)
mice_methods <- stats::setNames(rep("", ncol(train_set)), names(train_set))
mice_methods[vars_with_na] <- "cart"
log_step("需插补变量：", length(vars_with_na))

set.seed(seed_val)
mice_obj <- mice::mice(
  train_set,
  m = m_imputations, maxit = maxit_mice,
  method = mice_methods,
  predictorMatrix = pred_mat,
  seed = seed_val, printFlag = FALSE
)

imp_list <- mice::complete(mice_obj, action = "all")
imp_list <- imp_list[setdiff(names(imp_list), "0")]

## -------------------------------------------------------------------------
## 3. 清洗 + 残余缺失兜底
##   注意：preprocess_data() 定义在 R/preprocess.R，由 run_all.R 先行 source。
##   原脚本在定义之前调用它，在干净会话中必然报错。
## -------------------------------------------------------------------------
imp_list_clean <- clean_imputed_list(imp_list, outcome_var)
imp_list_clean <- lapply(imp_list_clean, function(d) {
  if (sum(is.na(d)) > 0) impute_residual_na(d, outcome_var) else d
})
n_na <- vapply(imp_list_clean, function(d) sum(is.na(d)), integer(1))
log_step("各插补集残余缺失：", paste(n_na, collapse = ", "))
log_step(sprintf("插补后维度：%d 行 x %d 列，共 %d 套",
             nrow(imp_list_clean[[1]]), ncol(imp_list_clean[[1]]), length(imp_list_clean)))

## 结局统一为因子
imp_list_clean <- lapply(imp_list_clean, function(d) {
  d[[outcome_var]] <- as_outcome_factor(d[[outcome_var]])
  d
})

## -------------------------------------------------------------------------
## 4. 单因素初筛（pooled logistic，Firth 校正）
## -------------------------------------------------------------------------
predictors <- setdiff(names(imp_list_clean[[1]]), outcome_var)
log_step("待筛选变量：", length(predictors))

uni_results <- list()
problem_vars <- character(0)

for (v in predictors) {
  res <- tryCatch({
    fml <- stats::as.formula(paste(outcome_var, "~", v))
    fits <- lapply(imp_list_clean, function(d) {
      if (is.factor(d[[v]]) && nlevels(d[[v]]) == 0) stop("空因子")
      stats::glm(fml, data = d, family = stats::binomial(), method = brglm2::brglmFit)
    })
    summ <- summary(mice::pool(fits))
    summ <- summ[summ$term != "(Intercept)", , drop = FALSE]
    if (nrow(summ) == 0) return(NULL)
    summ$variable <- v
    summ
  }, error = function(e) {
    problem_vars <<- c(problem_vars, v)
    NULL
  })
  if (!is.null(res)) uni_results[[length(uni_results) + 1]] <- res
}

if (length(problem_vars) > 0) {
  log_step("单因素分析失败的变量（剔除）：", paste(problem_vars, collapse = ", "))
  imp_list_clean <- lapply(imp_list_clean, function(d) d[, !(names(d) %in% problem_vars), drop = FALSE])
}

uni_table <- do.call(rbind, uni_results)
write_table(uni_table, "02_univariate_results.csv")

## 每个变量取最小 p（因子多水平取最显著水平）
var_p <- tapply(uni_table$p.value, uni_table$variable, min, na.rm = TRUE)
candidate_uni <- names(var_p)[var_p < p_threshold]
log_step(sprintf("单因素 P < %.2f 的候选变量：%d", p_threshold, length(candidate_uni)))

if (length(candidate_uni) == 0) {
  candidate_uni <- names(var_p)[var_p < p_threshold_fallback]
  log_step(sprintf("无显著变量，放宽到 P < %.2f：%d 个", p_threshold_fallback, length(candidate_uni)))
}
if (length(candidate_uni) == 0) stop("无候选变量，流程终止。", call. = FALSE)

## 临床共线性剔除（依据先验判断，写在 config 中）
excluded <- intersect(vars_clinical_exclude, candidate_uni)
if (length(excluded) > 0) {
  candidate_uni <- setdiff(candidate_uni, excluded)
  log_step("依据临床判断剔除：", paste(excluded, collapse = ", "))
}

## -------------------------------------------------------------------------
## 5. LASSO 共识筛选
## -------------------------------------------------------------------------
## 原脚本用 cv.glmnet(formula, data=) 的公式接口，该接口来自 glmnetUtils；
## 只用 glmnet 时会报错。此处显式构造设计矩阵。
lasso_selected <- vector("list", length(imp_list_clean))

for (i in seq_along(imp_list_clean)) {
  d <- imp_list_clean[[i]]
  res <- tryCatch({
    x <- stats::model.matrix(
      stats::as.formula(paste("~", paste(candidate_uni, collapse = " + "))),
      data = d
    )[, -1, drop = FALSE]
    y <- encode_y(d[[outcome_var]])
    cvfit <- glmnet::cv.glmnet(x, y, family = "binomial", alpha = 1,
                               nfolds = min(10, nrow(d)), type.measure = "auc")
    cf <- as.matrix(stats::coef(cvfit, s = "lambda.min"))
    nz <- setdiff(rownames(cf)[cf[, 1] != 0], "(Intercept)")
    ## 把哑变量列名映射回原变量名（按前缀匹配）
    sel <- candidate_uni[vapply(candidate_uni, function(v) {
      any(startsWith(nz, v))
    }, logical(1))]
    sel
  }, error = function(e) {
    log_step(sprintf("插补集 %d LASSO 失败：%s", i, conditionMessage(e)))
    character(0)
  })
  lasso_selected[[i]] <- res
  log_step(sprintf("插补集 %d LASSO 选中 %d 个变量", i, length(res)))
}

lasso_freq <- table(unlist(lasso_selected))
min_imp <- ceiling(length(imp_list_clean) * lasso_consensus_frac)
final_vars <- names(lasso_freq[lasso_freq >= min_imp])
log_step(sprintf("LASSO 共识阈值：>= %d/%d 套", min_imp, length(imp_list_clean)))

if (length(final_vars) == 0) {
  final_vars <- names(lasso_freq[lasso_freq >= 2])
  log_step("共识为空，降级为 >=2 套：", length(final_vars), " 个")
}
if (length(final_vars) == 0) {
  final_vars <- names(sort(lasso_freq, decreasing = TRUE))[1:min(10, length(lasso_freq))]
  log_step("仍为空，取频率最高前 10 个：", length(final_vars), " 个")
}
log_step("最终建模变量：", paste(final_vars, collapse = ", "))

lasso_freq_df <- data.frame(
  variable = names(sort(lasso_freq, decreasing = TRUE)),
  n_selected = as.vector(sort(lasso_freq, decreasing = TRUE)),
  stringsAsFactors = FALSE
)
write_table(lasso_freq_df, "02_lasso_selection_frequency.csv")

## -------------------------------------------------------------------------
## 6. 只保留最终变量
## -------------------------------------------------------------------------
primary_set <- final_vars
keep_cols <- c(primary_set, outcome_var)

train_imp_list <- lapply(imp_list_clean, function(d) {
  missing_cols <- setdiff(keep_cols, names(d))
  if (length(missing_cols) > 0) stop("变量缺失：", paste(missing_cols, collapse = ", "), call. = FALSE)
  d[, keep_cols, drop = FALSE]
})

## -------------------------------------------------------------------------
## 7. VIF 共线性诊断
## -------------------------------------------------------------------------
if (length(primary_set) >= 2 && length(primary_set) <= 30) {
  fml <- stats::as.formula(paste(outcome_var, "~", paste(primary_set, collapse = " + ")))
  fit_vif <- stats::glm(fml, data = train_imp_list[[1]], family = stats::binomial())
  vif_res <- tryCatch(car::vif(fit_vif), error = function(e) NULL)
  if (!is.null(vif_res)) {
    if (is.matrix(vif_res)) {
      high <- names(which(vif_res[, "GVIF^(1/(2*Df))"] > 2))
    } else {
      high <- names(which(vif_res > 5))
    }
    capture.output(print(vif_res), file = file.path(paths$tables, "02_VIF_diagnostic.txt"))
    if (length(high) > 0) {
      log_step("高共线性变量（剔除）：", paste(high, collapse = ", "))
      primary_set <- setdiff(primary_set, high)
      train_imp_list <- lapply(train_imp_list, function(d) d[, c(primary_set, outcome_var), drop = FALSE])
    } else {
      log_step("未发现严重共线性")
    }
  }
}

## -------------------------------------------------------------------------
## 8. 完美分离 / 退化变量
## -------------------------------------------------------------------------
bad_sep <- unique(unlist(lapply(train_imp_list, check_perfect_sep, outcome_var)))
if (length(bad_sep) > 0) {
  log_step("完美分离或退化变量（剔除）：", paste(bad_sep, collapse = ", "))
  primary_set <- setdiff(primary_set, bad_sep)
  train_imp_list <- lapply(train_imp_list, function(d) d[, c(primary_set, outcome_var), drop = FALSE])
}

train_imp_list <- lapply(train_imp_list, function(d) {
  if (sum(is.na(d)) > 0) impute_residual_na(d, outcome_var) else d
})

log_step(sprintf("最终变量数：%d；EPV = %.1f", length(primary_set),
             n_events / max(length(primary_set), 1)))

## -------------------------------------------------------------------------
## 9. 测试集对齐
## -------------------------------------------------------------------------
test_prep <- preprocess_data(test_set)
test_imp_list <- lapply(train_imp_list, function(tr) prep_test_safe(test_prep, tr))

log_step(sprintf("测试集维度：%d 行 x %d 列",
             nrow(test_imp_list[[1]]), ncol(test_imp_list[[1]])))

## -------------------------------------------------------------------------
## 10. 保存
## -------------------------------------------------------------------------
stage02 <- list(
  mice_obj        = mice_obj,
  imp_list_clean  = imp_list_clean,
  train_imp_list  = train_imp_list,
  test_imp_list   = test_imp_list,
  primary_set     = primary_set,
  candidate_uni   = candidate_uni,
  uni_table       = uni_table,
  lasso_freq_df   = lasso_freq_df,
  train_set       = train_set,
  test_set        = test_set,
  n_events        = n_events
)
save_stage(stage02, "stage02_select")
log_step("已保存阶段缓存：stage02_select")
