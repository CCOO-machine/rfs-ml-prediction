# =========================================================================
# scripts/03_train_models.R
# 8 个候选模型 x M 套插补数据
# 输出：cache/stage03_models.rds，cache/models/*.rds
# =========================================================================

section("03 建模")

stage02 <- get_or_load("stage02", "stage02_select")
train_imp_list <- stage02$train_imp_list
test_imp_list  <- stage02$test_imp_list
primary_set    <- stage02$primary_set

dir.create(paths$models, recursive = TRUE, showWarnings = FALSE)

n_train <- nrow(train_imp_list[[1]])
p       <- length(primary_set)

## 类别不平衡 -> 是否使用 SMOTE
imb <- detect_imbalance(train_imp_list[[1]][[outcome_var]])
log_step(sprintf("类别比例 %.1f:1；SMOTE %s", imb$ratio, ifelse(imb$use_smote, "启用", "不启用")))

set.seed(seed_val)
ctrl <- make_train_control(use_smote = imb$use_smote)

models_pooled <- list()

## -------------------------------------------------------------------------
## 1. 逻辑回归
## 注：caret 的 "glm" 方法拟合的是标准极大似然 logistic 回归，不含 Firth 惩罚。
##     原始脚本注释写"Firth"，与实际不符，此处已在 docs/CHANGES.md 说明。
## -------------------------------------------------------------------------
if ("LR" %in% model_list) {
  log_step("--- 逻辑回归 ---")
  models_pooled$LR <- train_across_imputations(
    "glm", train_imp_list, test_imp_list, ctrl,
    family = stats::binomial()
  )
}

## -------------------------------------------------------------------------
## 2. KNN
## -------------------------------------------------------------------------
if ("KNN" %in% model_list) {
  log_step("--- KNN ---")
  models_pooled$KNN <- train_across_imputations(
    "knn", train_imp_list, test_imp_list, ctrl,
    tuneGrid = knn_grid(n_train),
    preProcess = c("center", "scale")
  )
}

## -------------------------------------------------------------------------
## 3. 随机森林
## -------------------------------------------------------------------------
if ("RF" %in% model_list) {
  log_step("--- 随机森林 ---")
  rf_args <- rf_extra_args(n_train)
  models_pooled$RF <- train_across_imputations(
    "rf", train_imp_list, test_imp_list, ctrl,
    tuneGrid = rf_grid(p),
    ntree    = rf_args$ntree,
    nodesize = rf_args$nodesize,
    maxnodes = rf_args$maxnodes,
    sampsize = rf_args$sampsize
  )
}

## -------------------------------------------------------------------------
## 4. GBM
## -------------------------------------------------------------------------
if ("GBM" %in% model_list) {
  log_step("--- GBM ---")
  models_pooled$GBM <- train_across_imputations(
    "gbm", train_imp_list, test_imp_list, ctrl,
    tuneGrid = gbm_grid(), verbose = FALSE
  )
}

## -------------------------------------------------------------------------
## 5. SVM（RBF）
## -------------------------------------------------------------------------
if ("SVM" %in% model_list) {
  log_step("--- SVM ---")
  models_pooled$SVM <- train_across_imputations(
    "svmRadial", train_imp_list, test_imp_list, ctrl,
    tuneGrid = svm_grid(),
    preProcess = c("center", "scale")
  )
}

## -------------------------------------------------------------------------
## 6. 神经网络
## -------------------------------------------------------------------------
if ("NN" %in% model_list) {
  log_step("--- 神经网络 ---")
  models_pooled$NN <- train_across_imputations(
    "nnet", train_imp_list, test_imp_list, ctrl,
    tuneGrid = nnet_grid(), trace = FALSE,
    preProcess = c("center", "scale"), maxit = 200
  )
}

## -------------------------------------------------------------------------
## 7. XGBoost（原生接口）
## -------------------------------------------------------------------------
if ("XGBoost" %in% model_list) {
  log_step("--- XGBoost ---")
  models_pooled$XGBoost <- train_xgboost(train_imp_list, test_imp_list)
  saveRDS(models_pooled$XGBoost$recipe, file.path(paths$models, "xgb_recipe.rds"))
}

## -------------------------------------------------------------------------
## 8. 朴素贝叶斯
## -------------------------------------------------------------------------
if ("NB" %in% model_list) {
  log_step("--- 朴素贝叶斯 ---")
  models_pooled$NB <- train_naive_bayes(train_imp_list, test_imp_list, primary_set)
}

## -------------------------------------------------------------------------
## 清理与保存
## -------------------------------------------------------------------------
models_pooled <- models_pooled[!vapply(models_pooled, is.null, logical(1))]
if (length(models_pooled) == 0) stop("所有模型训练失败。", call. = FALSE)

log_step("成功训练模型：", paste(names(models_pooled), collapse = ", "))

for (m in names(models_pooled)) {
  saveRDS(models_pooled[[m]]$models, file.path(paths$models, paste0("models_", m, ".rds")))
}

## 供外部验证使用的元信息（不包含"最优模型"字段）
var_info <- list(
  primary_set     = primary_set,
  outcome_var     = outcome_var,
  positive_class  = positive_class,
  negative_class  = negative_class,
  all_model_names = names(models_pooled),
  factor_levels   = lapply(
    train_imp_list[[1]][, primary_set, drop = FALSE],
    function(x) if (is.factor(x)) levels(x) else NULL
  ),
  numeric_vars = names(which(vapply(train_imp_list[[1]][, primary_set, drop = FALSE], is.numeric, logical(1)))),
  factor_vars  = names(which(vapply(train_imp_list[[1]][, primary_set, drop = FALSE], is.factor,  logical(1))))
)
var_info$factor_levels <- var_info$factor_levels[!vapply(var_info$factor_levels, is.null, logical(1))]
saveRDS(var_info, file.path(paths$models, "var_info.rds"))
saveRDS(train_imp_list[[1]], file.path(paths$models, "train_ref.rds"))

stage03 <- list(
  models_pooled  = models_pooled,
  train_imp_list = train_imp_list,
  test_imp_list  = test_imp_list,
  primary_set    = primary_set,
  ctrl           = ctrl,
  var_info       = var_info,
  use_smote      = imb$use_smote,
  model_list     = names(models_pooled)
)
save_stage(stage03, "stage03_models")
log_step("已保存阶段缓存：stage03_models")
