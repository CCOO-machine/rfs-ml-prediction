# =========================================================================
# R/models.R
# 建模：caret 统一接口 + XGBoost / 朴素贝叶斯手写流程
# =========================================================================

## -------------------------------------------------------------------------
## 训练控制：5-fold x 3 repeats
## 原脚本 trainControl() 里引用了从未定义的 cv_repeats，会直接报错；
## 该参数现已在 config/config.R 中定义。
## -------------------------------------------------------------------------
make_train_control <- function(use_smote = FALSE) {
  caret::trainControl(
    method          = "repeatedcv",
    number          = cv_folds,
    repeats         = cv_repeats,
    classProbs      = TRUE,
    summaryFunction = caret::twoClassSummary,
    savePredictions = "final",
    sampling        = if (use_smote) "smote" else NULL
  )
}

## 类别不平衡判断
detect_imbalance <- function(y) {
  tab <- table(y)
  ratio <- max(tab) / min(tab)
  list(ratio = ratio, use_smote = ratio > smote_ratio_cutoff)
}

## -------------------------------------------------------------------------
## 单模型训练（带错误捕获）
## -------------------------------------------------------------------------
safe_train <- function(method, data, trControl, metric = "ROC", outcome = outcome_var, ...) {
  tryCatch(
    caret::train(
      stats::as.formula(paste(outcome, "~ .")),
      data = data, method = method, trControl = trControl, metric = metric, ...
    ),
    error = function(e) {
      log_step("模型 ", method, " 训练失败：", conditionMessage(e))
      NULL
    }
  )
}

## 取正类概率列
predict_positive <- function(fit, newdata) {
  p <- stats::predict(fit, newdata = newdata, type = "prob")
  if (is.data.frame(p)) {
    if (positive_class %in% names(p)) return(p[[positive_class]])
    return(p[[ncol(p)]])
  }
  as.numeric(p[, ncol(p)])
}

## -------------------------------------------------------------------------
## 跨插补集训练（统一接口）
##   pooled_prob      : 测试集预测概率，对 M 套取平均
##   train_prob       : 训练集回代概率（用于后续 DCA / 过拟合诊断）
##   cv_auc / cv_aucs : 各插补集内部 CV 的最优 ROC
## -------------------------------------------------------------------------
train_across_imputations <- function(method, train_list, test_list, trControl,
                                     outcome = outcome_var, ...) {
  n_imp <- length(train_list)
  test_prob_mat  <- matrix(NA_real_, nrow = nrow(test_list[[1]]),  ncol = n_imp)
  train_prob_mat <- matrix(NA_real_, nrow = nrow(train_list[[1]]), ncol = n_imp)
  cv_aucs <- rep(NA_real_, n_imp)
  cv_sds  <- rep(NA_real_, n_imp)
  models  <- vector("list", n_imp)

  for (i in seq_len(n_imp)) {
    log_step(sprintf("%s - 插补集 %d/%d", method, i, n_imp))
    fit <- safe_train(method, data = train_list[[i]], trControl = trControl, ...)
    models[[i]] <- fit
    if (is.null(fit)) next

    test_prob_mat[, i]  <- predict_positive(fit, test_list[[i]])
    train_prob_mat[, i] <- predict_positive(fit, train_list[[i]])

    if (!is.null(fit$results) && "ROC" %in% names(fit$results)) {
      best_i <- which.max(fit$results$ROC)
      cv_aucs[i] <- fit$results$ROC[best_i]
      if ("ROCSD" %in% names(fit$results)) cv_sds[i] <- fit$results$ROCSD[best_i]
    }
  }

  list(
    models      = models,
    pooled_prob = rowMeans(test_prob_mat, na.rm = TRUE),
    train_prob  = rowMeans(train_prob_mat, na.rm = TRUE),
    cv_auc      = mean(cv_aucs, na.rm = TRUE),
    cv_aucs     = cv_aucs,
    cv_auc_sds  = cv_sds,
    type        = "caret"
  )
}

## -------------------------------------------------------------------------
## 超参数网格（与原始脚本保持一致）
## -------------------------------------------------------------------------
knn_grid <- function(n_train) {
  k_min <- max(5, floor(sqrt(n_train) * 0.5))
  k_max <- max(15, floor(sqrt(n_train) * 2))
  expand.grid(k = unique(round(seq(k_min, k_max, length.out = 8))))
}

rf_grid <- function(p) {
  mtry_min <- max(2, floor(sqrt(p) * 0.3))
  mtry_max <- max(3, floor(sqrt(p) * 1.0))
  g <- expand.grid(mtry = unique(round(seq(mtry_min, mtry_max, length.out = 5))))
  g$mtry <- pmax(g$mtry, 1)
  g
}

rf_extra_args <- function(n_train) {
  nodesize <- 16
  sampsize <- floor(n_train * 0.65)
  maxnodes <- max(20, floor(n_train / 8))
  if (maxnodes * nodesize > sampsize * 0.8) {
    maxnodes <- max(floor(sampsize * 0.8 / nodesize), 10)
  }
  list(
    ntree    = 1000,
    nodesize = nodesize,
    maxnodes = maxnodes,
    sampsize = sampsize
  )
}

gbm_grid <- function(seed = seed_val) {
  g <- expand.grid(
    n.trees           = c(50, 80, 100, 150),
    interaction.depth = 1,
    shrinkage         = c(0.03, 0.05),
    n.minobsinnode    = c(50, 100, 150)
  )
  set.seed(seed)
  if (nrow(g) > 40) g <- g[sample(nrow(g), 40), , drop = FALSE]
  g
}

svm_grid <- function() {
  expand.grid(
    sigma = c(0.01, 0.05, 0.1, 0.2),
    C     = c(0.001, 0.01, 0.05, 0.1, 0.5)
  )
}

nnet_grid <- function() {
  expand.grid(size = c(2, 3, 5), decay = c(0.01, 0.05, 0.1, 0.2))
}

## -------------------------------------------------------------------------
## XGBoost（原生接口，逐插补集训练）
## -------------------------------------------------------------------------
train_xgboost <- function(train_list, test_list, outcome = outcome_var,
                          seed = seed_val, nrounds_max = 500) {
  d0 <- train_list[[1]]
  features <- d0[, setdiff(names(d0), outcome), drop = FALSE]
  x_train0 <- stats::model.matrix(~ . - 1, data = features)
  y_train0 <- encode_y(d0[[outcome]])

  tab <- table(d0[[outcome]])
  pos_ratio <- as.numeric(tab[negative_class]) / as.numeric(tab[positive_class])

  params <- list(
    objective        = "binary:logistic",
    eval_metric      = "auc",
    learning_rate    = 0.01,
    max_depth        = 3,
    subsample        = 0.6,
    colsample_bytree = 0.6,
    scale_pos_weight = pos_ratio,
    min_child_weight = 3,
    gamma            = 0.3,
    lambda           = 1.5,
    alpha            = 0.3,
    verbosity        = 0
  )

  ## 交叉验证确定轮数
  set.seed(seed)
  folds <- caret::createFolds(d0[[outcome]], k = cv_folds, list = TRUE, returnTrain = FALSE)
  dtrain0 <- xgboost::xgb.DMatrix(data = x_train0, label = y_train0)
  cv <- xgboost::xgb.cv(
    params = params, data = dtrain0, nrounds = nrounds_max,
    folds = folds, early_stopping_rounds = 30, verbose = 0
  )
  best_nrounds <- if (!is.null(cv$best_iteration)) cv$best_iteration else
    which.max(cv$evaluation_log$test_auc_mean)
  best_nrounds <- max(best_nrounds, 50)
  cv_auc <- max(cv$evaluation_log$test_auc_mean, na.rm = TRUE)
  log_step("XGBoost 最佳轮数：", best_nrounds, "；CV AUC：", sprintf("%.4f", cv_auc))

  n_imp <- length(train_list)
  test_prob_mat  <- matrix(NA_real_, nrow = nrow(test_list[[1]]),  ncol = n_imp)
  train_prob_mat <- matrix(NA_real_, nrow = nrow(train_list[[1]]), ncol = n_imp)
  models <- vector("list", n_imp)
  recipe <- NULL

  for (i in seq_len(n_imp)) {
    log_step(sprintf("XGBoost - 插补集 %d/%d", i, n_imp))
    d <- train_list[[i]]
    feats <- d[, setdiff(names(d), outcome), drop = FALSE]
    x_tr <- stats::model.matrix(~ . - 1, data = feats)

    if (i == 1) {
      recipe <- list(
        cols = colnames(x_tr),
        xlev = lapply(feats[vapply(feats, is.factor, logical(1))], levels)
      )
    }

    fit <- xgboost::xgb.train(
      params = params,
      data = xgboost::xgb.DMatrix(data = x_tr, label = encode_y(d[[outcome]])),
      nrounds = best_nrounds
    )
    models[[i]] <- fit

    ## 测试集：列对齐后预测
    te <- test_list[[i]]
    feats_te <- te[, setdiff(names(te), outcome), drop = FALSE]
    x_te <- tryCatch(
      stats::model.matrix(~ . - 1, data = feats_te),
      error = function(e) {
        stats::model.matrix(~ . - 1,
                            data = feats_te[, vapply(feats_te, is.numeric, logical(1)), drop = FALSE])
      }
    )
    x_te <- align_matrix_columns(x_te, recipe$cols)

    train_prob_mat[, i] <- stats::predict(fit, newdata = xgboost::xgb.DMatrix(data = x_tr))
    test_prob_mat[, i]  <- stats::predict(fit, newdata = xgboost::xgb.DMatrix(data = x_te))
  }

  list(
    models      = models,
    recipe      = recipe,
    pooled_prob = rowMeans(test_prob_mat, na.rm = TRUE),
    train_prob  = rowMeans(train_prob_mat, na.rm = TRUE),
    cv_auc      = cv_auc,
    cv_aucs     = rep(cv_auc, n_imp),
    cv_auc_sds  = rep(NA_real_, n_imp),
    type        = "xgb"
  )
}

## 矩阵列对齐：缺列补 0，多列丢弃，顺序按参考列名
align_matrix_columns <- function(mat, target_cols) {
  keep <- intersect(target_cols, colnames(mat))
  out <- mat[, keep, drop = FALSE]
  missing_cols <- setdiff(target_cols, colnames(out))
  if (length(missing_cols) > 0) {
    zeros <- matrix(0, nrow = nrow(out), ncol = length(missing_cols),
                    dimnames = list(NULL, missing_cols))
    out <- cbind(out, zeros)
  }
  out[, target_cols, drop = FALSE]
}

## -------------------------------------------------------------------------
## 朴素贝叶斯（e1071）
## 说明：e1071::naiveBayes 对因子支持有限，统一转为数值编码后训练。
## -------------------------------------------------------------------------
train_naive_bayes <- function(train_list, test_list, predictors, outcome = outcome_var,
                              seed = seed_val) {
  n_imp <- length(train_list)
  test_prob_mat  <- matrix(NA_real_, nrow = nrow(test_list[[1]]),  ncol = n_imp)
  train_prob_mat <- matrix(NA_real_, nrow = nrow(train_list[[1]]), ncol = n_imp)
  cv_aucs <- rep(NA_real_, n_imp)
  models  <- vector("list", n_imp)

  to_numeric_frame <- function(d, levels_ref) {
    out <- d[, names(levels_ref), drop = FALSE]
    for (v in names(out)) {
      if (is.factor(out[[v]])) {
        out[[v]] <- as.numeric(factor(as.character(out[[v]]), levels = levels_ref[[v]]))
      }
      out[[v]] <- as.numeric(out[[v]])
      if (any(is.na(out[[v]]))) {
        out[[v]][is.na(out[[v]])] <- stats::median(out[[v]], na.rm = TRUE)
      }
    }
    out
  }

  for (i in seq_len(n_imp)) {
    log_step(sprintf("朴素贝叶斯 - 插补集 %d/%d", i, n_imp))
    d_tr <- train_list[[i]]
    d_te <- test_list[[i]]

    levels_ref <- lapply(d_tr[, predictors, drop = FALSE], function(x) {
      if (is.factor(x)) levels(x) else NULL
    })
    levels_ref <- levels_ref[!vapply(levels_ref, is.null, logical(1))]

    x_tr <- to_numeric_frame(d_tr[, predictors, drop = FALSE], levels_ref)
    x_te <- to_numeric_frame(d_te[, predictors, drop = FALSE], levels_ref)

    ## 丢弃常数列
    keep <- names(x_tr)[vapply(x_tr, function(x) length(unique(x)) > 1, logical(1))]
    x_tr <- x_tr[, keep, drop = FALSE]
    x_te <- x_te[, keep, drop = FALSE]

    y_tr <- as_outcome_factor(d_tr[[outcome]])

    fit <- tryCatch(
      e1071::naiveBayes(x = x_tr, y = y_tr, laplace = 1, usekernel = FALSE),
      error = function(e) {
        tryCatch(e1071::naiveBayes(x = x_tr, y = y_tr, laplace = 1, usekernel = TRUE),
                 error = function(e2) NULL)
      }
    )
    models[[i]] <- fit
    if (is.null(fit)) next

    test_prob_mat[, i]  <- as.numeric(stats::predict(fit, x_te, type = "raw")[, positive_class])
    train_prob_mat[, i] <- as.numeric(stats::predict(fit, x_tr, type = "raw")[, positive_class])

    ## 5-fold CV
    set.seed(seed + i)
    folds <- caret::createFolds(y_tr, k = cv_folds, list = TRUE)
    cv_pred <- rep(NA_real_, nrow(x_tr))
    for (fold_idx in folds) {
      tr_idx <- setdiff(seq_len(nrow(x_tr)), fold_idx)
      cv_fit <- tryCatch(
        e1071::naiveBayes(x = x_tr[tr_idx, , drop = FALSE], y = y_tr[tr_idx],
                          laplace = 1, usekernel = FALSE),
        error = function(e) NULL
      )
      if (!is.null(cv_fit)) {
        cv_pred[fold_idx] <- as.numeric(stats::predict(cv_fit, x_tr[fold_idx, , drop = FALSE],
                                                type = "raw")[, positive_class])
      }
    }
    if (sum(!is.na(cv_pred)) > 10) {
      cv_aucs[i] <- calc_auc(cv_pred, as.character(y_tr))
    }
  }

  pooled <- rowMeans(test_prob_mat, na.rm = TRUE)
  if (all(!is.finite(pooled))) {
    warning("Naive Bayes 全部插补集训练失败，以无信息预测占位。")
    pooled <- rep(0.5, nrow(test_list[[1]]))
    train_pooled <- rep(0.5, nrow(train_list[[1]]))
    cv_aucs <- rep(0.5, n_imp)
  } else {
    train_pooled <- rowMeans(train_prob_mat, na.rm = TRUE)
  }

  list(
    models      = models,
    pooled_prob = pooled,
    train_prob  = train_pooled,
    cv_auc      = mean(cv_aucs, na.rm = TRUE),
    cv_aucs     = cv_aucs,
    cv_auc_sds  = rep(NA_real_, n_imp),
    type        = "nb"
  )
}

## -------------------------------------------------------------------------
## 兼容多类型的预测函数（外部验证使用）
## -------------------------------------------------------------------------
robust_predict <- function(model, newdata, positive = positive_class, recipe = NULL) {
  if (is.null(model) || is.character(model)) {
    stop("模型对象无效：", class(model)[1], call. = FALSE)
  }

  if (inherits(model, "train")) {
    p <- tryCatch(stats::predict(model, newdata = newdata, type = "prob"),
                  error = function(e) NULL)
    if (is.data.frame(p) || is.matrix(p)) {
      if (positive %in% colnames(p)) return(as.numeric(p[, positive]))
      return(as.numeric(p[, ncol(p)]))
    }
    return(as.numeric(stats::predict(model, newdata = newdata) == positive))
  }

  if (inherits(model, "xgb.Booster")) {
    if (is.null(recipe)) stop("XGBoost 预测缺少 recipe（列名与因子水平）", call. = FALSE)
    nd <- newdata
    if (!is.null(recipe$xlev)) {
      for (v in names(recipe$xlev)) {
        if (v %in% names(nd)) nd[[v]] <- factor(as.character(nd[[v]]), levels = recipe$xlev[[v]])
      }
    }
    mm <- stats::model.matrix(~ . - 1, data = nd)
    mm <- align_matrix_columns(mm, recipe$cols)
    return(as.numeric(stats::predict(model, newdata = xgboost::xgb.DMatrix(data = mm, missing = NA))))
  }

  if (inherits(model, "naiveBayes")) {
    p <- tryCatch(stats::predict(model, newdata = newdata, type = "raw"),
                  error = function(e) NULL)
    if (!is.null(p) && (is.matrix(p) || is.data.frame(p))) {
      return(as.numeric(p[, ncol(p)]))
    }
    return(as.numeric(stats::predict(model, newdata = newdata) == positive))
  }

  ## 通用回退
  p <- tryCatch(stats::predict(model, newdata = newdata, type = "response"),
                error = function(e) NULL)
  if (is.numeric(p) && length(p) == nrow(newdata)) return(as.numeric(p))
  as.numeric(stats::predict(model, newdata = newdata) == positive)
}
