# =========================================================================
# R/metrics.R
# 判别力、校准与临床效用指标
#
# 原脚本里 compute_dca() 定义两次、compute_calibration() 定义两次且签名不同，
# 后定义的版本会静默覆盖前面的版本。此处每种函数只保留一份实现。
# =========================================================================

## -------------------------------------------------------------------------
## 单指标计算（带防护，供 bootstrap 复用）
## -------------------------------------------------------------------------
calc_auc <- function(prob, label, positive = positive_class) {
  y <- as.character(label)
  if (length(unique(y[!is.na(y)])) < 2) return(NA_real_)
  if (length(unique(prob[!is.na(prob)])) < 2) return(NA_real_)
  r <- tryCatch(pROC::roc(response = y, predictor = prob,
                          levels = c(setdiff(unique(y), positive), positive),
                          direction = "<", quiet = TRUE),
                error = function(e) NULL)
  if (is.null(r)) return(NA_real_)
  as.numeric(pROC::auc(r))
}

calc_auprc <- function(prob, label, positive = positive_class) {
  pos <- prob[as.character(label) == positive]
  neg <- prob[as.character(label) != positive]
  pos <- pos[!is.na(pos)]; neg <- neg[!is.na(neg)]
  if (length(pos) < 1 || length(neg) < 1) return(NA_real_)
  if (length(unique(c(pos, neg))) < 2) return(NA_real_)
  pr <- tryCatch(PRROC::pr.curve(scores.class0 = pos, scores.class1 = neg, curve = FALSE),
                 error = function(e) NULL, warning = function(w) NULL)
  if (is.null(pr) || is.null(pr$auc.integral) || is.na(pr$auc.integral)) return(NA_real_)
  as.numeric(pr$auc.integral)
}

calc_brier <- function(prob, label, positive = positive_class) {
  y <- encode_y(label, positive)
  mean((prob - y)^2, na.rm = TRUE)
}

## -------------------------------------------------------------------------
## 分类指标（给定阈值）
## -------------------------------------------------------------------------
calc_class_metrics <- function(prob, label, threshold, positive = positive_class) {
  lv <- c(setdiff(unique(as.character(label)), positive), positive)
  pred <- factor(ifelse(prob >= threshold, positive, lv[1]), levels = lv)
  obs  <- factor(as.character(label), levels = lv)
  cm <- tryCatch(caret::confusionMatrix(pred, obs, positive = positive),
                 error = function(e) NULL)
  if (is.null(cm)) {
    return(list(threshold = threshold, Sensitivity = NA_real_, Specificity = NA_real_,
                PPV = NA_real_, NPV = NA_real_, F1 = NA_real_,
                Accuracy = NA_real_, Kappa = NA_real_))
  }
  list(
    threshold   = threshold,
    Sensitivity = unname(cm$byClass["Sensitivity"]),
    Specificity = unname(cm$byClass["Specificity"]),
    PPV         = unname(cm$byClass["Pos Pred Value"]),
    NPV         = unname(cm$byClass["Neg Pred Value"]),
    F1          = unname(cm$byClass["F1"]),
    Accuracy    = unname(cm$overall["Accuracy"]),
    Kappa       = unname(cm$overall["Kappa"])
  )
}

## -------------------------------------------------------------------------
## Youden 最优阈值（基于训练集，避免用测试集调阈值造成乐观偏倚）
## -------------------------------------------------------------------------
youden_threshold <- function(prob, label, positive = positive_class) {
  lv <- c(setdiff(unique(as.character(label)), positive), positive)
  r <- tryCatch(pROC::roc(response = as.character(label), predictor = prob,
                          levels = lv, direction = "<", quiet = TRUE),
                error = function(e) NULL)
  if (is.null(r)) return(0.5)
  co <- tryCatch(pROC::coords(r, "best", best.method = "youden",
                              ret = c("threshold"), transpose = FALSE),
                 error = function(e) NULL)
  if (is.null(co) || nrow(co) == 0) return(0.5)
  as.numeric(co$threshold[1])
}

## -------------------------------------------------------------------------
## Bootstrap 置信区间
## -------------------------------------------------------------------------
## 注意：默认值不能写成 n_boot = n_boot（参数自引用，会触发
## "promise already under evaluation" 错误），因此用 NULL + 取全局配置。
bootstrap_ci <- function(prob, label, metric_fun, n_boot = NULL,
                         alpha = 0.05, seed = NULL, verbose = FALSE) {
  if (is.null(n_boot)) n_boot <- get("n_boot", envir = .GlobalEnv, inherits = TRUE)
  if (is.null(seed))   seed   <- get("seed_val", envir = .GlobalEnv, inherits = TRUE)
  set.seed(seed)
  n <- length(label)
  if (n < 10) return(c(est = NA_real_, lower = NA_real_, upper = NA_real_))

  est <- tryCatch(metric_fun(prob, label), error = function(e) NA_real_)
  boots <- vapply(seq_len(n_boot), function(b) {
    idx <- sample.int(n, n, replace = TRUE)
    tryCatch(metric_fun(prob[idx], label[idx]), error = function(e) NA_real_)
  }, numeric(1))

  valid <- boots[is.finite(boots)]
  if (length(valid) < 100) {
    return(c(est = est, lower = NA_real_, upper = NA_real_))
  }
  ci <- stats::quantile(valid, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
  c(est = est, lower = unname(ci[1]), upper = unname(ci[2]))
}

## 便捷格式："0.812 (0.760-0.864)"
fmt_ci <- function(est, lower, upper, digits = 3) {
  ifelse(
    is.na(est), "NA",
    ifelse(is.na(lower) | is.na(upper),
           sprintf(paste0("%.", digits, "f"), est),
           sprintf(paste0("%.", digits, "f (%.", digits, "f-%.", digits, "f)"),
                   est, lower, upper))
  )
}

## -------------------------------------------------------------------------
## 决策曲线分析（净收益）
## -------------------------------------------------------------------------
compute_dca <- function(y_true, prob, thresholds = dca_thresholds) {
  y <- as.numeric(y_true)
  n <- length(y)
  nb <- vapply(thresholds, function(t) {
    if (t <= 0) return(mean(y, na.rm = TRUE))
    if (t >= 1) return(0)
    pred_pos <- as.numeric(prob > t)
    tp <- sum(pred_pos == 1 & y == 1, na.rm = TRUE)
    fp <- sum(pred_pos == 1 & y == 0, na.rm = TRUE)
    (tp / n) - (fp / n) * (t / (1 - t))
  }, numeric(1))
  data.frame(threshold = thresholds, net_benefit = nb)
}

## Treat All / Treat None 参考线（列名与模型曲线保持一致，便于直接 rbind）
dca_reference_lines <- function(y_true, thresholds = dca_thresholds) {
  p <- mean(as.numeric(y_true), na.rm = TRUE)
  all_nb <- p - (1 - p) * thresholds / (1 - thresholds)
  all_nb[!is.finite(all_nb)] <- NA_real_
  list(
    none = data.frame(threshold = thresholds, net_benefit = 0,
                      Model = "Treat None", stringsAsFactors = FALSE),
    all  = data.frame(threshold = thresholds, net_benefit = all_nb,
                      Model = "Treat All", stringsAsFactors = FALSE)
  )
}

## -------------------------------------------------------------------------
## 校准：分箱（十分位）+ Loess 平滑，含 Wilson 95% CI
## -------------------------------------------------------------------------
calibration_bins <- function(prob, y_num, n_bins = 10, min_n = 5) {
  breaks <- unique(stats::quantile(prob, probs = seq(0, 1, length.out = n_bins + 1),
                                   na.rm = TRUE))
  if (length(breaks) < 3) breaks <- seq(0, 1, length.out = n_bins + 1)
  bin_id <- cut(prob, breaks = breaks, include.lowest = TRUE)

  out <- data.frame(
    bin       = levels(bin_id),
    mean_pred = tapply(prob, bin_id, mean, na.rm = TRUE),
    mean_obs  = tapply(y_num, bin_id, mean, na.rm = TRUE),
    n         = as.numeric(tapply(y_num, bin_id, length)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$mean_pred) & out$n >= min_n, , drop = FALSE]
  if (nrow(out) == 0) return(out)

  ## Wilson 区间
  z <- stats::qnorm(0.975)
  ci <- t(mapply(function(p, k) {
    denom  <- 1 + z^2 / k
    center <- (p + z^2 / (2 * k)) / denom
    margin <- z * sqrt((p * (1 - p) + z^2 / (4 * k)) / k) / denom
    c(max(0, center - margin), min(1, center + margin))
  }, out$mean_obs, out$n))
  out$ci_low  <- ci[, 1]
  out$ci_high <- ci[, 2]
  out
}

calibration_loess <- function(prob, y_num, span = 0.75) {
  fit <- tryCatch(stats::loess(y_num ~ prob, span = span), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  pr <- stats::predict(fit, se = TRUE)
  out <- data.frame(
    pred      = prob,
    obs       = as.numeric(pr$fit),
    ci_low    = pmax(0, pr$fit - 1.96 * pr$se.fit),
    ci_high   = pmin(1, pr$fit + 1.96 * pr$se.fit)
  )
  out[order(out$pred), , drop = FALSE]
}

## 校准斜率 / 截距（logit 尺度，斜率 1、截距 0 为理想）
calibration_slope_intercept <- function(prob, y_num) {
  lp <- stats::qlogis(pmin(pmax(prob, 0.001), 0.999))
  fit_int <- tryCatch(stats::glm(y_num ~ offset(lp), family = stats::binomial()),
                      error = function(e) NULL)
  fit_slope <- tryCatch(stats::glm(y_num ~ lp, family = stats::binomial()),
                        error = function(e) NULL)
  list(
    intercept = if (is.null(fit_int)) NA_real_ else unname(stats::coef(fit_int)[1]),
    slope     = if (is.null(fit_slope)) NA_real_ else unname(stats::coef(fit_slope)[2])
  )
}

## 期望校准误差（分箱加权绝对差）
calibration_eci <- function(cal_df) {
  if (is.null(cal_df) || nrow(cal_df) == 0) return(NA_real_)
  stats::weighted.mean(abs(cal_df$mean_obs - cal_df$mean_pred), cal_df$n, na.rm = TRUE)
}

## -------------------------------------------------------------------------
## 过拟合诊断（Train-Test Gap / CV-Test Gap）
## -------------------------------------------------------------------------
overfit_diagnosis <- function(train_auc, test_auc, cv_auc) {
  gap <- train_auc - test_auc
  label <- if (is.na(gap)) {
    "NA"
  } else if (gap > 0.10) {
    "明显过拟合"
  } else if (gap > 0.05) {
    "轻度过拟合"
  } else if (gap < 0) {
    "泛化良好（Test > Train）"
  } else {
    "泛化良好"
  }
  data.frame(
    Train_AUC = train_auc, Test_AUC = test_auc, CV_AUC = cv_auc,
    Train_Test_Gap = gap, CV_Test_Gap = cv_auc - test_auc,
    Diagnosis = label, stringsAsFactors = FALSE
  )
}
