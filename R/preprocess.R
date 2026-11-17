# =========================================================================
# R/preprocess.R
# 数据清洗与插补后处理
#
# 说明：原脚本在第 7 节调用 preprocess_data()，却把它定义在第 8 节，
# 这在干净会话中会直接报 "could not find function" 错误。此处提前定义。
# =========================================================================

## -------------------------------------------------------------------------
## 单侧 IQR 缩尾
## -------------------------------------------------------------------------
winsorize_iqr <- function(x, k = 1.5) {
  if (!is.numeric(x)) return(x)
  q <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  if (!is.finite(iqr) || iqr == 0) return(x)
  pmin(pmax(x, q[1] - k * iqr), q[2] + k * iqr)
}

## -------------------------------------------------------------------------
## 主清洗函数
##   - 去掉空白水平因子 / 单水平因子
##   - 丢弃 ID 类列
##   - 可转数值的字符列自动转数值（>80% 可解析）
##   - 指定变量缩尾
##   - Inf/NaN -> NA
## -------------------------------------------------------------------------
preprocess_data <- function(data) {
  data <- as.data.frame(data)

  ## 空白水平 -> 显式缺失
  factor_cols <- vapply(data, is.factor, logical(1))
  if (any(factor_cols)) {
    data[, factor_cols] <- lapply(data[, factor_cols, drop = FALSE], function(f) {
      if (any(levels(f) == "")) {
        levels(f)[levels(f) == ""] <- NA
      }
      droplevels(f)
    })
  }

  ## 移除单水平因子
  single_level_vars <- names(data)[
    vapply(data, function(x) is.factor(x) && nlevels(x) <= 1, logical(1))
  ]
  if (length(single_level_vars) > 0) {
    data <- data[, !(names(data) %in% single_level_vars), drop = FALSE]
  }

  ## 移除 ID 类列
  remove_vars <- intersect(drop_columns, names(data))
  if (length(remove_vars) > 0) {
    data <- data[, !(names(data) %in% remove_vars), drop = FALSE]
  }

  ## 自动识别可转数值的列
  auto_num <- vapply(data, function(x) {
    if (is.character(x) || is.factor(x)) {
      num <- safe_numeric(x)
      mean(!is.na(num)) > 0.8
    } else {
      FALSE
    }
  }, logical(1))

  vars_to_convert <- union(vars_to_numeric, names(auto_num)[auto_num])
  for (col in intersect(vars_to_convert, names(data))) {
    if (is.factor(data[[col]]) || is.character(data[[col]])) {
      data[[col]] <- safe_numeric(data[[col]])
    }
  }

  ## 缩尾
  if (!is.null(winsorize_var) && winsorize_var %in% names(data) &&
      is.numeric(data[[winsorize_var]])) {
    data[[winsorize_var]] <- winsorize_iqr(data[[winsorize_var]])
  }

  ## Inf / NaN
  num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  for (col in num_cols) {
    x <- data[[col]]
    bad <- is.nan(x) | is.infinite(x)
    if (any(bad)) {
      x[bad] <- NA
      data[[col]] <- x
    }
  }

  ## 二次清理空白水平
  factor_cols <- vapply(data, is.factor, logical(1))
  for (col in names(data)[factor_cols]) {
    if ("" %in% levels(data[[col]])) {
      levels(data[[col]])[levels(data[[col]]) == ""] <- NA
      data[[col]] <- droplevels(data[[col]])
    }
  }

  data
}

## -------------------------------------------------------------------------
## 训练集补充清理：全 NA 列、结局缺失行、公共列、单水平因子
## -------------------------------------------------------------------------
clean_imputed_list <- function(imp_list, outcome = outcome_var) {
  imp_list <- lapply(imp_list, preprocess_data)

  ## 全 NA 列
  imp_list <- lapply(imp_list, function(d) {
    all_na <- vapply(d, function(x) all(is.na(x)), logical(1))
    if (any(all_na)) d <- d[, !all_na, drop = FALSE]
    d
  })

  ## 结局缺失样本
  n_dropped <- 0L
  imp_list <- lapply(imp_list, function(d) {
    miss <- is.na(d[[outcome]])
    if (any(miss)) {
      n_dropped <<- n_dropped + sum(miss)
      d <- d[!miss, , drop = FALSE]
    }
    d
  })
  if (n_dropped > 0) log_step("删除结局缺失样本：", n_dropped, " 例")

  ## 只保留各套共有的列
  common_cols <- Reduce(intersect, lapply(imp_list, names))
  imp_list <- lapply(imp_list, function(d) d[, common_cols, drop = FALSE])

  ## 删除任一插补集中退化为单水平的因子
  bad <- unique(unlist(lapply(imp_list, function(d) {
    names(d)[vapply(d, function(x) is.factor(x) && nlevels(x) <= 1, logical(1))]
  })))
  if (length(bad) > 0) {
    log_step("删除单水平因子：", paste(bad, collapse = ", "))
    imp_list <- lapply(imp_list, function(d) d[, !(names(d) %in% bad), drop = FALSE])
  }

  imp_list
}

## -------------------------------------------------------------------------
## 残余缺失兜底填充（中位数 / 众数）
## -------------------------------------------------------------------------
impute_residual_na <- function(d, outcome = outcome_var) {
  for (v in setdiff(names(d), outcome)) {
    idx <- is.na(d[[v]])
    if (!any(idx)) next
    if (is.numeric(d[[v]])) {
      med <- stats::median(d[[v]], na.rm = TRUE)
      if (is.na(med)) med <- 0
      d[[v]][idx] <- med
    } else if (is.factor(d[[v]])) {
      mode_val <- names(which.max(table(d[[v]])))
      if (is.null(mode_val)) mode_val <- levels(d[[v]])[1]
      d[[v]][idx] <- mode_val
    }
  }
  d
}

## -------------------------------------------------------------------------
## 测试集安全对齐：把测试集强行对齐到训练集的类型与水平，
## 避免 caret::predict 因水平不匹配报错
## -------------------------------------------------------------------------
prep_test_safe <- function(test, train_d, outcome = outcome_var) {
  common_vars <- intersect(names(test), names(train_d))
  test <- test[, common_vars, drop = FALSE]

  for (v in common_vars) {
    if (v == outcome) next
    train_col <- train_d[[v]]
    test_col  <- test[[v]]

    if (is.numeric(train_col)) {
      if (!is.numeric(test_col)) {
        if (is.factor(test_col)) test_col <- as.character(test_col)
        test_col <- suppressWarnings(as.numeric(test_col))
      }
      test_col[is.na(test_col)] <- stats::median(train_col, na.rm = TRUE)
      test[[v]] <- test_col

    } else if (is.factor(train_col)) {
      trl <- levels(train_col)
      if (!is.factor(test_col)) test_col <- factor(as.character(test_col), levels = trl)
      test_chr <- as.character(test_col)
      novel <- setdiff(unique(test_chr[!is.na(test_chr)]), trl)
      if (length(novel) > 0) test_chr[test_chr %in% novel] <- NA_character_
      test_col <- factor(test_chr, levels = trl)
      if (any(is.na(test_col))) {
        test_col[is.na(test_col)] <- names(which.max(table(train_col)))
      }
      test[[v]] <- test_col
    }
  }

  ## 最后兜底
  for (v in names(test)) {
    if (v == outcome || !any(is.na(test[[v]]))) next
    if (is.numeric(test[[v]])) {
      test[[v]][is.na(test[[v]])] <- stats::median(train_d[[v]], na.rm = TRUE)
    } else if (is.factor(test[[v]])) {
      test[[v]][is.na(test[[v]])] <- names(which.max(table(train_d[[v]])))
    }
  }
  test
}

## -------------------------------------------------------------------------
## 完美分离 / 退化变量检查
## -------------------------------------------------------------------------
check_perfect_sep <- function(d, outcome = outcome_var) {
  y <- d[[outcome]]
  to_remove <- character(0)

  for (v in setdiff(names(d), outcome)) {
    x <- d[[v]]

    if (all(is.na(x))) {
      to_remove <- c(to_remove, v)
      next
    }

    if (is.factor(x) && length(unique(stats::na.omit(x))) > 1) {
      tab <- table(x, y)
      zero_ratio <- sum(tab == 0) / (nrow(tab) * ncol(tab))
      if (zero_ratio > 0.5 && sum(tab) > 20) {
        to_remove <- c(to_remove, v)
        next
      }
    }

    if (is.numeric(x)) {
      valid <- !is.na(x) & !is.na(y)
      x_clean <- x[valid]
      y_num <- encode_y(y[valid])
      if (length(x_clean) < 5) {
        to_remove <- c(to_remove, v)
        next
      }
      x_sd <- stats::sd(x_clean, na.rm = TRUE)
      if (is.na(x_sd) || x_sd < 1e-10 || length(unique(x_clean)) <= 1) {
        to_remove <- c(to_remove, v)
        next
      }
      rho <- suppressWarnings(stats::cor(x_clean, y_num, method = "spearman"))
      if (!is.na(rho) && abs(rho) > 0.999) to_remove <- c(to_remove, v)
    }
  }
  unique(to_remove)
}
