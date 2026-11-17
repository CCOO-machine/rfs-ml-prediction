# =========================================================================
# scripts/01_data_prep.R
# 读入数据 -> 结局处理 -> EPV -> 训练/测试划分 -> 缺失率诊断 -> Table 1
# 输出：cache/stage01_split.rds
# =========================================================================

section("01 数据读入、划分与基线表")

## -------------------------------------------------------------------------
## 1. 读入与结局
## -------------------------------------------------------------------------
dat <- read_input_data(paths$raw_data) |>
  standardise_columns() |>
  coerce_base_types()

if (!outcome_var %in% names(dat)) {
  stop("结局变量 ", outcome_var, " 不在数据列名中，请检查 config/config.R。", call. = FALSE)
}
dat[[outcome_var]] <- as_outcome_factor(dat[[outcome_var]])

log_step("总样本：", nrow(dat))
print(table(dat[[outcome_var]], useNA = "ifany"))

## -------------------------------------------------------------------------
## 2. EPV
## -------------------------------------------------------------------------
n_events <- min(table(dat[[outcome_var]]))
log_step(sprintf("阴性事件：%d，阳性事件：%d", table(dat[[outcome_var]])[negative_class],
             table(dat[[outcome_var]])[positive_class]))
log_step(sprintf("若最终 10 个预测因子，EPV = %.1f（一般要求 >= 10）", n_events / 10))

## -------------------------------------------------------------------------
## 3. 分层划分训练/测试集
## -------------------------------------------------------------------------
set.seed(seed_val)
train_index <- caret::createDataPartition(dat[[outcome_var]], p = train_fraction, list = FALSE)
train_set <- dat[train_index, , drop = FALSE]
test_set  <- dat[-train_index, , drop = FALSE]

log_step(sprintf("训练集：%d（%.1f%%）", nrow(train_set), 100 * nrow(train_set) / nrow(dat)))
log_step(sprintf("测试集：%d（%.1f%%）", nrow(test_set),  100 * nrow(test_set)  / nrow(dat)))

## -------------------------------------------------------------------------
## 4. 缺失率诊断（只读，不改数据）
## -------------------------------------------------------------------------
miss_tab <- data.frame(
  variable      = names(train_set),
  type          = vapply(train_set, function(x) class(x)[1], character(1)),
  n_missing     = colSums(is.na(train_set)),
  pct_train     = round(100 * colMeans(is.na(train_set)), 2),
  pct_test      = round(100 * colMeans(is.na(test_set[, names(train_set), drop = FALSE])), 2),
  stringsAsFactors = FALSE
)
miss_tab <- miss_tab[order(-miss_tab$pct_train), , drop = FALSE]
write_table(miss_tab, "01_missing_summary.csv")
log_step(sprintf("缺失 >20%% 的变量：%d 个", sum(miss_tab$pct_train > miss_cutoff_pct)))
log_step("临床保留（高于阈值但不剔除）：", paste(vars_keep_high_miss, collapse = ", "))

## 高缺失变量清单（供 02 使用）
high_miss_vars <- setdiff(
  miss_tab$variable[miss_tab$pct_train > miss_cutoff_pct],
  c(vars_keep_high_miss, outcome_var)
)
if (length(high_miss_vars) > 0) {
  log_step("拟剔除的高缺失变量：", paste(high_miss_vars, collapse = ", "))
}

## -------------------------------------------------------------------------
## 5. 连续变量误判修正
## -------------------------------------------------------------------------
for (v in intersect(vars_force_numeric_tab1, names(train_set))) {
  if (is.factor(train_set[[v]]) || is.character(train_set[[v]])) {
    train_set[[v]] <- as.numeric(as.character(train_set[[v]]))
    test_set[[v]]  <- suppressWarnings(as.numeric(as.character(test_set[[v]])))
    log_step(v, " 转为数值型")
  }
}

## -------------------------------------------------------------------------
## 6. Table 1
##   原脚本把正态性检验结果以字符向量传给 tableone::CreateTableOne(testNormal = )，
##   该参数只接受函数，会报错。此处改为自建表格：逐变量选择检验方法，
##   连续变量按正态性决定 Mean±SD 或 Median(IQR)。
## -------------------------------------------------------------------------
build_table1 <- function(df, outcome) {
  grp <- df[[outcome]]
  lv  <- levels(grp)
  vars <- setdiff(names(df), outcome)
  rows <- list()

  for (v in vars) {
    x <- df[[v]]
    if (is.numeric(x)) {
      n_by <- tapply(x, grp, function(z) sum(!is.na(z)))
      ## 正态性：Shapiro-Wilk，按组
      normal <- all(vapply(lv, function(g) {
        z <- x[grp == g]; z <- z[!is.na(z)]
        if (length(z) < 3 || length(z) > 5000 || length(unique(z)) <= 1) return(FALSE)
        stats::shapiro.test(z)$p.value > 0.05
      }, logical(1)))
      ## 方差齐性
      eq_var <- tryCatch({
        z <- data.frame(x = x, g = grp)
        z <- z[!is.na(z$x), ]
        car::leveneTest(x ~ g, data = z)$`Pr(>F)`[1] > 0.05
      }, error = function(e) FALSE)

      p_val <- tryCatch({
        if (normal && isTRUE(eq_var)) {
          stats::t.test(x ~ grp)$p.value
        } else {
          stats::wilcox.test(x ~ grp)$p.value
        }
      }, error = function(e) NA_real_)

      if (normal) {
        desc <- sprintf("%.2f ± %.2f (%d)", tapply(x, grp, mean, na.rm = TRUE),
                        tapply(x, grp, stats::sd, na.rm = TRUE), n_by)
      } else {
        desc <- sprintf("%.2f (%.2f–%.2f) (%d)",
                        tapply(x, grp, stats::median, na.rm = TRUE),
                        tapply(x, grp, stats::quantile, probs = 0.25, na.rm = TRUE),
                        tapply(x, grp, stats::quantile, probs = 0.75, na.rm = TRUE), n_by)
      }
      rows[[length(rows) + 1]] <- data.frame(
        variable = v, level = "", distribution = if (normal) "Mean ± SD" else "Median (IQR)",
        value_1 = unname(desc[1]), value_2 = unname(desc[2]),
        p_value = p_val, test = if (normal && isTRUE(eq_var)) "t-test" else "Mann-Whitney",
        stringsAsFactors = FALSE
      )
    } else if (is.factor(x)) {
      tab <- table(x, grp)
      p_val <- tryCatch({
        if (any(tab < 5)) stats::fisher.test(tab)$p.value else stats::chisq.test(tab)$p.value
      }, error = function(e) NA_real_)
      for (k in rownames(tab)) {
        rows[[length(rows) + 1]] <- data.frame(
          variable = v, level = k,
          distribution = "n (%)",
          value_1 = sprintf("%d (%.1f)", tab[k, 1], 100 * tab[k, 1] / sum(tab[, 1])),
          value_2 = sprintf("%d (%.1f)", tab[k, 2], 100 * tab[k, 2] / sum(tab[, 2])),
          p_value = p_val,
          test = if (any(tab < 5)) "Fisher" else "Chi-square",
          stringsAsFactors = FALSE
        )
      }
    }
  }
  out <- do.call(rbind, rows)
  names(out)[names(out) == "value_1"] <- paste0("group_", lv[1])
  names(out)[names(out) == "value_2"] <- paste0("group_", lv[2])
  out
}

tab1 <- build_table1(train_set, outcome_var)
write_table(tab1, "01_table1_baseline.csv")
log_step("Table 1 行数：", nrow(tab1))

## -------------------------------------------------------------------------
## 7. 保存阶段
## -------------------------------------------------------------------------
stage01 <- list(
  train_set     = train_set,
  test_set      = test_set,
  n_events      = n_events,
  miss_tab      = miss_tab,
  high_miss_vars = high_miss_vars,
  table1        = tab1
)
save_stage(stage01, "stage01_split")
log_step("已保存阶段缓存：stage01_split")
