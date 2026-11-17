# =========================================================================
# R/utils.R
# 通用工具：空值运算、读数据、类型清洗、缓存读写、输出写入
# =========================================================================

## -------------------------------------------------------------------------
## 空值合并运算符（原脚本在内部建模部分使用但仅在外部验证脚本中定义，
## 是导致 "could not find function %||%" 的原因）
## -------------------------------------------------------------------------
`%||%` <- function(a, b) {
  if (length(a) == 0 || is.null(a)) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

## -------------------------------------------------------------------------
## 读入数据：按扩展名分派 xlsx / csv
## -------------------------------------------------------------------------
read_input_data <- function(path, sheet = 1) {
  if (!file.exists(path)) {
    stop("找不到输入文件：", path, "\n请按 data/README.md 的说明放置数据文件。",
         call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))
  df <- switch(
    ext,
    xlsx = , xls = as.data.frame(readxl::read_excel(path, sheet = sheet)),
    csv = utils::read.csv(path, stringsAsFactors = FALSE,
                          check.names = FALSE, fileEncoding = "UTF-8-BOM"),
    stop("不支持的文件类型：", ext, call. = FALSE)
  )
  df
}

## -------------------------------------------------------------------------
## 列名标准化 + 基础类型统一
## 注意：make.names 保留中文字符，仅替换空格等非法字符
## -------------------------------------------------------------------------
standardise_columns <- function(df) {
  names(df) <- make.names(names(df))
  df
}

coerce_base_types <- function(df) {
  list_cols <- vapply(df, is.list, logical(1))
  if (any(list_cols)) df <- df[, !list_cols, drop = FALSE]

  logical_cols <- vapply(df, is.logical, logical(1))
  if (any(logical_cols)) df[, logical_cols] <- lapply(df[, logical_cols, drop = FALSE], as.factor)

  char_cols <- vapply(df, is.character, logical(1))
  if (any(char_cols)) df[, char_cols] <- lapply(df[, char_cols, drop = FALSE], as.factor)

  date_cols <- vapply(df, function(x) inherits(x, c("Date", "POSIXct", "POSIXt")), logical(1))
  if (any(date_cols)) {
    for (col in names(df)[date_cols]) df[[col]] <- as.numeric(as.Date(df[[col]]))
  }
  df
}

## -------------------------------------------------------------------------
## 安全转数值：把 "未知" / "不详" / "" 等占位串统一视为缺失
## -------------------------------------------------------------------------
safe_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.factor(x)) x <- as.character(x)
  x <- trimws(x)
  invalid <- c("", "NA", "NULL", "None", "未知", "不详", "missing", "Missing", "NaN", "Inf", "-Inf")
  x[x %in% invalid] <- NA_character_
  suppressWarnings(as.numeric(x))
}

## 结局变量编码：正类 -> 1
encode_y <- function(x, positive = positive_class) as.integer(as.character(x) == positive)

## -------------------------------------------------------------------------
## 正类/负类因子统一（保证 levels 顺序为 否, 是）
## -------------------------------------------------------------------------
as_outcome_factor <- function(x, positive = positive_class, negative = negative_class) {
  factor(as.character(x), levels = c(negative, positive))
}

## -------------------------------------------------------------------------
## 阶段缓存：使 01-08 各脚本可单独重跑
## -------------------------------------------------------------------------
cache_file <- function(name) file.path(paths$cache, paste0(name, ".rds"))

save_stage <- function(obj, name) {
  saveRDS(obj, cache_file(name))
  invisible(obj)
}

load_stage <- function(name) {
  f <- cache_file(name)
  if (!file.exists(f)) {
    stop("缺少阶段缓存：", basename(f), "\n请先运行前置脚本，或运行 run_all.R。",
         call. = FALSE)
  }
  readRDS(f)
}

stage_ready <- function(name) file.exists(cache_file(name))

## 取当前会话对象；不存在则回落到缓存
get_or_load <- function(obj_name, stage) {
  if (exists(obj_name, envir = .GlobalEnv, inherits = FALSE)) {
    get(obj_name, envir = .GlobalEnv)
  } else {
    load_stage(stage)
  }
}

## -------------------------------------------------------------------------
## 统一写出表格 / 图形
## -------------------------------------------------------------------------
write_table <- function(x, filename, quote = TRUE) {
  utils::write.csv(x, file.path(paths$tables, filename),
                   row.names = FALSE, quote = quote, fileEncoding = "UTF-8")
}

write_plot_data <- function(x, filename, quote = TRUE) {
  utils::write.csv(x, file.path(paths$plot_data, filename),
                   row.names = FALSE, quote = quote, fileEncoding = "UTF-8")
}

save_fig <- function(plot, filename, width = 8, height = 6, dpi = 300) {
  ggplot2::ggsave(
    filename = file.path(paths$figures, filename),
    plot = plot, width = width, height = height, dpi = dpi
  )
}

## -------------------------------------------------------------------------
## 控制台小工具
## -------------------------------------------------------------------------
section <- function(title) {
  cat("\n", strrep("=", 74), "\n  ", title, "\n", strrep("=", 74), "\n", sep = "")
}

## 注意：不要命名为 step() —— 会遮蔽 stats::step 与 recipes::step
log_step <- function(...) cat(paste0("  - ", ...), "\n", sep = "")

## 因子文件名安全化（中文变量名不能直接做文件名）
safe_name <- function(x) gsub("[^A-Za-z0-9_.-]", "_", x)
