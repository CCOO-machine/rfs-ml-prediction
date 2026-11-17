#!/usr/bin/env Rscript
# =========================================================================
# run_all.R
# 全流程入口。请在仓库根目录执行：
#   Rscript run_all.R                  # 复用已有缓存，只跑缺失阶段
#   Rscript run_all.R --reset          # 全部重跑
#   Rscript run_all.R --from=05        # 从 05 开始（03/04 需已有缓存）
# =========================================================================

args <- commandArgs(trailingOnly = TRUE)
if (any(args == "--reset")) reset_cache <- TRUE

from_stage <- 1L
from_arg <- grep("^--from=", args, value = TRUE)
if (length(from_arg) == 1) from_stage <- as.integer(sub("^--from=", "", from_arg))

source("config/config.R")
source("R/utils.R")
source("R/preprocess.R")
source("R/metrics.R")
source("R/models.R")

stages <- list(
  list(id = 1L, name = "stage01_split",    file = "scripts/01_data_prep.R"),
  list(id = 2L, name = "stage02_select",   file = "scripts/02_impute_select.R"),
  list(id = 3L, name = "stage03_models",   file = "scripts/03_train_models.R"),
  list(id = 4L, name = "stage04_eval",     file = "scripts/04_evaluate.R"),
  list(id = 5L, name = "stage05_interpret",file = "scripts/05_interpret_and_figures.R"),
  list(id = 6L, name = NA_character_,      file = "scripts/06_risk_stratify.R"),
  list(id = 7L, name = NA_character_,      file = "scripts/07_export_plot_data.R"),
  list(id = 8L, name = "stage08_external", file = "scripts/08_external_validation.R")
)

t_start <- Sys.time()

for (st in stages) {
  if (st$id < from_stage) {
    message("[skip] 阶段 ", st$id, " 早于 --from 指定值")
    next
  }
  if (!is.na(st$name) && !reset_cache && stage_ready(st$name)) {
    message("[cached] 阶段 ", st$id, "（", st$name, "）已存在，跳过")
    next
  }
  message("\n>>> 运行阶段 ", st$id, "：", st$file)
  sys.source(st$file, envir = globalenv())
}

message("\n完成，用时 ", round(difftime(Sys.time(), t_start, units = "mins"), 1), " 分钟")
message("表格：", paths$tables)
message("图形：", paths$figures)
message("绘图数据：", paths$plot_data)
