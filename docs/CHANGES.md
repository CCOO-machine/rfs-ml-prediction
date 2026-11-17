# 相对原始脚本的变更记录 / Changes vs. the original script

原始脚本是一份约 2500 行的单文件 R 脚本（内部建模 + 外部验证连写）。本仓库把它
拆成 8 个可独立重跑的阶段，并修正了若干会导致运行中断或结论偏倚的问题。

## 一、按需求删除的部分

| 位置 | 处理 |
|---|---|
| 外部验证脚本 **"12. 模型再校准（最优模型）"** 整节 | **已删除**。原节用外部数据估计 α/β 再校准系数，且仅对硬编码的 `best_model_name` 生效。现不再估计、也不再施加再校准，外部验证只报告原始模型的 AUC/AUPRC/Brier/校准斜率与 ECI。 |
| 模型集成（Ensemble） | 原脚本中已删除，本仓库不再保留相关占位。 |
| 死代码 | 注释掉的 GBM 网格块、重复定义的 `compute_dca()` / `compute_calibration()`、结尾的 `str()` 调试打印、`install.packages()` 调用、"编码诊断"调试块，全部移除。 |
| 硬编码的本机路径 | `F:/myproject/...` 全部改为 `config/config.R` 中的相对路径。 |
| 硬编码阈值 `threshold <- 0.4645` | 改为由训练集 Youden 指数计算（`youden_threshold()`）。 |

## 二、关于"最优模型"

原脚本在评估之后用 5 条自动规则 + 多数投票给出推荐，然后**硬编码** `best_model_name <- "RF"`，
并在 SHAP、瀑布图、风险分层、部署导出中固定使用 RF。

本仓库的处理：

- 保留 `04_evaluate.R` 的全部指标与 DeLong 检验，**不再输出"哪个模型最好"的结论**；
- 需要指定模型的环节（SHAP、瀑布图、风险分层）改由 `config/config.R` 的
  `focus_model` 控制，默认 `"RF"`；
- 该参数应在看到测试集结果**之前**确定（例如依据预设的主要分析计划），
  否则等价于用测试集挑选模型，属于乐观偏倚。

> 如需把 `focus_model` 也彻底去掉、改为对全部模型批量执行 SHAP 与风险分层，
> 可把 `config/config.R::focus_models` 设为 `model_list`，并把 05/06 脚本中的
> `focus_model` 换成循环变量。目前的默认值只是为了控制运行时间。

## 三、修复的错误（原脚本会直接报错或静默出错）

| # | 问题 | 后果 | 处理 |
|---|---|---|---|
| 1 | `trainControl(repeats = cv_repeats)` 中 `cv_repeats` 从未定义 | 建模阶段第一行就报 `object 'cv_repeats' not found` | 在 `config.R` 定义 `cv_repeats <- 3` |
| 2 | `%||%` 运算符仅在外部验证脚本中定义，内部脚本却使用 | 综合评分段报 `could not find function "%||%"` | 统一在 `R/utils.R` 定义 |
| 3 | `preprocess_data()` 在第 7 节被调用，却定义在第 8 节 | 干净会话中报 `could not find function "preprocess_data"` | 提前到 `R/preprocess.R`，由 `run_all.R` 先加载 |
| 4 | `saveRDS(mice_obj, "saved_models/mids_train.rds")` 早于 `dir.create("saved_models")` | 报 "cannot open the connection" | 目录在 `config.R` 中统一创建 |
| 5 | `cv.glmnet(formula, data = d)` 使用公式接口 | 该接口来自 **glmnetUtils**，仅装 glmnet 会报错 | 改为显式构造 `model.matrix()` + `y` |
| 6 | `CreateTableOne(testNormal = cont_test_method)` 传入字符向量 | `testNormal` 只接受函数，报错 | 自建 `build_table1()`：逐变量做正态性/方差齐性检验并选择检验方法 |
| 7 | bootstrap 中 `quantile()` 返回值带 `"2.5%"` 名字 | `c(est=..., lower=...)` 混入非法名 | `unname()` 剥离（原脚本已修，此处保留） |
| 8 | `geom_line(size = )` / `geom_hline(size = )` | ggplot2 ≥ 3.4 报弃用警告 | 改为 `linewidth` |
| 9 | 分类指标阈值在**测试集**上用 Youden 选取 | 阈值信息泄漏，灵敏度和特异度偏乐观 | 阈值在训练集上确定，再固定用于测试集 |
| 10 | 注释称逻辑回归为 "Firth 偏倚校正"，实际 `caret::train(method="glm")` 是普通极大似然 | 方法学描述与实际实现不符 | 代码改为标准 logistic；论文中请按实际方法描述（如需 Firth，请显式用 `brglm2::brglmFit` 或 `logistf`） |
| 11 | 变量重要性把因子哑变量与数值变量混在一起排名 | 多水平因子被重复计数 | 按原变量名合并（因子各水平 Gini 求和） |
| 12 | 原脚本重复定义 `compute_dca()`、`compute_calibration()` 且签名不同 | 后定义者静默覆盖前者 | 每种函数只保留一份，签名统一放在 `R/metrics.R` |

## 四、结构性改动

- **阶段缓存**：每个阶段把结果写到 `cache/stage0X_*.rds`，`run_all.R` 默认跳过已完成阶段；
  `--reset` 全量重跑，`--from=05` 从指定阶段开始。
- **路径与参数集中**：所有可调项在 `config/config.R`，脚本中不再出现绝对路径。
- **输出分区**：`outputs/tables`（表）、`outputs/figures`（图）、`outputs/plot_data`（绘图数据）、
  `cache/models`（模型对象）。
- **不自动安装 R 包**：缺失依赖时给出安装命令并停止，避免在他人机器上静默改动环境。

## 五、仍需你确认的事项

1. `config/config.R::focus_model` 的取值是否符合你的预设分析计划。
2. `R/metrics.R` 中 AUC 的方向假设为“概率越高越可能事件”；若某模型输出方向相反，会得到 <0.5 的 AUC。
3. 变量名为中文，跨平台协作时请注意文件编码（建议全程 UTF-8，见 `.Rprofile`）。
4. 外部队列的“外部”性质（时间外部 / 地理外部）需在论文中明确，代码本身无法判断。
