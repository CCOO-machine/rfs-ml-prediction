# rfs-ml-prediction

重症患者再喂养综合征（refeeding syndrome, RFS）风险预测模型的建模与验证代码。

Machine-learning prediction of refeeding syndrome in critically ill patients: model
development, internal validation and external validation.

代码覆盖完整流程：数据清洗 → 多次插补（MICE）→ 单因素初筛 + LASSO 共识变量筛选 →
8 类候选模型（LR / KNN / RF / GBM / SVM / NN / XGBoost / 朴素贝叶斯）→ 判别力、校准与
临床效用评价 → SHAP 解释 → 风险分层 → 外部验证。

---

## 1. 环境要求

- R ≥ 4.2（推荐 4.4）
- 依赖包：

```r
install.packages(c(
  "readxl", "dplyr", "tidyr", "ggplot2", "caret", "mice", "glmnet",
  "brglm2", "randomForest", "gbm", "xgboost", "kernlab", "nnet",
  "e1071", "pROC", "PRROC", "car", "kernelshap", "shapviz"
))
```

脚本**不会自动安装**包：缺少依赖时会打印安装命令并停止，避免在你不知情的情况下改动环境。

可选：用 `renv` 固定版本以便完全复现

```r
install.packages("renv"); renv::init(); renv::snapshot()
```

## 2. 数据准备

把两个数据文件放进 `data/`（详见 `data/README.md`）：

```
data/Modeldata2.xlsx                   # 开发队列
data/External_validation_data.xlsx     # 外部验证队列
```

仓库**不含任何患者数据**，`.gitignore` 已排除所有数据文件。字段说明见
`data/data_dictionary.csv`。

## 3. 运行

在仓库根目录执行：

```bash
Rscript run_all.R               # 复用缓存，只跑未完成的阶段
Rscript run_all.R --reset       # 全量重跑
Rscript run_all.R --from=05     # 从第 5 阶段开始（需已有 03/04 缓存）
```

或在 RStudio 里：

```r
source("run_all.R")
```

各阶段也可以单独执行（需先有上一阶段的缓存）：

```r
source("config/config.R"); source("R/utils.R"); source("R/preprocess.R")
source("R/metrics.R"); source("R/models.R")
source("scripts/05_interpret_and_figures.R")
```

## 4. 目录结构

```
.
├── run_all.R                     # 全流程入口
├── config/
│   └── config.R                  # 所有可调参数、路径、模型清单
├── R/
│   ├── utils.R                   # 读数据、类型清洗、缓存、写出
│   ├── preprocess.R              # 数据清洗、插补后处理、测试集对齐、完美分离检查
│   ├── metrics.R                 # AUC/AUPRC/Brier、Bootstrap CI、DCA、校准
│   └── models.R                  # caret 统一接口、XGBoost、朴素贝叶斯、通用预测
├── scripts/
│   ├── 01_data_prep.R            # 读入、划分、缺失诊断、Table 1
│   ├── 02_impute_select.R        # MICE、单因素、LASSO 共识、VIF、分离检查
│   ├── 03_train_models.R         # 8 类模型 x M 套插补
│   ├── 04_evaluate.R             # 指标 + Bootstrap CI + DeLong + 校准 + DCA + 过拟合
│   ├── 05_interpret_and_figures.R# ROC/校准/DCA 图、森林图、Gini、SHAP、瀑布图
│   ├── 06_risk_stratify.R        # 风险四分位分层
│   ├── 07_export_plot_data.R     # 导出绘图数据
│   ├── 08_external_validation.R  # 外部验证
│   └── 99_session_info.R         # 记录运行环境
├── data/                         # 数据（不入库）
├── cache/                        # 阶段缓存、模型对象（不入库）
├── outputs/
│   ├── tables/                   # CSV 结果表
│   ├── figures/                  # PNG 图
│   └── plot_data/                # 绘图用中间数据
├── docs/
│   ├── CHANGES.md                # 相对原始脚本的变更与修复说明
│   ├── parse_check.R             # 语法检查（Rscript docs/parse_check.R）
│   └── smoke_test.R              # 合成数据端到端冒烟测试
```

## 5. 关键参数（`config/config.R`）

| 参数 | 默认 | 含义 |
|---|---|---|
| `outcome_var` | `"F1RFS"` | 结局变量列名 |
| `positive_class` | `"是"` | 事件标签 |
| `train_fraction` | 0.70 | 训练集比例（分层划分） |
| `m_imputations` | 5 | 多重插补套数 |
| `cv_folds` / `cv_repeats` | 5 / 3 | 5 折 × 3 次重复交叉验证 |
| `smote_ratio_cutoff` | 2 | 类别不平衡 >2:1 时启用 SMOTE |
| `p_threshold` | 0.05 | 单因素初筛阈值（可放宽到 0.10） |
| `lasso_consensus_frac` | 0.60 | LASSO 共识：变量需出现在 ≥60% 插补集 |
| `miss_cutoff_pct` | 20 | 缺失率剔除阈值（%） |
| `model_list` | 8 个模型 | 建模清单 |
| `focus_model` | `"RF"` | 需要做 SHAP / 风险分层的模型 |

## 6. 主要输出

| 文件 | 内容 |
|---|---|
| `outputs/tables/01_table1_baseline.csv` | 基线特征表（含检验方法与 P 值） |
| `outputs/tables/02_univariate_results.csv` | 单因素分析 |
| `outputs/tables/02_lasso_selection_frequency.csv` | LASSO 各变量入选频率 |
| `outputs/tables/04_model_metrics*.csv` | 训练/测试集全指标，含 Bootstrap 95% CI |
| `outputs/tables/04_overfitting_diagnostics.csv` | Train-Test Gap 过拟合诊断 |
| `outputs/tables/04_delong_test.csv` | 模型间 AUC 差异 |
| `outputs/tables/04_calibration_slope_intercept.csv` | 校准斜率/截距、ECI |
| `outputs/tables/04_dca_curves.csv` | 决策曲线净收益 |
| `outputs/tables/05_rf_variable_importance.csv` | Gini 重要性（按原变量合并） |
| `outputs/tables/05_shap_importance.csv` | SHAP 重要性 |
| `outputs/tables/06_risk_stratification.csv` | 风险分层与事件率 |
| `outputs/tables/08_external_validation_metrics*.csv` | 外部验证指标 |
| `outputs/tables/08_internal_vs_external.csv` | 内部 vs 外部性能衰减 |
| `outputs/tables/99_session_info.txt` | 运行环境与包版本 |

## 7. 可复现性说明

- 全流程固定 `seed_val = 123`；MICE、数据划分、SMOTE、Bootstrap、SHAP 采样均显式设种。
- 每阶段的中间结果落在 `cache/`，可单独重跑；`--reset` 保证从零开始可复现。
- 训练/测试划分与插补在**同一脚本内**完成，测试集不参与变量筛选与调参。
- 分类阈值在**训练集**上用 Youden 指数确定后固定用于测试集（避免阈值信息泄漏）。
- 运行 `scripts/99_session_info.R` 可导出包版本，投稿时可作为补充材料。

### 已验证

本仓库用合成数据跑通过完整流程（不含任何真实数据）：

```r
Rscript docs/parse_check.R    # 15 个 R 文件语法检查，全部 OK
Rscript docs/smoke_test.R     # 01-08 全流程，产出 23 张表 + 14 张图
```

冒烟测试使用 300 例开发队列 + 180 例外部队列、3 折 × 1 次重复、2 套插补、
`model_list = c("LR","RF","XGBoost","NB")`，约 2-4 分钟完成。
它覆盖了中文列名、缺失值插补、因子水平对齐、XGBoost 列对齐与
外部验证的完整链路。

## 8. 与 TRIPOD+AI 的对应

代码中的输出可直接填写 TRIPOD+AI 清单的主要条目：样本量与事件数（`01`）、
缺失数据与插补方法（`02`）、变量选择过程（`02`）、超参数与内部验证方式（`03`）、
判别力与校准及置信区间（`04`）、临床效用（`04`）、模型可解释性（`05`）、
风险分层（`06`）、外部验证（`08`）。

> 注意：TRIPOD+AI 要求报告“模型如何被他人使用”，本仓库只提供训练与验证代码；
> 网页计算器的部署代码如单独维护，请在论文中给出其仓库地址/Docker 镜像。

## 9. 数据与代码可用性

- 本仓库公开的是**代码**，不含患者数据。
- 建议把本仓库归档到 Zenodo 获取 DOI，并在论文的 Data Availability Statement 中引用。
- 示例引用格式（Vancouver）：

```
<Authors>. Code for: Machine-learning prediction of refeeding syndrome in critically
ill patients. Zenodo [Code] (2026) doi:10.5281/zenodo.XXXXXXX
```

## 10. 免责声明

本代码用于研究目的，**不是**经过监管批准的医疗器械或临床决策工具，
不得直接用于个体患者的诊疗决策。

## 11. 许可

MIT License，见 `LICENSE`。
