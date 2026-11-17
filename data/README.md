# 数据说明 / Data

本目录**不包含**任何患者数据。仓库有意通过 `.gitignore` 排除所有数据文件。

## 需要你自行放入的文件

| 文件名（约定） | 内容 | 说明 |
|---|---|---|
| `Modeldata2.xlsx` | 开发队列（建模用） | 第一行为变量名，含结局列 `F1RFS` |
| `External_validation_data.xlsx` | 外部验证队列 | 变量名与开发队列一致，含 `F1RFS` |

如文件名或路径不同，请修改 `config/config.R` 中的 `paths$raw_data` 与 `paths$ext_data`。
也支持 `.csv`（UTF-8 编码）。

## 字段说明

见 `data_dictionary.csv`。变量名为中文（源自原始数据库导出），
`R/utils.R::standardise_columns()` 会用 `make.names()` 处理，请勿手动改名，
除非同时更新 `config/config.R` 中的 `vars_to_numeric` 等列表。

## 结局编码

- 正类（事件）：`是`，对应 `config/config.R::positive_class`
- 负类：`否`
- 若你的数据用 0/1 或其他标签，请同时修改 `positive_class` /
  `negative_class`，并确认 `as_outcome_factor()` 能正确排序水平。

## 上传前必须做的自查

1. 数据文件里是否含姓名、住院号、身份证号、精确入院日期、机构名称？
2. 是否需要按《人类遗传资源管理条例》办理对外提供备案/审批？
3. 若对外公开去标识化数据，建议提交至 ScienceDB、Zenodo、figshare 等可分配
   DOI 的仓库，并在论文的 Data Availability Statement 中引用该 DOI。

> 提示：本仓库的“补充材料”一旦上传到期刊即公开发布且永久可访问，
> 切勿把原始患者数据作为补充材料上传。
