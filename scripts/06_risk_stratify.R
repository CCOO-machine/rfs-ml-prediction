# =========================================================================
# scripts/06_risk_stratify.R
# 风险分层（按预测概率四分位）：分组事件率、组间差异、分层图
# 输出：outputs/tables/06_*, outputs/figures/06_*
# =========================================================================

section("06 风险分层")

stage03 <- get_or_load("stage03", "stage03_models")
stage04 <- get_or_load("stage04", "stage04_eval")
stage05 <- get_or_load("stage05", "stage05_interpret")

focus_model <- stage05$focus_model
prob  <- stage03$models_pooled[[focus_model]]$pooled_prob
y_num <- stage04$y_test

## 四分位分组（用训练集分位点更稳妥；此处按测试集分位，便于展示分布）
brks <- unique(stats::quantile(prob, probs = seq(0, 1, 0.25), na.rm = TRUE))
if (length(brks) < 5) {
  ## 预测概率高度集中时退化为等距分组
  brks <- seq(min(prob, na.rm = TRUE), max(prob, na.rm = TRUE), length.out = 5)
}
risk_group <- cut(prob, breaks = brks, include.lowest = TRUE,
                  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)"))

risk_table <- data.frame(
  risk_group   = levels(risk_group),
  n            = as.numeric(table(risk_group)),
  events       = as.numeric(tapply(y_num, risk_group, sum)),
  event_rate   = round(as.numeric(tapply(y_num, risk_group, mean)), 3),
  mean_pred    = round(as.numeric(tapply(prob, risk_group, mean)), 3),
  sd_pred      = round(as.numeric(tapply(prob, risk_group, stats::sd)), 3),
  stringsAsFactors = FALSE
)
risk_table$event_pct <- sprintf("%d/%d (%.1f%%)",
                                risk_table$events, risk_table$n, 100 * risk_table$event_rate)
write_table(risk_table, "06_risk_stratification.csv")

cat("\n===== 风险分层（", focus_model, "）=====\n", sep = "")
print(risk_table[, c("risk_group", "n", "events", "event_rate", "mean_pred")], row.names = FALSE)

## 最高 vs 最低四分位
tab2 <- matrix(c(risk_table$events[4], risk_table$n[4] - risk_table$events[4],
                 risk_table$events[1], risk_table$n[1] - risk_table$events[1]),
               nrow = 2, byrow = TRUE,
               dimnames = list(c("Q4", "Q1"), c("event", "no_event")))
ft <- tryCatch(stats::fisher.test(tab2), error = function(e) NULL)
if (!is.null(ft)) {
  log_step(sprintf("Q4 vs Q1：OR = %.2f（95%% CI %.2f–%.2f），P = %.4g",
               ft$estimate, ft$conf.int[1], ft$conf.int[2], ft$p.value))
}

## 趋势检验（Cochran-Armitage 近似：对分组序号做 logistic）
trend_fit <- tryCatch(
  stats::glm(y_num ~ as.numeric(risk_group), family = stats::binomial()),
  error = function(e) NULL
)
if (!is.null(trend_fit)) {
  p_trend <- summary(trend_fit)$coefficients[2, 4]
  log_step(sprintf("趋势检验 P = %.4g", p_trend))
  write_table(data.frame(trend_p = p_trend), "06_trend_test.csv")
}

## -------------------------------------------------------------------------
## 图：分层事件率 + 分层内部构成
## -------------------------------------------------------------------------
p_risk <- ggplot(risk_table, aes(x = risk_group, y = event_rate, fill = risk_group)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = event_pct), vjust = -0.4, size = 3.4) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1, guide = "none") +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", 100 * x),
                     limits = c(0, max(risk_table$event_rate) * 1.25)) +
  labs(title = sprintf("Observed event rate by predicted-risk quartile - %s", focus_model),
       x = NULL, y = "Observed event rate") +
  theme_minimal(base_size = 12)
save_fig(p_risk, "06_risk_stratification_bar.png", width = 8, height = 5)

p_cal <- ggplot(risk_table, aes(x = mean_pred, y = event_rate)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(aes(size = n), colour = "firebrick", alpha = 0.85) +
  geom_errorbar(aes(ymin = pmax(0, event_rate - 1.96 * sqrt(event_rate * (1 - event_rate) / n)),
                    ymax = pmin(1, event_rate + 1.96 * sqrt(event_rate * (1 - event_rate) / n))),
                width = 0.02, colour = "firebrick", alpha = 0.6) +
  geom_text(aes(label = risk_group), vjust = -1.4, size = 3) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(title = "Mean predicted vs. observed event rate by quartile",
       x = "Mean predicted risk", y = "Observed event rate", size = "n") +
  theme_minimal(base_size = 12)
save_fig(p_cal, "06_risk_stratification_calibration.png", width = 7, height = 6)

## 分层分布箱线图
dist_df <- data.frame(risk = prob, group = risk_group, outcome = factor(y_num))
p_dist <- ggplot(dist_df, aes(x = group, y = risk, fill = outcome)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.6) +
  scale_fill_manual(values = c("0" = "#3288BD", "1" = "#D53E4F"),
                    labels = c("No event", "Event"), name = "Outcome") +
  labs(title = "Predicted risk distribution by quartile and outcome",
       x = NULL, y = "Predicted risk") +
  theme_minimal(base_size = 12)
save_fig(p_dist, "06_risk_distribution.png", width = 8, height = 5)

write_plot_data(dist_df, "06_risk_distribution_data.csv")
