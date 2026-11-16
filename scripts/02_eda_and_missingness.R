
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})


# 1. Load Clean Data

data_path <- file.path("data", "processed", "primary_sample.rds")
if (!file.exists(data_path)) {
  stop("Processed data not found. Please run 01_data_preprocessing.R first.")
}

data <- readRDS(data_path)
cat(sprintf("Loaded analytical sample: %d complete cases.\n", nrow(data)))

# 2. Demographic Overview & Variable Distributions

# Experience summary
exp_summary <- tibble(
  Metric = c("Years Coding", "Work Experience"),
  Mean = c(mean(data$YearsCode), mean(data$WorkExp)),
  SD = c(sd(data$YearsCode), sd(data$WorkExp)),
  Median = c(median(data$YearsCode), median(data$WorkExp)),
  IQR = c(IQR(data$YearsCode), IQR(data$WorkExp))
)

# Role distribution
role_summary <- data %>%
  count(Role_Clean, name = "N") %>%
  mutate(Percentage = round(N / sum(N) * 100, 2)) %>%
  arrange(desc(N))

# Tenure distribution
tenure_summary <- data %>%
  count(SOYears_Clean, name = "N") %>%
  mutate(Percentage = round(N / sum(N) * 100, 2))

# Combine demographic manifest
demographic_summary <- bind_rows(
  role_summary %>% mutate(Variable = "Role"),
  tenure_summary %>% rename(Role_Clean = SOYears_Clean) %>% mutate(Variable = "SO_Tenure")
)

write.csv(demographic_summary, "outputs/tables/demographic_summary.csv", row.names = FALSE)

# 3. Bivariate Association: AI Adoption vs Stack Overflow Disengagement

# 2x2 Contingency Table
cont_table <- table(
  AI_Adoption = factor(data$AIUse_binary, levels = c(0, 1), labels = c("Non-User", "AI User")),
  Visit_Less_Simple = factor(data$SOChange_VisitLessSimple, levels = c(0, 1), labels = c("No", "Yes"))
)

cat("\n=== 2x2 Contingency Table ===\n")
print(cont_table)

# Proportions
prop_table <- round(prop.table(cont_table, margin = 1) * 100, 2)
cat("\nRow Percentages (%):\n")
print(prop_table)

# Pearson Chi-square test
chi_test <- chisq.test(cont_table, correct = FALSE)
cat(sprintf("\nChi-Square Test: X-squared = %.2f, df = %d, p-value = %.3e\n",
            chi_test$statistic, chi_test$parameter, chi_test$p.value))

# Unadjusted Odds Ratio & 95% Confidence Interval
n11 <- cont_table[2, 2] # AI user, Visit less
n12 <- cont_table[2, 1] # AI user, No change/visit more
n21 <- cont_table[1, 2] # Non-user, Visit less
n22 <- cont_table[1, 1] # Non-user, No change/visit more

or_val <- (n11 * n22) / (n12 * n21)
se_log_or <- sqrt(1/n11 + 1/n12 + 1/n21 + 1/n22)
ci_lower <- exp(log(or_val) - 1.96 * se_log_or)
ci_upper <- exp(log(or_val) + 1.96 * se_log_or)

# Effect size: Cramer's V / Phi
phi_val <- sqrt(as.numeric(chi_test$statistic) / sum(cont_table))

bivariate_results <- tibble(
  Metric = c("Chi-Square Statistic", "Degrees of Freedom", "p-value",
             "Cramer's V (Phi)", "Odds Ratio (Unadjusted)", "95% CI Lower", "95% CI Upper"),
  Value = c(round(chi_test$statistic, 2), chi_test$parameter, chi_test$p.value,
            round(phi_val, 4), round(or_val, 2), round(ci_lower, 2), round(ci_upper, 2))
)

write.csv(bivariate_results, "outputs/tables/bivariate_inference_table.csv", row.names = FALSE)

# 4. Publication-Quality Visualizations

# Visual Theme setup
pub_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    axis.title = element_text(face = "bold", size = 11),
    legend.position = "none"
  )

# Figure 1: AI Adoption vs SO Visiting Less for Simple Questions
fig1_data <- data %>%
  group_by(AIUse_binary) %>%
  summarise(
    Total = n(),
    Yes_Count = sum(SOChange_VisitLessSimple),
    Prop = Yes_Count / Total,
    SE = sqrt(Prop * (1 - Prop) / Total),
    .groups = "drop"
  ) %>%
  mutate(Group = if_else(AIUse_binary == 1, "AI Users", "Non-Users"))

p1 <- ggplot(fig1_data, aes(x = Group, y = Prop, fill = Group)) +
  geom_col(width = 0.55, alpha = 0.85, color = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = Prop - 1.96 * SE, ymax = Prop + 1.96 * SE), width = 0.15) +
  geom_text(aes(label = sprintf("%.1f%%", Prop * 100)), vjust = -1.2, fontface = "bold") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_fill_manual(values = c("Non-Users" = "#4A7BB0", "AI Users" = "#D95F02")) +
  labs(
    title = "Stack Overflow Disengagement by AI Tool Adoption",
    subtitle = "Proportion reporting fewer visits for simple coding questions (95% CI)",
    x = NULL,
    y = "Percentage Visiting Less (%)"
  ) +
  pub_theme

ggsave("outputs/figures/eda_ai_vs_so_disengagement.png", p1, device = grDevices::png, width = 7, height = 5, dpi = 300)

# Figure 2: Tenure & Role Interaction with Disengagement
fig2_data <- data %>%
  filter(SOYears_Clean != "Non-user") %>%
  group_by(SOYears_Clean, AIUse_binary) %>%
  summarise(
    Prop = mean(SOChange_VisitLessSimple),
    N = n(),
    .groups = "drop"
  ) %>%
  mutate(AI_Status = if_else(AIUse_binary == 1, "AI Users", "Non-Users"))

p2 <- ggplot(fig2_data, aes(x = SOYears_Clean, y = Prop, group = AI_Status, color = AI_Status)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_color_manual(values = c("Non-Users" = "#4A7BB0", "AI Users" = "#D95F02")) +
  labs(
    title = "Disengagement Across Stack Overflow Tenure Levels",
    subtitle = "Comparing AI users and non-users across platform experience cohorts",
    x = "Platform Tenure",
    y = "Visiting Less for Simple Questions (%)",
    color = "Cohort"
  ) +
  pub_theme +
  theme(legend.position = "bottom")

ggsave("outputs/figures/eda_tenure_interaction.png", p2, device = grDevices::png, width = 8, height = 5, dpi = 300)

# Figure 3: Collinearity Check - YearsCode vs WorkExp
cor_exp <- cor(data$YearsCode, data$WorkExp, use = "complete.obs")
p3 <- ggplot(data, aes(x = YearsCode, y = WorkExp)) +
  geom_bin2d(bins = 40) +
  scale_fill_viridis_c(option = "inferno", trans = "log10") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "cyan", linewidth = 0.8) +
  annotate("text", x = 10, y = 45, label = sprintf("Pearson r = %.2f\n(Severe Multicollinearity)", cor_exp),
           fontface = "bold", size = 4.5, color = "white", hjust = 0) +
  labs(
    title = "Multicollinearity Diagnostic: Coding Experience vs. Work Experience",
    subtitle = "Hexagonal binning density with 45-degree identity line",
    x = "Years of Total Coding Experience (YearsCode)",
    y = "Years of Professional Work Experience (WorkExp)",
    fill = "Log Density"
  ) +
  pub_theme +
  theme(legend.position = "right")

ggsave("outputs/figures/eda_experience_collinearity.png", p3, device = grDevices::png, width = 8, height = 5.5, dpi = 300)

cat("\n=== Exploratory Analysis Completed Successfully ===\n")
