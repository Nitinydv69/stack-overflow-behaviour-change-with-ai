
suppressPackageStartupMessages({
  library(tidyverse)
  library(caret)
  library(pROC)
  library(car)
  library(rpart)
  library(rpart.plot)
  library(randomForest)
  library(scales)
})


# 1. Load Data & Prepare Train / Test Split

data_path <- file.path("data", "processed", "primary_sample.rds")
if (!file.exists(data_path)) {
  stop("Analytical dataset not found. Please run 01_data_preprocessing.R first.")
}

data <- readRDS(data_path)
cat(sprintf("Loaded dataset for supervised learning: %d observations.\n", nrow(data)))

# Set reproducible stratified train/test split (80/20)
set.seed(123)
train_idx <- createDataPartition(data$SOChange_VisitLessSimple, p = 0.80, list = FALSE)
train_data <- data[train_idx, ]
test_data <- data[-train_idx, ]

# Classification factor target for tree-based models
train_data$Y_fac <- factor(train_data$SOChange_VisitLessSimple, levels = c(0, 1), labels = c("No", "Yes"))
test_data$Y_fac  <- factor(test_data$SOChange_VisitLessSimple, levels = c(0, 1), labels = c("No", "Yes"))

# 2. Model 1: Logistic Regression (Inference & Odds Ratios)

cat("\nFitting Logistic Regression...\n")
log_model <- glm(
  SOChange_VisitLessSimple ~ AIUse_binary + YearsCode + Role_Clean + SOYears_Clean,
  data = train_data,
  family = binomial(link = "logit")
)

# Extract Odds Ratios and 95% Confidence Intervals
coef_tab <- coef(summary(log_model))
ci_tab   <- confint.default(log_model)

or_results <- tibble(
  Predictor = rownames(coef_tab),
  Estimate  = round(coef_tab[, "Estimate"], 4),
  Std_Error = round(coef_tab[, "Std. Error"], 4),
  z_value   = round(coef_tab[, "z value"], 2),
  p_value   = coef_tab[, "Pr(>|z|)"],
  Odds_Ratio = round(exp(coef_tab[, "Estimate"]), 2),
  CI_Lower   = round(exp(ci_tab[, 1]), 2),
  CI_Upper   = round(exp(ci_tab[, 2]), 2)
)

write.csv(or_results, "outputs/tables/supervised_logistic_odds_ratios.csv", row.names = FALSE)

# Collinearity check via Generalized Variance Inflation Factor (GVIF)
vif_res <- vif(log_model)
cat("\n--- Generalized VIF Diagnostics ---\n")
print(vif_res)

# Predict probabilities on test set
log_probs <- predict(log_model, newdata = test_data, type = "response")


# 3. Model 2: Decision Tree (CART with Complexity Pruning)

cat("\nFitting Decision Tree (CART)...\n")
tree_full <- rpart(
  Y_fac ~ AIUse_binary + YearsCode + Role_Clean + SOYears_Clean,
  data = train_data,
  method = "class",
  control = rpart.control(cp = 0.002, minsplit = 20, xval = 10)
)

# Select optimal complexity parameter (min cross-validation error)
opt_cp <- tree_full$cptable[which.min(tree_full$cptable[, "xerror"]), "CP"]
tree_model <- prune(tree_full, cp = opt_cp)

# Visualise pruned Decision Tree
png("outputs/figures/supervised_decision_tree.png", width = 1400, height = 900, res = 150)
rpart.plot(
  tree_model,
  type = 2,
  extra = 104,
  under = TRUE,
  fallen.leaves = TRUE,
  box.palette = "RdYlGn",
  main = "Pruned Decision Tree for Stack Overflow Disengagement"
)
dev.off()

tree_probs <- predict(tree_model, newdata = test_data, type = "prob")[, "Yes"]

# 4. Model 3: Random Forest Ensemble

cat("\nFitting Random Forest (ntree = 500)...\n")
set.seed(123)
rf_model <- randomForest(
  Y_fac ~ AIUse_binary + YearsCode + Role_Clean + SOYears_Clean,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

# Permutation variable importance
imp_mat <- importance(rf_model)
imp_df <- tibble(
  Feature = rownames(imp_mat),
  MeanDecreaseAccuracy = round(imp_mat[, "MeanDecreaseAccuracy"], 2),
  MeanDecreaseGini     = round(imp_mat[, "MeanDecreaseGini"], 2)
) %>%
  arrange(desc(MeanDecreaseAccuracy))

write.csv(imp_df, "outputs/tables/supervised_rf_importance.csv", row.names = FALSE)

# Variable importance plot
png("outputs/figures/supervised_rf_importance.png", width = 1200, height = 800, res = 150)
varImpPlot(
  rf_model,
  type = 1,
  main = "Random Forest: Predictor Importance (Mean Decrease in Accuracy)",
  pch = 19,
  col = "#2B5C8F"
)
dev.off()

rf_probs <- predict(rf_model, newdata = test_data, type = "prob")[, "Yes"]

# 5. Model Evaluation & Benchmark Comparison on Test Set

cat("\nComputing Test Set Performance Metrics...\n")

# ROC curves
roc_log  <- roc(test_data$SOChange_VisitLessSimple, log_probs, quiet = TRUE)
roc_tree <- roc(test_data$SOChange_VisitLessSimple, tree_probs, quiet = TRUE)
roc_rf   <- roc(test_data$SOChange_VisitLessSimple, rf_probs, quiet = TRUE)

calc_metrics <- function(actual, probs, threshold = 0.5) {
  preds <- factor(ifelse(probs >= threshold, 1, 0), levels = c(0, 1))
  cm <- confusionMatrix(preds, factor(actual, levels = c(0, 1)), positive = "1")
  
  tibble(
    Accuracy    = unname(cm$overall["Accuracy"]),
    Sensitivity = unname(cm$byClass["Sensitivity"]),
    Specificity = unname(cm$byClass["Specificity"]),
    Precision   = unname(cm$byClass["Pos Pred Value"]),
    F1          = unname(cm$byClass["F1"])
  )
}

m_log  <- calc_metrics(test_data$SOChange_VisitLessSimple, log_probs)
m_tree <- calc_metrics(test_data$SOChange_VisitLessSimple, tree_probs)
m_rf   <- calc_metrics(test_data$SOChange_VisitLessSimple, rf_probs)

model_comparison <- tibble(
  Model = c("Logistic Regression", "Decision Tree (CART)", "Random Forest"),
  Test_AUC = c(as.numeric(auc(roc_log)), as.numeric(auc(roc_tree)), as.numeric(auc(roc_rf))),
  Accuracy = c(m_log$Accuracy, m_tree$Accuracy, m_rf$Accuracy),
  Sensitivity = c(m_log$Sensitivity, m_tree$Sensitivity, m_rf$Sensitivity),
  Specificity = c(m_log$Specificity, m_tree$Specificity, m_rf$Specificity),
  Precision = c(m_log$Precision, m_tree$Precision, m_rf$Precision),
  F1_Score = c(m_log$F1, m_tree$F1, m_rf$F1)
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

print(model_comparison)
write.csv(model_comparison, "outputs/tables/supervised_model_comparison.csv", row.names = FALSE)

# Comparative ROC Plot
png("outputs/figures/supervised_roc_curves.png", width = 1200, height = 900, res = 150)
plot(roc_log, col = "#2B5C8F", lwd = 2.5, main = "Supervised Learning Test Set ROC Comparison")
plot(roc_tree, col = "#E7298A", lwd = 2, lty = 2, add = TRUE)
plot(roc_rf, col = "#D95F02", lwd = 2, lty = 4, add = TRUE)
legend(
  "bottomright",
  legend = c(
    sprintf("Logistic Regression (AUC = %.3f)", auc(roc_log)),
    sprintf("Decision Tree (AUC = %.3f)", auc(roc_tree)),
    sprintf("Random Forest (AUC = %.3f)", auc(roc_rf))
  ),
  col = c("#2B5C8F", "#E7298A", "#D95F02"),
  lwd = 2,
  lty = c(1, 2, 4),
  bty = "n"
)
dev.off()

cat("\n=== Supervised Learning Pipeline Completed Successfully ===\n")
