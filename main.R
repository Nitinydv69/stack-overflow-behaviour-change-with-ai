# Initialise the entire project as one run command
cat(" Statistical Learning Project \n")

start_time <- Sys.time()

# 1. Package verification
required_pkgs <- c("tidyverse", "cluster", "caret", "pROC", "car", "rpart", "rpart.plot", "randomForest", "scales")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]

if (length(missing_pkgs) > 0) {
  stop("Missing required packages: ", paste(missing_pkgs, collapse = ", "),
       "\nPlease install them before proceeding: install.packages(c('", paste(missing_pkgs, collapse = "', '"), "'))")
}

# 2. Pipeline execution
run_step <- function(script_file, step_title) {
  cat(sprintf("\n>>> Step [%s]: Executing %s...\n", step_title, script_file))
  step_start <- Sys.time()
  source(script_file, echo = FALSE)
  step_elapsed <- round(as.numeric(difftime(Sys.time(), step_start, units = "secs")), 1)
  cat(sprintf(">>> Finished [%s] in %s seconds.\n", step_title, step_elapsed))
}

run_step("scripts/01_data_preprocessing.R", "01. Data Preprocessing & Recoding")
run_step("scripts/02_eda_and_missingness.R", "02. Exploratory Data Analysis & Bivariate Inference")
run_step("scripts/03_unsupervised_learning.R", "03. Unsupervised Clustering & Archetype Profiling")
run_step("scripts/04_supervised_learning.R", "04. Supervised Predictive Modeling & Benchmark")

total_elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 2)

cat(sprintf("All project pipelines completed successfully in %.2f minutes.\n", total_elapsed))
cat("Outputs generated in:\n")
cat("  - outputs/tables/  (Summary statistics, odds ratios, model benchmarks)\n")
cat("  - outputs/figures/ (Publication plots, scree, trees, importance, ROC)\n")
