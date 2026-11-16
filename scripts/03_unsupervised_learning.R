
suppressPackageStartupMessages({
  library(tidyverse)
  library(cluster)
  library(scales)
})

# 1. Load Clustering Sample

sample_path <- file.path("data", "processed", "clustering_sample.rds")
if (!file.exists(sample_path)) {
  stop("Clustering sample not found. Please run 01_data_preprocessing.R first.")
}

sample_data <- readRDS(sample_path)
cat(sprintf("Loaded clustering sample: %d active AI users.\n", nrow(sample_data)))

# Extract tool frequency variables
freq_cols <- c("Coding_Freq", "General_Freq", "Agent_Freq", "Internal_Freq")
num_df <- sample_data[, freq_cols]

# Ordered factor matrix for Gower dissimilarity
ord_df <- num_df %>%
  mutate(across(everything(), ~ factor(.x, levels = 0:3, ordered = TRUE)))

# 2. Dimensionality Reduction: Principal Component Analysis (PCA)

# Standardize numeric frequencies
scaled_features <- scale(num_df)

pca_fit <- prcomp(scaled_features, center = FALSE, scale. = FALSE)
pca_var <- pca_fit$sdev^2
prop_var <- pca_var / sum(pca_var)
cum_var <- cumsum(prop_var)

pca_summary <- tibble(
  Component = paste0("PC", seq_along(prop_var)),
  Variance = round(pca_var, 3),
  Proportion = round(prop_var, 3),
  Cumulative = round(cum_var, 3)
)

write.csv(pca_summary, "outputs/tables/unsupervised_pca_variance.csv", row.names = FALSE)

# Scree Plot
scree_df <- tibble(
  PC = factor(paste0("PC", seq_along(prop_var)), levels = paste0("PC", seq_along(prop_var))),
  Variance = prop_var
)

p_scree <- ggplot(scree_df, aes(x = PC, y = Variance)) +
  geom_col(fill = "#2B5C8F", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", Variance * 100)), vjust = -0.5, fontface = "bold") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.6)) +
  labs(
    title = "PCA Scree Plot of AI Tooling Dimensions",
    subtitle = "Proportion of variance explained by each principal component",
    x = "Principal Component",
    y = "Explained Variance (%)"
  ) +
  theme_minimal(base_size = 12)

ggsave("outputs/figures/unsupervised_pca_scree.png", p_scree, device = grDevices::png, width = 7, height = 4.5, dpi = 300)

# 3. Model Comparison: K-Means (Euclidean) vs. PAM (Gower)

d_euc <- dist(scaled_features)
d_gower <- daisy(ord_df, metric = "gower")

k_range <- 2:8
comparison_records <- list()

set.seed(123)
for (k in k_range) {
  # 1. K-means on scaled Euclidean features
  km <- kmeans(scaled_features, centers = k, nstart = 25)
  sil_km <- mean(silhouette(km$cluster, d_euc)[, 3])
  
  # 2. PAM with Gower dissimilarity on ordinal rankings
  pm <- pam(d_gower, k = k, diss = TRUE)
  sil_pm <- pm$silinfo$avg.width
  
  comparison_records[[length(comparison_records) + 1]] <- tibble(
    K = k,
    Method = "K-Means (Euclidean)",
    Silhouette = sil_km,
    Total_WCSS = km$tot.withinss
  )
  comparison_records[[length(comparison_records) + 1]] <- tibble(
    K = k,
    Method = "PAM (Gower Ordinal)",
    Silhouette = sil_pm,
    Total_WCSS = NA_real_
  )
}

comparison_table <- bind_rows(comparison_records)
write.csv(comparison_table, "outputs/tables/unsupervised_model_comparison.csv", row.names = FALSE)

# Plot Silhouette Comparison
p_sil <- ggplot(comparison_table, aes(x = K, y = Silhouette, color = Method, group = Method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = k_range) +
  scale_color_manual(values = c("K-Means (Euclidean)" = "#4A7BB0", "PAM (Gower Ordinal)" = "#D95F02")) +
  labs(
    title = "Cluster Quality Benchmark: K-Means vs. PAM",
    subtitle = "Average silhouette width across candidate cluster counts (K = 2 to 8)",
    x = "Number of Clusters (K)",
    y = "Average Silhouette Width",
    color = "Algorithm"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("outputs/figures/unsupervised_cluster_silhouette.png", p_sil, device = grDevices::png, width = 8, height = 5, dpi = 300)

# 4. Final Segmentation: PAM with K = 7

# Fit final PAM model
final_pam <- pam(d_gower, k = 7, diss = TRUE)
sample_data$Cluster <- factor(final_pam$clustering)

# Profile Clusters
cluster_profile <- sample_data %>%
  group_by(Cluster) %>%
  summarise(
    Size = n(),
    Coding_Mean = mean(Coding_Freq),
    General_Mean = mean(General_Freq),
    Agent_Mean = mean(Agent_Freq),
    Internal_Mean = mean(Internal_Freq),
    Visit_Less_Pct = mean(SOChange_VisitLessSimple) * 100,
    .groups = "drop"
  )

# Label archetypes substantively based on centroids
archetype_names <- c(
  "1" = "Casual Multi-tool",
  "2" = "Agent & Copilot Power",
  "3" = "Enterprise Multi-tool",
  "4" = "Core Daily Devs (Chat+Copilot)",
  "5" = "Chat-Only Explorers",
  "6" = "Copilot Purists",
  "7" = "Agent Automators"
)

cluster_profile$Archetype <- archetype_names[as.character(cluster_profile$Cluster)]
write.csv(cluster_profile, "outputs/tables/unsupervised_cluster_profiles.csv", row.names = FALSE)

# Heatmap Visualization of Tool Usage by Cluster
profile_long <- cluster_profile %>%
  select(Archetype, Coding_Mean, General_Mean, Agent_Mean, Internal_Mean) %>%
  pivot_longer(-Archetype, names_to = "Tool", values_to = "Mean_Score") %>%
  mutate(
    Tool = case_when(
      Tool == "Coding_Mean" ~ "Coding Copilots",
      Tool == "General_Mean" ~ "General AI Chat",
      Tool == "Agent_Mean" ~ "Autonomous Agents",
      Tool == "Internal_Mean" ~ "Internal Company AI"
    )
  )

p_heat <- ggplot(profile_long, aes(x = Tool, y = Archetype, fill = Mean_Score)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", Mean_Score)), fontface = "bold", size = 4) +
  scale_fill_gradient(low = "#F7FBFF", high = "#084594", limits = c(0, 3)) +
  labs(
    title = "AI Adoption Archetypes: Tool Frequency Heatmap",
    subtitle = "Mean reported usage frequency (0 = None, 1 = Infrequent, 2 = Weekly, 3 = Daily)",
    x = "AI Tool Modality",
    y = "Derived Archetype",
    fill = "Usage Frequency"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

ggsave("outputs/figures/unsupervised_pam_heatmap.png", p_heat, device = grDevices::png, width = 9, height = 5.5, dpi = 300)

# Cross-tabulation of Cluster vs Stack Overflow Disengagement
cluster_tab <- table(sample_data$Cluster, sample_data$SOChange_VisitLessSimple)
chi_cluster <- chisq.test(cluster_tab)
cramer_v_cluster <- sqrt(as.numeric(chi_cluster$statistic) / (sum(cluster_tab) * (min(dim(cluster_tab)) - 1)))

cat(sprintf("\nCluster vs. Disengagement Chi-Square: X2 = %.2f, df = %d, p = %.3e, Cramer's V = %.3f\n",
            chi_cluster$statistic, chi_cluster$parameter, chi_cluster$p.value, cramer_v_cluster))

# Plot Disengagement across Clusters
p_dis <- ggplot(cluster_profile, aes(x = reorder(Archetype, Visit_Less_Pct), y = Visit_Less_Pct)) +
  geom_col(fill = "#2B5C8F", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", Visit_Less_Pct)), hjust = -0.2, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(
    title = "Stack Overflow Disengagement Across AI-Use Archetypes",
    subtitle = "Percentage reporting fewer visits for simple coding questions (p < 0.001)",
    x = NULL,
    y = "Visiting Stack Overflow Less (%)"
  ) +
  theme_minimal(base_size = 12)

ggsave("outputs/figures/unsupervised_cluster_disengagement.png", p_dis, device = grDevices::png, width = 8, height = 5, dpi = 300)

cat("\n=== Unsupervised Learning Pipeline Completed Successfully ===\n")
