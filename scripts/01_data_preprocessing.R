# Importing tidyverse

suppressPackageStartupMessages({
  library(tidyverse)
})

# 1. Create Path and folders

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

raw_path <- file.path("data", "raw", "survey_results_public.csv")
if (!file.exists(raw_path)) {
  stop("Raw survey data not found at: ", raw_path)
}

# loading the raw dataset & handling Unicode path encodings 

con <- file(raw_path, "rb")
survey_raw <- read_csv(con, na = c("", "NA"), show_col_types = FALSE)
close(con)

cat(sprintf("Loaded raw dataset: %d rows, %d columns.\n", nrow(survey_raw), ncol(survey_raw)))

# 2. Data Cleaning &  Filtering

# Standardize whitespace in character vectors
survey_clean <- survey_raw %>%
  mutate(across(where(is.character), str_trim))

# Identify contradictory multi-select responses in AISelect

has_ai_contradiction <- str_detect(coalesce(survey_clean$AISelect, ""), "I don[’']t use AI tools") &
  str_detect(coalesce(survey_clean$AISelect, ""), "AI coding|General-purpose|AI agents|Internal AI")

# Identify contradictory responses in SOChange 
has_so_contradiction <- str_detect(coalesce(survey_clean$SOChange, ""), "No change") &
  str_count(coalesce(survey_clean$SOChange, ""), ";") > 0

# non-missing key variables and valid responses
eligible_mask <- !is.na(survey_clean$SOChange) &
  !is.na(survey_clean$AISelect) &
  !has_ai_contradiction &
  !has_so_contradiction

research_core <- survey_clean[eligible_mask, ]

# 3. Feature Engineering & Target Encoding

# Target outcome: Visiting Stack Overflow less for simple questions
research_core <- research_core %>%
  mutate(
    SOChange_VisitLessSimple = as.integer(
      str_detect(SOChange, fixed("Visit less for simple questions"))
    ),
    AIUse_binary = as.integer(
      !str_detect(AISelect, fixed("I don't use AI tools")) &
      !str_detect(AISelect, fixed("I don’t use AI tools"))
    )
  )

# Recode Role into 8 professional archetypes
recode_role <- function(r) {
  case_when(
    r == "Developer, full-stack" ~ "Full-Stack Dev",
    r == "Developer, back-end" ~ "Back-End Dev",
    r == "Developer, front-end" ~ "Front-End Dev",
    str_detect(r, "desktop|enterprise|embedded|game|mobile|devices") ~ "Desktop/Systems/Mobile",
    str_detect(r, "Data|AI|ML|scientist|Applied") ~ "Data & AI",
    str_detect(r, "DevOps|Cloud|Platform|reliability|System admin|Security|Cybersecurity") ~ "DevOps/Cloud/Infra",
    str_detect(r, "Architect|manager|executive|Founder") ~ "Architecture & Lead",
    str_detect(r, "Student|Academic|Educator") ~ "Student / Academic",
    TRUE ~ "Other"
  )
}

research_core$Role_Clean <- factor(recode_role(research_core$Role))
# Set most representative developer segment as reference
research_core$Role_Clean <- relevel(research_core$Role_Clean, ref = "Full-Stack Dev")

# Order SOYears logically rather than alphabetically
so_levels <- c("< 1 year", "1-3 years", "3-5 years", "5-10 years", "10-15 years", "> 15 years", "Non-user")

research_core$SOYears_Clean <- case_when(
  research_core$SOYears == "Less than one year" ~ "< 1 year",
  research_core$SOYears == "Between 1 and 3 years" ~ "1-3 years",
  research_core$SOYears == "Between 3 and 5 years" ~ "3-5 years",
  research_core$SOYears == "Between 5 and 10 years" ~ "5-10 years",
  research_core$SOYears == "Between 10 and 15 years" ~ "10-15 years",
  research_core$SOYears == "More than 15 years, or since Stack Overflow started in 2008" ~ "> 15 years",
  research_core$SOYears == "I don't use Stack Overflow" ~ "Non-user",
  TRUE ~ NA_character_
)

research_core$SOYears_Clean <- factor(research_core$SOYears_Clean, levels = so_levels)
research_core$SOYears_Clean <- relevel(research_core$SOYears_Clean, ref = "< 1 year")

# Reconstruct AI usage frequencies (0 = None/Unselected, 1 = Infrequent, 2 = Weekly, 3 = Daily)
map_freq <- function(selected_indicator, freq_col) {
  case_when(
    !selected_indicator ~ 0,
    freq_col == "Monthly or Infrequent" ~ 1,
    freq_col == "Weekly" ~ 2,
    freq_col == "Daily" ~ 3,
    TRUE ~ 0
  )
}

research_core <- research_core %>%
  mutate(
    Coding_Freq = map_freq(
      str_detect(AISelect, fixed("AI coding assistants or coding agents")),
      AIFreq_1
    ),
    General_Freq = map_freq(
      str_detect(AISelect, fixed("General-purpose AI chat tools")),
      AIFreq_2
    ),
    Agent_Freq = map_freq(
      str_detect(AISelect, fixed("AI agents or automated workflows")),
      AIFreq_3
    ),
    Internal_Freq = map_freq(
      str_detect(AISelect, fixed("Internal AI tools built by my company")),
      AIFreq_4
    )
  )

# 4. Finalise Samples

# Primary sample: complete cases on primary demographic and outcome variables
primary_sample <- research_core %>%
  filter(
    !is.na(YearsCode),
    !is.na(WorkExp),
    !is.na(Role_Clean),
    !is.na(SOYears_Clean)
  )

# Clustering sample: AI users with tool frequency indicators
ai_users <- primary_sample %>% filter(AIUse_binary == 1)

set.seed(42)
sample_size <- min(3000, nrow(ai_users))
clustering_idx <- sample(nrow(ai_users), sample_size)
clustering_sample <- ai_users[clustering_idx, ]

# 5. Export Datasets & Sample Flow Manifest

saveRDS(primary_sample, "data/processed/primary_sample.rds")
saveRDS(clustering_sample, "data/processed/clustering_sample.rds")

sample_flow <- tibble(
  Stage = c(
    "1. Raw survey observations",
    "2. Filtered contradictory AI / SO responses",
    "3. Valid SOChange and AISelect responses",
    "4. Complete cases (Primary Supervised Sample)",
    "5. Active AI users sample",
    "6. Unsupervised clustering sample (sampled for distance matrix)"
  ),
  Respondents = c(
    nrow(survey_raw),
    nrow(survey_raw) - sum(has_ai_contradiction | has_so_contradiction),
    nrow(research_core),
    nrow(primary_sample),
    nrow(ai_users),
    nrow(clustering_sample)
  )
)

write.csv(sample_flow, "outputs/tables/sample_flow_table.csv", row.names = FALSE)

cat("\n=== Data Preprocessing Completed Successfully ===\n")
print(sample_flow)
