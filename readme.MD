# Stack Overflow Disengagement Analysis

### Investigating the Relationship Between Generative AI Adoption and Developer Engagement on Stack Overflow

## Overview

Generative AI tools are changing how developers search for information, solve programming problems, and learn new technologies. As AI-powered coding assistants become increasingly common, an important question emerges: **Is the adoption of generative AI associated with reduced developer engagement on Stack Overflow?**

This project investigates the relationship between generative AI adoption and developer disengagement using data from the Stack Overflow Developer Survey 2026.

The analysis focuses on developers who report visiting Stack Overflow less frequently for simple questions, exploring how AI adoption, professional experience, developer roles, and platform usage patterns relate to this behaviour.

The project combines exploratory data analysis, statistical inference, supervised machine learning, and unsupervised learning to investigate the research problem.

## Research Questions

The project addresses two main research questions:

**RQ1 – Unsupervised Learning**

Can developers be grouped into distinct behavioural archetypes based on their generative AI adoption and usage patterns?

**RQ2 – Supervised Learning**

Is generative AI adoption associated with a higher likelihood of developers visiting Stack Overflow less frequently for simple questions, after accounting for relevant developer characteristics?

## Dataset

The analysis uses the publicly available Stack Overflow Developer Survey 2026.

- Original observations: 30,903
- Original variables: 212
- Empty columns removed: 6
- Variables after initial cleaning: 208
- Strict primary analytical sample: 14,062 observations
- AI coding assistant frequency sample: 9,537 observations
- General-purpose AI frequency sample: 8,884 observations

**Data source:** [Stack Overflow Developer Survey](https://survey.stackoverflow.co/)

The raw dataset is not included in this repository. Download the public survey data and place `survey_results_public.csv` in the appropriate data directory.

## Methodology

The project follows a structured analytical pipeline.

### 1. Data Preprocessing

- Initial data auditing and structural inspection.
- Removal of empty columns.
- Investigation of missing values and inconsistent responses.
- Treatment of contradictory survey answers.
- Feature engineering for AI adoption and usage variables.
- Construction of analytical samples for different research questions.
- Recoding occupational categories into broader developer role groups.

### 2. Exploratory Data Analysis

Exploratory analysis examines:

- Developer experience and professional backgrounds.
- Distribution of AI adoption and usage.
- Stack Overflow engagement patterns.
- Relationships between developer characteristics and AI adoption.
- Missingness patterns and sample composition.

### 3. Unsupervised Learning

The unsupervised analysis investigates underlying behavioural patterns among developers.

Methods include:

- Principal Component Analysis (PCA) for dimensionality reduction.
- K-Means clustering using Euclidean distance.
- Partitioning Around Medoids (PAM) using Gower dissimilarity.
- Silhouette analysis for cluster evaluation.
- Comparison of clustering solutions.
- Profiling of identified behavioural groups.

**Key findings:**
- The first two principal components explain approximately 65.8% of the total variance.
- PC1 explains 40.6% of the variance.
- PC2 explains 25.2% of the variance.
- PAM identified seven behavioural archetypes.
- The selected PAM solution achieved a mean silhouette width of approximately 0.55.

### 4. Supervised Learning

The supervised analysis predicts whether developers report visiting Stack Overflow less frequently for simple questions.

The target variable is:

`SOChange_VisitLessSimple`

Models and techniques include:

- Logistic Regression
- Decision Tree
- Random Forest
- Train-test evaluation
- ROC and AUC analysis
- Confusion matrix evaluation
- Feature importance analysis
- Multicollinearity diagnostics using VIF

**Logistic Regression results:**

| Metric | Result |
|---|---:|
| AUC | 0.7375 |
| Accuracy | 0.7735 |
| Sensitivity | 0.9493 |
| Specificity | 0.4228 |
| Balanced Accuracy | 0.6860 |
| Cohen's Kappa | 0.4222 |

The ROC-AUC confidence interval was 0.7282–0.7469.

These results indicate that the model has useful discriminatory ability, although its default classification threshold produces an imbalance between sensitivity and specificity.

## Technology Stack

- **Programming language:** R
- **Environment:** Visual Studio Code
- **Data manipulation:** tidyverse
- **Statistical analysis:** Base R and statistical modelling packages
- **Unsupervised learning:** PCA, K-Means, PAM
- **Supervised learning:** Logistic Regression, Decision Trees, Random Forest
- **Visualisation:** ggplot2
- **Evaluation:** ROC-AUC, Silhouette Analysis, VIF and Confusion Matrix

## Repository Structure

```text
stackoverflow-disengagement/
│
├── data/
│   ├── raw/
│   │   └── survey_results_public.csv
│   │
│   └── processed/
│
├── scripts/
│   ├── 00_pipeline_helpers.R
│   ├── 01_data_preprocessing.R
│   ├── 02_eda_and_missingness.R
│   ├── 03_unsupervised_learning.R
│   ├── 04_supervised_learning.R
│   └── main.R
│
├── outputs/
│   ├── tables/
│   └── figures/
│
├── report/
│
├── README.md
└── .gitignore
```

*The structure above represents the intended organisation; adjust filenames and folders to match the actual repository.*

## How to Run the Project

### Prerequisites

- R 4.5.2 or later
- Visual Studio Code (or RStudio)
- Required R packages used throughout the scripts.

### 1. Clone the repository

```bash
git clone https://github.com/nitinydv69/stackoverflow-disengagement.git

cd stackoverflow-disengagement
```

### 2. Download the dataset

Download the Stack Overflow Developer Survey dataset from:

https://survey.stackoverflow.co/

Place the raw CSV file inside:

```text
data/raw/survey_results_public.csv
```

### 3. Run the analysis

Execute the main pipeline:

```r
source("scripts/main.R")
```

Alternatively, run the individual scripts in numerical order, beginning with data preprocessing.

## Key Outputs

The analysis generates several tables and results, including:

- Demographic summary statistics.
- Sample flow and missingness analysis.
- PCA explained variance.
- Cluster profiles.
- Clustering model comparison.
- Logistic regression odds ratios.
- Random Forest feature importance.
- Supervised model comparison.
- Bivariate statistical inference.

## Limitations

This research has several limitations:

1. **Cross-sectional data:** The survey captures responses at a particular point in time and cannot establish causal relationships.

2. **Self-reported behaviour:** AI adoption and platform engagement are based on respondents' reported experiences.

3. **Selection bias:** Survey participants may not fully represent the entire developer population.

4. **Missing data:** Different analytical samples are required depending on variable availability, potentially affecting comparability.

5. **Model limitations:** Predictive performance does not establish that AI adoption directly causes reduced Stack Overflow engagement.

The findings should therefore be interpreted as evidence of associations and behavioural patterns rather than causal effects.

## Research Significance

This project contributes to understanding how generative AI adoption may be associated with changes in developers' information-seeking behaviour.

By combining supervised and unsupervised learning, it approaches the problem from two perspectives: identifying developers who may be more likely to disengage from Stack Overflow and discovering broader behavioural patterns in AI adoption.

The results provide an analytical perspective on the evolving relationship between AI-assisted programming and traditional developer knowledge-sharing communities.

## Author

**Nitin Yadav**

MSc Data Science for Economics and Health  
University of Milan, Italy

[GitHub Profile](https://github.com/Nitinydv69)

---

*Academic data science project developed as part of the Statistical Learning coursework.*
