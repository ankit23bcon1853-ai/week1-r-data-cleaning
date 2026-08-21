## =============================================================
## Week 1 Task: Data Cleaning and Preliminary Analysis with R
## Dataset : Employee Attrition Dataset (HR analytics style)
## Author  : <Your Name>
## =============================================================

## ---- 0. Load required libraries -------------------------------
install.packages(c("tidyverse","naniar","corrplot","psych"))
library(tidyverse)   # dplyr, ggplot2, tidyr
library(naniar)      # missing value visualisation
library(corrplot)    # correlation heatmap
library(psych)        # describe()

## ---- 1. Import the dataset -------------------------------------
df <- read.csv("employee_attrition.csv", stringsAsFactors = FALSE)

## ---- 2. Initial inspection --------------------------------------
str(df)                 # structure: types, sample values
dim(df)                 # rows x columns
head(df, 5)
summary(df)             # summary statistics for every column

## ---- 3. Missing value analysis ----------------------------------
colSums(is.na(df))                          # count of NAs per column
round(colMeans(is.na(df)) * 100, 2)         # % missing per column
vis_miss(df)                                # visualise missing pattern (naniar)

## ---- 4. Handling missing values ---------------------------------
# Numeric columns -> impute with median (robust to outliers)
num_impute <- c("Age", "MonthlyIncome", "DistanceFromHome")
for (col in num_impute) {
  df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
}

# Categorical columns -> impute with mode (most frequent category)
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
cat_impute <- c("Department", "MaritalStatus")
for (col in cat_impute) {
  df[[col]][is.na(df[[col]])] <- get_mode(df[[col]])
}

colSums(is.na(df))   # confirm no missing values remain

## ---- 5. Outlier detection (IQR method) --------------------------
detect_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  list(lower = lower, upper = upper,
       n_outliers = sum(x < lower | x > upper, na.rm = TRUE))
}

detect_outliers(df$MonthlyIncome)
detect_outliers(df$YearsAtCompany)

boxplot(df$MonthlyIncome, main = "Monthly Income - Outlier Check",
        col = "orange", horizontal = TRUE)
boxplot(df$YearsAtCompany, main = "Years at Company - Outlier Check",
        col = "lightgreen", horizontal = TRUE)

# Cap (winsorize) extreme MonthlyIncome values at the IQR fences
bounds <- detect_outliers(df$MonthlyIncome)
df$MonthlyIncome_capped <- pmin(pmax(df$MonthlyIncome, bounds$lower), bounds$upper)

## ---- 6. Normalization (min-max scaling) -------------------------
min_max_scale <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

df <- df %>%
  mutate(
    Age_norm              = min_max_scale(Age),
    DistanceFromHome_norm = min_max_scale(DistanceFromHome),
    MonthlyIncome_norm    = min_max_scale(MonthlyIncome_capped),
    YearsAtCompany_norm   = min_max_scale(YearsAtCompany)
  )

## ---- 7. Encoding categorical variables ---------------------------
# Binary label encoding
df$OverTime_enc  <- ifelse(df$OverTime == "Yes", 1, 0)
df$Attrition_enc <- ifelse(df$Attrition == "Yes", 1, 0)

# One-hot encoding for nominal variables (Department, Gender, MaritalStatus)
df_encoded <- df %>%
  mutate(across(c(Department, Gender, MaritalStatus), as.factor)) %>%
  { model.matrix(~ Department + Gender + MaritalStatus - 1, data = .) } %>%
  as.data.frame() %>%
  bind_cols(df, .)

write.csv(df_encoded, "employee_attrition_cleaned.csv", row.names = FALSE)

## ---- 8. Exploratory Data Analysis --------------------------------

## 8.1 Descriptive statistics
describe(df[, c("Age","MonthlyIncome_capped","YearsAtCompany","DistanceFromHome")])

## 8.2 Age distribution
ggplot(df, aes(x = Age)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "#4C72B0", alpha = 0.8) +
  geom_density(color = "darkred", linewidth = 1) +
  labs(title = "Distribution of Employee Age", x = "Age", y = "Density") +
  theme_minimal()

## 8.3 Monthly Income by Attrition
ggplot(df, aes(x = Attrition, y = MonthlyIncome_capped, fill = Attrition)) +
  geom_boxplot() +
  labs(title = "Monthly Income by Attrition Status") +
  theme_minimal()

## 8.4 Attrition rate by Department
df %>%
  count(Department, Attrition) %>%
  group_by(Department) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = Department, y = pct, fill = Attrition)) +
  geom_col(position = "stack") +
  labs(title = "Attrition Rate by Department (%)", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

## 8.5 Correlation heatmap of key numeric variables
corr_vars <- df[, c("Age","DistanceFromHome","MonthlyIncome_capped",
                     "YearsAtCompany","JobSatisfaction","OverTime_enc","Attrition_enc")]
corr_matrix <- round(cor(corr_vars, use = "complete.obs"), 2)
corrplot(corr_matrix, method = "color", addCoef.col = "black",
         type = "upper", tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of Key Variables", mar = c(0,0,2,0))

## 8.6 OverTime vs Attrition
ggplot(df, aes(x = OverTime, fill = Attrition)) +
  geom_bar(position = "dodge") +
  labs(title = "Attrition Count by OverTime Status") +
  theme_minimal()

## ---- 9. Key Insight Summary (printed to console) ------------------
cat("Correlation of numeric variables with Attrition:\n")
print(sort(corr_matrix[, "Attrition_enc"], decreasing = TRUE))

cat("\nAttrition rate by OverTime status:\n")
print(prop.table(table(df$OverTime, df$Attrition), margin = 1) * 100)

## =============================================================
## End of script
## =============================================================
