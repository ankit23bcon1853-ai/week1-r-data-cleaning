## =============================================================
## Week 2 Task: Data Visualization and Insight Communication using R
## Dataset : Employee Attrition Dataset (continued from Week 1)
## Author  : <Your Name>
## =============================================================

## ---- 0. Load required libraries -------------------------------
# install.packages(c("tidyverse","lattice"))
library(tidyverse)   # ggplot2, dplyr
library(lattice)     # lattice plotting system

## ---- 1. Load the cleaned dataset from Week 1 --------------------
df <- read.csv("employee_attrition.csv", stringsAsFactors = FALSE)

# Re-apply Week 1 cleaning steps (median/mode imputation, outlier capping)
df$Age[is.na(df$Age)] <- median(df$Age, na.rm = TRUE)
df$MonthlyIncome[is.na(df$MonthlyIncome)] <- median(df$MonthlyIncome, na.rm = TRUE)
df$DistanceFromHome[is.na(df$DistanceFromHome)] <- median(df$DistanceFromHome, na.rm = TRUE)

get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
df$Department[is.na(df$Department)] <- get_mode(df$Department)
df$MaritalStatus[is.na(df$MaritalStatus)] <- get_mode(df$MaritalStatus)

q1 <- quantile(df$MonthlyIncome, 0.25); q3 <- quantile(df$MonthlyIncome, 0.75)
iqr <- q3 - q1
df$MonthlyIncome_capped <- pmin(pmax(df$MonthlyIncome, q1 - 1.5*iqr), q3 + 1.5*iqr)

str(df)
summary(df[, c("Age","MonthlyIncome_capped","YearsAtCompany","DistanceFromHome")])

## =============================================================
## VISUALIZATION 1 (ggplot2) : Bar Chart
## Average Monthly Income by Department
## =============================================================
avg_income_dept <- df %>%
  group_by(Department) %>%
  summarise(avg_income = mean(MonthlyIncome_capped)) %>%
  arrange(avg_income)

ggplot(avg_income_dept, aes(x = reorder(Department, avg_income), y = avg_income, fill = Department)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Average Monthly Income by Department",
       x = "Department", y = "Average Monthly Income") +
  theme_minimal()

## =============================================================
## VISUALIZATION 2 (ggplot2) : Scatter Plot
## Monthly Income vs Years at Company, coloured by Attrition
## =============================================================
ggplot(df, aes(x = YearsAtCompany, y = MonthlyIncome_capped, color = Attrition)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("No" = "#4C72B0", "Yes" = "#DD8452")) +
  labs(title = "Monthly Income vs Years at Company",
       x = "Years at Company", y = "Monthly Income") +
  theme_minimal()

## =============================================================
## VISUALIZATION 3 (ggplot2) : Histogram
## Distribution of Distance From Home
## =============================================================
ggplot(df, aes(x = DistanceFromHome)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15, fill = "#55A868", alpha = 0.85) +
  geom_density(color = "darkred", linewidth = 1) +
  labs(title = "Distribution of Distance From Home",
       x = "Distance From Home (km)", y = "Density") +
  theme_minimal()

## =============================================================
## VISUALIZATION 4 (ggplot2) : Line Chart
## Average Monthly Income Trend by Tenure Group
## =============================================================
df$YearsBucket <- cut(df$YearsAtCompany,
                       breaks = c(0,2,4,6,8,10,15,40),
                       labels = c("0-2","2-4","4-6","6-8","8-10","10-15","15+"))

trend <- df %>%
  filter(!is.na(YearsBucket)) %>%
  group_by(YearsBucket) %>%
  summarise(avg_income = mean(MonthlyIncome_capped))

ggplot(trend, aes(x = YearsBucket, y = avg_income, group = 1)) +
  geom_line(color = "#C44E52", linewidth = 1) +
  geom_point(color = "#C44E52", size = 2) +
  labs(title = "Average Monthly Income Trend by Tenure Group",
       x = "Years at Company (grouped)", y = "Average Monthly Income") +
  theme_minimal()

## =============================================================
## VISUALIZATION 5 (ggplot2) : Grouped Boxplot
## Monthly Income by Job Satisfaction and Attrition
## =============================================================
ggplot(df, aes(x = factor(JobSatisfaction), y = MonthlyIncome_capped, fill = Attrition)) +
  geom_boxplot() +
  labs(title = "Monthly Income by Job Satisfaction and Attrition",
       x = "Job Satisfaction (1 = Low, 4 = High)", y = "Monthly Income") +
  theme_minimal()

## =============================================================
## VISUALIZATION 6 (Base R plotting system) : Pie Chart
## Gender Distribution of Employees
## =============================================================
gender_counts <- table(df$Gender)
pct <- round(100 * gender_counts / sum(gender_counts), 1)
labels <- paste0(names(gender_counts), " (", pct, "%)")

pie(gender_counts, labels = labels,
    col = c("#8172B2", "#CCB974"),
    main = "Gender Distribution of Employees")

## =============================================================
## VISUALIZATION 7 (Lattice plotting system) : Faceted Scatter Plot
## Age vs Monthly Income, split by Department
## =============================================================
xyplot(MonthlyIncome_capped ~ Age | Department, data = df,
       main = "Age vs Monthly Income by Department",
       xlab = "Age", ylab = "Monthly Income",
       pch = 16, col = "#4C72B0", alpha = 0.6,
       layout = c(3, 1))

## ---- Key summary numbers referenced in the report -----------------
cat("Average income by department:\n"); print(avg_income_dept)
cat("\nIncome trend by tenure group:\n"); print(trend)
cat("\nGender distribution:\n"); print(gender_counts)

## =============================================================
## End of script
## =============================================================
