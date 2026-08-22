## =============================================================
## Week 3 Task: Statistical Analysis and Predictive Modeling using R
## Dataset : Employee Attrition Dataset (continued from Week 1 & 2)
## Author  : <Your Name>
## =============================================================

## ---- 0. Load required libraries -------------------------------
# install.packages(c("tidyverse","caret","pROC","car"))
library(tidyverse)   # dplyr, ggplot2
library(caret)       # train/test split, cross-validation, confusion matrix
library(pROC)        # ROC curve and AUC
library(car)         # VIF (multicollinearity check)

set.seed(42)

## ---- 1. Load and re-clean data (same steps as Week 1) -----------
df <- read.csv("employee_attrition.csv", stringsAsFactors = FALSE)

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

df$Attrition <- factor(df$Attrition, levels = c("No","Yes"))
df$OverTime_bin <- ifelse(df$OverTime == "Yes", 1, 0)

## =============================================================
## 2. HYPOTHESIS TESTING
## =============================================================

## 2.1 Normality check on Monthly Income (Shapiro-Wilk test)
## H0: Monthly Income is normally distributed
## H1: Monthly Income is NOT normally distributed
shapiro.test(df$MonthlyIncome_capped[1:500])

qqnorm(df$MonthlyIncome_capped, main = "Q-Q Plot: Monthly Income (capped)")
qqline(df$MonthlyIncome_capped, col = "red", lwd = 2)

## 2.2 Independent-samples t-test
## H0: Mean income is the same for employees who stayed vs left
## H1: Mean income differs between the two groups
t.test(MonthlyIncome_capped ~ Attrition, data = df)

ggplot(df, aes(x = Attrition, y = MonthlyIncome_capped, fill = Attrition)) +
  geom_boxplot() +
  labs(title = "Monthly Income by Attrition (t-test comparison)") +
  theme_minimal()

## 2.3 Chi-square test: OverTime vs Attrition
## H0: OverTime and Attrition are independent
## H1: OverTime and Attrition are associated
overtime_table <- table(df$OverTime, df$Attrition)
print(overtime_table)
chisq.test(overtime_table)

## 2.4 Chi-square test: Department vs Attrition
dept_table <- table(df$Department, df$Attrition)
print(dept_table)
chisq.test(dept_table)

## 2.5 Pearson correlation: Income vs Tenure
## H0: rho = 0 (no linear correlation)
## H1: rho != 0
cor.test(df$MonthlyIncome_capped, df$YearsAtCompany, method = "pearson")

## =============================================================
## 3. MODEL BUILDING: Logistic Regression Classification
## =============================================================

model_data <- df %>%
  select(Attrition, Age, DistanceFromHome, MonthlyIncome_capped,
         YearsAtCompany, JobSatisfaction, OverTime_bin, Department) %>%
  mutate(Department = as.factor(Department))

## 3.1 Train/test split (75/25, stratified on outcome)
train_index <- createDataPartition(model_data$Attrition, p = 0.75, list = FALSE)
train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]

## 3.2 5-fold cross-validation setup
ctrl <- trainControl(method = "cv", number = 5,
                      classProbs = TRUE, summaryFunction = twoClassSummary,
                      sampling = "up")   # up-sample minority class (attrition = Yes)

## 3.3 Train logistic regression model with cross-validation
log_model <- train(Attrition ~ Age + DistanceFromHome + MonthlyIncome_capped +
                      YearsAtCompany + JobSatisfaction + OverTime_bin + Department,
                    data = train_data,
                    method = "glm",
                    family = "binomial",
                    trControl = ctrl,
                    metric = "ROC")

print(log_model)
summary(log_model$finalModel)

## 3.4 Check multicollinearity (VIF)
vif(log_model$finalModel)

## =============================================================
## 4. MODEL DIAGNOSTICS & EVALUATION
## =============================================================

## 4.1 Predictions on test set
pred_prob  <- predict(log_model, newdata = test_data, type = "prob")[, "Yes"]
pred_class <- predict(log_model, newdata = test_data)

## 4.2 Confusion matrix
conf_mat <- confusionMatrix(pred_class, test_data$Attrition, positive = "Yes")
print(conf_mat)

## 4.3 ROC curve and AUC
roc_obj <- roc(response = test_data$Attrition, predictor = pred_prob, levels = c("No","Yes"))
plot(roc_obj, main = paste0("ROC Curve - Attrition Prediction (AUC = ", round(auc(roc_obj), 3), ")"),
     col = "#C44E52", lwd = 2)
auc(roc_obj)

## 4.4 Residual / deviance diagnostics for the underlying GLM
par(mfrow = c(2, 2))
plot(log_model$finalModel)
par(mfrow = c(1, 1))

## 4.5 Coefficient plot (standardized effect direction)
coefs <- broom::tidy(log_model$finalModel) %>% filter(term != "(Intercept)")
ggplot(coefs, aes(x = reorder(term, estimate), y = estimate, fill = estimate > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Logistic Regression Coefficients", x = "Predictor", y = "Coefficient (log-odds)") +
  theme_minimal()

## =============================================================
## 5. Summary of key results (printed for the report)
## =============================================================
cat("Cross-validated ROC (mean):", max(log_model$results$ROC), "\n")
cat("Test Accuracy:", conf_mat$overall["Accuracy"], "\n")
cat("Test Sensitivity (Recall):", conf_mat$byClass["Sensitivity"], "\n")
cat("Test Precision:", conf_mat$byClass["Precision"], "\n")
cat("Test AUC:", as.numeric(auc(roc_obj)), "\n")

## =============================================================
## End of script
## =============================================================
