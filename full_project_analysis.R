## Employee Attrition Analysis - Full Project (Week 1 to Week 3)
## This script has all my work combined - cleaning, visualization and the model
## Made this by putting together my week1, week2 and week3 scripts into one file

## just load everything I need at the top
library(tidyverse)   # for dplyr and ggplot2 mainly
library(naniar)       # to see missing values properly
library(corrplot)     # for the correlation heatmap
library(lattice)      # used this for one of the plots
library(caret)        # train/test split + cross validation
library(pROC)         # for ROC curve and AUC
library(car)          # checking VIF

set.seed(42)  # so results dont change every time I run it

# ---------------------------------------------------------
# PART 1 - DATA CLEANING (this is basically my week 1 work)
# ---------------------------------------------------------

df <- read.csv("employee_attrition.csv", stringsAsFactors = FALSE)

# just checking what the data looks like first
str(df)
summary(df)
dim(df)

# how much missing data do we actually have
colSums(is.na(df))
vis_miss(df)   # this gives a nice plot of missing values

# for number columns I'm just filling missing with the median
# median is better than mean here since income is not normal (checked this later in week3)
df$Age[is.na(df$Age)] <- median(df$Age, na.rm = TRUE)
df$MonthlyIncome[is.na(df$MonthlyIncome)] <- median(df$MonthlyIncome, na.rm = TRUE)
df$DistanceFromHome[is.na(df$DistanceFromHome)] <- median(df$DistanceFromHome, na.rm = TRUE)

# for categorical ones, filling with whatever value shows up most (the mode)
# R doesnt have a built in mode function so writing a quick one
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
df$Department[is.na(df$Department)] <- get_mode(df$Department)
df$MaritalStatus[is.na(df$MaritalStatus)] <- get_mode(df$MaritalStatus)

# double check - should be all zeros now
colSums(is.na(df))

# now checking for outliers using IQR, mainly worried about income and years at company
find_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_val <- q3 - q1
  low <- q1 - 1.5 * iqr_val
  high <- q3 + 1.5 * iqr_val
  cat("lower limit:", low, " upper limit:", high, " outliers found:",
      sum(x < low | x > high, na.rm = TRUE), "\n")
  return(list(low = low, high = high))
}

find_outliers(df$MonthlyIncome)
find_outliers(df$YearsAtCompany)

boxplot(df$MonthlyIncome, main = "checking income outliers", col = "orange")
boxplot(df$YearsAtCompany, main = "checking tenure outliers", col = "lightgreen")

# capping the income outliers instead of deleting rows, dont want to lose data
bounds <- find_outliers(df$MonthlyIncome)
df$MonthlyIncome_capped <- pmin(pmax(df$MonthlyIncome, bounds$low), bounds$high)

# normalizing the numeric stuff (0 to 1 scale) so everything is comparable
normalize <- function(x) (x - min(x)) / (max(x) - min(x))
df$Age_norm <- normalize(df$Age)
df$Income_norm <- normalize(df$MonthlyIncome_capped)
df$Years_norm <- normalize(df$YearsAtCompany)

# encoding categorical stuff so it can be used in the model later
df$OverTime_bin <- ifelse(df$OverTime == "Yes", 1, 0)
df$Attrition_bin <- ifelse(df$Attrition == "Yes", 1, 0)
df$Attrition <- factor(df$Attrition, levels = c("No", "Yes"))  # need this as factor for the model part

write.csv(df, "employee_attrition_cleaned.csv", row.names = FALSE)


# ---------------------------------------------------------
# PART 2 - VISUALIZATIONS (this was my week 2 stuff)
# ---------------------------------------------------------

# bar chart - avg income per department
avg_income_dept <- df %>%
  group_by(Department) %>%
  summarise(avg_income = mean(MonthlyIncome_capped)) %>%
  arrange(avg_income)

ggplot(avg_income_dept, aes(x = reorder(Department, avg_income), y = avg_income, fill = Department)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Average Income by Department", x = "", y = "Avg Monthly Income") +
  theme_minimal()

# scatter plot - income vs years, colored by whether they left or not
# this one honestly gave me the biggest clue in the whole project
ggplot(df, aes(x = YearsAtCompany, y = MonthlyIncome_capped, color = Attrition)) +
  geom_point(alpha = 0.7) +
  labs(title = "Income vs Years at Company") +
  theme_minimal()

# histogram for distance from home
ggplot(df, aes(x = DistanceFromHome)) +
  geom_histogram(bins = 15, fill = "seagreen", alpha = 0.8) +
  labs(title = "Distance From Home Distribution") +
  theme_minimal()

# line chart - grouping years into buckets and seeing avg income trend
df$YearsBucket <- cut(df$YearsAtCompany, breaks = c(0,2,4,6,8,10,15,40),
                       labels = c("0-2","2-4","4-6","6-8","8-10","10-15","15+"))

income_trend <- df %>%
  filter(!is.na(YearsBucket)) %>%
  group_by(YearsBucket) %>%
  summarise(avg_income = mean(MonthlyIncome_capped))

ggplot(income_trend, aes(x = YearsBucket, y = avg_income, group = 1)) +
  geom_line(color = "red") +
  geom_point() +
  labs(title = "Income Trend by Tenure Group") +
  theme_minimal()

# boxplot - income by satisfaction, split by attrition
ggplot(df, aes(x = factor(JobSatisfaction), y = MonthlyIncome_capped, fill = Attrition)) +
  geom_boxplot() +
  labs(title = "Income by Job Satisfaction and Attrition", x = "Satisfaction Level") +
  theme_minimal()

# pie chart using base R just to try a different plotting system
gender_counts <- table(df$Gender)
pie(gender_counts, main = "Gender Split", col = c("purple","gold"))

# lattice plot - age vs income split by department, side by side
xyplot(MonthlyIncome_capped ~ Age | Department, data = df,
       main = "Age vs Income by Department", pch = 16, col = "steelblue")


# ---------------------------------------------------------
# PART 3 - STATS + MODEL (week 3 stuff)
# ---------------------------------------------------------

## first checking if income is normally distributed or not
shapiro.test(df$MonthlyIncome_capped[1:500])
qqnorm(df$MonthlyIncome_capped)
qqline(df$MonthlyIncome_capped, col = "red")
# turned out p value is basically 0 so income is NOT normal, its skewed
# this is why median imputation made sense earlier instead of mean

## t-test - does income actually differ for people who left vs stayed
t.test(MonthlyIncome_capped ~ Attrition, data = df)
# p value came out less than 0.05 so yes there is a real difference

## chi-square - is overtime connected to attrition
overtime_tab <- table(df$OverTime, df$Attrition)
print(overtime_tab)
chisq.test(overtime_tab)
# this came out really significant, overtime people leave a lot more

## chi-square - department vs attrition
dept_tab <- table(df$Department, df$Attrition)
chisq.test(dept_tab)

## correlation - income and years at company
cor.test(df$MonthlyIncome_capped, df$YearsAtCompany)

## now building the actual prediction model
## going with logistic regression since attrition is yes/no (classification problem)

model_df <- df %>%
  select(Attrition, Age, DistanceFromHome, MonthlyIncome_capped,
         YearsAtCompany, JobSatisfaction, OverTime_bin, Department) %>%
  mutate(Department = as.factor(Department))

# splitting into train and test, 75/25
train_idx <- createDataPartition(model_df$Attrition, p = 0.75, list = FALSE)
train_set <- model_df[train_idx, ]
test_set  <- model_df[-train_idx, ]

# using 5 fold cross validation, and upsampling since not many people
# actually left the company (data was imbalanced)
ctrl_settings <- trainControl(method = "cv", number = 5,
                               classProbs = TRUE, summaryFunction = twoClassSummary,
                               sampling = "up")

my_model <- train(Attrition ~ Age + DistanceFromHome + MonthlyIncome_capped +
                     YearsAtCompany + JobSatisfaction + OverTime_bin + Department,
                   data = train_set, method = "glm", family = "binomial",
                   trControl = ctrl_settings, metric = "ROC")

print(my_model)
summary(my_model$finalModel)

# checking multicollinearity just to be safe
vif(my_model$finalModel)

# now testing on data the model hasnt seen yet
predicted_probs <- predict(my_model, newdata = test_set, type = "prob")[, "Yes"]
predicted_class <- predict(my_model, newdata = test_set)

# confusion matrix to see how many it got right/wrong
conf_matrix <- confusionMatrix(predicted_class, test_set$Attrition, positive = "Yes")
print(conf_matrix)

# ROC curve
roc_result <- roc(test_set$Attrition, predicted_probs, levels = c("No","Yes"))
plot(roc_result, main = "ROC Curve for Attrition Model", col = "darkred")
auc(roc_result)

# residual diagnostic plots for the model
par(mfrow = c(2,2))
plot(my_model$finalModel)
par(mfrow = c(1,1))

# quick summary print at the end so I can see the important numbers together
cat("\n---- FINAL RESULTS ----\n")
cat("Accuracy:", conf_matrix$overall["Accuracy"], "\n")
cat("Recall:", conf_matrix$byClass["Sensitivity"], "\n")
cat("Precision:", conf_matrix$byClass["Precision"], "\n")
cat("AUC:", as.numeric(auc(roc_result)), "\n")

## that's basically the whole project - clean data, visualize it, then predict from it
