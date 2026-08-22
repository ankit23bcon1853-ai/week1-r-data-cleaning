# Employee Attrition Analysis using R

This repo has all my work for a 4-week internship task on data analysis using R. I used one dataset (employee attrition / HR data) throughout all 4 weeks and built on top of it each week - cleaning it, visualizing it, and then building a model to predict attrition.

## What's in this repo

| File | What it does |
|---|---|
| `employee_attrition.csv` | The raw dataset before any cleaning (has missing values and outliers) |
| `employee_attrition_cleaned.csv` | The dataset after cleaning, normalizing and encoding (output of week 1) |
| `employee_attrition_analysis.R` | Week 1 script - data cleaning, handling missing values, outliers, normalization, encoding |
| `week2_visualization.R` | Week 2 script - all the charts (bar, scatter, histogram, line, boxplot, pie, lattice) |
| `week3_statistical_modeling.R` | Week 3 script - hypothesis testing + logistic regression model to predict attrition |
| `full_project_analysis.R` | Combined script with everything from week 1-3 in one file, in order |
| `Week1_Data_Cleaning_Report.docx` | Week 1 report doc |
| `Week2_Data_Visualization_Report.docx` | Week 2 report doc |
| `Week3_Statistical_Modeling_Report.docx` | Week 3 report doc |
| `Week4_Comprehensive_Final_Report.docx` | Final report combining all 3 weeks |

## Dataset

500 employee records with columns like Age, Department, Gender, MaritalStatus, DistanceFromHome, MonthlyIncome, YearsAtCompany, OverTime, JobSatisfaction, and Attrition (the target - whether they left or not). Has missing values in a few columns and some outliers in income, which made it good for practicing cleaning techniques.

## How to run

1. Put the `.R` file and `employee_attrition.csv` in the same folder
2. Open the script in RStudio
3. Install the required packages if you don't have them already:
```r
install.packages(c("tidyverse","naniar","corrplot","lattice","caret","pROC","car"))
```
4. Click Source (or run line by line) to see all the outputs and plots

## Summary of what I found

- Missing values were handled with median (numeric columns) and mode (categorical columns) imputation
- Outliers in income were capped using the IQR method instead of removing rows
- Overtime turned out to be the strongest factor linked to attrition (confirmed with a chi-square test, p < 0.001)
- Employees who left the company generally had lower income and less tenure than those who stayed
- Built a logistic regression model to predict attrition - got around 0.70-0.78 AUC depending on the train/test split, with the model being better at catching actual leavers (recall) than avoiding false alarms (precision)

## Tools used

R, tidyverse (dplyr + ggplot2), naniar, corrplot, lattice, caret, pROC, car
