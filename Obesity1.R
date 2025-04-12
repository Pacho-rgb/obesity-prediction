# Load necessary libraries
library(tidyverse)
library(car) # For VIF calculation
library(ResourceSelection) # For Hosmer-Lemeshow test
library(pROC) # For ROC curve
library(MASS) # For stepwise regression
library(pscl)
#library(dplyr)
#library(flextable)


# Read the data
data <- read.csv("Project Data.csv")

# Clean column names
colnames(data) <- c("ID", "Sex", "Age", "Obese_Family", "Fast_Food", 
                    "Vegetables", "Main_Meals", "Between_Meals", "Smoking", 
                    "Liquid_Intake", "Calorie_Calculation", "Physical_Exercise", 
                    "Transportation", "Obese")

# Convert variables to appropriate types (factors for categorical variables)
data <- data %>%
  mutate(
    Sex = factor(Sex, levels = 1:2, labels = c("Male", "Female")),
    Obese_Family = factor(Obese_Family, levels = 1:2, labels = c("Yes", "No")),
    Fast_Food = factor(Fast_Food, levels = 1:2, labels = c("Yes", "No")),
    Smoking = factor(Smoking, levels = 1:2, labels = c("Yes", "No")),
    Calorie_Calculation = factor(Calorie_Calculation, levels = 1:2, labels = c("Yes", "No")),
    Transportation = factor(Transportation, levels = 1:5, 
                            labels = c("Automobile", "Motorbike", "Bike", 
                                       "Public_Transportation", "Walking")),
    Vegetables = factor(Vegetables, levels = 1:3, 
                        labels = c("Rarely", "Sometimes", "Always"), ordered = TRUE),
    Main_Meals = factor(Main_Meals, levels = 1:3, 
                        labels = c("At_most_twice", "Three_times", "More_than_three"), ordered = TRUE),
    Between_Meals = factor(Between_Meals, levels = 1:4, 
                           labels = c("Rarely", "Sometimes", "Usually", "Always"), ordered = TRUE),
    Liquid_Intake = factor(Liquid_Intake, levels = 1:3, 
                           labels = c("Less_than_1L", "Between_1_2L", "More_than_2L"), ordered = TRUE),
    Physical_Exercise = factor(Physical_Exercise, levels = 1:5, 
                               labels = c("None", "1-2_days", "3-4_days", "5-6_days", "6+_days"), ordered = TRUE),
    Obese = factor(Obese, levels = 0:1, labels = c("No", "Yes")))

# Summary of the data
summary(data)

# Check the proportion of obese vs non-obese
prop.table(table(data$Obese))

# Visualize the relationship between obesity and categorical variables
ggplot(data, aes(x = Obese, fill = Obese_Family)) + 
  geom_bar(position = "fill") + 
  labs(title = "Obesity by Family History", y = "Proportion")

ggplot(data, aes(x = Obese, fill = Physical_Exercise)) + 
  geom_bar(position = "fill") + 
  labs(title = "Obesity by Physical Exercise Level", y = "Proportion")

# Age distribution by obesity status
ggplot(data, aes(x = Age, fill = Obese)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Age Distribution by Obesity Status")

# Convert ordered factors to numeric for modeling (treating as continuous)
data <- data %>%
  mutate(
    Vegetables_num = as.numeric(Vegetables),
    Main_Meals_num = as.numeric(Main_Meals),
    Between_Meals_num = as.numeric(Between_Meals),
    Liquid_Intake_num = as.numeric(Liquid_Intake),
    Physical_Exercise_num = as.numeric(Physical_Exercise)
  )


# Running correlation of continuous and numeric variables
# Select relevant variables
numeric_vars <- data %>% dplyr::select(Age, Vegetables_num, Main_Meals_num, Between_Meals_num, Liquid_Intake_num, Physical_Exercise_num)
# Verify structure
str(numeric_vars)
# Use Pearson correlation for linear relationships between numeric variables:
cor_matrix <- cor(numeric_vars, method = "pearson")
rounded_cor <- round(cor_matrix, 2)  # Round to 2 decimal places
rounded_cor

# Build the full logistic regression model
full_model <- glm(Obese ~ Sex + Age + Obese_Family + Fast_Food + 
                    Vegetables_num + Main_Meals_num + Between_Meals_num + 
                    Smoking + Liquid_Intake_num + Calorie_Calculation + 
                    Physical_Exercise_num + Transportation,
                  data = data, family = binomial(link = "logit"))

# Summary of the full model
summary(full_model)

# Calculate odds ratios and confidence intervals
exp(cbind(OR = coef(full_model), confint(full_model)))

# Hosmer-Lemeshow test for goodness of fit
hoslem.test(data$Obese, fitted(full_model), g = 10)

# ROC curve and AUC
roc_curve <- roc(data$Obese, predict(full_model, type = "response"))
plot(roc_curve)
auc(roc_curve)


vif_full <- vif(full_model)
print("VIF (Full Model):")
print(vif_full)

# Variance Inflation Factor (VIF) - for continuous predictors only
# Need to create a version of the model with only continuous predictors
cont_model <- glm(Obese ~ Age + Vegetables_num + Main_Meals_num + 
                    Between_Meals_num + Liquid_Intake_num + Physical_Exercise_num,
                  data = data, family = binomial)
vif_cont <- vif(cont_model)
print("VIF (Cont Model):")
print(vif_cont)

# Stepwise selection based on AIC
step_model <- stepAIC(full_model, direction = "both", trace = FALSE)
summary(step_model)

# ROC curve and AUC
roc_curve1 <- roc(data$Obese, predict(step_model, type = "response"))
plot(roc_curve1)
auc(roc_curve1)

# Compare BIC for both models
BIC(full_model, step_model)

# Compare with full model
anova(step_model, full_model, test = "Chisq")

# McFadden's R²
full_r2 <- pR2(full_model)["McFadden"]
full_r2
step_r2 <- pR2(step_model)["McFadden"]
step_r2

# Multicollinearity
# For the STEPWISE MODEL
vif_step <- vif(step_model)
print("VIF (Stepwise Model):")
print(vif_step)
