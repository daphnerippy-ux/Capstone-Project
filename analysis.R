# =========================
# Capstone Churn Analysis
# =========================

# 1. Load libraries
library(tidyverse)
library(psych)
library(corrplot)
library(caret)
library(pROC)

# 2. Load dataset
telco <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv",
                  stringsAsFactors = FALSE,
                  check.names = FALSE)

# 3. Data inspection
head(telco)
str(telco)
summary(telco)

# 4. Data cleaning
telco$Total.Charges <- as.numeric(telco$Total.Charges)

# Ensure churn is numeric for modeling (0/1 format assumed)
telco$Churn.Value <- as.numeric(telco$Churn.Value)

# 5. Correlation analysis
telco_num <- telco[, c("Tenure.Months", "Monthly.Charges", "Total.Charges")]

cor_matrix <- cor(telco_num, use = "complete.obs")
round(cor_matrix, 2)

corrplot(cor_matrix, method = "circle")

# 6. Logistic regression model
model <- glm(Churn.Value ~ Tenure.Months + Monthly.Charges + Total.Charges,
             data = telco,
             family = binomial)

summary(model)
exp(coef(model))

# 7. Odds ratio table
summary_model <- summary(model)

log_table <- data.frame(
  Variable = rownames(summary_model$coefficients),
  Coefficient = summary_model$coefficients[, 1],
  P_value = summary_model$coefficients[, 4],
  Odds_Ratio = exp(summary_model$coefficients[, 1])
)

log_table

# 8. ROC Curve + AUC
pred_prob <- predict(model, type = "response")

roc_obj <- roc(telco$Churn.Value, pred_prob)

plot(roc_obj, col = "blue", main = "ROC Curve - Churn Model")
auc(roc_obj)

# 9. Visualizations

# Churn distribution
ggplot(telco, aes(x = factor(Churn.Value))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Customer Churn Distribution",
       x = "Churn",
       y = "Count")

# Monthly charges vs churn
ggplot(telco, aes(x = factor(Churn.Value), y = Monthly.Charges)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Monthly Charges by Churn",
       x = "Churn",
       y = "Monthly Charges")

# Tenure vs churn
ggplot(telco, aes(x = factor(Churn.Value), y = Tenure.Months)) +
  geom_boxplot(fill = "green") +
  labs(title = "Customer Tenure by Churn",
       x = "Churn",
       y = "Tenure (Months)")

