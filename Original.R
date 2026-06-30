install.packages("tidyverse")
install.packages("psych")
install.packages("corrplot")
install.packages("caret")
library(tidyverse)
library(psych)
library(corrplot)
library(caret)
telco <- read.csv("Telco-Customer-Churn.csv")
getwd()
list.files()
telco <- read.csv("C:/Users/YourName/Downloads/Telco-Customer-Churn.csv")
setwd("/Users/jdrippy/Downloads")
getwd()
list.files()
list.dirs()
telco <- read.csv(file.choose())
telco <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv",
                  row.names = NULL)
telco <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv",
                  stringsAsFactors = FALSE,
                  check.names = FALSE)
list.files()
telco <- read.csv(file.choose())
unzip("Telco-Customer-Churn.zip")
telco <- read.csv(file.choose(), stringsAsFactors = FALSE)
head(telco)
str(telco)
summary(telco)
telco$TotalCharges <- as.numeric(telco$TotalCharges)
colnames(telco)
telco$Churn <- telco$Churn.Value
table(telco$Churn)
telco$Total.Charges <- as.numeric(telco$Total.Charges)
telco$Total.Charges <- as.numeric(telco$Total.Charges)
summary(telco[, c("Tenure.Months", "Monthly.Charges", "Total.Charges")])
numeric_data <- telco[, c("Tenure.Months", "Monthly.Charges", "Total.Charges", "Churn.Value")]

cor(numeric_data)
model <- glm(Churn.Value ~ Tenure.Months + Monthly.Charges + Total.Charges,
             data = telco,
             family = binomial)

summary(model)
exp(coef(model))
str(telco)
numeric_data <- telco[, c("Tenure.Months", "Monthly.Charges", "Total.Charges", "Churn.Value")]
str(numeric_data)
cor(numeric_data, use = "complete.obs")
model <- glm(Churn.Value ~ Tenure.Months + Monthly.Charges + Total.Charges,
             data = telco,
             family = binomial)
summary(model)
exp(coef(model))
numeric_data <- telco[, c("Tenure.Months", "Monthly.Charges", "Total.Charges", "Churn.Value")]

cor(numeric_data, use = "complete.obs")

model <- glm(Churn.Value ~ Tenure.Months + Monthly.Charges + Total.Charges,
             data = telco,
             family = binomial)

summary(model)
exp(coef(model))
install.packages("pROC")
library(pROC)
pred_prob <- predict(model, type = "response")
roc_obj <- roc(telco$Churn.Value, pred_prob)
model_data <- model.frame(model)
pred_prob <- predict(model, type = "response")
library(pROC)

roc_obj <- roc(model_data$Churn.Value, pred_prob)
plot(roc_obj, col = "blue", main = "ROC Curve - Churn Prediction Model")
auc(roc_obj)
auc(roc_obj)
plot(roc_obj, col = "blue", main = "ROC Curve - Churn Prediction Model")
library(ggplot2)

ggplot(telco, aes(x = factor(Churn.Value))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Customer Churn Distribution",
    x = "Churn",
    y = "Number of Customers"
  )
ggplot(telco, aes(x = factor(Churn.Value), y = Monthly.Charges)) +
  geom_boxplot(fill = "orange") +
  labs(
    title = "Monthly Charges by Customer Churn",
    x = "Churn",
    y = "Monthly Charges"
  )
library(ggplot2)
install.packages("ggplot2")
library(ggplot2)
ggplot(telco, aes(x = factor(Churn.Value), y = Monthly.Charges)) +
  geom_boxplot(fill = "orange") +
  labs(
    title = "Monthly Charges by Customer Churn",
    x = "Churn",
    y = "Monthly Charges"
  )
ggplot(telco, aes(x = factor(Churn.Value), y = Tenure.Months)) +
  geom_boxplot(fill = "green") +
  labs(
    title = "Customer Tenure by Churn",
    x = "Churn",
    y = "Tenure (Months)"
  )
# Select only numeric variables
telco_num <- telco[, c("Tenure.Months", "Monthly.Charges", "Total.Charges")]

# Run correlation matrix
cor(telco_num, use = "complete.obs")
ls()
colnames(telco)
telco_num <- telco[, c("Tenure.Months", "Monthly.Charges", "Total.Charges")]
str(telco$Total.Charges)
telco_num <- telco[, c("Tenure.Months", "Monthly.Charges", "Total.Charges")]
cor(telco_num, use = "complete.obs")
install.packages("psych")
library(psych)

corr.test(telco_num, use = "pairwise")
round(cor(telco_num, use = "complete.obs"), 2)
install.packages("corrplot")
library(corrplot)
cor_matrix <- cor(telco_num, use = "complete.obs")
corrplot(cor_matrix, method = "circle")
telco$Churn.Value <- as.factor(telco$Churn.Value)
model <- glm(Churn.Value ~ Tenure.Months + Monthly.Charges + Total.Charges,
             data = telco,
  
                        family = binomial)
summary(model)

model <- glm(Churn.Value ~ Tenure.Months + Monthly.Charges + Total.Charges,
             data = telco,
             family = binomial)
summary_model <- summary(model)

log_table <- data.frame(
  Variable = rownames(summary_model$coefficients),
  Coefficient = summary_model$coefficients[, 1],
  P_value = summary_model$coefficients[, 4],
  Odds_Ratio = exp(summary_model$coefficients[, 1])
)
model <- glm(Churn.Value ~ Tenure.Months + Monthly.Charges + Total.Charges,
             data = telco,
             family = binomial)
summary_model <- summary(model)
log_table <- data.frame(
  Variable = rownames(summary_model$coefficients),
  Coefficient = summary_model$coefficients[, 1],
  P_value = summary_model$coefficients[, 4],
  Odds_Ratio = exp(summary_model$coefficients[, 1])
)
log_table
auc(roc_curve)
getwd()
