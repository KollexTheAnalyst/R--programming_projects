library(tidyverse)
library(dplyr)
library(ROCR)

#Loading the dataset
############################################
# install.packages("readxl")
#library(readxl)
#data <- read_excel("your_file.xlsx")
############################################
# install.packages("jsonlite")
#library(jsonlite)
#data <- fromJSON("your_file.json")
###########################################
# Load a CSV file from a URL
#url <- "https://example.com/your_data.csv"
#data <- read.csv(url)
##########################################
train <- read.csv("C:\\Users\\My PC\\Documents\\DATA ANALYSIS\\Datasets\\Train_.csv")

head(train)
View(train)
summary(train)

#Columns with missing values:: MONTANT, FREQUENCE_RECH, REVENUE, ARPU_SEGMENT,
#FREQUENCE, DATA_VOLUME, ON_NET, ORANGE, TIGO, ZONE1, ZONE1, REGULARITY, FREQ_TOP_PACK.

##########DATA CLEANING#############################
#First - Dropping columns with too muching of missing values
train$DATA_VOLUME <- NULL #1060433
train$TIGO <- NULL #1290016
train$ZONE1 <- NULL #1984327
train$ZONE2 <- NULL #2017224

#Secondly - Dropping every rows that contains a missing value
train <- train[complete.cases(train), ] # 1,056,844rows left

#Thirdly - Dropping columns which are not important
train$user_id <- NULL
train$REGION <- NULL
train$TENURE <- NULL
train$MRG <- NULL
train$TOP_PACK <- NULL

glimpse(train)

# Changing the variable types
# CHURN
train$CHURN <- as.numeric(train$CHURN)

# Set seed for reproducibility
set.seed(123)  
# Sample 10% of the data for training
train_data <- sample_frac(train, 0.1)
###########################################################

test <- read.csv("C:\\Users\\My PC\\Documents\\DATA ANALYSIS\\Datasets\\Test_.csv")

summary(test)
#Apply same data cleaning process
##########DATA CLEANING#############################
#First - Dropping columns with too much of missing values
test$DATA_VOLUME <- NULL #187040
test$TIGO <- NULL #227001
test$ZONE1 <- NULL #350266
test$ZONE2 <- NULL #356051

#Secondly - Dropping every rows that contains a missing value
test <- test[complete.cases(test), ] # 186,919rows left

summary(test)

#Thirdly - Dropping columns which are not important
test$user_id <- NULL
test$REGION <- NULL
test$TENURE <- NULL
test$MRG <- NULL
test$TOP_PACK <- NULL

glimpse(test)

# Sample 10% of the data for training
test_data <- sample_frac(test, 0.1)
##############################################

########MODEL#################################
#Using Logistic Regression algorithm
model_log_reg <- glm(CHURN~.,data = train,family = "binomial")

model_log_reg_1 <- glm(CHURN~.,data = train_data,family = "binomial")

#Using SVM algorithms
library(e1071)
model_svm_linear <- svm(CHURN ~ ., data = train_data, kernel = "linear")

model_svm_poly <- svm(CHURN ~ ., data = train_data, kernel = "polynomial", degree = 2)

####################################################################
###############MODEL TEST###########################################
####################################################################
CHURN_PRED_model_log_reg <- predict(model_log_reg,test,type = "response")

CHURN_PRED_model_log_reg_1 <- predict(model_log_reg_1,test_data,type = "response")

CHURN_PRED_model_svm_linear <- predict(model_svm_linear,test_data)

CHURN_PRED_model_svm_poly <- predict(model_svm_poly,test_data)

############################################################################
###########################################################################

# Checking the Accuracy of the model for log_reg
#table(ActualValue = new_test_model_log_reg$CHURN_PRED_model_log_reg, PredctedValue = CHURN_PRED_model_log_reg>0.3)
#Rocpred = prediction(CHURN_PRED_model_log_reg,train$CHURN)
#any(is.na(test$CHURN))

#any(is.nan(binary_CHURN_PRED_model_log_reg))

# Check for infinite values
#any(is.infinite(binary_CHURN_PRED_model_log_reg))
#any(is.infinite(test$CHURN))
########################################################

# For logistic regression models
binary_CHURN_PRED_model_log_reg <- ifelse(CHURN_PRED_model_log_reg > 0.5, 1, 0)
accuracy <- mean(binary_CHURN_PRED_model_log_reg == train$CHURN, na.rm = TRUE)

glimpse(binary_CHURN_PRED_model_log_reg)

glimpse(CHURN_PRED_model_log_reg)

print(paste("Accuracy: ", round(accuracy, 4)))
########################################################################
binary_CHURN_PRED_model_log_reg_1 <- ifelse(CHURN_PRED_model_log_reg_1 > 0.5, 1, 0)
accuracy <- mean(binary_CHURN_PRED_model_log_reg_1 == train_data$CHURN, na.rm = TRUE)

print(paste("Accuracy: ", round(accuracy, 4)))
##########################################################################
# Accuracy of the SVM Models

binary_CHURN_PRED_model_svm_linear <- ifelse(CHURN_PRED_model_svm_linear > 0.5, 1, 0)
accuracy <- mean(binary_CHURN_PRED_model_svm_linear == train_data$CHURN)

print(paste("Accuracy: ", round(accuracy, 4)))
###############################################################
binary_CHURN_PRED_model_svm_poly <- ifelse(CHURN_PRED_model_svm_poly > 0.5, 1, 0)
accuracy <- mean(binary_CHURN_PRED_model_svm_poly == train_data$CHURN)

print(paste("Accuracy: ", round(accuracy, 4)))
###############################################################
