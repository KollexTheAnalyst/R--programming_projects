library(tidyverse)
library(caTools)
library(ggplot2)
library(ROCR)
library(corrplot)
install.packages("car")

# Loading the dataset
train <- read_csv("C:/Users/LOLADE/Desktop/Finance/Train_.csv")
test <- read_csv("C:/Users/LOLADE/Desktop/Finance/Test_.csv")
view(train)
summary(train)
# MONTANT,FREQUENCE_RECH,REVENUE,ARPU_SEGMENT,FREQUENCE,DATA_VOLUME,ON_NET,
# ORANGE,TIGO,FREQ_TOP_PACK has some missing values
glimpse(train)

#####################################################
################### Data Cleaning ###################
#####################################################

# Fot the train Dataset
# For Missing Values
mean(train$MONTANT, na.rm =  TRUE)
train$MONTANT[is.na(train$MONTANT)] <- mean(train$MONTANT, na.rm =  TRUE)
train$FREQUENCE_RECH[is.na(train$FREQUENCE_RECH)] <- mean(train$FREQUENCE_RECH, na.rm =  TRUE)
train$REVENUE[is.na(train$REVENUE)] <- mean(train$REVENUE, na.rm =  TRUE)
train$ARPU_SEGMENT[is.na(train$ARPU_SEGMENT)] <- mean(train$ARPU_SEGMENT, na.rm =  TRUE)
train$DATA_VOLUME[is.na(train$DATA_VOLUME)] <- mean(train$DATA_VOLUME, na.rm =  TRUE)
train$ON_NET[is.na(train$ON_NET)] <- mean(train$ON_NET, na.rm =  TRUE)
train$ORANGE[is.na(train$ORANGE)] <- mean(train$ORANGE, na.rm =  TRUE)
train$TIGO[is.na(train$TIGO)] <- mean(train$TIGO, na.rm =  TRUE)
train$FREQ_TOP_PACK[is.na(train$FREQ_TOP_PACK)] <- mean(train$FREQ_TOP_PACK, na.rm =  TRUE)
train$FREQUENCE[is.na(train$FREQUENCE)] <- mean(train$FREQUENCE, na.rm =  TRUE)

# For Useless variables
# MRG,TOP_PACK, user_id,REGION,TENURE are Useless variables
# Zone 1 and 2 contains too much of missing values to be included in our model
# table(train$MRG)

train <- train[,-c(1:3)] #Removing the first 3 useless variables
train <- train[,-c(10:12)] #Renoving the next 3 column
train <- train[,-11] #Removing the TOP_PACK variable

# Changing the variable types
# CHURN
train$CHURN <- as.numeric(train$CHURN)

# For the Trst Dataset
glimpse(test)
summary(test)

# To clear the missing values

test$MONTANT[is.na(test$MONTANT)] <- mean(test$MONTANT, na.rm =  TRUE)
test$FREQUENCE_RECH[is.na(test$FREQUENCE_RECH)] <- mean(test$FREQUENCE_RECH, na.rm =  TRUE)
test$REVENUE[is.na(test$REVENUE)] <- mean(test$REVENUE, na.rm =  TRUE)
test$ARPU_SEGMENT[is.na(test$ARPU_SEGMENT)] <- mean(test$ARPU_SEGMENT, na.rm =  TRUE)
test$DATA_VOLUME[is.na(test$DATA_VOLUME)] <- mean(test$DATA_VOLUME, na.rm =  TRUE)
test$ON_NET[is.na(test$ON_NET)] <- mean(test$ON_NET, na.rm =  TRUE)
test$ORANGE[is.na(test$ORANGE)] <- mean(test$ORANGE, na.rm =  TRUE)
test$TIGO[is.na(test$TIGO)] <- mean(test$TIGO, na.rm =  TRUE)
test$FREQ_TOP_PACK[is.na(test$FREQ_TOP_PACK)] <- mean(test$FREQ_TOP_PACK, na.rm =  TRUE)
test$FREQUENCE[is.na(test$FREQUENCE)] <- mean(test$FREQUENCE, na.rm =  TRUE)

# To remove Useless variables

test <- test[,-c(1:3)] #Removing the first 3 useless variables
test <- test[,-c(10:12)] #Renoving the next 3 column
test <- test[,-11] #Removing the TOP_PACK variable

###########################################
################## model ##################
###########################################
model <- glm(CHURN~.,data = train,family = "binomial")
CHURN_PRED <- predict(model,test,type = "response")
test_mod <- data.frame(CHURN_PRED)
test_mod$CHURN_PRED <- as.factor(test_mod$CHURN_PRED)

new_test <- cbind(test,test_mod)
glimpse(new_test)

# Checking the Accuracy of the model
table(ActualValue = new_test$CHURN_PRED, PredctedValue = CHURN_PRED>0.3)
Rocpred = prediction(res,train$CHURN)
class(new_test$CHURN_PRED)

















