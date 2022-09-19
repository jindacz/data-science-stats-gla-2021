# This code computes the logistic regression model
# for German Credit data set.

# Load and pre-processing data
library(plyr)
train <- read.csv("German_train.csv")
train <- train[,c(-1,-5,-13,-14,-19,-21)]
train$Account.Balance <- as.factor(train$Account.Balance)
train$Sex...Marital.Status <- as.factor(train$Sex...Marital.Status)
train$Type.of.apartment <- as.factor(train$Type.of.apartment)
train$Purpose <- as.factor(train$Purpose)
train$Length.of.current.employment <- as.factor(train$Length.of.current.employment)
train$Creditability <- as.factor(train$Creditability)
train$Creditability <- revalue(train$Creditability, c("0"="No_Credit","1"="Credit"))
colnames(train)[c(2,3,5:9,11:16)] <- c("Account_balance","Duration_of_credit",
                                       "Credit_amount","Value_savings_stocks","Length_of_cur_employment",
                                       "Installment_rate_in_percent","Sex_and_marital_status","Age","Concurrent_credits",
                                       "Apartment_type","Credits_at_bank","Dependents","Foreign_worker")

# Build a logistic regression model
glm_credit <- glm(Creditability ~ Account_balance+Purpose,
                  family=binomial, data=train)
summary(glm_credit)
#-------------------------------comment-------------------------------#
# 1. The baseline model refers to an applicant with Account_balance=1 #
# and Purpose=1.                                                      #
# 2. The coefficients for the Account_balance variable are positive,  #
# which means that applicants that have an account or an account with #
# some balance have a higher chance of being considered "good" risks. #
# 3. An applicant who applied for credit with any other purpose       #
# besides buying a new car will have a lower chance of being          #
# considered a "good" credit risk.                                    #
#---------------------------------------------------------------------#

# Quantify the effect of each predictor using the odds ratios
library(sjPlot);library(ggplot2)
ggplot_odds <- plot_model(glm_credit)
ggplot_odds

# Compute the training cross entropy loss
ce_loss <- function(x,y){
  -(x*log(y)+(1-x)*log(1-y))
}
glm_credit_loss <- sum(ce_loss(as.numeric(train$Creditability)-1, glm_credit$fitted.values))
paste("CE loss function from logistic regression:", round(glm_credit_loss,3))

# Compute the test cross entropy loss
newtest <- test[,c("Account_balance","Purpose","Sex_and_marital_status")]
pr.log <- predict(glm_credit,newtest,type="response")
Credit_test_loss_logistic <-sum(ce_loss(test$Creditability,pr.log))
paste("CE Test Loss from logistic regression: ", round(Credit_test_loss_logistic, 3))
