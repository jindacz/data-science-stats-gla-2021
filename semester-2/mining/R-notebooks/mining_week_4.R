#mining_week_4
iris
head(train.set)
setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/semester\ 2/mining/Datasets\ for\ week\ 4-20210602")

#Let’s have a look at the training data first
library(plyr)
library(skimr)
library(knitr)
train=read.csv("German_train.csv")
train=train[,c(-1,-5,-13,-14,-19,-21)]
View(train)
train$Account.Balance <- as.factor(train$Account.Balance)
train$Sex...Marital.Status <- as.factor(train$Sex...Marital.Status)
train$Type.of.apartment <- as.factor(train$Type.of.apartment)
train$Purpose <- as.factor(train$Purpose)
train$Length.of.current.employment <- as.factor(train$Length.of.current.employment)
train$Creditability <- as.factor(train$Creditability)
train$Creditability=revalue(train$Creditability,c("0"="No_Credit",
                                                  "1"="Credit"))
colnames(train)[c(2,3,5:9,11:16)] <- c("Account_balance","Duration_of_credit",
                                       "Credit_amount","Value_savings_stocks","Length_of_cur_employment",
                                       "Installment_rate_in_percent","Sex_and_marital_status","Age","Concurrent_credits",
                                       "Apartment_type","Credits_at_bank","Dependents","Foreign_worker")
?skim_with
my_skim <- skim_with(base=sfl(n=length),factor=sfl(ordered=NULL),
                     numeric=sfl(p0=NULL,p100=NULL,hist = NULL))
knit_print(my_skim(train))

#We should try to perform some exploratory analysis before we do anything else. Here, we will check:
#• the relationship between creditability and account balance using the crossTable functions
#• and a histogram of the credit that the applicants ask for.
#(In reality we should do way more exploratory analysis)
install.packages("gmodels")
library(gmodels)
CrossTable(train$Creditability,train$Account_balance,digits=2,
           prop.r=F,prop.t=F,prop.chisq = F)

#Task 1.
#Can you find out how many applicants who ask for more than 11, 000 DM get rejected by the bank?

#We can use the table command to find the number of applicants that get rejected.
table(train[train$Credit_amount>=11000,"Creditability"])

#Task 2 Ridgeline plots can be useful for visualising different distributions (and/or visualising changes in distributions over time or space). Can you use these plots to see the distributions of the amount of credit that
#the applicants are asking for, for each of the four different purposes of credit, and colour it according to
#their applications being accepted or rejected.
install.packages("ggridges");library(ggridges)
library(ggplot2)
View(train)
# Replace the four question marks with the correct variables and data set
ggplot(train, aes(x=Credit_amount, y=Purpose, fill=Creditability)) +
  geom_density_ridges(alpha=0.5) +
  theme_ridges()

#Some interesting things to note here:
#• The distribution of the amount of credit for the rejected applicants is shifted to the right compared to the same
#distribution for the approved applicants. This seems to be the case across all purposes. This reaffirms our belief
#that when asking for a larger amount of credit there is a higher probability of being rejected.
#• If a new applicant wants to apply for credit for buying a new car (i.e. train$Purpose==1), there is a really high
#probability of being accepted if they ask for an amount of 2, 500DM. To notice that, focus on the two distributions
#of the first purpose and see how different they are at the value of 2, 500DM.
#• For the first and third purpose (i.e. buying a new car and home related), the two distributions (i.e. credit versus
#no credit) are quite different. There seems to be a bimodal distribution in the rejected (no credit) case, which is
#not evident in the approved (credit) counterpart.

#Introduction to ‘ggridges‘
#https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html

#example_2
#In this case, we want to classify the variable Creditability using two features (age and account balance),
#so our call to rpart should look like
install.packages("rpart.plot")
library(rpart);library(rpart.plot)
first_tree=rpart(Creditability~Account_balance+Age,data=train,
                 ,method="class")
#use method="class" for factors
#i.e for a classification tree rather than a regression tree
rpart.plot(first_tree,type=2,extra=4)

first_tree

#Task 3.
#What would be your prediction for a 27 year old applicant that does not have an account? And what is
#the probability of misclassification in that case?

#To change the splitting criterion, the following command can be used. You could try it and compare with
#the previous tree. For this example, it turns out that there is no difference at all on the resulting tree.
second_tree=rpart(Creditability~Account_balance+Age, data=train, method="class",
                  parms = list(split='information'))
rpart.plot(second_tree,type=2,extra=4)

#Example 4
#We could change the number of observations that must exist in a node in order for a split to be attempted
#by writing
third_tree <- rpart(Creditability~Account_balance+Age, data=train, method="class",
                    minsplit=100)
rpart.plot(third_tree,type=2,extra=4)

#Compared to first_tree in Example 2, this is a smaller tree with less splits. It makes sense since we
#only allow splits to happen when there are at least 100 observations in that node.

#If we want a tree to have at most three consecutive splits, we would set maxdepth to the value 3.
complex_tree_maxdepth <- rpart(Creditability~Account_balance+Age+Purpose+Credit_amount,
                               data = train, method = "class", maxdepth=3)
rpart.plot(complex_tree_maxdepth,type=2,extra=4)

#By default, the complexity parameter is 0.01. To grow a larger tree, we will need to decrease its value.
first_tree_grown <- rpart(Creditability~Account_balance+Age, data=train,
                          method="class", cp=0.004)
rpart.plot(first_tree_grown,type=2,extra=4)

#This is not always a good idea since it will typically produce overfitted trees. Having said that, trees can
#be pruned back which we will talk about shortly.

#Let’s start with a tree that is fully grown (notice the negative value on the complexity parameter argument)
set.seed(1)
second_tree_fully_grown <- rpart(Creditability~Age+Length_of_cur_employment
                                 +Purpose, data=train, method="class",
                                 parms=list(split='information'),
                                 cp=-1, minsplit=2, minbucket=1)
rpart.plot(second_tree_fully_grown)

#To see the cross validation results use the printcp() function
printcp(second_tree_fully_grown)
#cp: the value of the complexity parameter at each split of the tree;
#• nsplit: the number of splits;
#• rel error: the relative error of each iteration of the tree. This is the fraction of mislabelled elements in the iteration, relative to the fraction of mislabelled elements in the root. In this example,
#if we had to label the elements while being in the root node, the error would be 28.6%. Labelling
#the cases after 2 splits would produce an error rate of 27.20% which is 95.105% of the root node

#Tree 2 has the minimal cross valiation error rate (xerror 0.99301). Therefore, if we prune the tree following
#the minimum error strategy, then we would want to prune our tree with a cp slightly greater than the value
#0.0139860.

#Alternatively, we could prune the tree using the cp of the tree that is within one standard deviation
#of the tree with the smallest xerror; in the case that there are multiple trees with the same xerror, we
#will choose the smaller one (since that tree would predict as well as the other one but it would also
#have fewer branches, thus avoiding overfitting). In this example, the smallest xerror is 0.99301 with the
#corresponding standard deviation of 0.070512. So, we want the tree with xerror less than 1.063522 (=
#0.99301+0.070512). Since only tree 3 has xerror smaller than this value, we would want to prune our tree
#with a cp slightly greater than the value 0.006993.
second_tree_pruned=prune(second_tree_fully_grown,cp=0.007)
rpart.plot(second_tree_pruned)

#For the fully grown tree (i.e. second_tree_fully_grown), compute:
#• the cross validation error rate when there are 9 splits in that tree;
#• the training data error rate when we do not have to prune the tree at all (i.e. after 178 splits).

#If you want to challenge yourself, you can use R commands to double check that the result for the training
#data error rate you just computed is correct.
class.pred=predict(second_tree_fully_grown,type="class")
mean(class.pred!=train$Creditability)

#Example 6.
#If the bank wants to identify a high percentage of incorrectly labelled approved applications without worrying too much about investigating incorrectly labelled accepted applications they can set the loss matrix
#to penalise applications incorrectly labeled as approved four times as applications incorrectly labeled as
#rejected.
lossmatrix=matrix(c(0,4,1,0),byrow=T,nrow=2)
lossmatrix

complex_tree_maxdepth_penalty <- rpart(Creditability~Account_balance+Age
                                       +Purpose+Credit_amount,
                                       data=train, method="class",
                                       maxdepth=3, parms=list(loss=lossmatrix))
rpart.plot(complex_tree_maxdepth_penalty,type=2,extra=4)

#Example 7.
#For the pruned tree built in Example 5, we can see that the most important variable is Purpose and the
#least important value is Length_of_cur_employment.
second_tree_pruned$variable.importance

#Making predictions on future observations
#From here we can use our classification tree to predict the outcome of applications on a test data set. In this way we
#can assess the performance of the model on unseen data

#We can do this using the predict function, with either type="prob" or type="class" where the first
#will return the probabilities of each class in the predicted terminal node for the case and the second will
#simply give the predicted class based on the class with maximum probability in the predicted terminal
#node of the case:
test <- read.csv("German_test.csv")
test$Account.Balance <- as.factor(test$Account.Balance)
test$Sex...Marital.Status <- as.factor(test$Sex...Marital.Status)
test$Type.of.apartment <- as.factor(test$Type.of.apartment)
test$Purpose <- as.factor(test$Purpose)
test$Length.of.current.employment <- as.factor(test$Length.of.current.employment)
test$Creditability <- as.factor(test$Creditability)
test$Creditability <- revalue(test$Creditability, c("0"="No_Credit","1"="Credit"))
test <- test[,c(-1,-5,-13,-14,-19,-21)]
colnames(test)[c(2,3,5:9,11:16)] <- c("Account_balance","Duration_of_credit",
                                      "Credit_amount","Value_savings_stocks","Length_of_cur_employment",
                                      "Installment_rate_in_percent","Sex_and_marital_status","Age","Concurrent_credits",
                                      "Apartment_type","Credits_at_bank","Dependents","Foreign_worker")
newtest <- test[,c("Creditability","Age","Length_of_cur_employment","Purpose")]
newtest$CreditProb <- predict(second_tree_pruned, newdata=newtest, type="prob")
newtest$CreditClass <- predict(second_tree_pruned, newdata=newtest, type="class")
newtest$CreditProb[1:3,]
newtest$CreditClass[1:3]

#For the pruned version of the tree (second_tree_pruned in Example 7), compute:
#• the sensitivity and the specificity,
#• the false discovery rate and
#• the test accuracy,
#where positive is having the application approved and negative is having the application rejected.
cross.class.tab=table(newtest$Creditability,newtest$CreditClass)
#divide by the sum of rows
cross.class.rates=prop.table(cross.class.tab,1)
#sensitivity
cross.class.rates[2,2]

#specificity
cross.class.rates[1,1]
#Comment: The sensitivity is really high but the specificity is really low.

cross.class.tab<-table(newtest$Creditability,newtest$CreditClass)
cross.class.tab
#divide by the sum of columns
cross.class.rates<-prop.table(cross.class.tab,2)
#False discovery rate
cross.class.rates[1,2]

sum(diag(cross.class.tab))/sum(cross.class.tab)
#Comment: Although the overall accuracy of 0.69 isn’t bad, the false discovery rate seems high. Of the people who got
#classified as given credit, 31% of them weren’t.

#Bagging involves:
#1. repeatedly sampling with replacement from a sample (the resulting sample is called a bootstrap sample),
#2. estimating a parameter (or set of parameters) for each bootstrap sample, and
#3. averaging across the bootstrap samples to form the estimate of the parameter

library(purrr); set.seed(1)
data <- seq(from = 1, to = 10)
# sample without replacement
rerun(3,sample(data,replace=F))

# sample with replacement
rerun(3, sample(data, replace = TRUE))

set.seed(1)
data=rnorm(50,5,3)
head(data)

#We want to estimate the standard error of the median. How are we going to do that if we only have one sample?
#We can create more (bootstrap) samples and find the median at each one and then compute the standard error.
resamples <- lapply(1:1000, function(i) sample(data, replace = TRUE))
resamples[1]

r.median <- sapply(resamples, median)
head(r.median)

sd(r.median)

hist(r.median, main="Histogram of median", cex.main=0.6, cex.lab=0.6)

install.packages("randomForest")
library(randomForest)
set.seed(11)

bag_train=randomForest(Creditability~Account_balance+Age+Credit_amount
                       +Purpose+Dependents+Apartment_type,data=train,
                       mtry=6,ntree=200)
bag_train

predictions_bag=predict(bag_train,test,"class")
overall_class_rate_bag=mean(predictions_bag==test$Creditability)

?rpart
set.seed(11)
single_tree=rpart(Creditability~Account_balance+Age+Credit_amount+
                    Purpose+Dependents+Apartment_type,data=train)
predictions=predict(single_tree,test,"class")
overall_class_rate_tree=mean(predictions==test$Creditability)

print(c(overall_class_rate_bag,overall_class_rate_tree))

#The classification rate from a single tree is a bit lower than that using bagging. Was it worth it though?
#You have to remember that even this small increase in the classification rate could save the bank a large
#amount of money. So maybe it is worth it (especially since bagging is not computationally intensive).

#example_10
install.packages("caret")
library(forcats)
library(ggplot2)
library(caret)
library(dplyr)

?fct_reorder
bag_df <- data_frame(var = rownames(randomForest::importance(bag_train)),
                     MeanDecreaseGini = randomForest::importance(bag_train)[, 1]) %>%
  mutate(var = fct_reorder(var, MeanDecreaseGini, median))

bag_ggplot <- ggplot(bag_df, aes(var, MeanDecreaseGini)) +
  geom_point() +
  coord_flip() +
  labs(title = "Predicting credit approval on the German Credit Data",
       subtitle = "Bagging", x = NULL, y = "Average decrease in the Gini Index") +
  theme(plot.title = element_text(hjust = 0.5, size=10),
        plot.subtitle = element_text(hjust = 0.5))

bag_ggplot

#For classification trees, larger values are better. So for the bagged German Credit data, the amount of
#credit, the age of the applicant and the status of his/her account balance are the most important features in determining whether credit will be given or not, whereas the number of dependents is relatively
#unimportant.

#example_11
#Let’s compare the results of the bagged German Credit Data model from Example 9 with the model obtained from using the random forest method (the only difference in the commands is that we do not
#change the default value of the mtry argument):
set.seed(11)
rf_train <- randomForest(Creditability~Account_balance +Age+Credit_amount+Purpose
                         +Dependents+Apartment_type, data = train, ntree = 200)
rf_train

library(tidyr)
rf_df <- data_frame(var = rownames(randomForest::importance(rf_train)),
                    `Random forest` = randomForest::importance(rf_train)[,1]) %>%
  left_join(data_frame(var = rownames(randomForest::importance(rf_train)),
                       Bagging = randomForest::importance(bag_train)[,1])) %>%
  mutate(var = fct_reorder(var, Bagging, median)) %>%
  gather(model, gini, -var)
rf_ggplot <- ggplot(rf_df,aes(var, gini, color = model, shape=model)) +
  geom_point() + coord_flip() +
  labs(title = "Predicting credit approval on the German Credit Data",
       x = NULL,y = "Average decrease in the Gini Index",
       color = "Method",shape="Method") +
  theme(plot.title = element_text(hjust = 0.5))

rf_ggplot










