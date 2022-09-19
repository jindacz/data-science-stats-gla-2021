#mining_w3
true.class<-c(1,1,1,1,1,1,2,2,2,2)
pred.class<-c(1,1,2,1,1,2,1,1,2,2)
# We use table to produce a cross classification table
# with truth in the rows, first entry in the command
# and prediction in the columns, second entry in the command

corr.class.boolean<-true.class==pred.class #this creates a Boolean vector
#which returns TRUE when the observation was correctly classified and
#FALSE when it is not
corr.class.boolean #we can see TRUE for all observations except 3, 6, 7 and 8

corr.class.rate<-sum(corr.class.boolean)/length(corr.class.boolean)
corr.class.rate

tab.pred=table(true.class,pred.class)
tab.pred
# To get a table of class specific rates, we divide each value by its row sum

tab.rate=prop.table(tab.pred,margin=1) #row percentages
tab.rate

# The class specific correct classification rates are on the diagonal
diag(tab.rate)

# The class specific misclassification rates are 1 minus the diagonal
1-diag(tab.rate)

#Comment: We see that this classification method does a better job classifying the first class than the
#second with a higher (lower) correct classification (misclassification) rate

#example_3
head(iris)
dim(iris)

#We want to split the 150 observations randomly into our 3 subsets: training, validation and test in the
#ration 50:25:25.
#We start by creating indices indicating which observations go into each subset and then pulling out the
#relevant rows for each data set.
n <- nrow(iris)
?round
ind1 <- sample(c(1:n),round(n/2))
ind2 <- sample(c(1:n)[-ind1],round(n/4))
ind3=setdiff(c(1:n),c(ind1,ind2))
train.data=iris[ind1,]
valid.data=iris[ind2,]
test.data=iris[ind3,]
dim(train.data)
dim(valid.data)
dim(test.data)

#If we wanted to run cross validation manually, we could use something like the following code. However, many popular
#methods have a cross-validation option built in.
#For 5-fold CV, create the indices of the observations going into each set
n <- nrow(iris)
ind1 <- sample(c(1:n),round(n/5))
ind2 <- sample(c(1:n)[-ind1],round(n/5))
ind3 <- sample(c(1:n)[-c(ind1, ind2)],round(n/5))
ind4 <- sample(c(1:n)[-c(ind1, ind2, ind3)],round(n/5))
ind5 <- setdiff(c(1:n),c(ind1,ind2,ind3,ind4))
ind <- list(ind1,ind2,ind3,ind4, ind5)
#Set up a vector of 5 numbers to store the classification rates for each set
#when it is used as a validation set
#Assume our data is in a matrix called data
#Our class variable is called y
#The generic method for fitting some model is called function.fit
#with the corresponding prediction function, function.pred
corr.class.rate<-numeric(5)
for (i in 1:5) {
  #For each run, we use one set for test/validation
  test.data <- data[ind[[i]],]
  test.label <- y[ind[[i]]]
  #The remaining sets are our training data
  train.ind <- setdiff(c(1:n),ind[[i]])
  train.data <- data[train.ind,]
  train.label <- y[train.ind]
  #Fit the model on the training data
  model.fit <- function.fit(train.data, train.label)
  #Predict using the test data
  pred.class <- function.pred(model.fit, test.data)
  #Calculate the test data correct classification rate
  9
  corr.class.rate[i] <- sum(test.label==pred.class)/length(test.label)
}
#average the 5 correct classification rates to find the overall rate
cv.corr.class.rate <- mean(corr.class.rate)

library(MASS)
head(train.data)
??knn
library(class)

#exmaple_5
#For our example data from the previous figure, we plot the correct classification rate for the validation
#data versus 𝑘 to choose the optimal 𝑘.

#create the data and the class
library(class)
n=100;n.val=10000

set.seed(1)
x=round(runif(n,1,n))
set.seed(2)
y=round(runif(n,1,n))
train.df=data.frame(x,y)

set.seed(3)
x.val=round(runif(n.val,1,n))
set.seed(4)
y.val=round(runif(n.val,1,n))
val.df=data.frame(x.val,y.val)

classes=ifelse(x^2+y^2>60^2,"blue","orange")
classes.val=ifelse(x.val^2+y.val^2>60^2,"blue","orange")
# Run kNN for different values of k from 1 to 25 and record the validation error rate
class.rate<-numeric(25)
for(k in 1:25) {
  pred.class <- knn(train.df, val.df, classes, k=k)
  class.rate[k] <- sum(pred.class==classes.val)/length(pred.class)
}
plot(c(1:25), class.rate, type="b",
     main="Correct classification rates on the validation data for a range of k",
     xlab="k",ylab="Correct Classification Rate",cex.main=0.7)

#We can read off the 𝑘 which gives the maximum value from the graph or use the which.max command
#(applied to the class.rate object) to find the optimal 𝑘.
#We can now draw the decision boundary for this optimal value of 𝑘.

grid=expand.grid(x=1:100,y=1:100)

k.opt=which.max(class.rate)
classes.grid=knn(train.df,grid,classes,k=k.opt,prob=T) #note last argument
prob.grid=attr(class.grid,"prob")
prob.grid=ifelse(classes.grid=="blue",prob.grid,1-prob.grid)
contour(x=1:100, y=1:100, z=matrix(prob.grid, nrow=100), levels=0.5,
        col="grey", drawlabels=FALSE, lwd=2) #plot the boundary
points(train.df, col=classes,pch=20) # add points from train data set
title(main=paste("k =",k.opt),cex.main=0.9,font.main=4)

setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/semester\ 2/mining")

admit=read.csv("admission.csv");names(admit)
table(admit$De)

library(MASS)
admit.lda=lda(De~.,data=admit)
admit.lda

admit.lda.pred=predict(admit.lda)
names(admit.lda.pred)
Acc=mean(admit.lda.pred$class==admit$De);Acc

ldahist(data=admit.lda.pred$x[,1],g=admit$De)

#Implementing linear discriminant analysis in R
install.packages("ISLR")
library(ISLR)

aggregate(Default$balance,by=list(Default$default),FUN=var) #calculate var by group

aggregate(Default$income,by=list(Default$default),FUN=var)

library(GGally)
ggpairs(Default,columns = 3:4,ggplot2::aes(col=default,alpha=0.2))

#diagonal: density plots
#top right: Pearson correlation between variables (overall and by group)
#bottom left: scatterplot

#create training and test data
n=nrow(Default)
set.seed(1)
ind=sample(c(1:n),0.7*n) #70%-30% training and test split
def.tr=Default[ind,-2]
def.te=Default[-ind,-2]

#apply LDA
library(MASS)
def.lda=lda(default~.,data=def.tr)
def.lda

plot(def.lda)

def.pred=predict(def.lda,def.te)
names(def.pred)

head(def.pred$class,3)

head(def.pred$posterior,3)

#Now we can compute some numerical summaries to assess the performance of LDA.
acc=mean(def.te$default==def.pred$class);1-acc

tab.pred=table(def.te$default,def.pred$class);tab.pred

tab.rate=prop.table(tab.pred,margin=1);tab.rate

#adjust the classification thresholds
def.pred.new=ifelse(def.pred$posterior[,2]>0.1,"Yes","No")
acc=mean(def.te$default==def.pred.new);1-acc

tab.rate=prop.table(tab.pred,margin=1);tab.rate

#Roc curve
install.packages("ROCR")
library(ROCR)
pred=prediction(def.pred$posterior[,2],def.te$default)
perf=performance(pred,"tpr","fpr")
plot(perf,main="ROC curve")

#To calculate AUC, we can directly read the value by changing the measure to auc
AUC=performance(pred,"auc")
AUC@y.values[[1]]

#task 4
set.seed(1)
data(iris)
n=nrow(iris)
ind1=sample(c(1:n),round(n/2))
ind2=sample(c(1:n)[-ind1],round(n/4))



