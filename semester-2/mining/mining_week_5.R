#mining W5
set.seed(147)
library(MASS)
library(e1071)
data(crabs)
dim(crabs)

#Split the data 50% training, 25% validation and 25% test
n<-nrow(crabs)
intrain<-sample(c(1:n),round(n/2))
invalid<-sample((c(1:n)[-intrain]),round(n/4))
train.data<-crabs[intrain,-c(2:3)]
valid.data<-crabs[invalid,-c(2:3)]
test.data<-crabs[-c(intrain,invalid),-c(2:3)]

#Fit the classification SVM for different values of C
#and calculate the validation prediction error

pred.error<-function(pred,truth){
  mean(pred!=truth)
}
C.val <- c(0.1,0.5,1,2,5,10)
C.error <- numeric(length(C.val))
for (i in 1:length(C.val)) {
  model <- svm(sp~.,data=train.data,type="C-classification",kernel="linear",
               cost=C.val[i]) #kernel will be explained in the next section
  pred.model <- predict(model, valid.data)
  C.error[i] <- pred.error(pred.model, valid.data$sp)
}
C.sel <- C.val[min(which.min(C.error))]
C.sel

plot(C.val,C.error,type="b")
abline(v=C.sel,lty=2)

#We can also use the tune.svm command to do something similar
#using cross validation on the training data.
tune.svm(sp~.,data=train.data,type="C-classification",kernel="linear",
         cost=C.val)

final.svm<-svm(sp~.,data=train.data,kernel="linear",cost=C.sel,type="C-classification")
summary(final.svm)

pred.test<-predict(final.svm,test.data)
pred.error(pred.test,test.data$sp)

table(test.data$sp,pred.test)

final.svm$index

