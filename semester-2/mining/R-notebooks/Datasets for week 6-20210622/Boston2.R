# This code computes the linear regression model and neural network
# for Boston data set. It consists of two parts:
# 1. Linear regression with training-test split
# 2. Linear regression and neural nework on cross-validation

#----------------------
# 1. Linear regression
#----------------------

# Load and pre-processing data
library(MASS)
data(Boston)
Boston <- Boston[,c("medv","crim","lstat","rm","rad","chas")]
colnames(Boston) <- c("median.value","crime.rate","low.socio.status",
                      "aver.rooms","index.radial.highways","river.bounds")
maxs <- apply(Boston, 2, max) 
mins <- apply(Boston, 2, min)
scaled <- as.data.frame(scale(Boston, center=mins, scale=maxs - mins))
          # min-max normalisation

# Train-test random splitting
set.seed(84)
index <- sample(1:nrow(Boston),round(0.75*nrow(Boston)))
train_Boston<- scaled[ index,]
test_Boston <- scaled[-index,]

# Build a linear regression model
glm_boston <- glm(median.value~crime.rate+low.socio.status+aver.rooms,
                  data=train_Boston)
pr.lm <- predict(glm_boston,
                 test_Boston[,c("crime.rate","low.socio.status",
                                "aver.rooms")])
SSE.lm <- sum((pr.lm - test_Boston[, "median.value"])^2)/2
SSE.lm
#-----------------------comment----------------------#
# The test SSE from using a neural network is 0.423. #
# It seems that the neural network is doing better   #
# (at predicting median house values) compared to    #
# the simple glm alternative.                        #
#----------------------------------------------------#



#----------------------------------------------------------------
# 2. Linear regression and neural network using cross-validation
#----------------------------------------------------------------

# Linear regression
library(boot)
set.seed(84)
glm_boston <- glm(median.value~crime.rate+low.socio.status+aver.rooms,
                  data=train_Boston)
glm_MSE <- cv.glm(train_Boston,glm_boston, K=10)$delta[1]
glm_MSE
#-----------------------------------------------------------------#
# Note that this is an estimate for the MSE (mean squared error). #
# So we need to compute this quantity for neural network as well. #
# Unfortunately, there isn't a specific function that does that,  #
# so we have to create one ourselves.                             #
#-----------------------------------------------------------------#

# Neural network
library(neuralnet)
set.seed(84)
cv.error <- NULL
k <- 10

# Uncomment the following lines of R code (that include pbar)
# and the last line within the loop to have a progress bar
# library(plyr)
# pbar <- create_progress_bar("text")
# pbar$init(k)
# Create 10 equally size folds
folds <- cut(seq(1,nrow(Boston)),breaks=k,labels=FALSE)
for(i in 1:k){
  index <- which(folds==i,arr.ind=TRUE)
  train.cv <- scaled[-index,]
  test.cv  <- scaled[index,]
  neural_net_boston_cv <- neuralnet(median.value ~ crime.rate+low.socio.status
                                    +aver.rooms,data=train.cv,hidden=c(5),
                                    linear.output = TRUE)
  pr.nn <- neuralnet::compute(neural_net_boston_cv,
                              test.cv[, c("crime.rate","low.socio.status",
                                          "aver.rooms")])$net.result
  cv.error[i] <- sum((pr.nn-test.cv[, "median.value"])^2)/nrow(test.cv)
  # pbar$step()
}
mean(cv.error)
#-----------------------comment------------------------#
# The MSE for the neural network (0.012) is lower than #
# the one of the linear model (0.017).                 #
#------------------------------------------------------#


# Boxplot to visualise the MSEs of both models
boxplot(cv.error, xlab="MSE CV", horizontal=TRUE)
abline(v=glm_MSE, lty=3)
#------------------------------comment------------------------------#
# There seems to be a certain degree of variation in the MSEs of    #
# neural networks, which may be caused by the splitting of the data #
# or the random initialisation of the weights in neural networks.   #               #
# But overall we would say that it outperforms the glm approach,    #
# since the majority of the MSEs from the neural network are        #
# smaller compared to the one from the glm.                         #
#-------------------------------------------------------------------#
