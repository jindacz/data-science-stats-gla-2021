setwd('/Users/kurisuuu/Documents/glasgow_stats_2021/regreesion/')
oxygen=read.csv("oxygen.csv")

#x <- oxygen$ # Define x
y <- oxygen$Oxygen # Define y
###2.1)Find betahat0 and betahat1
#sxx <- sum((x - mean(x))^2) # Compute sums of squares
#sxy <- sum((x - mean(x)) * (y - mean(y)))
#beta.1 <- sxy / sxx # Compute estimates
#beta.0 <- mean(y) - beta.1 * mean(x)
#c(beta.0,beta.1)
###2.2)Create a vector of predictions y.hat
#y.hat <- beta.0 + beta.1 * x
###2.3)Compute the estimated variance of the residuals
#sigma2 <- sum((y.hat-y)^2) / (length(y) - 2)
###2.4)Compute the coefficient of determination
#R2 <- 1 - sum((y.hat-y)^2) / sum((y-mean(y))^2)
###3.1)Create the design matrix
X <- cbind(1,oxygen$Age,oxygen$Weight,oxygen$RunPulse)
###3.2)Create XtX and Xty
XtX <- t(X)%*%X # Prepare calculation of beta
Xty <- t(X)%*%y 
###3.3)Solve the system of equations
beta <- solve(XtX,Xty) # Compute beta
beta
###3.4)Compute the fitted values y.hat
y.hat <- X%*%beta
y.hat
