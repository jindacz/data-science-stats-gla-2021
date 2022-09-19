#######################################################################
#
# Script for analysis of haddock stocks data, . 
# Start by reading in the data.



#
#######################################################################
haddock.all <- read.table("haddock.dat",header=TRUE)
haddock.data <- haddock.all[haddock.all$Year <= 2000,]
#######################################################################
#
# 2. Exploratory analysis
#
#######################################################################
par(lwd=2)
plot(haddock.data$Year,haddock.data$Biomass,type="l",
     xlab="Year",ylab="Thousands of tonnes")

par(mfrow=c(1,2),mar=c(4,4,3,2))
plot(haddock.data$Year,log10(haddock.data$Biomass),type="l",
     xlab="Year",ylab=expression(paste(log[10],"(1000 tonnes)")),
     ylim=c(1,3))
plot(haddock.data$Year,haddock.data$Biomass,type="l",
     xlab="Year",ylab="Thousands of tonnes",log="y",ylim=c(10,1000))
#######################################################################
#
# 3. Linear trend model
#
#######################################################################
nyears <- nrow(haddock.data)
haddock.data <- cbind(haddock.data,1:nyears,log10(haddock.data$Biomass))
names(haddock.data)[3:4] <- c("Time","logBiomass")
trend.model0 <- lm(logBiomass ~ Time, data=haddock.data)
cat("NAIVE LINEAR TREND MODEL:\n")
cat("=========================")
print(summary(trend.model0))

pred.t <- data.frame(Time=39:48)	
haddock.pred0 <- predict(trend.model0,newdata=pred.t,
	                 se.fit=TRUE,interval="prediction")

naive.pred <- (haddock.pred0$fit)[,1]
naive.ll95 <- (haddock.pred0$fit)[,2]     # Lower & upper ends of
naive.ul95 <- (haddock.pred0$fit)[,3]     # 95% prediction intervals
pred.years <- 1962+pred.t$Time            # This will allow a plot
                                          # labelled with "Year"

plotpred <- function(ylim) {		
#
# 	The ylim argument controls the y axis scaling, to ensure that
# 	the prediction intervals will fit on the plot
#
 par(mfrow=c(1,1))
 attach(haddock.data)
 plot(Year[Year >= 1980],logBiomass[Year >= 1980],type="l",
     xlim=c(1980,max(pred.years)),ylim=ylim,
     xlab="Year",ylab=expression(paste(log[10],"(1000 tonnes)")))
 detach(haddock.data)
 lines(pred.years,naive.pred,lty=2,col="red")
 lines(pred.years,naive.ll95,lty=3,col="red")
 lines(pred.years,naive.ul95,lty=3,col="red")
}

attach(haddock.data)
ylim0 <- range(c(naive.ll95,naive.ul95,logBiomass[Year >= 1980]))
detach(haddock.data)
plotpred(ylim0)

par(mfrow=c(2,2))
plot(trend.model0,which=1:4)

e0 <- resid(trend.model0)
par(mfrow=c(2,1))
plot(haddock.data$Year,e0,type="l",xlab="Year",ylab="Residual",
     main="Residual time series")
abline(0,0,lty=2)
acf(e0,main="Residual ACF")
pacf(e0,main="Residual PACF")



#######################################################################
#
# 4  Time series regression - first an AR(1)
#
#######################################################################
X <- model.matrix(trend.model0)
trend.model1 <- arima(haddock.data$logBiomass,order=c(1,0,0),
                      xreg=X,include.mean=FALSE)
cat("\nLINEAR TREND WITH AR(1) ERRORS:\n")
cat("===============================")
print(trend.model1)

n.pred <- length(pred.years)
pred.X <- cbind(rep(1,n.pred),pred.t)
haddock.pred1 <- predict(trend.model1,n.ahead=n.pred,newxreg=pred.X)
ar1.pred <- haddock.pred1$pred
ar1.ll95 <- ar1.pred - (1.96*haddock.pred1$se)
ar1.ul95 <- ar1.pred + (1.96*haddock.pred1$se)
ylim1 <- range(c(ylim0,ar1.ll95,ar1.ul95))
plotpred(ylim1)
lines(pred.years,ar1.pred,lty=2,col="blue")
lines(pred.years,ar1.ll95,lty=3,col="blue")
lines(pred.years,ar1.ul95,lty=3,col="blue")

tsdiag(trend.model1)
e1<-resid(trend.model1)
par(mfrow=c(2,1))
acf(e1,main="Residual ACF for AR(1) model")
pacf(e1,main="Residual PACF for AR(1) model")

#


