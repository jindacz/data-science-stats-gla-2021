
######### example of Central england temperature, thanks to Howard Grubb


cet <- read.csv(file="cet2015.csv",header=T)

##### a utility function to produce confidence intervals
alpha <- 0.05

simple.ci <- function (this.mean, this.stderr, this.dof, alpha = 0.05)
{
	return(array(c(this.mean,
			   this.mean + c(-1,1) * qt(1-alpha/2, this.dof) * this.stderr,
			   this.stderr),
			dim=4, dimnames=list(c("mean","lower","upper","stderr"))) ) 
}


lm.par.ci <- function (model, alpha=0.05, par=2)
{
	return(simple.ci(model$coefficients[par],
				summary(model)$coefficients[par,2],
				model$df.residual,alpha))
}
################################################################

attach(cet)
cet

nyear <- nrow(cet)


par(mfrow=c(1,1))
plot(Year,AnnMean,type="l",ylab="Annual mean temperature 'C",main="Annual mean CET")

abline(h=mean(AnnMean))

#-----------------------------------------------------------------------------

#fitting some simple trends, a linear model and also a smooth curve using LOESS to annual data

#--------------------------------------------------------------------------------

smooth.mod <- loess(AnnMean~Year)
lines(Year,smooth.mod$fit,lwd=3)

ann.mod <- lm(AnnMean~Year)
lines(Year,ann.mod$fit,lwd=3)
summary(ann.mod)

# intercept is for year=0, not very sensible so we can change this, 
MYear <- Year - max(Year)

ann.mod <- lm(AnnMean ~ MYear)
summary(ann.mod)

#intercept is now an estimate of annual mean average temp at end of data (2015)


####check the assumptions

par(mfrow=c(2,2))
ann.mod.res <- resid(ann.mod)
plot(Year, ann.mod.res, ylab="residuals", type="l", main="Residuals from
	annual linear model")
abline(h=c(-1,1)*1.96*summary(ann.mod)$sigma)
sum(abs(ann.mod.res) > 1.96*summary(ann.mod)$sigma)/nyear

hist(ann.mod.res, xlab="Residuals", breaks=30, prob=T,
	main="Histogram of Residuals from annual linear model")
lines(density(ann.mod.res))

qqnorm(ann.mod.res, ylab="Residuals", main="Q-Q plot of 
	residuals from annual linear model")
abline(c(0,summary(ann.mod)$sigma))

ann.res.acf <- acf(ann.mod.res, main="Annual linear model residuals")
array(c(ann.res.acf$acf[2], 2/sqrt(nyear) ), dim=2, 
	dimnames=list(c("rho","2/sqrt(n)")))

#---------------------------------------------------
# ACF to check on correlations

#------------------------------------------------------


par(mfrow=c(2,2))
plot(ann.mod,smooths=T)

#------------------------------------------------------------------------------

# what happens if we have a shorter time series, so a different start year

startYear<-1878
ann.mod.short<-lm(AnnMean[Year>=startYear]~MYear[Year>=startYear])
par(mfrow=c(1,1))
plot(Year, AnnMean, type="l", ylab="Annual mean temp")
abline(h=mean(AnnMean))
lines(Year,ann.mod$fit,lwd=3)
lines(Year[Year>=startYear],ann.mod.short$fit,lwd=3,col=8)

# Estimated mean annual temperature 2015
# Confidence interval for expectation
lm.par.ci(ann.mod,alpha,par=1)
# Prediction interval for 2015 value
ann.pred<-predict.lm(ann.mod,int="p")
last.ann.pred<-c(ann.pred[nyear,1],ann.pred[nyear,2],
 ann.pred[nyear,3])
names(last.ann.pred)<-c("mean","lower","upper")
last.ann.pred

# find estimated change in deg C per century
ann.trend<-100*lm.par.ci(ann.mod,alpha)
ann.trend
#


###################################################################
# look at the monthly trends

monthord <- 1:12
par(mfrow=c(1,1))
boxplot(cet[,monthord+1],names=month.abb[monthord],ylab="Average temperature 'C", main="Average monthly temperatures")



month.ranges <- apply(cet, 2, "range")[,2:13]
month.range.max <- max(month.ranges[2,] - month.ranges[1,])

each.month.trend <- array(0, c(12,4),dimnames=list(month.abb,
						c("trend","lower","upper","stderr")))
each.month.mean <- array(0, c(12,4),dimnames=list(month.abb,
						c("mean 2003","lower","upper","stderr")))
each.month.res <- array(0, c(nyear,12), dimnames=list(NULL,month.abb))
each.month.sigma <- array(0, 12, dimnames=list(month.abb))


par(mfrow=c(3,4))
for (i in monthord)
{
	plot(Year, cet[,i+1], type="l", ylab="Average temperature 'C",
		ylim=mean(cet[,i+1])+c(-1,1)*month.range.max/2,
		main=month.name[i])
	
	month.mod <- lm(cet[,i+1]~MYear)

	lines(Year, month.mod$fit, lwd=3, col=8)

	each.month.mean[i,] <- lm.par.ci(month.mod, alpha, par=1)
	each.month.trend[i,] <- 100 * lm.par.ci(month.mod, alpha)
	each.month.sigma[i] <- summary(month.mod)$sigma
	each.month.res[,i] <- resid(month.mod)
}


each.month.trend[monthord,]


par(mfrow=c(1,1))
matplot(c(1:12), each.month.trend[monthord,1:3], axes=T, type="l",
	lty=c(1,4,4), col=rep(1,3), ylab="Temperature trend 'C/Cent",
	xlab="Months", main="Monthly Trends")

month.axis <- function()
{
	box()
	axis(2)
	axis(1, at=c(1:12), labels=month.abb[monthord])
}

for (i in c(1:12))
{
lines(c(i,i), each.month.trend[monthord[i],c(2:3)], lty=2)
}
abline(h=0,col=6)
abline(h=ann.trend[1], col=8)
abline(h=ann.trend[2:3], lty=4, col=8)