#apm_lab_1
## Load the package and data
library(faraway)

library(MASS)
data(orings)

## Fit a logistic model
logitmod <- glm(cbind(damage, 6-damage) ~ temp, family = binomial(), orings)
lsumm <- summary(logitmod)
lsumm

## Fit a probit model
probitmod <- glm(cbind(damage, 6-damage) ~ temp, family = binomial(link=probit), orings)
psumm <- summary(probitmod)
psumm

## Compute the 95% confidence interval for exp(beta_1) using the delta method
se.exp.beta1 <- exp(logitmod$coefficients[2])*lsumm$coefficients[2,2]
exp(logitmod$coefficients[2]) + c(-1, 1)*qnorm(0.975)*se.exp.beta1

#The results from the logistic regression model indicate that the log-odds of damage decreases by 0.22
#(95%CI=0.11,0.32) for each unit increase in the temperature. Equivalently, each unit increase in the
#temperature decreases the odds of damage by a factor of 0.81(95%CI=0.72,0.89). This is strongly significant
#based on both the Wald test (z = −4.07, p < .001) and the likelihood ratio test (G2 = 38.898−16.912 = 21.99
 
#2. You can see the z-values and tail probabilities from the summary of the logistic model. Explain how these
#values are obtained and what they are used for.

#3. Compute Wald and likelihood ratio confidence intervals for the model parameters
## Standard errors
se <- lsumm$coeff[, "Std. Error"]
## Compute the Wald confidence intervals (95% confidence)
logitmod$coeff + qnorm(0.975)*outer(se, c(-1,1))
?outer

confint(logitmod)

#4. Show how to use the likelihood ratio test for comparing the logistic model and the intercept-only model.
G=logitmod$null.deviance-logitmod$deviance
G
pchisq(G, 1, lower.tail = F)

#or using anova table
## Fit the intercept-only model
nullmod <- glm(cbind(damage, 6-damage) ~ 1, family=binomial(), data=orings)
## Compare the null model and the logistic model
anova(nullmod, logitmod, test="LRT")

#5. Use deviance to determine if the logistic model fits the data well
pchisq(logitmod$deviance, logitmod$df.residual, lower.tail=F)

#The deviance (16.91) of the logistic model follows an approximate χ
#The resulting p-value is 0.72, thus there is no evidence of lack of fit for the logistic model.

#6. Compare the logistic model and the probit model for this dataset.
## AIC values for the logistic model and the probit model
c(logitmod$aic, probitmod$aic)

#7. Add fitted lines to the original plot
## Explore raw data
plot(damage/6 ~ temp, orings, xlim=c(25,85), ylim=c(0, 1), xlab="Temperature",
     ylab="Prob of damage")
x <- seq(25, 85, 1)
## Logistic
lines(x, ilogit(logitmod$coefficients[1]+logitmod$coefficients[2]*x), lty=1)
## Probit
lines(x, pnorm(probitmod$coefficients[1]+probitmod$coefficients[2]*x), lty=2)
legend("topright", legend=c("Logit", "Probit"), lty=1:2, cex=0.8)

#8. Estimate the probability of damage at 31 °F.
## Logistic model
lpred <- predict(logitmod, newdata=data.frame(temp=31), se.fit=T, type="link")
ilogit(lpred$fit)

## 95% confidence interval
ilogit(lpred$fit + c(-1, 1) * qnorm(0.975) * lpred$se.fit)

## Probit model
ppred <- predict(probitmod, newdata=data.frame(temp=31), se.fit=T, type="link")
pnorm(ppred$fit)

## 95% confidence interval
pnorm(ppred$fit + c(-1, 1) * qnorm(0.975) * ppred$se.fit)

#The estimated probability of damage at 31 °F is 0.99 for both models with 95% confidence intervals being
#(0.84, 1.00) for the logistic model and (0.71, 1.00) for the probit model.









