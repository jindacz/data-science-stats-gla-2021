## Load the package and data
library(faraway)
library(MASS)
data(orings)

## Explore raw data
plot(damage/6 ~ temp, orings, xlim=c(25,85), ylim=c(0, 1), xlab="Temperature", ylab="Prob of damage")
## Fit a linear model
lmod <- lm(damage/6 ~ temp, orings)
## Add the fitted line
abline(lmod)

## Fit a logistic model
logitmod <- glm(cbind(damage, 6-damage) ~ temp, family = binomial(), orings)
summary(logitmod)

## Fit a probit model
probitmod <- glm(cbind(damage, 6-damage) ~ temp, family = binomial(link=probit), orings)
summary(probitmod)
