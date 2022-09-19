#APM_week_1
library(tidyverse)
library(ggplot2)
bollywood <-
  read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/bollywood_boxoffice.csv"))
head(bollywood)

#We can plot the gross revenue against the budget to explore the relationship between the two variables.
b.plot <- ggplot(data = bollywood, aes(y = Gross, x = Budget)) +
  geom_point(col = "#66a61e") +
  scale_x_continuous("Budget (crore)") + scale_y_continuous("Gross (crore)")
b.plot

b.plot.l <- ggplot(data = bollywood, aes(y = log10(Gross), x = log10(Budget))) +
  geom_point(col = "#1b9e77") +
  scale_x_continuous("log(Budget) (crore)") +
  scale_y_continuous("log(Gross) (crore)")
b.plot.l

bol.lm=lm(log10(Gross)~log10(Budget),data=bollywood)
summary(bol.lm)

b.plot.lm <- ggplot(data = bollywood, aes(y = log10(Gross), x = log10(Budget))) +
  geom_point(col ="#1b9e77") +
  scale_x_continuous("log(Budget) (crore)") +
  scale_y_continuous("log(Gross) (crore)") +
  geom_smooth(method = lm, colour="#e7298a", se=FALSE)
#`geom_smooth()` using formula 'y ~ x'
b.plot.lm

#Task 1.
#Using the fitted model equation, predict the gross revenue for a film with a budget of (i) 10, (ii) 50, and
#(iii) 100 crore.
#Hint: Remember that the variables have been log-transformed.

#example 2
install.packages("Stat2Data")
library(Stat2Data)
data(MedGPA)

#Let us look at a plot of acceptance against GPA, adding a bit of jitter to make overlapping points more
#visible
medgpa.plot <- ggplot(data = MedGPA, aes(y = Acceptance, x = GPA)) +
  geom_jitter(width =0, height =0.01, alpha =0.5, colour ="#984ea3")

#We can add the linear regression line for Acceptance as a function of GPA to the plot.
medgpa.plot + geom_smooth(method = "lm", se = FALSE,
                          fullrange = TRUE, colour = "#984ea3")
#`geom_smooth()` using formula 'y ~ x'

med.lm=lm(Acceptance~GPA,data=MedGPA)
summary(med.lm)

med.glm=glm(Acceptance~GPA,data=MedGPA,family=binomial)
#That is, family= binomial implies family = binomial(link="logit").
summary(med.glm)

#Task 2.
#Predict the acceptance probability for an applicant with a GPA of (i) 2.5, (ii) 3 (iii) 4. First do this “by hand”
#using the regression equation, then in R using the predict() function.
#Hint: The predict() function will return values on the linear predictor scale unless you specify
#type='response' which returns probabilities instead.
predict(med.glm, data.frame(GPA = c(2.5, 3, 4)), type = 'response')

#example_7
summary(med.glm)

vcov(med.glm)

sqrt(diag(vcov(med.glm)))

#example 11
glm(formula=Acceptance~GPA,family=binomial,data=MedGPA)




