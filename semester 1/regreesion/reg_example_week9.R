#reg_example_17
data(swiss)
swiss.lm=lm(Fertility ~ . , data=swiss)
summary(swiss.lm)

#Perform an analysis to find the significant predictors of Fertility.
#income is skewed
#positive association
#educaton and afqt postive income




swiss.md1<-ols_step_both_aic(swiss.lm,details=TRUE)

summary(swiss.md1)
swiss.md1[c("predictors","method","aic","rss","arsq","steps")]

swiss.lm2=lm(Fertility ~Education+Catholic+Infant.Mortality +
              Agriculture, data=swiss)
#"Education"        "Catholic"         "Infant.Mortality" "Agriculture" 
summary(swiss.lm2)



#reg_example_18
library(modelr)
data(heights)
#We begin by plotting and summarising these data. Describe relationships between income
#and the other variables.

#2. Suppose we opt for all subset regression. How many possible models would we need to fit?
#2^8 

#3. Below is output from the ‘best’ models with 1, 2, 3, 4, 5, 6 and 7 variables
library(olsrr)
model <- lm(income ~ ., data = heights)
model.selection <- ols_step_best_subset(model)

#a) Based on AIC, which model would you choose?
#model6

#b) Based on Cp, which model would you choose?
#m5,6,7

#c) Based on BIC, which model would you choose?
#m5

#d) Based on R2, which model would you choose?
#m7

#e) Based on R2 adj, which model would you choose?
#m3

#4. Overall, which model would you choose?
#Overall, I would choose model 5. This minimises BIC and is the model with the lowest number
#of explanatory variables with Cp≤p. Also, the difference in AIC between model 6 and model 5 is
#extremely small. We should weigh up the advantages of including an additional variables, in this
#case age, with the benefit to AIC. I would not recommend using R2 as a selection criterion.


#5. Now suppose we opt to use stepwise selection using AIC as a selection criterion. The output
#is given below.
#Stepwise selection using AIC
FB_selection<-ols_step_both_aic(model)
FB_selection
#y=a0+b1*x1+b2*x2+b3*x3+b4*x4+b5*x5+b6*x6

#6. Below in output that shows this fitted model.
model<-lm(income~afqt+weight+education+sex+marital+age,data=heights)
print(summary(model)$coefficients,3)
#Provide an interpretation of this output
#???????



#7. Compare your model from part 4 and part 6. Provide an explanation as to why the two models do
#not necessarily have to be the same.
#because they have different criteria


#8. Below you will find residuals plots from the model obtained in part 6. Describe the validity of the
#model assumptions.
library(ggfortify)
autoplot(model)

#9. Write down the next steps you would take in this analyses.
#???????




