
Modelling the progression of world records in athletics - Events over middle distances

Population:
  • Female Dungeness crabs found in the Pacific Coast of North America

Primary Objective: 
  modeling the response variable time against date

Plan


1 read in data and look for potential outliers

2.look at scatter plots between covariates, look for linear relationships in covariates, and for presence of collinearity. Then only use one from set of colinear variables; use interaction.plot(), ggpairs() to check correlation between covariates and response


3 For each event separately, plot time against date, to check for shape of relationship

4 fit and assess a simle linear lm() to the world record times, to find whether there is a curvature

5 If the linear model is not appropriate, i.e. there is a curvature in the plots,
try fit linear models to various transformations of the record time - for example, the log world record time, the percentage of new to very first recorded world record time, new world record speed for this event 

6 try fit polynomial regression, use plot to see whether polynomial model fit the data well

7 use gam function in mgcv to fit and assess a generalized additive model (GAM) to the trend in world record times, in consideration of other variables, or use gamm function for correlated variables, check overfit of GAM model

8 Use the best-fitting model type to compare the patterns of progress in the three events for men and women separately. 

9 look into how other variables except date effect the model, leave out potential outliers

10 explore other potential research questions, and question of interest

