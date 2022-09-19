#Introduction
#介绍
#在第 7 周，我们开始考虑为表 1 中列出的总体参数（复制如下）构建和使用置信区间 (CI)。特别是，我们使用 bootstrap 方法来估计场景 1-4 中估计值的抽样分布，并使用这些来构建相应总体参数的 CI。

本周我们将在场景 5 和 6 中继续这个过程，即为简单和多元线性回归模型中的参数构建 CI。我们将从 bootstrap 方法开始，并在标准假设成立时基于理论结果考虑 CI。我们还将通过考虑基于模型比较的客观度量的模型选择策略来考虑如何使用 CI 进行变量选择和完成。

现在您已经熟悉 RMarkdown，我们鼓励您将本教程中的工作整理到 RMarkdown 文件中。出于这个原因，这些教程不再包含“代码块”，您可以在其中运行自己的 R 代码。改用.Rmd文件。

创建一个.Rmd文件以将以下包加载到 R 中：

library(dplyr)
library(ggplot2)
library(janitor)
library(moderndive)
library(infer)
library(broom)

坡度的 Bootstrap 置信区间 β 在简单线性回归 (SLR) 中
正如我们在第 7 周对表 1 中的场景 1-4 所做的那样，我们可以使用该infer包从数据集中重复采样，以估计截距估计值的抽样分布和标准误差(α^) 和协变量的参数 (β^) 在简单线性回归模型中 是一世= α + βX一世. 这些抽样分布使我们能够直接找到模型参数的 bootstrap 置信区间。通常，兴趣在于β 这将是我们在这里的重点。

为了说明这一点，让我们回到上周分析evals的moderndive包中的教学评价数据，以SLR模型age作为单一解释变量，教师评价scores作为响应变量。此处显示了此数据和拟合模型。

slr.model=lm(score~age,data=evals)
coeff=slr.model%>%
  coef()

#这里斜率参数的点估计是 β^=-0.006。下面的代码估计采样分布β^ 通过引导方法。

bootstrap_beta_distn=evals%>%
  specify(score~age)%>%
  generate(reps=1000,type="bootstrap")%>%
  calculate(stat="slope")
bootstrap_beta_distn%>%
  visualize()

#现在我们可以使用该get_ci函数来计算 95% 的置信区间。我们可以通过两种不同的方式做到这一点。请记住，这些表示score在 上回归教学的未知真实总体斜率参数的合理值范围age。

percentile_beta_ci <- bootstrap_beta_distn %>% 
  get_ci(level = 0.95, type = "percentile")
percentile_beta_ci

se_beta_ci <- bootstrap_beta_distn %>% 
  get_ci(level = 0.95, type = "se", point_estimate = coeff[2])
se_beta_ci

#使用模拟引导抽样分布的 2.5% 和 97.5% 百分位数，95% 置信区间为 (-0.011,-0.001)，95% 置信区间使用抽样分布的标准偏差（即估计的标准误差为 β^) 是 (-0.011,-0.001)。由于 bootstrap 分布接近对称，因此两个结果置信区间相似是有道理的。

#多重回归中参数的置信区间
#让我们继续使用我们在第 7 周中第一次看到的一个数值和一个分类解释变量来拟合多元回归模型，继续使用教学评估数据。在这个模型中：

#是: 教师评价的响应变量 score
#解释变量
#X1: 数值解释变量 age
#X2: 的分类解释变量 gender

evals_multiple <- evals %>%
  select(score, gender, age)

#首先，回想一下，我们有两个相互竞争的潜在模型来解释教授的教学评估分数：

#模型 1：平行线模型（无交互项）——男性和女性教授在描述年龄对教学分数的相关影响时具有相同的斜率
#模型 2：交互模型——允许男性和女性教授有不同的斜率来描述年龄对教学分数的相关影响

#复习：可视化
#回想一下我们为这两个模型绘制的图：

#复习：回归表

#让我们还回顾一下回归模型。一、无交互作用的回归模型：注意+公式中的使用。
par.model=lm(score~age+gender,data=evals_multiple)
get_regression_table(par.model)%>%
  knitr::kable(
    digits = 3,
    caption = "Model 1: Regression model with no interaction effect included.", 
    booktabs = TRUE
  )

#二、具有交互作用的回归模型：注意*公式中的使用。

int.model <- lm(score ~ age * gender, data = evals_multiple)

get_regression_table(int.model) %>% 
  knitr::kable(
    digits = 3,
    caption = "Model 2: Regression model with interaction effect included.", 
    booktabs = TRUE
  )

#请注意，除了估计参数值外，这些表还包括有关模型中每个估计参数的其他信息，即：

#std_error：每个参数估计的标准误差；
#statistic：用于检验总体参数为零的零假设的检验统计值；
#p_value：磷在原假设下与检验统计量相关的值；和
#lower_ci和upper_ci：总体参数的 95% 置信区间的下限和上限
#这些值是根据您在第一学期的回归建模中看到的标准假设，使用理论结果计算得出的。这些值不是基于引导技术，因为当使用多个变量时，这些值变得更加难以实现，并且超出了本课程的范围。

#使用置信区间进行推理
#已经描述了计算模型参数置信区间的几种方法，我们现在可以出于统计推断的目的来解释它们。

#简单线性回归： 是^一世= α + βX一世

#我们是否获得了置信区间 β在通过引导或基于假设的理论结果的简单线性回归模型中，区间的解释是相同的。正如我们在第 7 周看到的，

#置信区间给出了总体参数的合理值范围。

#因此，我们可以使用置信区间 β说出一个合理的范围值，而且同样有效，什么样的价值观是不是合理。比较置信区间的最常见值β with 是 0（零），因为 β= 0说，有没有响应变量之间（（线性）关系是) 和解释变量 (X）。因此，如果 0 位于置信区间内β 那么没有足够的证据表明两者之间存在线性关系 是 和 X. 但是，如果 0不在置信区间内，那么我们得出结论β 显着不同于零，因此有证据表明两者之间存在线性关系 是 和 X.
                                                                                                          
get_regression_table(slr.model) %>% 
  knitr::kable(
    digits = 3,
    caption = "Estimates from the SLR model of `score` on `age`.", 
    booktabs = TRUE
  )                                                                                                          让我们使用基于理论结果的置信区间，将 SLR 模型中的斜率参数应用于教师评价分数，age作为单一解释变量，教师评价scores 作为结果变量。

#多重回归

#再次考虑score使用age和gender作为两个解释变量的拟合交互模型。

int.model <- lm(score ~ age * gender, data = evals_multiple)
get_regression_table(int.model)

#使用置信区间的变量选择
#当模型中有多个解释变量时，与每个解释变量相关的参数被解释为基于相应解释变量 1 单位变化而保持所有其他变量不变的平均响应的变化。因此，在解释每个参数的置信区间时必须小心，承认每个参数都是以模型中所有其他解释变量为条件的合理值。

#由于参数估计值与模型中包含的变量之间存在相互依赖性，因此选择将哪些变量包含在模型中是一项相当复杂的任务。我们将在我们有 2 个潜在解释变量（X1 和 X2) 并使用置信区间来决定哪些变量将有助于预测响应变量 (是）。
                                                                                                         
#在这种结构中，我们可能会采用自上而下的方法：

#拟合最通用的模型，即 是一世= α +β1X1我+β2X2我 因为我们相信这可能会很好地描述数据
#构建置信区间 β1 和 β2
#如果两个区间都排除 0，则保留带有两个区间的模型 X1 和 X2.
#如果区间为 β1 包含 0 但对于 β2 不，适合模型 X2 独自的。
#如果区间为 β2 包含 0 但对于 β1 不，适合模型 X1 独自的。
#如果两个区间都包含 0，那么具有一个变量的模型可能仍然有用。在这种情况下，应该拟合具有单个变量的两个模型，并且间隔为β1 和 β2 构造并与 0 进行比较。
#如果我们只有几个解释变量，那么上述策略的扩展将是有效的，即从完整模型开始并通过删除项进行简化，直到无法删除更多项为止。当解释变量的数量很大时，问题变得更加困难。我们将在下一节中考虑这种更具挑战性的情况。

#回想一下，以及age和gender，也有一个潜在的解释变量bty_avg的evals数据，从六个学生的1和10之间的分数一个面板的平均得分美容的即数值变量我们可以将两个适合的多重回归模型连续解释性变量age和bty_avg，如下所示：                                                                                                       

mlr.model <- lm(score ~ age + bty_avg, data = evals)

#使用客观标准进行模型比较
#如上一节所述，当潜在解释变量的数量很大时，选择将哪些变量包括在最终模型中的问题变得更加困难。最终回归模型的选择总是涉及妥协：

#预测准确性（通过包含更多预测变量/解释变量来提高）
#可解释性（通过减少预测变量/解释变量来实现）
#有许多客观标准可用于比较应用于同一数据集的不同模型。所有这些都权衡了上述两个目标，即适应数据与复杂性。常见的例子包括：

#为了说明这一点，让我们回归到evals数据和MLR对教学评价分数score与两个连续的解释变量age，并bty_avg和比较这与刚刚单反机型bty_avg。要访问这些用于模型比较的度量，我们可以使用包中的glance函数broom（不要与包中的glimpse函数混淆dplyr）。

model.comp.values.slr.age <- glance(lm(score ~ age, data = evals))
model.comp.values.slr.age

model.comp.values.slr.bty_avg <- glance(lm(score ~ bty_avg, data = evals))
model.comp.values.slr.bty_avg

model.comp.values.mlr <- glance(lm(score ~ age + bty_avg, data = evals))

Models <- c('SLR(age)','SLR(bty_avg)','MLR') 
bind_rows(model.comp.values.slr.age, model.comp.values.slr.bty_avg, 
          model.comp.values.mlr, .id = "Model") %>%
  select(Model, adj.r.squared, AIC, BIC) %>%
  mutate(Model = Models) %>%  
  knitr::kable(
    digits = 2,
    caption = "Model comparison values for different models.", 
  )

#关于模型选择的最后一句话
#在为模型选择预测变量/解释变量时应非常小心，因为回归系数的值取决于模型中包含的变量。因此，包含的预测变量以及它们进入模型的顺序会产生很大的影响。在理想情况下，应根据过去的研究选择预测变量，并根据变量的理论重要性将新的预测变量添加到现有模型中。不要做的一件事是选择数百个随机预测变量，将它们全部放入回归分析中，并希望得到最好的结果。

#但在实践中，有一些自动策略，例如Stepwise（参见第 6 周关于使用AIC 的逐步回归）和最佳子集回归，它们基于系统地搜索当前模型中不存在的整个变量列表来决定是否应该包括每个变量. 这些策略需要小心处理，对它们的适当讨论超出了本课程的范围。我们最好的策略是综合判断哪些变量应该作为潜在的解释变量，以及用于评估这些变量的区间估计和假设检验策略。应根据问题上下文的建议做出判断。

#建模的黄金法则

#建模数据的关键是仅使用客观度量作为粗略的指导。最终模型的选择将涉及您自己的判断。您必须能够为选择特定模型的原因辩护。


#进一步的任务
#鼓励您通过使用 RMarkdown 生成一个总结您所有工作的单个文档来完成以下任务，即原始问题、您的 R 代码、您的评论和反思等。

#收集了 2010 年美国洛杉矶 (LA) 市房屋特征的数据，可LAhomes.csv在 Moodle 页面的文件中找到。数据包含以下变量：
#city - 房子所在的洛杉矶地区
#type- SFR（单户住宅）或Condo/Twh（公寓/联排别墅）
#bed - 卧室数量
#bath - 浴室的数量
#garage - 车库中的车位数量
#sqft - 房屋的建筑面积（​​平方英尺）
#pool-Y如果房子有游泳池
#spa-TRUE如果房子有水疗中心
#price - 最近的销售价格（美元）
#我们有兴趣探索price与其他变量之间的关系。

#将数据读入一个名为的对象LAhomes并回答以下问题。

#通过查看下面price和sqft变量的单变量和双变量分布，如果我们想对这些数据进行建模，什么是明智的处理方式？如果您要这样做，必须注意什么？

setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/semester\ 2/das/Data\ for\ Further\ Tasks\ Week\ 8-20210628")
LAhomes=read.csv("LAhomes.csv")

library(gridExtra) # Package to display plots side by side 

hist1 <- ggplot(LAhomes, aes(x = price)) +
  geom_histogram()

hist2 <- ggplot(LAhomes, aes(x = sqft)) +
  geom_histogram()

hist1log <- ggplot(LAhomes, aes(x = log(price))) +
  geom_histogram()

hist2log <- ggplot(LAhomes, aes(x = log(sqft))) +
  geom_histogram()

plot1 <- ggplot(LAhomes, aes(x = sqft, y = price)) +
  geom_point()

plot2 <- ggplot(LAhomes, aes(x = log(sqft), y = log(price))) +
  geom_point()

library(gridExtra)
grid.arrange(hist1, hist2, hist1log, hist2log, plot1, plot2,
             ncol = 2, nrow = 3)
??grid.arrange

#Solution Given the highly skewed nature of both price and sqft (as seen in both their histograms and their
#                                                                scatterplot) the use of the log tranformation significantly reduces the skewness in the data and makes the
#scatterplot look more linear. Hence we should model the tranformed variables log(price) and log(sqft)
#instead of the original variables.
#If these transformations are employed, special care must be taken when interpretting the meaning of estimated model parameters since the data are no longer on the original units and the linear models of the
#logged variables will have a multiplacitive effect, rather than an additive effect, in the orgiginal units.

#b. Fit the simple linear model with log(price) as the response and log(sqft) as the predictor.
#Display the fitted model on a scatterplot of the data and construct a bootstrap confidence
#interval (using the percentiles of the bootstrap distribution) for the slope parameter in the
#model and interpret its point and interval estimates.
LAhomes=mutate(LAhomes,log.price=log(price),log.sqft=log(sqft))

slr.model1 <- lm(log(price) ~ log(sqft), data = LAhomes)
ggplot(LAhomes,aes(x=log(sqft),y=log(price)))+
  geom_point()+
  geom_smooth(method=lm, se = FALSE)

coeff <- slr.model1 %>% coef()
percentile_beta_ci <- LAhomes %>%
  specify(log.price ~ log.sqft) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "slope") %>%
  get_ci(level = 0.95, type = "percentile")

#The above plot shows the fitted line log(price)=2.7 + 1.44 log(sqft). The point estimate of the slope
#parameter estimates that for every unit increase in log(sqft) the average log(price) of houses will increase
#by 1.44 US dollars. Another way of saying this is that each additional 1% of square footage produces an
#estimate of the average price which is 1.44% higher (i.e. there is a multiplicative effect in the original units).
#Note: you must be careful to avoid causative interpretations. Additional square footage does not necessarily
#cause the price of a specific house to go up.
#Furthermore, based on the bootstrap confidence interval, we are 95% confident that the interval from 1.4 up
#to 1.48 contains the true rate of increase in the average of the logged prices as log(sqft) increase. Because
#this interval does not contain zero we conclude that the relationship between log(price) and log(sqft) is
#statistically significant.

#c. Repeat the analysis in part b. but with the log of the number of bathrooms (bath) as the
#single explanatory variable.
LAhomes <- mutate(LAhomes,log.bath=log(bath))
slr.model2 <- lm(log(price) ~ log(bath), data = LAhomes)
ggplot(LAhomes,aes(x=log(bath),y=log(price)))+
  geom_point()+
  geom_smooth(method=lm, se = FALSE)

coeff2 <- slr.model2 %>% coef()
percentile_beta_ci2 <- LAhomes %>%
  specify(log.price ~ log.bath) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "slope") %>%
  get_ci(level = 0.95, type = "percentile")

#The above plot shows the fitted line log(price)=12.23 + 1.43 log(bath). The point estimate of the slope
#parameter estimates that for every unit increase in the log of the number of bathrooms in a house the average

#log of the prices of houses will increase by 1.43 US dollars. Another way of saying this is that each doubling
#of bathroom produces an estimate of the average price which is 143% higher (i.e. there is a multiplicative
                                                                            effect in the original units).
#Furthermore, based on the bootstrap confidence interval, we are 95% confident that the interval from 1.36
#up to 1.49 contains the true rate of increase in the average of the logged prices as the log of the number of
#bathrooms increases. Because this interval does not contain zero we conclude that the relationship between
#log(price) and log(bath) is statistically significant.

#d. Fit the multiple linear regression model using the log transform of all the variables price
#(as the response) and both sqft and bath (as the explanatory variables). Calculate the point
#and interval estimates of the coefficients of the two predictors separately. Compare their point
#and interval estimates to those you calculated in parts b. and c. Can you account for the
#differences?

mlr.model <- lm(log(price) ~ log(sqft)+log(bath), data = LAhomes)
coeff3 <- mlr.model %>% coef()
coeff3
mlr.model.est <- get_regression_table(mlr.model)
str(mlr.model.est)

percentile_beta1_ci <- c(mlr.model.est$lower_ci[2],mlr.model.est$upper_ci[2])
percentile_beta2_ci <- c(mlr.model.est$lower_ci[3],mlr.model.est$upper_ci[3])
percentile_beta1_ci
percentile_beta2_ci

###
The fitted model is log(price)=2.51 + 1.47 log(sqft) - 0.04 log(bath). The first thing we notice is that
the parameter associated with (the log of) the number of bathrooms has changed from 1.43 to -0.04. That it,
its gone from having a positive relationship with the (log of) house prices in the single explanatory variable
model to having a negative relationship when the size of the house was also included in the model. One
reason for the switch in sign of the parameter estimate could be that for a house with a given size in (log)
square feet, more (log) bathrooms means that less of the (log) square footage is used for bedrooms and other
desirable space, thus reflecting a lower average home price. This illustrates the importance of taking all
other variables in the model into account and holding them constant when we interpret individual
parameter estimates in a multiple linear regression model. (See the “Formal Analysis” section here from
                                                            Week 6’s lab).
Turning to the confidence intervals, the model still predicts that the (log) square footage of the home
significantly positively affects the average (log) price, since the 95% confidence interval for the log(sqft)
parameter is from from 1.39 up to 1.55 which doesn’t contain zero. However, now the number of bathrooms
no longer significantly affects the price since the 95% confidence interval for the log(bath) parameter is
from from -0.13 up to 0.05 which does contain zero. This suggests that we drop the log(bath) term from
the model and return to the simple linear regression model we used in part b.
###

#e. Using the objective measures for model comparisons, which of the models in parts b., c. and
#d. would you favour? Is this consistent with your conclusions in part d.?
model.comp.values.slr.model1 <- glance(slr.model1)
model.comp.values.slr.model2 <- glance(slr.model2)
model.comp.values.mlr.model <- glance(mlr.model)
Models <- c("SLR(log(sqft))","SLR(log(bath))","MLR")
bind_rows(model.comp.values.slr.model1, model.comp.values.slr.model2,
          model.comp.values.mlr.model,.id="Model") %>%
  select(Model,adj.r.squared,AIC,BIC) %>%
  mutate(Model=Models) %>%
  knitr::kable(
    digits = 2,
    caption = "Model comparison values for different models",
  )

#Question 2
You have been asked to determine the pricing of a New York City (NYC) Italian restaurant’s dinner menu
such that it is competitively positioned with other high-end Italian restaurants by analyzing pricing data
that have been collected in order to produce a regression model to predict the price of dinner.
Data from surveys of customers of 168 Italian restaurants in the target area are available. The data can be
found in the file restNYC.csv on the Moodle page. Each row represents one customer survey from Italian
restaurants in NYC and includes the key variables:
  • Price - price (in $US) of dinner (including a tip and one drink)
• Food - customer rating of the food (from 1 to 30)
• Decor - customer rating fo the decor (from 1 to 30)
• Service - customer rating of the service (from 1 to 30)
• East - dummy variable with the value 1 if the restaurant is east of Fifth Avenue, 0 otherwise

#a. Use the ggpairs function in the GGally package (see the following code) to generate an
#informative set of graphical and numberical summaries which illuminate the relationships
#bewteen pairs of variables. Where do you see the strongest evidence of relationships between
#price and the potential explanatory variables? Is there evidence of multicollineatity in the
#data?

restNYC=read.csv("restNYC.csv")
library(GGally)
#glimpse(restNYC)
restNYC$East <- as.factor(restNYC$East) # East needs to be a factor
ggpairs(restNYC[,4:8], aes(colour = East, alpha = 0.4)) # Including the `East` factor
ggpairs(restNYC[,4:7], aes(alpha = 0.4)) # Without the `East` factor
  
#price shows a moderate to strong correlation with ’Food,ServiceandDecor‘.
#• The correlation between Service and Food (0.795) is the strongest evidence of multicollinearity in the
#data, followed by the correlation between Service and Decor (0.645)

#b. Fit the simple linear model with Price as the response and Service as the predictor and
#display the fitted model on a scatterplot of the data. Construct a bootstrap confidence interval
#(using the standard error from the bootstrap distribution) for the slope parameter in the
#model.

slr.Service <- lm(Price ~ Service, data=restNYC)
ggplot(restNYC,aes(x=Service,y=Price))+
  geom_point()+
  geom_smooth(method=lm, se = FALSE)

coeff.Service <- slr.Service %>% coef()
percentile_beta_Service_ci <- restNYC %>%
  specify(Price ~ Service) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "slope") %>%
  get_ci(level = 0.95, type = "se", point_estimate=coeff.Service[2])
mlr.Service.Food.Decor <- lm(Price ~ Service + Food + Decor, data=restNYC)
get_regression_table(mlr.Service.Food.Decor) %>%
  knitr::kable(
    digits = 2,
    caption = "Parameter estimates for MLR model of Price on Service, Food, and Decor",
  )

#When only Service is included in the model, it appears significant (confidence interval using the standard
#                                                                    error from the bootstrap: 2.24 up to 3.4, which doesn’t contain zero). However, once Food and Decor are
#added into the model, that is no longer the case as can be seen in the confidence interval table above, which
#does contain zero.

#c. What is the correct interpretation of the coefficient on Service in the linear model which
#regresses Price on Service, Food, and Decor?

#When Food and Decor are in the model, Service is not statistically significant, therefore we cannot know
#whether it has a significant effect on modeling Price.



