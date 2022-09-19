#das_week_4
#现在您已熟悉RMarkdown，现在鼓励您在RMarkdown文件中整理本实验中的工作。因此，这些实验室不再包含“代码块”，您可以在其中运行自己的R代码。请改用.Rmd文件。
#有许多不同的建模技术。但是，我们将从线性回归这一更容易理解和常用的方法开始。特别是，我们将从简单的线性回归开始，其中只有一个解释变量。

#具有一个数值解释变量的简单线性回归
library(ggplot2)
library(dplyr)
library(moderndive)
library(gapminder)
library(skimr)

View(evals)
evals.scores=evals%>%
  select(score,bty_avg)
?sample_n

#任务：用该select功能替换该功能，sample_n以查看10位讲师的随机子集。
sample_n(evals,10)

evals.scores%>%
  skim()

min(evals.scores$score)
max(evals.scores$bty_avg)

#相关性
#上面的摘要统计信息分别提供有关每个变量的信息。但是，我们对两个变量之间的潜在关系很感兴趣，因此，评估一些同时考虑两个变量的统计量将很有意义。此类统计数据之一就是相关性，其范围在-1和1之间，描述了两个数值变量之间的线性关系的强度，因此

#可以使用包中的get_correlation函数在R中计算相关系数moderndive。该函数需要两个数字变量，它们之间用~（或“波浪号”）分隔，非常类似于公式语法，因此结果变量score位于公式的左侧，而说明变量bty_avg位于右侧-公式的一面：
evals.scores%>%
  get_correlation(formula=score~bty_avg)

#在这里，我们给出的教学（score）与美女（bty_avg）得分之间的关系的相关系数为0.187 。这表明两个变量之间的线性关系比较弱。围绕相关系数存在一些主观解释，这些系数不太接近-1、0、1。下表提供了有关相关系数的口头解释的粗略指南。
?cor

#我们探索性数据分析的下一步是使用适当的绘图技术将数据可视化。在这里，散点图是合适的，因为score和bty_avg都是数值变量：
ggplot(evals.scores,aes(x=bty_avg,y=score))+
  geom_point()

#任务：更新上面的代码，为绘图提供更多合适的坐标轴标签和类似于上述散点图的标题。

#提示：您需要包括该labs层并为其提供x，y和title参数。

#注意：结果变量应始终在y轴上绘制。

#完成探索性数据分析后，下一步是对数据进行形式化分析。这涉及从探索性数据分析步骤中收集的信息构建适当的统计模型。在这里，我们将为教学和美容分数数据拟合一个简单的线性回归模型，我们的目标是获得最佳拟合回归线。这是通过找到截距的估计值来完成的（α）和斜率（β），这为我们提供了最合适的数据线。可以在R中使用以下lm函数将线性模型拟合到数据中来完成此操作：
model=lm(score~bty_avg,data=evals.scores)
model

ggplot(evals.scores, aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(x = "Beauty Score", y = "Teaching Score", 
       title = "Relationship of teaching and beauty scores") +
  geom_smooth(method="lm",se=F)

#Task: Obtain the observed score and bty_avg for the 27th instructor.

#Hint: Pick out the 27th row of the evals.scores data set.
evals.scores[27,]

#要获得数据集中所有教师的拟合值和残差，我们可以使用以下get_regression_points函数：
regression.points=get_regression_points(model)
regression.points

#我们可以检查第一个假设的一种方法是相residuals对于解释变量（bty_avg）绘制残差（）。由此，我们应该能够检查说明变量与结果变量（score）是否具有线性关系。我们可以使用我们的解释变量来绘制残差：
ggplot(regression.points,aes(x=bty_avg,y=residual))+
  geom_point()+
  labs(x = "Beauty Score", y = "Residual") +
  geom_hline(yintercept=0,col="blue",size=1)

#我们还可以通过按拟合值绘制残差来检查我们的前两个假设，如下所示：
ggplot(regression.points, aes(x = score_hat, y = residual)) +
  geom_point() +
  labs(x = "Fitted values", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1)

#为了评估残差呈正态分布的第三个假设，我们可以简单地绘制残差的直方图：
ggplot(regression.points,aes(x=residual))+
  geom_histogram(binwidth=0.25,color="white")+
  labs(x="Residual")

#在这里，我们将拟合一个简单的线性回归模型，其解释变量是分类的。甲分类变量是固定数目的可能值的一个变量，分配单元基于定性性质特定组（或类别）。

#我们将检查库中的gapminder数据集gapminder。这是有关世界各国的预期寿命的数据集。我们将探讨预期寿命及其潜在差异：
#探索性数据分析
#让我们检查一下gapminder与2007年有关的数据集的子集。也就是说，我们使用该filter函数仅选择与2007年有关的观测值，然后select选择我们感兴趣的变量：
?gapminder
gapminder2007=gapminder%>%
  filter(year==2007)%>%
  select(country,continent,lifeExp)
glimpse(gapminder2007)
View(gapminder2007)

#在这里，我们可以看到country和continent都是因子（fct），这就是R存储分类变量的方式。与我们以前的探索性数据分析类似，我们可以使用skim函数获取摘要统计信息。首先，让我们看一下预期寿命（lifeExp）和continent变量：
gapminder2007%>%
  select(continent,lifeExp)%>%
  skim()

lifeExp.continent=gapminder2007%>%
  group_by(continent)%>%
  summarize(median=median(lifeExp),mean=mean(lifeExp))

#任务：从gapminder2007数据集中获得全球平均预期寿命和平均预期寿命。

#提示：在函数内使用median和函数。meansummarize
lifeExp.continent=gapminder2007%>%
  summarize(median=median(lifeExp),mean=mean(lifeExp))

#在检查数字结果变量在分类变量的不同级别上的分布时，经常使用箱形图：
ggplot(gapminder2007,aes(x=continent,y=lifeExp))+
  geom_boxplot()+
  labs(x = "Continent", y = "Life expectancy (years)", 
       title = "Life expectancy by continent")

Asia=gapminder2007%>%
  filter(continent=="Asia")%>%
  group_by(country)%>%
  summarize(median=median(lifeExp),mean=mean(lifeExp))
  
lifeExp.model=lm(lifeExp~continent,data=gapminder2007)

#评估模型拟合
#拟合值是多少 ÿˆ 和残差 ÿ-ÿˆ对应于我们在处理分类解释变量时的情况？让我们探索gapminder2007数据集以了解它们是如何工作的。
regression_points=get_regression_points(lifeExp.model)

#提示：这可以通过查看上面的回归表来找到。您也可以尝试使用列which.max上的功能residual。

#为了评估围绕分类解释变量的残差的假设，我们可以绘制每个大陆的残差：
ggplot(regression_points, aes(x = continent, y = residual)) +
  geom_jitter(width = 0.1) + 
  labs(x = "Continent", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue")

#注意：我们已将每个大陆的点抖动了，以便更清楚地看到每个国家的残差。

#在这里，我们看到每个大陆的零线上方和下方的残差分布均匀，因此，我们假设残差均值为零似乎是有效的。在亚洲观察到一个异常值较大的负残差（与阿富汗有关）。

#为了检查残差是否正态分布，我们绘制了它们的直方图：
ggplot(regression_points, aes(x = residual)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(x = "Residual")

#further task 2
#在evals数据集中检查教学得分与年龄之间的关系。相关系数的值是多少？您将如何口头解释这一点？最后，制作一个教学得分和年龄的散点图。
#提示：首先查看evals参考score和的数据集的子集age。然后，您可以使用该get_correlation函数获取相关系数。散点图可以使用ggplot和生成geom_point。请注意，请记住为散点图提供适当的标签。
evals%>%
  get_correlation(formula=score~age)

ggplot(evals, aes(x = age, y = score)) +
  geom_point() +
  labs(x = "Age", y = "Teaching Score",
       title = "Relationship between Teaching Score and Age")
#3 3. Perform a formal analysis of the relationship between teaching score and age by fitting a simple linear
#regression model. Superimpose your best-fitting line onto your scatterplot from Task 1.
model=lm(score~age,data=evals)
model

ggplot(evals, aes(x = age, y = score)) +
  geom_point() +
  labs(x = "Age", y = "Teaching Score",
       title = "Relationship between Teaching Score and Age") +
  geom_smooth(method = "lm", se = FALSE)

#4 Assess the model assumptions from Task 2 by plotting the residuals against the explanatory variable
#and fitted values, respectively. Also, plot a histogram of the residuals to assess whether they are
#normally distributed.
model <- lm(score ~ age, data = evals)
regression.points <- get_regression_points(model)
ggplot(regression.points, aes(x = age, y = residual)) +
  geom_point() +
  labs(x = "Age", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1)

ggplot(regression.points, aes(x = score_hat, y = residual)) +
  geom_point() +
  labs(x = "Fitted values", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1)

ggplot(regression.points, aes(x = residual)) +
  geom_histogram(binwidth = 0.25, color = "white") +
  labs(x = "Residual")

#. If we were interested in exploring the relationship between teaching score and age for male and female
#professors separately we could produce a scatterplot of teaching score and age with different symbols
#(and an appropriate legend) for males and females. Create this plot and superimpose on it the linear
#regression models fitted to the male and female data separately. Note you do not need to examine the
#models formally, we’ll look at that in a future lab.

ggplot(evals, aes(x = age, y = score, shape = gender, color=gender)) +
  geom_point() +
  labs(x = "Age", y = "Teaching Score",
       title = "Relationship between Teaching Score and Age",
       subtitle = "Data and fitted linear regression models shown for female and male staff") +
  geom_smooth(method = "lm", se = FALSE, aes(color=gender, linetype=gender))

#6. Perform the same analysis we did on life expectancy from the gapminder data set in 2007. However,
#subset the data for the year 1997. Are there any differences in the results across this 10 year period?
gapminder1997 <- gapminder %>%
  filter(year == 1997) %>%
  select(country, continent, lifeExp)
lifeExp.by.continent <- gapminder1997 %>%
  group_by(continent) %>%
  summarize(median = median(lifeExp), mean = mean(lifeExp))
lifeExp.by.continent

lifeExp.model <- lm(lifeExp ~ continent, data = gapminder1997)
lifeExp.model







