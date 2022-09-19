#Data Analysis: Generalised Linear Models

###
Generalised linear models
The main objective this week is to introduce Generalised Linear Models (GLMs), which extend the linear model framework to response variables that don’t follow the normal distribution. GLMs can be used to model non-normal continuous response variables, but they are most frequently used to model binary, categorical or count data. Here we shall focus on binary/categorical response variables. The generalised linear model can be written as:
  
  yiμi∼f(g(μi))=x⊤iβ,

where the response yi is predicted though the linear combination μi of explanatory variables by the link function g(⋅), assuming some distribution f(⋅) for yi, and x⊤i is the ith row of the design matrix X. For example, the simple linear regression model above for a continuous response variable has the normal distribution distribution as f(⋅), with corresponding link function equal to the Identity function, that is, g(μi)=μi.

What if our response variable y is binary (e.g. yes/no, success/failure, alive/dead)? That is, the independent responses yi can either be:
  
  binary, taking the value 1 (say success, with probability pi) or 0 (failure, with probability 1−pi) or

binomial, where yi is the number of successes in a given number of trials ni, with the probability of success being pi and the probability of failure being 1−pi.

In both cases the distribution of yi is assumed to be binomial, but in the first case it is Bin(1,pi) and in the second case it is Bin(ni,pi). Hence, a binary response variable yi has a binomial distribution with corresponding link function g(⋅) equal to the logit link function, that is

g(pi)=log(pi1−pi),
###

library(tidyverse)
library(moderndive)
library(gapminder)
library(sjPlot)
library(stats)
library(jtools)

#具有一个数值解释变量的二元逻辑回归
#在这里，我们将首先用一个数值解释变量拟合逻辑回归模型。让我们回到我们在第 3 周检查evals的moderndive包中的数据。

#教学评价分数
#回想前几周，高等教育中的学生反馈在评估教学技术、材料以及教学方法和技术的改进方面极为重要。然而，已经研究了提供反馈时潜在的偏见因素，例如教师的外貌；有关详细信息，请参阅教育经济学评论。在这里，我们将回到学生评价的研究。n = 463 德克萨斯大学奥斯汀分校的教授。

#以前，我们将教学分数视为我们的连续响应变量，将美貌分数视为我们的解释变量。现在我们将考虑性别作为我们的响应变量，因此应该有一个二元响应变量（女性/男性）。我们将检查数据集中教师的年龄是否存在性别差异。evals

#首先，让我们从evals数据集中选择感兴趣的变量开始：

evals.gender=evals%>%select(gender,age)
evals.gender

#现在，让我们看一下ageby的箱线图，gender以获得对数据的初步印象：
ggplot(data=evals.gender,aes(x=gender,y=age,fill=gender))+
  geom_boxplot()+
  labs(x="Gender",y="Age")+
  theme(legend.position="non")

#按性别划分的教师年龄。

#在这里我们可以看到，男教师的年龄往往比女同事的年龄大。现在，让我们拟合一个逻辑回归模型，看看年龄是否是教师是男性还是女性的几率的重要预测因素。

#对数赔率
#为了拟合逻辑回归模型，我们将使用广义线性模型函数glm，其作用方式与lm我们之前使用的函数非常相似。我们只需要处理一个额外的参数。以性别为响应，以年龄为解释变量的逻辑回归模型由下式给出：
model <- glm(gender ~ age, data = evals.gender, 
             family = binomial(link = "logit"))

#这里我们包含了附加family参数，它说明了我们想要使用的分布和链接函数。因此，family = binomial(link = "logit")我们有一个二元响应变量，因此有一个二项式分布，及其相应的logit 链接函数。现在，让我们看看我们的逻辑回归模型产生的总结：
model%>%summary()

#一个替代方法summary()是包中的summ()函数，它允许对汇总表中包含的内容进行更多控制，并提供更好的格式输出。这是上面拟合模型的默认输出......jtools
summ(model)

#为了解释这个拟合模型，首先我们注意到我们的二元响应的基线类别是female。这是因为 R 中的默认基线被视为按字母顺序排在第一位的基线，这可以从levels函数中看出：
levels(evals.gender$gender)

#This means that estimates from the logistic regression model are for a change on the log-odds scale for males in comparison to the response baseline females. We can extract the estimated coefficients using mod1coefs <- round(coef(model), 2) and then use the inline code `r mod1coefs[1]` and `r mod1coefs[2]` to report the fitted model as follows…

mod1coefs <- round(coef(model), 2)

#输入(磷1 - p)= α + β⋅年龄= − 2.7 + 0.06 ⋅年龄，

#在哪里 p =概率（男） 和 1 − p =概率（女性）. 因此，教师为男性的对数几率每增加 1 个单位就增加 0.06 age。这为我们提供了对数优势如何随年龄变化的点估计，但是，我们也有兴趣为这些对数优势产生 95% 的置信区间。这可以使用包中的confint函数来完成MASS：

confint(model) %>%
  knitr::kable()

#要了解如何计算这些端点，请考虑以下代码：

mod.coef.logodds <- model %>%
  summary() %>%
  coef()
age.logodds.lower <- mod.coef.logodds["age", "Estimate"] - 
  1.96 * mod.coef.logodds["age", "Std. Error"]
#[1] 0.04221777
age.logodds.upper <- mod.coef.logodds["age", "Estimate"] + 
  1.96 * mod.coef.logodds["age", "Std. Error"]

#因此，对数优势的点估计值为 0.06，相应的 95% 置信区间为 (0.04, 0.08)。这可以使用包中的plot_model函数以图形方式显示sjPlot，只需将 ourmodel作为参数传递：
plot_model(model, show.values = TRUE, transform = NULL,
           title = "Log-Odds (Male instructor)", show.p = FALSE)
#可以传递给plot_model函数的一些有趣的参数是：

#show.values = TRUE/FALSE：是否应显示对数赔率/赔率值；
#show.p = TRUE/FALSE：在值标签中添加表示估计显着性水平的星号；
#transform：命名将应用于估计的函数的字符向量。默认转换用于exp显示优势比，同时transform = NULL显示对数优势；和
#vline.color：垂直“零效应”线的颜色。
#plot_model可以在此处和此处找到有关使用的更多详细信息。

#现在，让我们将 log-odds 的估计值添加到我们的数据集中：

evals.gender <- evals.gender %>%
  mutate(logodds.male = predict(model))

#赔率
#通常，我们希望在赔率范围内工作，因为与对数赔率比相比，赔率比更容易解释。为了获得赔率，我们只需对对数赔率取幂，即

#磷1 - p=经验( α + β⋅年龄) ,
model %>%
  coef() %>%
  exp()

#在几率量表上，截距值 (0.07) 给出了教师为男性的几率age = 0，这显然不是教师的可行年龄，因此为什么这个值非常接近于零。因为age我们有一个 1.06 的几率，这表明年龄每增加 1 个单位，教师是男性的几率增加 1.06 倍。那么这是如何计算的呢？让我们看看从 51 岁和 52 岁的教练那里得到的比值比，即一个单位的差异：

#赔率年龄=52赔率年龄=51=⎛⎝⎜⎜⎜磷年龄=521 -磷年龄=52磷年龄=511 -磷年龄=51⎞⎠⎟⎟⎟=经验值( α + β⋅ 52 )经验值( α + β⋅ 51 )=经验(乙⋅ ( 52 − 51 ) ) =exp( 0.06 ) = 1.06。


#例如，一名 45 岁的教员是男性的几率为

#磷1 - p=经验( α + β⋅年龄) =exp( − 2.7 + 0.06 ⋅ 45 ) = 1.15。

#这可以解释为 45 岁的教练是男性的几率比他们是女性的几率高 15%。我们可以通过简单地对对数赔率区间的下限和上限取幂来获得赔率的 95% 置信区间：

age.odds.lower <- exp(age.logodds.lower)
age.odds.lower
age.odds.upper <- exp(age.logodds.upper)
age.odds.upper

#因此，赔率的点估计值为 1.06，相应的 95% 置信区间为 (1.04, 1.09)。这可以使用包中的plot_model函数以图形方式显示sjPlot，只需将 ourmodel作为参数传递并删除transform = NULL（默认转换是指数转换）
plot_model(model, show.values = TRUE, axis.lim = c(1,1.5),
           title = "Odds (Male instructor)", show.p = FALSE)

#注意：由于 95% 置信区间太窄，很难看到它显示在图中，但默认情况下包括在内。如此处所示，该axis.lim = c(1,1.5)参数提高了其可见性。

#现在，让我们将几率的估计添加到我们的数据集中：
evals.gender <- evals.gender %>%
  mutate(odds.male = exp(logodds.male))

#概率
#我们可以得到概率 p =概率（男） 使用以下转换：

#磷=经验值( α + β⋅年龄)1 +经验( α + β⋅年龄).

#例如，一名 52 岁的教员是男性的概率是

#磷=经验值( α + β⋅年龄)1 +经验( α + β⋅年龄)=经验值( − 2.697946 + 0.0629647 ⋅ 52 )1 +经验( − 2.697946 + 0.0629647 ⋅ 52 )= 0.64 ,

#可以在 R 中计算如下：

p.num <- exp(mod.coef.logodds["(Intercept)", "Estimate"] + mod.coef.logodds["age", "Estimate"] * 52)
p.denom <- 1 + p.num
p.num / p.denom

#库中的plogis()函数stats也可用于从对数赔率中获取概率：
plogis(mod.coef.logodds["(Intercept)", "Estimate"] + mod.coef.logodds["age", "Estimate"] * 52)

#让我们将概率添加到我们的数据中，这是使用fitted()函数完成的：
evals.gender <- evals.gender %>%
  mutate(probs.male = fitted(model))
evals.gender

#注意：predict(model, type = "response")还将提供估计的概率。

#最后，我们可以geom_smooth()通过给出method = "glm"和使用该函数绘制成为男性的概率，methods.args = list(family = "binomial")如下所示：

ggplot(data = evals.gender, aes(x = age, y = probs.male)) +
  geom_smooth(method="glm", 
              method.args = list(family="binomial"), 
              se = FALSE) +
  labs(x = "Age", y = "Probability of instructor being male")

#包中的plot_model()函数sjPlot还可以通过age如下方式产生估计概率：
plot_model(model, type = "pred", title = "",
           axis.title = c("Age", "Prob. of instructor being male"))

#具有一个分类解释变量的二元逻辑回归
#age现在让我们使用二进制分类变量ethnicity作为我们的解释变量，而不是像 那样使用数值解释变量。
evals.ethnic <- evals %>%
  select(gender, ethnicity)

#正如我们在第 7 周中首次看到的，我们可以使用该janitor包以表格格式汇总这些数据：
library(janitor)
evals %>% 
  tabyl(ethnicity, gender) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns() # To show original counts

#我们可以使用的barplot形象化的分布gender和ethnicity：

ggplot(evals, aes(x= gender,  y = ..prop.., group=ethnicity, fill=ethnicity)) + 
  geom_bar(position="dodge", stat="count") +
  labs(y = "Proportion")

#我们可以看到，minority族群中女性讲师的比例较大（56.3% 对 43.8%），而not minority族群中男性讲师较多（60.02% 对 39.85%）。现在我们将拟合一个逻辑回归模型来确定是否可以从他们的种族来预测教师的性别。

#对数赔率
#逻辑回归模型由下式给出：
model.ethnic <- glm(gender ~ ethnicity, data = evals.ethnic, family = binomial(link = "logit"))
model.ethnic %>%
  summary()

#同样，我们的二元响应的基线类别是female。此外，我们的解释变量的基线类别是minority，与 一样gender，默认情况下由 R 按字母顺序完成：

levels(evals.ethnic$ethnicity)

#This means that estimates from the logistic regression model are for a change on the log-odds scale for males (p=Prob(Males)) in comparison to the response baseline females. That is

#ln(p1−p)=α+β⋅ethnicity=−0.25+0.66⋅Iethnicity(not minority),

#where Iethnicity(not minority) is an indicator function. Hence, the log-odds of an instructor being male increase by 0.66 if they are in the ethnicity group not minority. This provides us with a point estimate of how the log-odds changes with ethnicity, however, we are also interested in producing a 95% confidence interval for these log-odds. This can be done using the confint function in the MASS package:

confint(model.ethnic) %>%
  knitr::kable()

#要了解如何计算这些端点，请考虑以下代码：

mod.ethnic.coef.logodds <- model.ethnic %>%
  summary() %>%
  coef()
ethnic.logodds.lower <- mod.ethnic.coef.logodds["ethnicitynot minority", "Estimate"] - 
  1.96 * mod.ethnic.coef.logodds["ethnicitynot minority", "Std. Error"]
#[1] 0.1300587
ethnic.logodds.upper <- mod.ethnic.coef.logodds["ethnicitynot minority", "Estimate"] + 
  1.96 * mod.ethnic.coef.logodds["ethnicitynot minority", "Std. Error"]
#[1] 1.19604

#因此，对数赔率的点估计值为 0.66，相应的 95% 置信区间为 (0.13, 1.2)。这可以使用包中的plot_model函数以图形方式显示sjPlot，只需将 ourmodel作为参数传递：
plot_model(model.ethnic, show.values = TRUE, transform = NULL,
           title = "Log-Odds (Male instructor)", show.p = FALSE)

#现在，让我们将 log-odds 的估计值添加到我们的数据集中：

evals.ethnic <- evals.ethnic %>%
  mutate(logodds.male = predict(model.ethnic))

#赔率
#在赔率尺度上，回归系数由下式给出

model.ethnic %>%
  coef() %>%
  exp()

#该(Intercept)给我们的教练给他们的是男性的可能性minority族群，也就是0.78（指示灯功能为零在这种情况下）。考虑到他们在not minority族群中，教练是男性的几率是他们在minority族群中的几率的 1.94 倍。

#在继续之前，让我们看看这些值是如何计算的。首先，minority可以通过以下方式获得讲师为男性的几率，因为他们属于该族群：

#磷少数民族1 -磷少数民族=经验( α ) = exp( − 0.25 ) = 0.78。

# the number of instructors in the minority
pmin <- evals.ethnic %>%
  filter(ethnicity == "minority") %>%
  summarize(n()) %>%
  pull()

# the number of male instructors in the minority
pmin.male <- evals.ethnic %>%
  filter(ethnicity == "minority", gender == "male") %>%
  summarize(n()) %>%
  pull()

# the proportion/probability of males in the minority
prob.min.male <- pmin.male / pmin
odds.min.male <- prob.min.male / (1 - prob.min.male)
odds.min.male

#Now, the odds-ratio of an instructor being male in the not minority compared to the minority ethnic group is found as follows:
  
#  Oddsnot minorityOddsminority=pnot minority1−pnot minoritypminority1−pminority=exp(α+β)exp(α)=exp(α+β−α)=exp(β)=exp(0.66)=1.93.
# the number of instructors not in the minority
pnotmin <- evals.ethnic %>%
  filter(ethnicity == "not minority") %>%
  summarize(n()) %>%
  pull()

# the number of male instructors not in the minority
pnotmin.male <- evals.ethnic %>%
  filter(ethnicity == "not minority", gender == "male") %>%
  summarize(n()) %>%
  pull()

# the proportion/probability of males not in the minority
prob.notmin.male <- pnotmin.male / pnotmin
odds.notmin.male <- prob.notmin.male / (1 - prob.notmin.male)
odds.ratio.notmin <- odds.notmin.male / odds.min.male

#我们可以通过简单地对对数赔率区间的下限和上限取幂来获得赔率的 95% 置信区间：

ethnic.odds.lower <- exp(ethnic.logodds.lower)
#[1] 1.138895
ethnic.odds.upper <- exp(ethnic.logodds.upper)

#因此，优势比的点估计值为 1.94，相应的 95% 置信区间为 (1.14, 3.31)。同样，我们可以使用包中的plot_model函数以图形方式显示sjPlot：

plot_model(model.ethnic, show.values = TRUE,
           title = "Odds (Male instructor)", show.p = FALSE)

#现在，让我们将几率的估计添加到我们的数据集中：

evals.ethnic <- evals.ethnic %>%
  mutate(odds.male = exp(logodds.male))

#概率
#考虑到他们在minority和not minority小组中，一名教师是男性的概率是
?plogis
plogis(mod.ethnic.coef.logodds["(Intercept)", "Estimate"])

plogis(mod.ethnic.coef.logodds["(Intercept)", "Estimate"] + 
         mod.ethnic.coef.logodds["ethnicitynot minority", "Estimate"]) 

#因此，考虑到他们属于minority和not minority族群，教师为男性的概率分别为 0.437 和 0.602。让我们将概率添加到我们的数据中：

evals.ethnic <- evals.ethnic %>%
  mutate(probs.male = fitted(model.ethnic))

#最后，我们可以使用包中的plot_model()函数sjPlot来产生估计概率，ethnicity如下所示：

plot_model(model.ethnic, type = "pred", title = "",
           axis.title = c("Ethnicity", "Prob. of instructor being male"))

#进一步的任务
#雅尼还是劳雷尔？
#这种听觉错觉于 2018 年 5 月首次出现在互联网上。可以在这个短视频中找到人们为什么听到不同事物的解释，这只是讨论这种现象的众多互联网资源之一。差异背后的主要原因似乎是随着年龄的增长，我们失去了听到某些声音的能力。为了查看我们是否能找到这种年龄效应的证据，我们请格拉斯哥大学数学与统计学院的学生和教职员工填写一份关于他们所听到内容的调查问卷。您可以在下面看到回复摘要。

#听力Yanny和的比例Laurel非常相似，有些受访者听到两者甚至完全不同的东西。这可能是因为人们不会使用相同的设备收听音频文件，这是我们在调查中无法控制的。忽略Yanny或以外的响应Laurel，我们有 53 个观察结果。

#下载数据（yanny.csv从Moodle的）和适合逻辑回归模型与hear作为二元响应变量，age和gender作为解释变量。你的发现是什么？
setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/semester\ 2/das/Week\ 9\ Data\ For\ Further\ Tasks-20210706")

#Further Tasks
#Yanny or Laurel?
#  This auditory illusion first appeared on the internet in May 2018. An explanation of why people hear different
#things can be found in this short video, just one of many internet sources discussing the phenomenon. The
#main reason behind the difference appears to be that as we age we lose the ability to hear certain sounds. To
#see if we could find evidence of such an age effect, we asked students and staff at the School of Mathematics
#and Statistics at the University of Glasgow to fill out a survey on what they hear. Below you can see
#summaries of the responses.

yanny=read.csv("yanny.csv")
yanny=yanny%>%
  select(hear,gender,age)
yanny$hear=as.factor(yanny$hear)
yanny$gender=as.factor(yanny$gender)

ggplot(data=yanny,aes(x=hear,y=age,fill=hear))+
  geom_boxplot()+
  labs(x="what do you hear?",y="Age")+
  theme(legend.position="none")

#We see in the boxplot that the people who hear Yanny are, on average, younger, however there is some
#overlap in the IQR’s.
yanny%>%tabyl(gender,hear)%>%adorn_percentages()%>%adorn_pct_formatting()%>%adorn_ns()

ggplot(data = yanny, aes(x = hear, group = gender)) +
  geom_bar(aes(y = ..prop.., fill = gender), stat = "count", position = "dodge") +
  labs(x = "What do you hear?", y = "Proportion")

#There is a slightly smaller proportion of men hearing Yanny, but the proportions are very similar overall.
mod.yanny <- glm(hear ~ age + gender, data = yanny, family = binomial(link = "logit"))
mod.yanny %>%
  summary()

mod.yanny <- glm(hear ~ age, data = yanny, family = binomial(link = "logit"))
mod.yanny %>%
  summary()

#Notice that the coefficient of age is negative, suggesting that older people are less likely to hear Yanny.
#However, the coefficient of age is not significant (𝑝-value of 0.16). Still, if we wanted to use the estimated
#coefficient to quantify the effect of age, we would need to look at exp(-0.04812) = 0.953. This suggests that
#for two people who differ by one year in age, the older person’s odds of hearing Yanny are 0.953 times those
#of the younger person. If we want to look at a ten-year age difference then the odds multiplier becomes
#exp(0.04812 * 10) = 1.618. Hence, for two people who differ by 10 years in age, the older person’s odds of
#hearing Yanny are 1.618 times those of the younger person.
plot_model(mod.yanny, show.values = TRUE,
           title = "Odds (Age)", show.p = TRUE)

plot_model(mod.yanny, type = "pred", title = "",
           axis.title = c("Age", "Probability of hearing Yanny"))

#Download the data (titanic.csv) from Moodle for 𝑛 = 891 passengers aboard the Titanic and fit a logistic
#regression model with survived as the binary response variable, and age, gender, and passenger.class
#as the explanatory variables. What are your findings?
titanic <- read_csv("titanic.csv")
titanic <- titanic %>%
  select(survived, age, gender, passenger.class)
titanic$survived <- as.factor(titanic$survived)
levels(titanic$survived) <- c("Died", "Survived")
titanic$gender <- as.factor(titanic$gender)
titanic$passenger.class <- as.factor(titanic$passenger.class)

ggplot(data = titanic, aes(x = survived, y = age, fill = survived)) +
  geom_boxplot() +
  labs(x = "Survived the Titanic?", y = "Age") +
  theme(legend.position = "none")

titanic %>%
  tabyl(gender, survived) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns() # To show original counts

ggplot(data = titanic, aes(x = survived, group = gender)) +
  geom_bar(aes(y = ..prop.., fill = gender), stat = "count", position = "dodge") +
  labs(x = "Survived the Titanic?", y = "Proportion")

titanic %>%
  tabyl(passenger.class, survived) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns() # To show original counts

ggplot(data = titanic, aes(x = survived, group = passenger.class)) +
  geom_bar(aes(y = ..prop.., fill = passenger.class),
           stat = "count", position = "dodge") +
  labs(x = "Survived the Titanic?", y = "Proportion")

#The largest group of passengers who died were third class passengers, while among those who survived the
#largest group was first class passengers.
mod.titanic <- glm(survived ~ gender + passenger.class + age, data = titanic,
                   family = binomial(link = "logit"))
mod.titanic %>%
  summary()

plot_model(mod.titanic, show.values = TRUE,
           title = "", show.p = FALSE, value.offset = 0.25)

#We interpret the odds ratios as follows: men’s odds of survival were 0.07 times those of women, third class
#passengers’ odds of survival were 0.10 times those of first class passengers, and second class passengers’ odds
#of survival were 0.33 times those of first class passengers. Finally, for each year increase in the passenger’s
#age, their odds of survival decrease (by a factor of 0.97).


