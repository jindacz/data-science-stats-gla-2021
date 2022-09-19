library(ggplot2)
library(dplyr)
library(moderndive)
library(ISLR)
library(skimr)
library(plotly)
library(tidyr)
library(jtools)

str(Credit)
#首先对Credit数据集进行子集化，以便我们只有我们感兴趣的变量，即Balance、Limit和Income。请注意，最好为您的新数据集指定一个与 Credit 不同的名称，以免覆盖原始Credit数据集。
Cred=Credit%>%
  select(Balance,Limit,Income)

glimpse(Cred)

#现在，让我们使用以下skim函数查看与我们的数据集相关的汇总统计信息：
Cred%>%skim()

#现在我们正在研究结果变量和多个解释变量之间的关系，我们需要检查它们之间的相关性。我们可以检查之间的相互关系Balance，Limit并Income通过建立相关的表如下
Cred%>%
  cor()

#现在让我们生成结果变量和解释变量之间关系的散点图。首先，我们将看一下Balance反对的散点图Limit：
ggplot(Cred,aes(x=Limit,y=Balance))+
  geom_point()+
  labs(x="Credit limit",y="Credit card balance",
       title="Relationship btw balance and credit limit")+
  geom_smooth(method="lm",se=F)

#现在，让我们看一下Balanceand的散点图Income：
ggplot(Cred, aes(x = Income, y = Balance)) +
  geom_point() +
  labs(x = "Income (in $1000)", y = "Credit card balance (in $)", 
       title = "Relationship between balance and income") +
  geom_smooth(method = "lm", se = FALSE)

#上面的两个散点图分别关注结果变量Balance与每个解释变量之间的关系。为了了解所有三个变量之间的关系，我们可以使用库plot_ly中的plotly函数来绘制 3 维散点图，如下所示（您可能需要将鼠标悬停在/单击下面的区域以显示该图）：
plot_ly(Cred,x=~Income,y=~Limit,z=~Balance,type="scatter3d",mode="markers")

#在第 3 周，当我们用一个连续的解释变量拟合回归模型时，我们正在寻找最佳拟合线。然而，现在我们有不止一个解释变量，我们正在研究最佳拟合平面，它是最佳拟合线的 3 维概括。

#与第 3 周类似，我们使用该lm函数拟合回归模型并使用该get_regression_table函数查看我们的参数估计：
Balance.model=lm(Balance~Limit+Income,data=Cred)
get_regression_table(Balance.model)

#要在回归模型中包含多个解释变量，我们只需使用+符号，即Balance ~ Limit + Income。

#另一种方法get_regression_table是包中的summ函数，它允许对汇总表中包含的内容进行更多控制。这是默认输出...jtools
summ(par.model)

#这是由于一种称为辛普森悖论的现象。当数据的不同类别（或组）中存在趋势时，就会发生这种情况，但当类别作为一个整体分组时，这些趋势就会消失。

#现在我们需要评估我们的模型假设。作为第 3 周的提醒，我们的模型假设是：

#模型的确定性部分捕获数据中的所有非随机结构，即残差的均值为零。
#残差的可变性尺度在所有解释变量值上都是恒定的。
#残差呈正态分布。
#残差是独立的。
#解释变量的值被无误地记录下来。
#首先，我们需要从回归模型中获得拟合值和残差：
regression.points=get_regression_points(Balance.model)
regression.points

#我们可以通过针对每个解释变量生成残差散点图来评估我们的前两个模型假设。首先，让我们从残差对信用额度的散点图开始：
ggplot(regression.points, aes(x = Limit, y = residual)) +
  geom_point() +
  labs(x = "Credit limit (in $)", y = "Residual", title = "Residuals vs credit limit")  +
  geom_hline(yintercept = 0, col = "blue", size = 1)

#现在，让我们绘制残差与收入的散点图：
ggplot(regression.points, aes(x = Income, y = residual)) +
  geom_point() +
  labs(x = "Income (in $1000)", y = "Residual", title = "Residuals vs income") +
  geom_hline(yintercept = 0, col = "blue", size = 1)

#最后，我们可以通过生成直方图来检查残差是否呈正态分布：
ggplot(regression.points,aes(x=residual))+
  geom_histogram(color="white")+
  labs(x="Residual")

#具有一个连续解释变量和一个分类解释变量的回归建模
#我们将检查：

#教学分数 ( score) 作为我们的结果变量是;
#年龄 ( age) 作为我们的连续解释变量X1; 和
#性别 ( gender) 作为我们的分类解释变量X2.
eval.score=evals%>%
  select(score,age,gender)

eval.score %>%
  skim()

#现在，让我们计算我们的结果变量score和我们的连续解释变量之间的相关系数age：
eval.score %>% 
  get_correlation(formula = score ~ age)

#我们现在可以通过生成散点图来可视化我们的数据，其中看到我们有分类变量gender，我们将为每个性别使用不同的颜色绘制点：
ggplot(eval.score, aes(x = age, y = score, color = gender)) +
  geom_jitter() +
  labs(x = "Age", y = "Teaching Score", color = "Gender") +
  geom_smooth(method = "lm", se = FALSE)

#多元回归：平行斜率模型
#在这里，我们将首先拟合所谓的平行回归线模型。该模型意味着男女教师的教学分数 ( score) 与年龄 ( age)之间的关系斜率相同，只是回归线的截距发生了变化。因此，我们的平行回归线模型如下：
par.model <- lm(score ~ age + gender, data = eval.score)
get_regression_table(par.model)

#现在，让我们将平行回归线叠加到教学分数与年龄的散点图上：
coeff  <- par.model %>% 
  coef() %>%
  as.numeric()

slopes <- eval.score %>%
  group_by(gender) %>%
  summarise(min = min(age), max = max(age)) %>%
  mutate(intercept = coeff[1]) %>%
  mutate(intercept = ifelse(gender == "male", intercept + coeff[3], intercept)) %>%
  gather(point, age, -c(gender, intercept)) %>% #gathers columns into rows
  #See Data Wrangling Cheat Sheet
  mutate(y_hat = intercept + age * coeff[2])

ggplot(eval.score, aes(x = age, y = score, col = gender)) +
  geom_jitter() +
  labs(x = "Age", y = "Teaching Score", color = "Gender") +
  geom_line(data = slopes, aes(y = y_hat), size = 1)

#从平行回归线模型来看，男性和女性具有相同的斜率，即年龄对教学成绩的相关影响对男性和女性来说是相同的。因此，年龄每增加一年，教学分数就会相应下降 0.009。但是男导师有更高的截取项，即在教学成绩中男性的回归线有一个垂直的凹凸。这与男性相对于女性获得的教学分数的平均差异有关。

#问题：我们之前的教学分数与年龄的散点图（图 7）和我们刚刚创建的叠加平行线的散点图（图 8）有什么不同？在原始图中，我们有所谓的年龄和性别之间的交互作用。因此，性别在不同年龄的男性和女性中以不同的方式相互作用，因此我们应该有不同的截距和斜率。

#为了在我们的回归模型中拟合交互项，我们将+符号替换为符号，*如下所示：
int.model <- lm(score ~ age * gender, data = eval.score)
get_regression_table(int.model)

ggplot(eval.score, aes(x = age, y = score, color = gender)) +
  geom_point() +
  labs(x = "Age", y = "Teaching Score", color = "Gender") +
  geom_smooth(method = "lm", se = FALSE)

#评估模型拟合
#现在我们必须通过查看残差图来评估模型的拟合。我们将为交互模型执行此操作。首先，我们需要从交互模型中获取拟合值和残差，如下所示：
regression.points <- get_regression_points(int.model)

#让我们首先查看残差与按性别划分的解释变量的散点图：
ggplot(regression.points, aes(x = age, y = residual)) +
  geom_point() +
  labs(x = "age", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1) +
  facet_wrap(~ gender)

#现在，我们可以根据拟合值绘制残差：
ggplot(regression.points, aes(x = score_hat, y = residual)) +
  geom_point() +
  labs(x = "Fitted values", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1) +
  facet_wrap(~ gender)

#最后，让我们绘制残差的直方图以评估它们是否以均值为零正态分布：
ggplot(regression.points, aes(x = residual)) +
  geom_histogram(binwidth = 0.25, color = "white") +
  labs(x = "Residual") +
  facet_wrap(~gender)

#1 Assess the model assumptions for the parallel regression lines model. Do they appear valid?
par.model=lm(score~age+gender,data=eval.score)
regression.points=get_regression_points(par.model)

ggplot(regression.points, aes(x = age, y = residual)) +
  geom_point() +
  labs(x = "age", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1) +
  facet_wrap(~ gender)

ggplot(regression.points, aes(x = score_hat, y = residual)) +
  geom_point() +
  labs(x = "Fitted values", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1) +
  facet_wrap(~ gender)

ggplot(regression.points, aes(x = residual)) +
  geom_histogram(binwidth = 0.25, color = "white") +
  labs(x = "Residual") +
  facet_wrap(~gender)

#2. Return to the Credit data set and fit a multiple regression model with Balance as the outcome variable,
#and Income and Age as the explanatory variables, respectively. Assess the assumptions of the multiple
#regression model.
Cred <- Credit %>%
  select(Balance, Income, Age)

Cred$Balance <- as.numeric(Cred$Balance)
Cred$Age <- as.numeric(Cred$Age)

skim_with(numeric = list(hist = NULL, missing = NULL, complete = NULL))
Cred %>%
  skim_to_list() %>%
  .$numeric %>%
  kable(col.names = c("Variable", "n", "Mean", "SD", "Minimum", "1st quartile", "Median",
                      "3rd quartile","Maximum"), caption =
          '\\label{tab:summary} Summary statistics on Credit Card Balance, Income and Age.',
        booktabs = TRUE, format = "latex") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")



Cred %>%
  cor() %>%
  kable(caption =
          '\\label{tab:cor} Correlation Coefficients between Credit Card Balance,
Income and Age.', booktabs = TRUE, format = "latex") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")

ggplot(Cred, aes(x = Age, y = Balance)) +
  geom_point() +
  labs(x = "Age (in years)", y = "Credit card balance (in $)",
       title = "Relationship between balance and age") +
  geom_smooth(method = "lm", se = FALSE)

Balance.model <- lm(Balance ~ Age + Income, data = Cred)
get_regression_table(Balance.model) %>%
  kable(caption =
          '\\label{tab:reg} Estimated Coefficients from the fitted model
Balance = Age + Income ', booktabs = TRUE, format = "latex") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")

regression.points <- get_regression_points(Balance.model)
ggplot(regression.points, aes(x = Income, y = residual)) +
  geom_point() +
  labs(x = "Income (in $1000)", y = "Residual", title = "Residuals vs income") +
  geom_hline(yintercept = 0, col = "blue", size = 1)

ggplot(regression.points, aes(x = Age, y = residual)) +
  geom_point() +
  labs(x = "Age (in years)", y = "Residual", title = "Residuals vs age") +
  geom_hline(yintercept = 0, col = "blue", size = 1)

ggplot(regression.points, aes(x = residual)) +
  geom_histogram(color = "white") +
  labs(x = "Residual")

#3. Return to the Credit data set and fit a parallel regression lines model with Balance as the outcome
#variable, and Income and Student as the explanatory variables, respectively. Assess the assumptions of
#the fitted model.
Cred <- Credit %>%
  select(Balance, Income, Student)
# Cred %>%
# skim()
Cred %>%
  group_by(Student) %>%
  summarise(n()) %>%
  kable(col.names = c("Student", "n"),caption =
          '\\label{tab:T3Student} Numbers of students and non-students',
        booktabs = TRUE, format = "latex") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")


Cred$Balance <- as.numeric(Cred$Balance)
skim_with(numeric = list(hist = NULL, missing = NULL, complete = NULL))
Cred %>%
  skim_to_list() %>%
  .$numeric %>%
  kable(col.names = c("Variable", "n", "Mean", "SD", "Minimum", "1st quartile", "Median",
                      "3rd quartile","Maximum"), caption =
          '\\label{tab:T3summary} Summary statistics on Credit Card Balance and Income.',
        booktabs = TRUE, format = "latex") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")

ggplot(Cred, aes(x = Income, y = Balance, color = Student)) +
  geom_jitter() +
  labs(x = "Income (in $1000)", y = "Credit card balance (in $)", color = "Student") +
  geom_smooth(method = "lm", se = FALSE)

par.model <- lm(Balance ~ Income + Student, data = Cred)
get_regression_table(par.model) %>%
  kable(caption =
          '\\label{tab:T3reg} Estimated Coefficients from the fitted model
Balance = Income + Student', booktabs = TRUE, format = "latex") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")

regression.points <- get_regression_points(par.model)
ggplot(regression.points, aes(x = Income, y = residual)) +
  geom_point() +
  labs(x = "Income (in $1000)", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1) +
  facet_wrap(~ Student)

ggplot(regression.points, aes(x = Balance_hat, y = residual)) +
  geom_point() +
  labs(x = "Fitted values", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1) +
  facet_wrap(~ Student)

ggplot(regression.points, aes(x = residual)) +
  geom_histogram(color = "white") +
  labs(x = "Residual") +
  facet_wrap(~Student)

#Trickier
#4. Load the library datasets and look at the iris data set of Edgar Anderson containing measurements
#(in centimetres) on 150 different flowers across three different species of iris. Fit an interaction model with
#Sepal.Width as the outcome variable, and Sepal.Length and Species as the explanatory variables.
#Assess the assumptions of the fitted model.
library(datasets)
Irs <- iris %>%
  select(Sepal.Width, Sepal.Length, Species)

Irs %>%
  skim()

Irs %>%
  group_by(Species) %>%
  summarise(n()) %>%
  kable(col.names = c("Species", "n"),caption =
          '\\label{tab:T4Species} Numbers of different species',
        booktabs = TRUE, format = "latex") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")

Irs %>%
  group_by(Species) %>%
  summarise(n()) %>%
  kable(col.names = c("Species", "n"),caption =
          '\\label{tab:T4Species} Numbers of different species',
        booktabs = TRUE, format = "latex") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")

skim_with(numeric = list(hist = NULL, missing = NULL, complete = NULL))
Irs %>%
  skim_to_list() %>%
  .$numeric %>%
  kable(col.names = c("Variable", "n", "Mean", "SD", "Minimum", "1st quartile", "Median",
                      "3rd quartile","Maximum"), caption =
          '\\label{tab:T4summary} Summary statistics on Iris variables.',
        booktabs = TRUE, format = "latex") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")

Irs %>%
  get_correlation(formula = Sepal.Width ~ Sepal.Length) %>%
  kable(caption =
          '\\label{tab:T4cor} Correlation Coefficient bewteen Sepal.Width and Sepal.Length',
        booktabs = TRUE, format = "latex") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")

ggplot(Irs, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  labs(x = "Sepal length (in centimetres)", y = "Sepal width (in centimetres)",
       color = "Species") +
  geom_smooth(method = "lm", se = FALSE)

int.model <- lm(Sepal.Width ~ Sepal.Length * Species, data = Irs)
get_regression_table(int.model) %>%
  kable(caption =
          '\\label{tab:T4reg} Estimated Coefficients from the fitted model
Sepal.Width = Sepal.Length . Species', booktabs = TRUE, format = "latex") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")

regression.points <- get_regression_points(int.model)
ggplot(regression.points, aes(x = Sepal.Length, y = residual)) +
  geom_point() +
  labs(x = "Sepal length (in centimetres)", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1) +
  facet_wrap(~ Species)

ggplot(regression.points, aes(x = Sepal.Width_hat, y = residual)) +
  geom_point() +
  labs(x = "Fitted values", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1) +
  facet_wrap(~ Species)

ggplot(regression.points, aes(x = residual)) +
  geom_histogram(color = "white") +
  labs(x = "Residual") +
  facet_wrap(~ Species)

