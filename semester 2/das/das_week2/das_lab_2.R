#das_lab_2
#tidyverse core packages
install.packages("tidyverse")
install.packages("fivethirtyeight")
install.packages("scales") 
install.packages("broom", type="binary")
install.packages("pillar", type="binary")
library(tidyverse)


# packages containing interesting data
library(nycflights13)
library(fivethirtyeight)

print("Well done!  You've loaded the libraries")

dim(flights) #Returns the dimensions of a dataframe
head(flights) #Returns the first 6 rows of the object
glimpse(flights) #Lists the variables in an object with their first few values 

#help(package = "packagename")
help(package = "nycflights13")

glimpse(airports)
?airports
#`lat` `long` represent the airport geographic coordinates, 
#`alt` is the altitude above sea level of the airport 
#`tz` is the time zone difference with respect to GMT in London UK, 
#`dst` is the daylight savings time zone, and `tzone` is the time zone label.

setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/semester\ 2/das")
library(readr)
dem_score=read.csv("dem_score.csv")
dem_score

library(readxl)
dem_score <- read_excel("dem_score.xlsx")
View(dem_score)

library(readr)
life_exp_scores <- read_csv("https://moderndive.com/data/le_mess.csv")
head(life_esp_scores)


guat_dem <- dem_score %>%  #does not work
  filter(country == "Guatemala")  

#guat_dem <- subset(dem_score, dem_score$country == "Guatemala")
?filter
guat_dem

#The aesthetic mapping is set by aes(x = year, y = democracy_score)

#软件包中的gather()功能tidyr可以为我们完成此任务。第一个参数gather()，
#就如同用ggplot2()，是data我们指定我们想整齐的数据帧的说法。接下来的两个参数
#gather()是key和value，它们指定了我们想要调用的新列，这些新列将我们的宽数据转换
#为长格式。最后，我们使用列出了我们不希望在此整理过程中不包括的变量的规范-
guat_tidy=gather(data=guat_dem,
                 key=year,
                 value=democracy_score,
                 -country)
guat_tidy

#现在，我们可以使用线图和来创建该图，以显示危地马拉的民主得分从1952年到1992年是如何变化的ggplot2。
ggplot(data=guat_tidy,mapping=aes(x=year,y=democracy_score))+
  geom_line()+
  labs(x="year")

#整理后，我们将在后面看到如何使用该mutate()函数将其更改year为数字变量

#Convert the dem_score data frame into a tidy data frame and assign the name of 
#dem_score_tidy to the resulting long-formatted data frame.
dem_score_tidy=gather(data=dem_score,
                      key=year,
                      value=democracy_score,
                      -country)
head(dem_score_tidy)

#Convert the life expectancy data set you created above into a tidy data frame.
life_exp_scores=read_csv("https://moderndive.com/data/le_mess.csv")
life_exp_tidy=gather(data=life_exp_scores,
                     key=year,
                     value=life_exp,
                     -country)

#Introducing the pipe (%>%), tibbles and data wrangling
#现在，我们可以导入数据并对数据执行基本操作，以使其成为“整齐”的格式。
#在本节及后续各节中，我们将使用dplyr软件包中的工具tiyverse来执行数据“整理”，
#其中包括转换，映射和汇总数据中的变量。

#小标题的一个主要优点是它们可以存储任何内容。
#data.frame每个“单元格”只能存储一个值，例如数字或字符串。
#但是，在小标题中，您可以将列表甚至其他小标题存储在单元格中。
#一个例子就是starwars包装中的小标题dplyr。
#该starships列为每行包含该角色飞行的星际飞船列表（根据角色的不同，其长度也有所不同）。
?starwars
library(dplyr) # Load library dplyr which contains the data
starwars[,c("name", "starships")] # Print columns name and starships

starwars[10,"starships"][[1]] # Starships flown by Obi-Wan Kenobi

#创建小标题
#强制：可以使用函数将数据帧或矩阵转换为小标题as_tibble。让我们从一个名为的数据帧开始kids：
kids=data.frame(age=c(4,11),weight=c(15,28),height=c(101,132),gender=c("f","m"))
rownames(kids)=c("Sarah","John")
kids

library(tibble)
kidstibble=as_tibble(kids)
kidstibble

#从输出中可以看到，小标题不使用行名称（尽管它们存储行名称，
#以便在将小标题转换回数据帧时可以重新添加行名称）。
#因此，在这种情况下，最好添加一个名为name的列：我们将首先添加该列，
#然后重新排列名称优先的列。
kidstibble$name=rownames(kids) # Create a column with names
kidstibble=kidstibble[,c("name", "age", "weight", "height", "gender")]
# Re-arrange columns that names comes first

#还有一个rownames_to_column我们也可以用于此目的的功能。
#它将行名作为第一列添加到小标题。
kidstibble2=rownames_to_column(kids)

#尽管并非所有R函数都适用于小标题，但我们可以用与数据帧几乎相同的方式来处理小标题。
#在这种情况下，您可以使用将小标题转换为数据帧as.data.frame。
kidsdf=as.data.frame(kidstibble)
kidsdf

#创建：我们可以使用函数创建小标题tibble。我们可以使用
kidstibble <- tibble(name=c("Sarah", "John"), age=c(4,11), weight=c(15,28),
                     height=c(101,132), gender=c("f", "m"))

#换句话说，该函数tibble在逐列的基础上组合一个小标题（类似于使用cbind）。
#函数tribble（“转置的小标题”）使您可以逐行创建小标题（类似于使用rbind），
#当在代码中创建矩阵时，通常更容易理解。
kidstibble=tribble(~name,~age,~weight,~height,~gender,
                   "Sarah", 4, 15, 101, "f",
                   "John", 11, 28, 132, "m")

#变量/列可以使用来访问和添加tibble$varname（变量名需要完全拼写）。您也可以使用tibble[,"varname"]或访问列tibble[["varname"]]。
#可以使用选择行tibble[rowindices,]（请注意，不能在小标题中使用行名，但可以在数据框中使用行名）。
#可以使用访问单个单元格tibble[rowindices, colindices]。

#subsetting tibbles always results in a tibble, meaning they are more consistent than data frames.
kidstibble[,1] # Result is a tibble
## # A tibble: 2 x 1
## name
## <chr>
## 1 Sarah
## 2 John

#相反，子集数据框或矩阵并不能保证导致数据框或矩阵（除非使用drop = FALSE）。如果结果是单列或单行，则对数据帧或矩阵进行子集将生成一个向量。
kids[,1]              # Result is "dropped" to a vector
## [1] 4 11

#数据整理动词
#dplyr软件包中的功能tidyverse旨在与小标题一起使用，但它们也与数据帧一起使用。当使用数据框调用时，只要可能，它们将返回一个数据框。

#目前，我们专注于最常用的函数，这些函数可以帮助整理和汇总数据。接下来是对这些动词的描述，每个后续部分专门介绍该动词的示例或几个动词的组合。

#filter()：根据有关其值的条件选择行
#summarize()：计算称为变量“摘要统计”的摘要度量
#group_by()：将观察值行分组在一起
#mutate()：通过更改现有变量在数据框中创建一个新变量
#arrange()：根据一个或多个变量对行进行排列/排序
#join()：通过沿“键”变量进行匹配来联接/合并两个数据框。有许多不同join()的。在这里，我们将重点介绍inner_join()功能。

#filter observation using filter
#该filter函数允许您指定有关数据集中变量值的条件，然后仅选择与该条件匹配的那些行。
#我们从返回flights数据nycflights13包中的数据框开始，仅关注从纽约市到俄勒冈州波特兰
#的航班。dest俄勒冈州波特兰的代码（或机场代码）为"PDX"。运行以下命令并查看生成的电子表格
#以确保此处仅选择前往波特兰的航班：
portland_flights <- flights %>% 
  filter(dest == "PDX")
head(portland_flights[,seq(-6,-12)])

#命令的顺序：
#然后取数据框flights
#filter数据帧，以便仅包含dest等于的那些数据"PDX"。
#==用于测试是否相等的双等号，而不是=。（几乎可以肯定会犯至少只包含一个等号一次的错误！）
#您可以使用进行比较的运算符将多个条件组合在一起：

#| 对应于“或”
#& 对应于“和”
#我们通常可以跳过使用&条件，而只用逗号分隔条件。您将在下面的示例中看到这一点。

#此外，您可以使用其他数学检查（类似于==）：

#> 对应于“大于”
#< 对应于“小于”
#>= 对应于“大于或等于”
#<= 对应于“小于或等于”
#!= 对应于“不等于”

View(flights)
#要查看其中的许多操作，让我们选择在10月，11月或12月离开肯尼迪国际机场前往伯灵顿，佛蒙特州（"BTV"）或华盛顿西雅图（）的所有航班"SEA"。运行以下命令
btv_sea_flights_fall=flights%>%
  filter(origin=="JFK",(dest=="BTV"|dest=="SEA"),month>=10)
head(btv_sea_flights_fall[,6:12])

#另一个示例使用!来选择不符合条件的行。该!可以理解为“不”。在这里，我们选择的行与未飞往佛蒙特州伯灵顿或华盛顿州西雅图的航班相对应。
not_BTV_SEA=flights%>%
  filter(!(dest=="BTV"|dest=="SEA"))
head(not_BTV_SEA[,-6:-12])
#We leave out columns 6-11 from the display so we can see the "origin" and "dest" variables)

#最后一点，我们指出，它filter()通常应该是将应用于数据的第一个动词。这会将数据缩小为您感兴趣的观测值。

#What’s another way using the “not” operator ! we could filter only the rows that are not going to Burlington nor Seattle in the flights data frame?
not_BTV_SEA=flights%>%
  filter(!dest=="BTV"&!dest=="SEA")
head(not_BTV_SEA[,-6:-12])

# Yet another way
not_BTV_SEA <- flights %>% 
  filter(dest != "BTV" & dest != "SEA")
head(not_BTV_SEA[,-6:-12])

#使用总结来汇总变量
#处理数据时，下一个常见任务是能够汇总数据：获取大量值并使用单个值汇总它们。
#尽管这似乎是一个非常抽象的想法，但总和，最小值和最大值之类的简单事物都是
#大量值的总和。

#我们可以计算出标准偏差和温度变量是指temp在weather的数据帧nycflights13使用在一个步骤中summarize（或等效使用英国的拼写summarise）函数dplyr
summary_temp=weather%>%
  summarize(mean=mean(temp),std_dev=sd(temp))
summary_temp

#您可以通过将na.rm参数设置为TRUE来汇总所有非缺失值（rm“ remove”的缩写）。这将删除所有NA缺失值，并且仅返回所有非缺失值的汇总值。因此，以下代码将计算所有非缺失值的均值和标准差。请注意，如何将na.rm=TRUE和设置为mean()和sd()函数而不是summarize()函数的参数
summary_temp <- weather %>% 
  summarize(mean = mean(temp, na.rm = TRUE), std_dev = sd(temp, na.rm = TRUE))
summary_temp

#我们可以在summarize()动词中使用哪些其他摘要功能？R中的任何函数都采用值的向量并且仅返回一个。这里仅仅是少数：
#mean()：平均值或平均值
#sd()：标准差，它是对价差的度量
#min()和max()：分别为最小值和最大值
#IQR()：四分位间距
#sum()： 总和
#n()：每个组中行/观测的数量的计数。group_by()在下一节中使用时，此特定的摘要功能将更有意义

#1 假设有一位医生正在研究吸烟对肺癌的影响，这些患者的病史每隔五年进行一次测量。她注意到大量患者因死亡而缺少数据点，因此她选择在分析中忽略这些患者。这名医生的治疗方法有什么问题？

#2 Modify the code above to create summary_temp to also use the n() 
#summary function: summarize(count = n()). What does the returned value correspond to?
weather %>% 
  summarize(count = n())
#It corresponds to a count of the number of observations/rows.
#We can check this using the dim() function which returns the dimensions (rows and columns)
dim(weather)

#3 为什么以下代码不起作用？提示：逐行而不是一次运行代码，然后查看数据。换句话说，weather %>% summarize(mean = mean(temp, na.rm = TRUE))首先运行并查看其产生的结果
summary_temp <- weather %>%   
  summarize(mean = mean(temp, na.rm = TRUE)) %>% 
  summarize(std_dev = sd(temp, na.rm = TRUE))

#Consider the output of only running the first two lines:
weather %>%   
  summarize(mean = mean(temp, na.rm = TRUE))
#Because after the first `summarize()`, the variable `temp` disappears 
#as it has been collapsed to the value `mean`. So when we try to run 
#the second `summarize()`, it can't find the variable temp` to compute 
#the standard deviation of. 

###Group rows using group_by
#基于另一个变量的分组总结一个变量通常是很有趣的。比方说，例如，我们对每个月的温度平均值和标准偏差很感兴趣。我们可以通过运行以下代码来生成此代码：

summary_monthly_temp <- weather %>% 
  group_by(month) %>% 
  summarize(mean = mean(temp, na.rm = TRUE), 
            std_dev = sd(temp, na.rm = TRUE))
summary_monthly_temp
View(summary_monthly_temp)

#如果我们要删除此组结构元数据，则可以将结果数据帧通过管道传递到ungroup()函数中。例如，假设组结构元数据通过设置为按月group_by(month)，则所有将来的摘要将按月报告。但是，如果我们不想再使用此数据，而是将所有汇总归入单个组中的所有数据（在这种情况下，是整个2013年），则通过管道传输有问题的数据框并将ungroup()其删除。

summary_monthly_temp <- weather %>% 
  group_by(month) %>% 
  ungroup() %>%
  summarize(mean = mean(temp, na.rm = TRUE), 
            std_dev = sd(temp, na.rm = TRUE))
summary_monthly_temp

#现在，我们重新访问n()在上一节中介绍的计数汇总功能。例如，假设我们想了解一下纽约市三个机场分别离开了多少个航班：
by_origin <- flights %>% 
  group_by(origin) %>% 
  summarize(count = n())
by_origin
#We see that Newark ("EWR") had the most flights departing in 2013 followed by "JFK" and lastly by LaGuardia ("LGA"). Note there is a subtle but important difference between sum() and n(). While sum() simply adds up a large set of numbers, the latter counts the number of times each of many different values occur.

#Grouping by more than one variable
#You are not limited to grouping by one variable! Say you wanted to know the number of flights leaving each of the three New York City airports for each month, we can also group by a second variable month: group_by(origin, month).
by_origin_monthly <- flights %>% 
  group_by(origin,month) %>% 
  summarize(count = n())
by_origin_monthly

#We see there are 36 rows to by_origin_monthly because there are 12 months times 3 airports (EWR, JFK, and LGA). Let’s now pose two questions. First, what if we reverse the order of the grouping i.e. we group_by(month, origin)?
by_monthly_origin <- flights %>% 
  group_by(month, origin) %>% 
  summarize(count = n())
by_monthly_origin

#二，为什么我们group_by(origin, month)不group_by(origin)然后group_by(month)？让我们调查一下：
by_origin_monthly_incorrect <- flights %>% 
  group_by(origin) %>% 
  group_by(month) %>% 
  summarize(count = n())
by_origin_monthly_incorrect
#此处发生的是第二个group_by(month)覆盖了第一个group_by(origin)，因此最后我们仅按进行分组month。这里的教训是，如果您想要group_by()两个或多个变量，则应在单个group_by()函数调用中包含所有这些变量。

#任务
#1 回想一下第1周的内容，当时我们查看了纽约市几个月内的温度曲线图。summary_monthly_temp数据框中的标准偏差列告诉我们全年纽约市的温度吗？
#The standard deviation is a quantification of spread or variability.
#We see that the period in November, December, and January has the most variation in weather, so you can expect very different temperatures on different days.

#2 Write code to produce the mean and standard deviation temperature for each day in 2013 for NYC?
summary_temp_by_day=weather%>%
  group_by(year,month,day)%>%
  summarize(
    mean=mean(temp,na.rm=T),
    std_dev=sd(temp,na.rm=T)
  )
summary_temp_by_day
# Note: group_by(day) is not enough, because `day` is a value between 1-31. 
# We need to `group_by(year, month, day)`

#3 Recreate by_monthly_origin, but instead of grouping via group_by(origin, month), group variables in a different order group_by(month, origin). What differs in the resulting data set?
by_monthly_origin <- flights %>% 
  group_by(month, origin) %>% 
  summarize(count = n())
by_monthly_origin
##The difference is they are organized/sorted by `month` first, then `origin`

#4 How could we identify how many flights left each of the three airports for each carrier?
# We could summarize the count from each airport using the `n()` function, 
# which *counts rows*.
count_flights_by_airport <- flights %>% 
  group_by(origin, carrier) %>% 
  summarize(count=n())
count_flights_by_airport
#Note: the `n()` function counts rows, whereas the `sum(VARIABLE_NAME)`
#funciton sums all values of a certain numerical variable `VARIABLE_NAME`.

#5 How does the filter operation differ from a group_by followed by a summarize?

#filter() picks out rows from the original data set without modifying them, whereas group_by %>% summarize computes summaries of numerical variables, and hence reports new values.

###
#使用mutate创建新变量/更改旧变量
flights=flights%>%
  mutate(gain=dep_delay-arr_delay)
View(flights)
#请注意，该mutate()命令输出一个由原始数据帧组成的新数据帧，并添加了新变量gain，该变量随后替换了原始flights数据帧。

#让我们来看看dep_delay，arr_delay和所产生的gain在我们的新的前5行变量flights使用一些新的数据帧dplyr功能，即select()和slice()：
flights%>%
  select(dep_delay,arr_delay,gain)%>%
  slice(1:5)

#让我们看一下该gain变量的汇总度量，并以直方图的形式对其进行绘制：
gain_summary=flights%>%
  summarize(
    min=min(gain,na.rm=T),
    q1 = quantile(gain, 0.25, na.rm = TRUE),
    median = quantile(gain, 0.5, na.rm = TRUE),
    q3 = quantile(gain, 0.75, na.rm = TRUE),
    max = max(gain, na.rm = TRUE),
    mean = mean(gain, na.rm = TRUE),
    sd = sd(gain, na.rm = TRUE),
    missing = sum(is.na(gain))
  )
gain_summary

#我们summary使用中的summarize功能重新创建了在第1周中看到的功能dplyr。
ggplot(data=flights,mapping=aes(x=gain))+
  geom_histogram(col="white",fill="skyblue",bins=20)

#我们还可以一次创建多个列，甚至可以引用刚在新列中创建的列。
flights=flights%>%
  mutate(
    gain=dep_delay-arr_delay,
    hours=air_time/60,
    gain_per_hour=gain/hours
  )
flights%>%
  select(gain,hours,gain_per_hour)%>%
  slice(1:5) #切片1:5

#task 1
#What do positive values of the gain variable in flights correspond to? 
#What about negative values? And what about a zero value?

#Say a flight departed 20 minutes late, i.e. dep_delay = 20 then arrived 10 minutes late, i.e. arr_delay = 10. + Then gain = dep_delay - arr_delay = 20 - 10  = 10 is positive, so it “made up/gained time in the air”. + 0 means the departure and arrival delays were the same, so no time was made up in the air.
#We see in most cases that the gain is near 0 minutes.

#2 Could we create the dep_delay and arr_delay columns by simply subtracting dep_time from sched_dep_time and similarly for arrivals? Try the code out and explain any differences between the result and what actually appears in flights.
View(flights)
# No because you can't do direct arithmetic on times. 
# The difference in time between  12:03 and 11:59 is 4 minutes, 
# but `1203-1159 = 44`

#What can we say about the distribution of gain? 
#Describe it in a few sentences using the plot and the gain_summary data frame values.

#Ans: Most of the time the gain is between -25 and 25 minutes.
#There are some extreme cases however, e.g. min(flights$gain, na.rm=T) returns -196 and max(flights$gain, na.rm=T) returns 109.

###
#Reorder the data frame using arrange

#Let’s suppose we were interested in determining the most frequent destination airports from New York City in 2013:
freq_dest=flights%>%
  group_by(dest)%>%
  summarize(num_flights=n())
freq_dest

#您会看到，默认情况下，dest此处的值以字母顺序显示。我们对寻找出现次数最多的机场感兴趣：
freq_dest%>%
  arrange(num_flights)

#实际上，这与我们所寻找的相反。它首先告诉我们最不频繁的目的地机场。要将顺序切换为降序而不是升序，我们使用desc（desc结束）函数：
freq_dest %>% 
  arrange(desc(num_flights))

###
#连接数据框
#另一个常见的任务是合并或合并两个不同的数据集。例如，在flights数据中，变量carrier列出了不同航班的航空公司代码。虽然"UA"和"AA"可能有点容易被猜到的一些（中国和美国航空公司），什么是“VX”，“HA”和“B6”？此信息在单独的数据帧中提供airlines
head(airlines)

#在flights和中airlines，我们要联接/合并/匹配两个数据框的键变量在两个数据集中具有相同的名称：carriers。我们利用该inner_join()函数通过变量进行连接carrier。
flights_joined=flights%>%
  inner_join(airlines,by="carrier")
names(flights)
names(flights_joined)

flights_joined %>% select(flight, carrier, name) 
#我们观察到flights和flights_joined相同，除了flights_joined具有一个附加变量，name其值来自airlines。

head(airports)
#但是，通过查看上图中数据框之间的airports和flights和的可视化表示，我们可以看到：
#airports 机场代码在变量中 faa
#flights 出发机场代码在变量中 origin
#因此，要结合这两个数据集，我们的inner_join操作涉及一个by说明不同名称的参数：

flights%>%
  inner_join(airports,by=c("dest"="faa"))

#让我们构造命令序列，该命令序列计算从纽约市到每个目的地的航班数量，但还包括有关每个目的地机场的信息：
View(flights)
named_dests=flights%>%
  group_by(dest)%>%
  summarize(num_flights=n())%>%
  arrange(desc(num_flights))%>%
  inner_join(airports,by=c("dest"="faa"))%>%
  rename(airport_name=name)
named_dests

#如果您不知道，这"ORD"是芝加哥奥黑尔机场的机场代码，"FLL"也是佛罗里达州劳德代尔堡的主要机场，现在我们可以在named_dests数据框中看到该机场。

#通过多个“关键”变量进行联接
#假设我们处于一种需要通过多个变量联接的情况。例如，在本节中的第一个图，我们看到，为了加入flights和weather数据帧，我们需要一个以上的关键变量：year，month，day，hour，和origin。这是因为这5个变量的组合可唯一标识weather数据框中的每个观测单位：纽约3个机场中每个机场的每小时气象记录。
flights_weather_joined <- flights %>%
  inner_join(weather, by = c("year", "month", "day", "hour", "origin"))
head(flights_weather_joined[,c(1:4,10:11,22:32)])

#在加入时看着在本节中，第一个数字flights和weather（或，换句话说，每个飞行匹配每小时的天气值），为什么我们需要所有的加盟year，month，day，hour，和origin，而不是仅仅hour？
#Because hour is simply a value between 0 and 23;
#to identify a specific hour, we need to also know which year, month, day and at which airport.

#other verbs
#我们已经看到，包中的flights数据框nycflights13包含许多不同的变量。该names函数列出了数据框中的所有列。在我们的情况下，你会跑的names(flights)。您还可以通过运行包中的glimpse函数来标识这些变量dplyr：
glimpse(flights)

#但是，假设您只想考虑其中两个变量，例如carrier和flight。您可以select：
flights%>%
  select(carrier,flight)

flights_no_year <- flights %>% 
  select(-year)
names(flights_no_year)

#或者我们可以指定列的范围：
flight_arr_times <- flights %>% 
  select(month:dep_time, arr_time:sched_arr_time)
flight_arr_times

#该select功能还可以与everything辅助功能一起用于重新排序列。假设我们想要的hour，minute和time_hour变量，它们出现在年底flights的数据集，后竟立即出现day变量：
flights_reorder <- flights %>% 
  select(month:day, hour:time_hour, everything())
names(flights_reorder)

#在这种情况下，将everything()拾取所有剩余的变量。最后，辅助函数starts_with，ends_with和contains可用于选择与这些条件匹配的变量/列名：
flights_begin_a=flights%>%
  select(starts_with("a"))
head(flights_begin_a)

flights_delays <- flights %>% 
  select(ends_with("delay"))
head(flights_delays)

flights_time <- flights %>% 
  select(contains("time"))
head(flights_time)

#使用重命名来重命名变量
#另一个有用的功能是rename，您可能会怀疑它会将一列重命名为另一名称。
#假设我们想要在数据框中成为，dep_time然后arr_time成为：departure_timearrival_timeflights_time
flights_time <- flights %>% 
  select(contains("time")) %>% 
  rename(departure_time = dep_time,
         arrival_time = arr_time)
names(flights_time)

#请注意，在这种情况下，我们在上使用了单个=符号rename()。例如。departure_time = dep_time。这是因为我们没有像我们将要使用的那样测试相等性==，而是希望分配一个新变量departure_time以具有与相同的值dep_time，然后删除该变量dep_time。

#很容易忘记新名称是在等号之前还是之后。我通常将其记为“新的之前，旧的之后”或NBOA。如果尝试以其他方式执行此操作，则会收到错误消息：

#使用top_n查找值的最高数量
#我们还可以使用top_n自动告诉我们最常出现的功能num_flights。我们在此处指定前10个机场：
named_dests%>%
  top_n(n=10,wt=num_flights)

named_dests  %>% 
  top_n(n = 10, wt = num_flights) %>% 
  arrange(desc(num_flights))

?top_n
#注意：请记住，我并没有突然提出nandwt参数。可以使用上的?函数找到它们top_n。

#我们可以再进一步一步，将用于查找最频繁航班的group_byandsummarize函数结合在一起：

ten_freq_dests <- flights %>%
  group_by(dest) %>%
  summarize(num_flights = n()) %>%
  arrange(desc(num_flights)) %>%
  top_n(n = 10) 
ten_freq_dests

#任务
#1 What are some ways to select all three of the dest, air_time, and distance variables from flights? Give the code showing how to do this in at least three different ways.
# The regular way:
flights %>% 
  select(dest, air_time, distance)

# Since they are sequential columns in the dataset
flights %>% 
  select(dest:distance)

# Not as effective, by removing everything else
flights %>% 
  select(-year, -month, -day, -dep_time, -sched_dep_time, -dep_delay, -arr_time,
         -sched_arr_time, -arr_delay, -carrier, -flight, -tailnum, -origin, 
         -hour, -minute, -time_hour)

#2 How could one use starts_with, ends_with, and contains to select columns from the flights data frame? Provide three different examples in total: one for starts_with, one for ends_with, and one for contains.
# Anything that starts with "d"
flights %>% 
  select(starts_with("d"))
# Anything related to delays:
flights %>% 
  select(ends_with("delay"))
# Anything related to departures:
flights %>% 
  select(contains("dep"))

#3 Create a new data frame that shows the top 5 airports with the largest average arrival delays from NYC in 2013.
top5_arr_delays <- flights %>%
  group_by(dest) %>%
  summarize(mean_arr_delay = mean(arr_delay, na.rm=T)) %>%
  arrange(desc(mean_arr_delay)) %>%
  top_n(n = 5)
View(flights)
top5_arr_delays %>% inner_join(airports %>% select(faa, name), by = c("dest" = "faa") )

#persudo code
#https://twitter.com/rudeboybert/status/964181298691629056
#使用nycflights13包装中包含的数据集，计算按降序排列的每个航空公司的可用座位里程。完成所有必要的数据整理步骤后，结果数据框应具有16行（每家航空公司一个）和2列（航空公司名称和可用的座位英里数）。这里有一些提示：
View(planes)
View(flights)
flights%>%
#Step 1: To compute the available seat miles for a given flight, we need the distance variable from the flights data frame and the seats variable from the planes data frame, necessitating a join by the key variable tailnum. To keep the resulting data frame easy to view, we’ll select() only these two variables and carrier:
  inner_join(planes,by="tailnum")%>%
  select(carrier,seats,distance)%>% 
#Step 2: Now for each flight we can compute the available seat miles ASM by multiplying the number of seats by the distance via a mutate():
  mutate(ASM=seats*distance)%>%
#Step 3: Next we want to sum the ASM for each carrier. We achieve this by first grouping by carrier and then summarizing using the sum() function:
  group_by(carrier)%>%
#Step 4: However, if it was the case that some carriers had certain flights with missing NA values, the resulting table above would also returns NA’s
  summarize(ASM=sum(ASM,na.rm=T))%>%
#Finally, we arrange() the data in desc()ending order of ASM.
  arrange(desc(ASM))

library(tidyverse)
library(nycflights13)
library(fivethirtyeight)

#Further Task 1
#In this task we will work with the data set analysed and reported in the 2016 article from FiveThirtyEight.com entitled “Some People Are Too Superstitious To Have A Baby On Friday The 13th” here.
  
#1 Create an object called US_births_2013 which focuses only on data corresponding to 2013 births.  
str(US_births_2000_2014)
US_births_2013=US_births_2000_2014%>%
  filter(year==2013)

#2 By only choosing births data for the years 2010, 2011, 2012, and 2014 create a new dataframe called
#US_births_small and check that this resulting data frame has 1461 rows. Note that there are many
#different ways to do this, but try and come up with three different ways using:
US_births_small <- US_births_2000_2014 %>%
  filter(year %in% c(2010, 2011, 2012, 2014))
US_births_small <- US_births_2000_2014 %>%
  filter(!(year %in% c(2000:2009, 2013, 2015)))
US_births_small <- US_births_2000_2014 %>%
  filter(year == 2010 | year == 2011 | year == 2012 | year == 2014)

#3 Suppose we are interested in choosing rows for only weekdays (not Saturdays or Sundays) for
#day_of_week in year 2013. Write the code to do so and give the name US_births_weekdays_2013
#to the resulting data frame. (Note that you may want to run US_births_2000_2014 %>%
#distinct(day_of_week) to identify the specific values of day_of_week.)
US_births_2000_2014 %>%
  distinct(day_of_week)

US_births_2013=US_births_2000_2014%>%
  filter(!(day_of_week %in% c("Sat","Sun")),year==2013)
# You could also use & instead of the last comma, i.e.
US_births_weekdays_2013 <- US_births_2000_2014 %>%
  filter(!(day_of_week %in% c('Sat','Sun')) & year==2013)

#4. Using what you covered in Week 1: Visualization, produce an appropriate plot looking at the pattern
#of births on all weekdays in 2013 coloured by the particular day of the week. (Remember to load the
library(ggplot2)
ggplot(US_births_weekdays_2013, aes(x=date,y=births, color=day_of_week))+
  geom_line()+
  labs(x='Date', y='Number of births',
       title='Number US births on week days in 2013')
#5. The plot in the previous task has shown there are some outliers in the data for US births on weekdays in
#2013. We can use the summarize function to get an idea for how these outliers may affect the shape of
#the births variable in US_births_weekdays_2013. Write some code to calculate the mean and median
#values for all weekday birth totals in 2013. Store this aggregated data in the data frame birth_summ.
#What do these values suggest about the effects of the outliers?
birth_summ=US_births_weekdays_2013%>%
  summarize(mean_birth=mean(births),
            median_births=median(births))
birth_summ
#Comparing the mean and the median values we see they are not very different,
#showing that the potential outliers are not distorting the shape of

#6. Instead of looking at the overall mean and median across all of 2013 weekdays, calculate the mean
#and median for each of the five different weekdays throughout 2013. Using the same names for the
#columns as in the birth_summ data frame in the previous exercise, create a new data frame called
#birth_day_summ.
birth_day_summ=US_births_weekdays_2013%>%
  group_by(day_of_week)%>%
  summarize(mean_births=mean(births),
            median_births=median(births))
birth_day_summ

#7. Using the aggregated data in the birth_day_summ data frame, produce this barplot.
ggplot(birth_day_summ,aes(x=day_of_week,y=mean_births,fill=day_of_week))+
  geom_col()+
  labs(x='Day of Week', y='Average number of births',
       title='Average US births by weekday, 2013')+
  guides(fill=FALSE)

#FT 2
#1. Write code to determine the proportion of respondents in the survey that responded with “Very” when
#asked if a passenger reclining their seat was rude. You should determine this proportion across the
#different levels of age and gender resulting in a data frame of size 8 x 3. Assign the name prop_very
#to this calculated proportion in this aggregated data frame.
View(flying)

two_group_prop_1 <- flying %>%
  group_by(gender,age) %>%
  summarize(prop_very = mean(recline_rude=='Very', na.rm=TRUE))
#or
two_group_prop_2 <- flying %>%
  na.omit(TRUE) %>%
  group_by(gender,age) %>%
  summarize(prop_very = mean(recline_rude=='Very'))

#Using the aggregated data you’ve created, produce two bar plots (one stacked, the other side-by-side)
#to show the differences between the sexes of the proportion of people who believe reclining your seat is
#‘very’ rude, within each age group

ggplot(two_group_prop_2, aes(x=age,y=prop_very,fill=gender))+geom_col()

ggplot(two_group_prop_2, aes(x=age,y=prop_very,fill=gender))+geom_col(position = "dodge")

