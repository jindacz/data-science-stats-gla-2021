#das_lab_1
install.packages("ggplot2") #Only include if the package hasn't already been installed
library(ggplot2)
install.packages("nycflights13") #Only include if the package hasn't already been installed
library(nycflights13)

library(ggplot2)
library(nycflights13)
print("Well done!  You've loaded the libraries")

help("package=nycflights13")

head(flights,n=3)

dim(flights)
glimpse(flights)

Alaska=flights[flights$carrier=="AS",]

Alaska[1:5,]
#answer
head(Alaska,n=5)

dim(Alaska)
library(tidyverse)
?glimpse
glimpse(c(1,1))

# First, we can set up the plotting region for our scatterplot of departure against arrival delays as follows:
ggplot(data = Alaska, mapping = aes(x = dep_delay, y = arr_delay)) + 
  geom_point() +
  labs(x = "Departure delay (minutes)", y = "Arrival delay (minutes)",
       title = "Alaska Airlines flights leaving NYC in 2013") 

ggplot(data = Alaska, mapping = aes(x = dep_delay, y = arr_delay)) + 
  geom_point(alpha = 0.2) +
  labs(x = "Departure delay (minutes)", y = "Arrival delay (minutes)",
       title = "Alaska Airlines flights leaving NYC in 2013")      

#该alpha命令的范围是0到1，其中0表示100％透明度，而1（默认值）将点设置为100％不透明。
#通过更改点的透明度级别，我们可以观察到彼此接近的点簇，因为它们将比点聚在一起的点较少的图区域更暗。

jitter.example=matrix(0,nrow=10,ncol=2)
jitter.example       
jitter.example=as.data.frame(jitter.example)       
?is.matrix
?is.data.frame


ggplot(data=jitter.example,mapping=aes(x=V1,y=V2))+
  geom_point()

#请注意，由于更改jitter.example为数据框，因此为列指定了默认变量名称V1和V2。
#从图中可以看出，如果您从未见过我们的玩具示例，您会认为仅绘制了一个值，因为它们都为零。
#如果我们使用抖动稍微移动每个点，我们将能够更清楚地看到它们：

ggplot(data = jitter.example, mapping = aes(x = V1, y = V2)) + 
  geom_jitter(width = 0.1, height = 0.1)


ggplot(data=Alaska,mapping=aes(x=dep_delay,y=arr_delay))+
  geom_jitter(width=30,height=30)+
  labs(x = "Departure delay (minutes)", y = "Arrival delay (minutes)",
       title = "Alaska Airlines flights leaving NYC in 2013")

#散点图矩阵
#程序包的ggpairs()功能将GGally生成一个散点图矩阵，其中将生成几个数值变量之间的成对关系，以及它们的单变量分布和相关系数的密度估计。我们将使用来自www.r-graph-gallery.com的此代码进行说明。
install.packages("GGally")
library(GGally)

#create data
data=data.frame(var1=1:100+rnorm(100,sd=20),v2=1:100+rnorm(100,sd=27),
                v3=rep(1,100)+rnorm(100,sd=1))
data$v4=data$var1**2
data$v5=-(data$var1**2)

#check corr(as scatterplots),distribution and print correlation coefficient
ggpairs(data,title="scatterplot matrix w/ ggpairs")

#直方图
#直方图使我们可以查看变量的统计分布。它们向我们显示了在指定的bin中有多少个变量值。这些bin给出了变量所位于的值的范围。可以更改垃圾箱，即通过更改垃圾箱的宽度或增加垃圾箱的数量，以使我们看到更高分辨率的分布。

#在这里，让我们看一下库中的weather数据集nycflights13。该数据集包含2013年纽约市三个机场（LGA，JFK和EWR）的每小时天气数据。我们可以通过以下方式查看其内容：

#要使用ggplot我们创建geom_histogram命令直方图，请使用命令或图层，而不是geom_point使用散点图。我们可以使用以下方法创建2013年纽约市每小时温度数据的直方图ggplot：

head(weather,n=3)
ggplot(data=weather,mapping=aes(x=temp))+
  geom_histogram()

#在这里，ggplot告诉我们创建直方图时它使用了30个bin。我们可以ggplot使用两种不同的方法来调整垃圾箱。通过调整

#使用bins参数的垃圾箱数量；或者
#使用binwidth参数的垃圾箱宽度。
#首先让我们从指定箱数开始，如下所示：

ggplot(data=weather,mapping=aes(x=temp))+
  geom_histogram(bins=60,color="white")

#注意，我们还指定了垃圾箱的轮廓颜色，以便于区分它们。可以通过包含fill参数来更改垃圾箱本身的颜色。通过在R控制台中键入以下内容，可以找到657种可用的颜色选项：
colors()

#binwith to control width
ggplot(data=weather,mapping=aes(x=temp))+
  geom_histogram(binwidth=5,color="white")

#boxplot
summary(weather$temp)

weather$month
factor(weather$month)

ggplot(data=weather,mapping=aes(x=factor(month),y=temp))+
  geom_boxplot(fill="steelblue")+
  labs(x = "Month", y = "Temperature (Hourly)",
       title = "Hourly temperatures from NYC in 2013 by month")+
  scale_x_discrete(labels=c("Jan","Feb","Mar","Apr","May","Jun",
                            "Jul","Aug","Sep","Oct","Nov","Dec"))

#注意，我们引入了一个新函数scale_x_discrete，该函数用于重命名箱线图的标签。使用此函数是因为我们的分类变量本质上是离散的。

#在按小时划分小时温度后，我们现在看到的点超出了箱线图的晶须。这些被称为离群值，并且可能被认为是异常小的值或很大的值。但是，此处异常值的定义有些随意，因为它们由晶须的长度（不超过1.5 x IQR）定义。


#barplot
carrier.freq=table(flights$carrier)
carrier.freq=as.data.frame(carrier.freq)
colnames(carrier.freq)=c("carrier","number")
ggplot(data=carrier.freq,mapping=aes(x=carrier,y=number))+
  geom_col()

#Barplots也可以用来比较两个类别变量。例如，假设我们要查看2013年从每个承运商和每个机场（LGA，JFK和EWR）飞出纽约市的航班数量。要获取此信息的表格，我们只需将航班起点添加到上一张表格中，如下所示：
carrier.origin=table(flights$origin,flights$carrier)
carrier.origin=as.data.frame(carrier.origin)
colnames(carrier.origin)=c("origin","carrier","number")

#在这种情况下，用于比较两个类别变量的条形图非常相似，我们只需将附加fill参数传递给aes函数即可。包含fill参数使ggplotplot知道我们要根据附加分类变量来分割barplot，origin在这种情况下就是这样。然后，该小图由下式给出：
ggplot(data = carrier.origin, mapping = aes(x = carrier, y = number, fill = origin)) +
  geom_col() +
  labs(x = "Carrier", y = "Count",
       title = "Carriers who flew out of New York City in 2013")

#这就是所谓的堆叠式条形图，因为每个条形图的条形图origin都简单地堆叠在每个载体上。您可以通过添加一个额外的图层，使用两个类别变量来控制堆积的条形图中的颜色+ scale_fill_manual(values = c(...))。

ggplot(data = carrier.origin, mapping = aes(x = carrier, y = number, fill = origin)) +
  geom_col(position="dodge") +
  labs(x = "Carrier", y = "Count",
       title = "Carriers who flew out of New York City in 2013")

#最后，让我们看一下所谓的多面barplot。它们提供了一种按来源比较载波分布的简便方法，可以通过以下方式获得：
ggplot(data = carrier.origin, mapping = aes(x = carrier, y = number, fill = origin)) +
  geom_col() +
  facet_wrap(~ origin, ncol = 1) +
  labs(x = "Carrier", y = "Count",
       title = "Carriers who flew out of New York City in 2013") 

#线图
#线图通常在查看时间序列数据时使用，也就是说，当我们掌握有关变量如何随时间变化的信息时。
#因此，在观察某些事物随时间的变化时，数据具有自然排序，因此，如果没有变量的顺序排序，则应避免使用线图。
#让我们再次查看每小时的温度数据，但这一次仅针对1月份的纽瓦克国际机场。这可以通过首先如下子设置数据来完成：
Newark.Jan=weather[weather$origin=="EWR"&weather$month==1,]
ggplot(data = Newark.Jan, mapping = aes(x = time_hour, y = temp)) +
  geom_line() +
  labs(x = "Time (Hours)", y = "Temperature",
       title = "Hourly Temperature at Newark Airport in January 2013") 

#在继续下一部分之前，请考虑以下事项：
#在上面的线图代码中，为什么time_hour绘制在x轴上而不是绘制在x轴上hour？
#没有数据的顺序排序时，为什么要避免折线图？
#当时间是解释变量时，为什么要使用线图？
#描述2013年1月纽瓦克国际机场每小时温度的趋势。

#在页面上排列多个图
#通常，我们需要生成不同类型的图并将它们显示在一起。
#包中的grid.arrange()函数gridExtra是执行此操作的一种方法，
#如上面的代码所示，该代码使用上面我们看到的图：

library(gridExtra)

p1 <- ggplot(data = Alaska, mapping = aes(x = dep_delay, y = arr_delay)) + 
  geom_jitter(width = 30, height = 30) +
  labs(x = "Departure delay (minutes)", y = "Arrival delay (minutes)",
       title = "Alaska Airlines flights leaving NYC in 2013") + 
  theme(plot.title = element_text(size=7))

p2 <- ggplot(data = weather, mapping = aes(x = factor(month), y = temp)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "Month", y = "Temperature (Hourly)",
       title = "Hourly temperatures from NYC in 2013 by month")  +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+ 
  theme(plot.title = element_text(size=7))

p3 <-  ggplot(data = carrier.freq, mapping = aes(x = carrier, y = number)) +
  geom_col() +
  labs(x = "Carrier", y = "Count",
       title = "Carriers who flew out of New York City in 2013") + 
  theme(plot.title = element_text(size=7))

p4 <-  ggplot(data = Newark.Jan, mapping = aes(x = time_hour, y = temp)) +
  geom_line() +
  labs(x = "Time (Hours)", y = "Temperature",
       title = "Hourly Temperature at Newark Airport in January 2013") + 
  theme(plot.title = element_text(size=7))

grid.arrange(p1, p2, p3, p4, ncol=2)

#https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html


#Further Tasks
#The tasks below should be answered by creating your own .R script file (hence no inbuild R consoles are included below).

#The first step is to load into R all of the libraries you will need. This can be done by typing/copying and running the following code in your R script:
  
library(ggplot2)
library(nycflights13)

#1. From the flights data set, subset the data for the airline carrier JetBlue Airways and produce a
#scatterplot of their departure delays against arrival delays using ggplot. Interpret the scatterplot.
airlines
JetBlue=flights[flights$carrier=="B6",]
ggplot(data=JetBlue,mapping=aes(x=dep_delay,y=arr_delay))+
  geom_point()+
  labs(x = "Departure delay (minutes)", y = "Arrival delay (minutes)",
       title = "JetBlue Airways flights leaving NYC in 2013")

#2. Produce a histogram of the hourly temperature from Newark Liberty International (EWR) Airport in
#2013 using ggplot. How does the temperature distribution compare with that from all airports in New
#York City in 2013?
EWR.temps=weather[weather$origin=="EWR",]
ggplot(data=EWR.temps,mapping=aes(x=temp))+
  geom_histogram(bins=60,col="white")+
  labs(x = "Temperature", y = "Frequency",
       title = "Hourly temperature at Newark Liberty International Airport in 2013")
#bins tells R how many bars to construct 

#3. For John F. Kennedy Airport, produce boxplots (using a single ggplot command) of the hourly
#temperature for the months May, June, July, August and September. How does the hourly temperature
#change during this period?
JFK.temps <- weather[weather$origin == "JFK" & weather$month %in% 5:9, ]
ggplot(data = JFK.temps, mapping = aes(x = factor(month), y = temp)) +
  geom_boxplot(fill = "blue") +
  labs(x = "Month", y = "Temperature (Hourly)",
       title = "Hourly temperature at John F. Kennedy Airport in 2013") +
  scale_x_discrete(labels = c("May", "June", "July", "August", "September"))

#Note, we have introduced a new function scale_x_discrete, 
#which is used to rename the labels of the boxplots. 
#This function is used as our categorical variables are discrete in nature.

#4. Take a look at the mtcars data set within the datasets library relating to data extracted from the
#1974 Motor Trend US magazine. Using ggplot, produce a faceted barplot of the categorical variables
#relating to the number of cylinders (cyl) and the automobiles transmission (am). Interpret the barplot.
mt.cyls=as.data.frame(table(mtcars$cyl,mtcars$am))
colnames(mt.cyls)=c("cylinders","transmission","number")
ggplot(data=mt.cyls,mapping=aes(x=cylinders,y=number,fill=transmission))+
  geom_col()+
  facet_wrap(~transmission,ncol=1)
labs(x = "Cylinders", y = "Count",
     title = "Number of cylinders by transmission from the 1974 Motor Trend US magazine")

#5. Produce a linegraph of the hourly temperature at LAGuardia (LGA) Airport for the month of October
#2013. Interpret the linegraph.
LGA.Oct <- weather[weather$origin == "LGA" & weather$month == 10, ]
ggplot(data=LGA.Oct,mapping=aes(x=time_hour,y=temp))+
  geom_line()+
  labs(x = "Time (Hours)", y = "Temperature",
       title = "Hourly Temperature at LAGuardia Airport in October 2013")





