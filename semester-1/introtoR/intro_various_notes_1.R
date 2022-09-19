load(url("http://www.stats.gla.ac.uk/~levers/rp/chol.RData"))
chol
chol$log.hdl.ldl <- log(chol$hdl/chol$ldl)

#Q1
#Define a vector of length 7 called missing where each element cor. to a column
#in starwars containning the average number of missing value in that column.
#The elements of the vector should be named to show the cor. names of the columns.

missing=colSums(is.na(starwars))


#Q2
  kids <- data.frame(age=c(4,11), weight=c(15,28), height=c(101,132),
                                                     gender=c("f", "m"))
rownames(kids) <- c("Sarah", "John") 
#and I want to create a colum called BMI, how could I use apply funtion to do that?
kids=transform(kids,BMI=weight/age)

1. apply
查看R语言中apply函数的帮助文档，对apply函数是这样说明的：
Returns a vector or array or list of values obtained by applying a function to margins of an array or matrix.
这句话的意思就是apply会把一个函数同时作用于一个数组或者矩阵的一个margin，然后返回值存在一个向量或者数组中，也就是说把每一个margin作为一个函数的输入，对应一个输出，所有的输出放在一起返回来。
那么这个margin如何理解？margin可以是数组的每一行／每一列。值得注意的是这里的数组未必是2维的，更高维也可以。
一个具体例子，求一个2维数组每一列和每一行的平均值：
> x=array(rnorm(12),c(3,4))
> x
[,1]       [,2]      [,3]       [,4]
[1,]  0.8972996 -0.8049946 -1.870765 -0.5850348
[2,]  0.7341293 -0.3148228  1.023150 -0.2756939
[3,] -2.0388308 -2.7536031 -1.500011  0.9308724
> y=apply(x,1,mean)
> y
[1] -0.5908738  0.2916906 -1.3403932
> y=apply(x,2,mean)
> y
[1] -0.13580066 -1.29114016 -0.78254226  0.02338122
apply第一个参数是输入数据。
apply第二个参数，指定是哪一种margin，1对应每一列，2对应每一行。
如果有更高维，以此类推，比如对3维情况：
> x=array(c(1:24),c(2,3,4))
> x
, , 1

[,1] [,2] [,3]
[1,]    1    3    5
[2,]    2    4    6

, , 2

[,1] [,2] [,3]
[1,]    7    9   11
[2,]    8   10   12

, , 3

[,1] [,2] [,3]
[1,]   13   15   17
[2,]   14   16   18

, , 4

[,1] [,2] [,3]
[1,]   19   21   23
[2,]   20   22   24

> apply(x,1,mean)
[1] 12 13
> apply(x,2,mean)
[1] 10.5 12.5 14.5
> apply(x,3,mean)
[1]  3.5  9.5 15.5 21.5

以上例子，apply(x,1,mean)计算的是1到23的所有奇数的平均值。apply第三个参数，指定了具体应用什么函数，上面例子计算mean，其实可以用更简单办法，colMeans和rowMeans函数，那如果是一个自己写的函数呢？看下面例子：
> myFun=function(x){sum(x^2)}
> apply(x,1,myFun)
[1]  5.295192  1.760902 14.855718
上面例子计算每一行的平方和。
如果一个函数fun1有多个参数，margin只是这个函数的一个参数，想固定其他参数怎么办？很简单，自己定义一个只有一个参数的新函数fun2，在fun2里面调用fun1，并且对其他参数赋值，然后把fun2传递给apply。

通常情况大家使用apply之后是需要把apply的返回值作为输入在其他代码中使用的，这里尤其重要一点是apply的返回值的维度。上面例子计算每一行／列的mean，使用apply之后返回的都是一个向量，并不会因为apply计算行（列）的mean就会自动返回一个列（行）向量。
下面我们用apply函数来实现一个R的内置函数scale，就是标准化一个数组／矩阵。具体来说，是把每一列先减去这一列的中心，然后除以这一列的标准差。
> x=array(1:20,c(4,5))
> x
[,1] [,2] [,3] [,4] [,5]
[1,]    1    5    9   13   17
[2,]    2    6   10   14   18
[3,]    3    7   11   15   19
[4,]    4    8   12   16   20
> scale(x,center = T,scale = T)
[,1]       [,2]       [,3]       [,4]       [,5]
[1,] -1.1618950 -1.1618950 -1.1618950 -1.1618950 -1.1618950
[2,] -0.3872983 -0.3872983 -0.3872983 -0.3872983 -0.3872983
[3,]  0.3872983  0.3872983  0.3872983  0.3872983  0.3872983
[4,]  1.1618950  1.1618950  1.1618950  1.1618950  1.1618950
attr(,"scaled:center")
[1]  2.5  6.5 10.5 14.5 18.5
attr(,"scaled:scale")
[1] 1.290994 1.290994 1.290994 1.290994 1.290994
> 
  > myScale=function(x){
    + x.Mean=apply(x,2,mean)
    + x.sd=apply(x,2,sd)
    + #注意下面一行代码
      + t((t(x)-x.Mean)/x.sd)
    + }
> myScale(x)
[,1]       [,2]       [,3]       [,4]       [,5]
[1,] -1.1618950 -1.1618950 -1.1618950 -1.1618950 -1.1618950
[2,] -0.3872983 -0.3872983 -0.3872983 -0.3872983 -0.3872983
[3,]  0.3872983  0.3872983  0.3872983  0.3872983  0.3872983
[4,]  1.1618950  1.1618950  1.1618950  1.1618950  1.1618950
可见myScale于scale函数返回值一样。
在上面例子中，最关键的代码是
t((t(x)-x.Mean)/x.sd)
因为在R语言中，一个矩阵和一个向量运算，向量是作为列向量，与矩阵的每一个元素（从一列开始，而不是第一行）进行运算。所以我们先转置矩阵，变成矩阵的元素按行来与向量的元素运算，然后把结果再转置回去。
2. lapply和sapply
这两个apply函数很像，都是应用于一个vector/list上面，上面的apply是用于一个数组／矩阵。所以通常apply需要三个参数，而lapply/sapply一般需要两个参数，第一个参数是输入数据，第二个是函数。两者的区别在于返回值上面，sapply返回的是一个vector，但是lapply返回的是一个list，见下例：
> sapply(x,function(x) {x^2})
[1]  1  4  9 16 25
> x=c(1:5)
> sapply(x,function(x) {x^2})
[1]  1  4  9 16 25
> lapply(x,function(x) {x^2})
[[1]]
[1] 1

[[2]]
[1] 4

[[3]]
[1] 9

[[4]]
[1] 16

[[5]]
[1] 25

sapply还有更复杂的用法，要使用第三个参数。在上面例子中，sapply应用的函数返回值是一个元素，也就是一个数值，然后sapply把这些数值放在一个向量中返回，但是如果sapply应用的函数返回的不是一个元素呢？这时sapply会把返回值作为一个vector，见下面例子：
> sapply(1:5,function(x) matrix(x,2,2))
[,1] [,2] [,3] [,4] [,5]
[1,]    1    2    3    4    5
[2,]    1    2    3    4    5
[3,]    1    2    3    4    5
[4,]    1    2    3    4    5
显然上面例子的返回值并非我们想要的，要处理返回值是数组／矩阵的情况，在sapply里面使用第三个参数simplify：
> sapply(1:5,function(x) matrix(x,2,2), simplify = "array")
, , 1

[,1] [,2]
[1,]    1    1
[2,]    1    1

, , 2

[,1] [,2]
[1,]    2    2
[2,]    2    2

, , 3

[,1] [,2]
[1,]    3    3
[2,]    3    3

, , 4

[,1] [,2]
[1,]    4    4
[2,]    4    4

, , 5

[,1] [,2]
[1,]    5    5
[2,]    5    5


3. tapply
tapply通常也有三个参数，第一个指定输入数据，第二个是指定输入数据如何分组，第三个参数指定在每一个分组内，应用什么函数，所以tapply的功能就是把数据按照某种分组，在每个组内进行某个运算，见下面例子：
> medical.example
patient      age treatment
1        1 54.70208 Treatment
2        2 64.91654 Treatment
3        3 51.80260 Treatment
4        4 61.63020 Treatment
5        5 72.79920 Treatment
6        6 59.99898 Treatment
7        7 40.85752 Treatment
8        8 51.00623 Treatment
9        9 64.25137 Treatment
10      10 59.97824 Treatment
11      11 58.19639   Control
12      12 34.42546   Control
13      13 53.11569   Control
14      14 31.19479   Control
15      15 67.27431   Control
16      16 64.89839   Control
17      17 67.29181   Control
18      18 61.83809   Control
19      19 82.79484   Control
20      20 68.93832   Control
> tapply(medical.example$age,medical.example$treatment,mean)
Treatment   Control 
58.19430  58.99681


#################################################

a=rbind(c(-0.93,0.11,0.01),c(0.11,-1.1,-0.27),c(0.01,-0.27,-1.02))
b=c(0.01,1.21,0.91)
xs=-0.5*a%*%b
y=8.14-0.25*t(b)%*%a%*%b

det(a)


###########################
图形是进行数据的趋势观察和数据展示的一种很好的手段。R语言基本函数， plot函数，属于graphics包。

Ⅰ 可用参数：

type：表现a,b之间的关系的形式：

"p"：point；"l"：线，lines；"b"：断点为点，线连接，点线不相交，both；"c"：仅线，不连续；"o"：点、线且相交，overplot；"n"：空图；

pch：符号类型，如下图所示：


lty：控制连线的线型,可以是整数(1: 实线,2: 虚线,3: 点线,4: 点虚线,5: 长虚线,6: 双虚线)

bty：控制图形边框形状,可用的值为: "o", "l", "7", "c", "u" 和"]" (边框和字符 的外表相像),bty="n"则不绘制边框

box：在当前的图上加上边框

main：主标题

sub：副标题

xlab,ylab ：X Y坐标轴标题

xlim,ylim：X Y坐标轴范围

cex：控制缺省状态下符号和文字大小的值，用于表示对默认的绘图文本和符号放大多少倍。

cex.axis 坐标轴刻度标记的缩放倍数

cex.lab 坐标轴标题的缩放倍数

cex.main 图主标题的缩放倍数

cex.sub 图副标题的缩放倍数

col 图中符号（点、线等）的颜色，与cex参数类似，具体如下：

col.axis 坐标轴刻度标记的颜色

col.lab 坐标轴标题的颜色

col.main 图主标题的颜色

col.sub 图副标题的颜色

cex.font：指定绘图使用的字体样式。 1=常规， 2=粗体， 3=斜体， 4=粗斜体， 5=符号字体

font.axis 坐标轴刻度文字的字体样式

font.lab 坐标轴标签（名称）的字体样式

font.main 标题的字体样式

font.sub 副标题的字体样式

family：设置文本的字体族（衬线、无衬线、等宽、符号字体等）；标准取值有：serif, sans, mono, symbol

Ⅱ 可添加简单函数

legend()：除了利用x,y设置图例的坐标外，用”topleft”， "center"，"bottomright"等设置位置非常方便。ncol设置图例的列数, horiz设置图例的排列方向。

las：坐标轴标签样式；取0、1、2、3,四个整数之一，分别表示“总是平行于坐标轴”、“总是水平”、“总是垂直于坐标轴”和“总是竖直”。

segments：(x0, y0,x1, y1)从(x0,y0)各点到(x1,y1)各点画线段

lend：线段的端点样式，参数值可以为一个整数或者一个字符串。参数值为0或者"round"时，表示端点样式为圆角（默认值）；为1或者"butt"时，表示端点直接截断；为2或者"square"表示延伸末端。

arrows(x0, y0,x1, y1)：箭头：code=1则在各(x1,y1)处画箭头，code=2则在各(x0,y0)处画箭头，code=3则在两端都画箭头; angle控制箭头轴到箭头边的角度；length箭头长度；

abline(h=y)在纵坐标y处画水平线，abline(v=x)在横坐标x处画垂直线；abline(a,b)绘制斜率为b和截距为a的直线；abline(lm.obj)画由lm.obj确定的回归线。

rect(x1, y1, x2, y2) ：绘制长方形,(x1, y1)为左下角,(x2,y2)为右上角

polygon(x, y)：绘制连接各x,y坐标确定的点的多边形

text()（绘图区内）在给定坐标的位置写字。text(x, y, labels,…)在(x,y)处添加用labels指定的文字;

srt:字符串旋转度数，只支持函数text。

mtext（绘图区外）为四个坐标轴添加标签。mtext(text,side=3, line=0,…)在边空添加用text指定的文字,用side指定添加到哪一边;line指定添加的文字距离绘图区域的行数，不够的话，可以mar参数调整

bg：设定绘图区域的背景颜色

fg ：设置前景色

X=cbind(c(1,1,1,1),c(9,4,7,1))
y=c(2,5,3,2)
t(X)%*%X
solve(t(X)%*%X)
t(X)%*%y
solve(t(X)%*%X)%*%t(X)%*%y

setwd("/Users/kurisuuu/Downloads")
data <- read.csv("data.csv")
x1=c(4,4.7,3,3.5,3.1,3.8,4.3,4.9,3.4)
x2=c(9,8.6,7.3,8.7,7.7,8.4,7,8.2,8)
X=cbind(x1,x2)
t(X)%*%X
y=c(1.61,1.84,1.78,1.93,2.48,1.52,1.9,2,2.4)
t(X)%*%y
solve(t(X)%*%X)%*%t(X)%*%y
m1=lm(y~x1+x2-1)
summary(m1)
anova(m1)

#create a vector called data.types that contains the names of
#the 12 unique sub datasets in the datasaurus_dozens data frame
?unique
x=c(1,2,3,1,1,11,1,1,1,4,5)
unique(x)

data.types=names(unique(datasaurus_dozens$dataset))

#修改坐标大小
plot(a,b,type="b",xlab="网络节点数目 x1000",ylab="算法时间消耗",
     col="blue",xlim=c(16,25),ylim=c(57,211),pch=15,xaxt="n",yaxt="n"）
     
     x=c(1,2,3,1,1,11,1,1,1,4,5)  
x[1]  
x[5]  


diag(1,2,nrow=2)
t(c(-1,1))%*%diag(c(2/3,1/3))%*%c(-1,1)


qnorm(1.1759) #inverse function
qnorm(0.3303)
pnorm(-1.1759) #f(z) function


x=rbind(c(6.859102,-0.445975969,-1.079557001),
        c(-0.445976,0.161895369,-0.009155426),
        c(-1.079557,-0.009155426,0.218577990))
b=c(1,4,6)
t(b)%*%x%*%b

t(27,0.975)
??tdistribution


To use the library, you need to do

library(datasets)


And then do, for example, data(airquality) (to call the dataset “airquality”). Same for all the other datasets. Remember that you can also do ?airquality to read the R documentation on the data.

I’d advice you to choose a dataset from here and start exploring the data, playing around and “asking questions.” For example, using the airquality data:
  
  
  
  # Create a new column, called Windkm with the wind in kmph:
  
  airquality$Windkm = airquality$Wind*1.60934



# What is the average wind speed in kmph?

mean(airquality$Windkm)



# Categorise the Windkm column into two categories, "low" (Windkm<15km) and "high" (Windkm>=15km). 

# For this, we create a new column:

airquality = transform(airquality, WindkmType = cut(Windkm, breaks = c(-Inf,15,Inf), labels = c('low', 'high')))



# How can we visualise the WindkmType variable?

plot(airquality$WindkmType)



# How can we visualise the relation between Temperature and Ozone?

plot(airquality$Temp, airquality$Ozone)



# How is this relation according to the wind speed categories?

plot(airquality$Temp, airquality$Ozone, pch = 16)

points(airquality$Temp[airquality$WindkmType == 'low'], airquality$Ozone[airquality$WindkmType == 'low'], col = 2, pch = 16)



# Are the ozone concentrations considerably different in May and September?

tmp = airquality[airquality$Month == 5 | airquality$Month == 9, ]

boxplot(tmp$Ozone ~ tmp$Month, ylab = 'Ozone', xlab = '', names = c('May', 'September'))

# based on the boxplot and since there is a lot of overlap, it is hard to say they are considerably different.


In this way, you get more familiar with R and do some exploratory analysis at the same time, so it’s a win-win 




a=c(2,5,1,4,4,3,2,5,2,3)
length(a)
sum(a)/10
var(a)*10/9

?qnorm
dnorm((1-0.05)/2)

#r的第二题是怎么删list里面的列的?
  myList <- list(A = data.frame(ID = c("A", "A"), 
                                Test = c(1, 1), 
                                Value = 1:2), 
                 B = data.frame(ID = c("B", "B", "B"), 
                                Test = c(1, 3, 5), 
                                Value = 1:3))
# Keep just the "ID" and "Value" columns
lapply(myList, function(x) x[(names(x) %in% c("ID", "Value"))])
# Drop the "ID" and "Value" columns
lapply(myList, function(x) x[!(names(x) %in% c("ID", "Value"))])
#or
lapply(myList, function(x) { x["ID"] = NULL; x })

?match

x=c(1,2,3,4)
y=c(1,3)
x%in%y
match(x,y)
#the second element in y match the third in x

#We can use  df[!is.na(df$x1),]  or na.omit(df) to delete rows with missing values,
#and how to delete columns with missing value?  t(na.omit(t(df))) or other ways?
df=data.frame(x1=c(1,NA,3,4),y1=c("cat","dog","bird","chicken"))
df[!is.na(df$x1),]
na.omit(df)
#check missing value by column
?any
apply(df,2,function(x) is.na(x))
idx=apply(df,2,function(x) any(is.na(x)))
df=df[,-idx]

head(cars)
#data frame of ordered value
cars[order(cars$dist),]

?qt
qt(0.975,94)

install.packages('R2OpenBUGS',type='source')
library(R2OpenBUGS)




