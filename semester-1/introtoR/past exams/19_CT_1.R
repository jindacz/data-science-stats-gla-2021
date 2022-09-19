#19_CT_1.R
setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/introtoR/2019_CT1-20210210/M")

#1 [2 marks] Use R to read in the file julymovies.csv *correctly* and 
#save it as a dataframe called movies.
movies=read.csv('julymovies.csv',na.string="*")
#correct

#2. [2 marks] Define a variable called lionsgate which contains the 
#number of movies within the dataframe movies which were distributed by Lionsgate.
#阅读理解错误，是求sum
#lionsgate=subset(movie,movie$distributor=='Lionsgate')
#i=0
#sum=0
#lionsgate=length(lionsgate$days)
#一开始错，修改后也work，直接用length导出
lionsgate=sum(movies$distributor=="Lionsgate")


#3. [2 marks] Define a vector called missing which contains the number of missing 
#values for each variable in the movies dataframe.
#In other words, the vector missing should have many elements as there are columns in the movies
#dataframe and each element in missing should correspond to one of the columns.
subset(movies,(movies$title=='NA'))
subset(movies,(movies$dis=='NA'))
subset(movies,(movies$g=='NA'))
subset(movies,(movies$thea=='NA'))
subset(movies,(movies$days=='NA'))
subset(movies,(movies$date=='NA'))
subset(movies,(movies$top=='NA'))
### WRONG!
#用is.na()，利用colsum，生成每一个col的sum，从而实现分col求和
missing=colSums(is.na(movies));missing

#4. [3 marks] Update the movies data frame by removing all rows where the values of days and top are
#missing. The updated dataframe should be called movies.
movies=subset(movies,!is.na(movies$days))
movies=subset(movies,!is.na(movies$top))
###correct
#Ans
movies <- subset(movies, !is.na(movies$top)&!is.na(movies$days))

#5. [2 marks] Define a variable called average.gross which contains the average 
#revenue per theatre for movies distributed by Walt Disney
#movies_yuno=subset(movies,movies$dis=="Walt Disney")
#movies_yuno=transform(movies_yuno,average.rev=gross/theaters)
#average.gross=movies_yuno$average.rev
#wrong(changed)
#ans 
disney=subset(movies, movies$distributor=="Walt Disney")
average.gross=sum(disney$gross)/sum(disney$theaters)

#6. [2 marks] Define a variable called log.gross which contains, for each movie 
#in the dataframe, the log transformed gross revenue. Add this variable to the 
#movies dataframe. (The additional column name should be log.gross and the 
#resulting dataframe should still be called movies)
#movies=transform(movies,log.gross=log(gross))
#correct
log.gross <- log(movies$gross)
movies <- transform(movies, log.gross=log.gross)

#7. [2 marks] Sort the movies data frame by number of theaters. 
#The sorted dataframe should be called movies.
#a=sort(movies$theaters)
#movies=movies[a,] 
#怎么重新排序呢？没思路 wrong!
#按行排序
#answer
movies=movies[order(movies$theaters),]
#x=c(1,9,5,6)
#p=order(x);p
#x[p]
#we can obtained sorted vector by apply the permutation obtained from order to x

#8. [5 marks] Produce a plot of log.gross by theaters
#Points representing movies which were in the top 100 grossing American movies of all time should be
#coloured red, while those which were not in the top 100 should be coloured blue.
#• You should set pch=19 (this changes the plotting character to an solid circle)
#• Points representing the movies which were in the top 100 grossing American movies of all time should
#be twice the size of those representing movies not in the top 100.
#• The title of the plot “log(Gross Revenue) vs Number of Theaters”.
#• There should be a legend the same as the one shown on the plot on page 3.

#Wrong! 怎么弄top100呢？
#如何控制cex呢？1+unclass
mycol=c("blue","red")
plot(log.gross~theaters,data=movies, #画log图，先加一行
     #cex.axis=1.1, cex.lab=1.1, cex.main=1.1,
     col=mycol[unclass(movies$top)+1],
     main="log(Gross Revenue) vs Number of Theaters",
     pch=19,
     cex = unclass(movies$top)+1)
legend("bottomright",pch=19,col=c("red","blue"),
       legend=c("in top 100","outside top 100"))

#9 Define a matrix X which is the design matrix required for fitting a fractional polynomial regression
#model where log transformed gross revenue is the response, y, and number of theaters is the covariate,
#x.
x=movies$theaters
y=movies$log.gross
X=cbind(x^-1,x^-0.5,1,sqrt(x),x)

#10. [3 marks] Using the design matrix, X, from Question 9 define a vector y.hat which contains,
#ˆy = (yˆ1, ...yˆn), the fitted values for the fractional polynomial regression between the log transformed
#revenue (response) and number of theaters (covariate).
beta=solve(t(X)%*%X,t(X)%*%y)
y.hat=X%*%beta

#11. [2 marks] Add the fitted fractional polynomial regression line to the plot produced in Question 8. You
#should add a thick green line to represent the fitted model. Your plot should look like the one at the
#top of page 4.
#怎么添加这条线啊？？？
#不是abline，是lines
lines(movies$theaters,y.hat,col="green",lwd=3)

#12. Define a variable Rsq which contains the R2 value for the fitted fractional polynomial regression line
#computed in Question 10.
Rsq=1-sum((y-y.hat)^2)/sum((y-mean(y))^2)
#or
y <- log.gross
n <- length(y)
f <- y.hat
Rsq <- 1-(sum((y-f)^2)/sum((y-mean(y))^2))

#13 Use a simulation based on 1000 possible journeys to define a vector called late which contains the
#probability Susan arrives after the movie starts at 8pm.
set.seed(123)
#Ans 这也太漂亮了吧，直接就是sum()/1000，算出了概率
leaves=runif(1000,0,30) #模拟leave的时间
journey=runif(1000,30,45) #模拟交通time
total=leaves+journey #模拟求和
late=sum(total>=60)/1000 #直接用sum求概率

#14. [4 marks] For this question you should answer based on your simulation from Question 13.
#i) Define a variable called waiting which contains the average number of minutes that Susan has to
#wait before the film begins.
#ii) Define a variable called unseen which contains average number of minutes of the movie Susan will
#miss if she is not there for the start of the movie.
#waiting=mean(60-total)
#Wrong
waiting <- abs(mean(total[total<60])-60) 
unseen=mean(total[total>60])-60





