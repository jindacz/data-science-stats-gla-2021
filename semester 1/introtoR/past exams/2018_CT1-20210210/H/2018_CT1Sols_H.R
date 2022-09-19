##############################
###  Class Test 1 Honours  ###
##############################


# Question 1 

#1.  1 mark
cats <- read.csv("cats.csv")


#2.  2 marks (1 for subset - 1 for nrow)
cats.male <- subset(cats,Sex == "M")
cats.female <- subset(cats,Sex == "F")
nrow(cats.male)
nrow(cats.female)


#3.  1 mark
mean(subset(cats,Sex=="M" & Bwt >3)$Hwt)


#4.  1 mark
cats[which.max(cats$Bwt),]


#5.  1 mark 
cats <- transform(cats, Hearth.percent = Hwt/(Bwt*1000))


#6.  2 marks (1 for boxplot - 1 for color)
boxplot(cats$Hearth.percent~cats$Sex,col=c("pink","blue"))


#7.  2 marks (1 for plot an labels - 1 for symbol)
plot(cats$Hwt~cats$Bwt,pch=unclass(cats$Sex),
     xlab="Body weight", ylab="Hearth weight")


#8.  2 marks (1 for colors - 1 for legend)
mycols <- c("pink","blue")
plot(cats$Hwt~cats$Bwt,pch=unclass(cats$Sex),xlab="Body weight",
     ylab="Hearth weight",col=mycols[unclass(cats$Sex)])
legend("topleft",pch=1:2,col=c("pink","blue"),legend=c("Female","Male"))


# 9. (and 10)  3 marks (1 for overall regression line - 1 for gender regression lines - 1 for appropriate color/type)
mycols <- c("pink","blue")
plot(cats$Hwt~cats$Bwt,pch=unclass(cats$Sex),xlab="Body weight", 
     ylab="Hearth weight",col=mycols[unclass(cats$Sex)])
legend("topleft",pch=1:2,col=c("pink","blue"),legend=c("Female","Male"))
abline(-0.36,4.03)
abline(-1.18,4.32,col="blue",lty=2)
abline(2.98,2.64,col="pink",lty=2)


# 11. 2 marks (perhaps no splitting?) 
```{r}
med <- sort(cats$Bwt)[0.5*nrow(cats)]
```

# 12. 1 mark
```{r}
Btw.discrete <- cut(cats$Bwt,breaks=c(0,med,Inf),labels = c("small","big"))
```

# 13. 2 marks (1 for appropriate barplot - 1 for coloring)
```{r}
barplot(table(cats$Sex,Btw.discrete),col=c("pink","blue"))
```


# Question 2 

#1. 1 mark
A <- cbind(c(1,0,0,3),c(0,4,2,0),c(0,2,17,8),c(3,0,8,49))
A


#2. 1 mark (no explicit definition c(1,2,4,5))
b <- c(1:2,4:5)
b


#3.  1 mark
solve(A,b)


#4. 3 marks (1 for eigen - 2 for appropriate formula)
eig <- eigen(A)
round(eig$vectors%*%diag(eig$values)%*%solve(eig$vectors))


# Question 3

#1. 1 mark (10000 or 1000 is ok either way (there was a typo))
arrival <- as.data.frame(cbind(runif(10000,0,120),runif(10000,0,120)))

#2. 2 marks (1 for appropriate formula - 1 for adding to column meet)
arrival$meet <- abs(arrival[,1]-arrival[,2])<= 20

#3.  1 mark
sum(arrival$meet)/nrow(arrival)
