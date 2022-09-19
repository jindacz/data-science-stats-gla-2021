#intro_lab_7 
setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/introtoR/labs")
load(url("http://www.stats.gla.ac.uk/~rhaggarty/rp/p7_2020.RData"))

###Task 1 Create the following matrix in R

A=rbind(c(1,8,5),c(4,3,6)) 
A

apply(A,2,sum)
#lapply to a list
apply(A,1,sum)

#The code below subtracts the vector (1, 3, 5) from each row of the matrix A, i.e. it subtracts 1 from the first
#column, 3 from the second column, etc.
sweep(A,2,c(1,3,5),"-")

#The code above divides each column of the matrix by the vector (8, 6), i.e. it divides the first row by 8 and
#the second row by 6.
sweep(A,1,c(8,6),"/")


#Task 2
library(MASS)
crabs <- crabs
#(a) Define a new data frame called standcrabs which contains the standardized values each of continuous
#variables FL,RW,CL,CW and BD by removing the column mean and diving by the standard deviation (you should
#not use the scale function).
stand=function(x){(x-mean(x))/sd(x)}
standcrabs=apply(crabs[,4:8],2,stand)
#or
stand_crabs=apply(crabs[,4:8],2,function(x){(x-mean(x))/sd(x)})

#(b) Define a vector called trim_means which contains the 10% trimmed mean of each of the continuous
#variables FL,RW,CL,CW and BD. To compute the 10% trimmed mean we exclude the largest 10% and smallest
#10% of values and calculate the arithmetic mean of the remaining 80% of values.
trimmed=function(x){sort(x)[21:180]} #定义trimmed函数，先sort，然后截取
trim=apply(crabs[,4:8],2,trimmed) #到底是用a之前的还是包括a的？
trim_means=apply(trim,2,mean)
#or
?apply
?mean #是mean里面带有trim噢
apply(crabs[,4:8], 2, mean, trim = 0.1)

#(c) Define a variable called gratio_sp which contains the average gratio defined as
gratio=function(x){return(x$CW/x$BD)} #输入x，然后用x$来制造
gratio_sp=by(crabs,crabs$sp,gratio) 
#利用by做分组计算!!!
#结果是按照species出来的，格式by(data,分组，function)
lapply(gratio_sp,mean) #用list apply来计算list的平均数
head(gratio_sp)


#Task 3
#(a) What is the total number of dwellings across all of Scotland?
load(url("http://www.stats.gla.ac.uk/~rhaggarty/rp/p7_2020.RData"))
str(taxbands)
sum(apply(taxbands[,4:11],2,sum))

#(b) For each tax band, find the total number of dwellings in Scotland in that tax band.
apply(taxbands[,4:11],2,sum)

#(c) For each intermediate geography find the proportion of dwellings in the different tax bands
ratios <- taxbands[,4:11] / apply(taxbands[,4:11], 1,sum)
#or
number.of.dwellings=apply(taxbands[,4:11],1,sum) #行求和
proportions=sweep(taxbands[,4:11],1,number.of.dwellings,'/')
#利用sweep来横扫一下，导致每一行都出结果
#or
f=function(x) x/sum(x)
props=t(apply(taxbands[,4:11],1,f))

#(d) Which intermediate geography has the highest proportion of dwellings in tax band A (or, alternatively,
#tax band H)?
idx=which.max(proportions$BandA)
taxbands[idx,1:3]
idx=which.max(proportions$BandH)
taxbands[idx,1:3]

#(e) Which are the ten intermediate geographies with the highest proportions of dwellings in tax band A (or,
#alternatively, tax band H)?
taxbands[order(proportions$BandA,decreasing=TRUE)[1:10],1:3]
taxbands[order(proportions$BandH,decreasing=TRUE)[1:10],1:3]


#(f) For each local authority find the number of dwellings in each council tax band.
tb.numerical <- taxbands[,4:11] 
LA.bands <- by(tb.numerical, taxbands["LA"], apply, 2, sum) 
#by(数据，分组，函数)
#by里面套一个apply函数
#or
taxbands.la=do.call(rbind,by(taxbands[,4:11],taxbands$LA,colSums))
?do.call
#or
unique.las=levels(taxbands$LA)
taxbands.la=matrix(nrow=length(unique.las),ncol=8)
rownames(taxbands.la)=unique.las
colnames(taxbands.la)=colnames(taxbands)[4:11]
for(i in unique.las){ #好处在哪？好处在于可以select rows
  select.rows=taxbands$LA==i #利用select.rows=taxband$LA==i来选中特定的种群
  taxbands.la[i,]=colSums(taxbands[select.rows,4:11]) #求和
}
taxbands.la

#(g) Which local authority has the highest proportion of dwellings in tax band A (or, alternatively, tax band H)?
number.of.dwellings.la=apply(taxbands.la,1,sum)
proportions.la=sweep(taxbands.la,1,number.of.dwellings.la,"/") #计算百分比
rownames(taxbands.la)[which.max(proportions.la[,"BandA"])]#直接引号选中矩阵列
rownames(taxbands.la)[which.max(proportions.la[,"BandH"])]

#Task 4
#(a) Write an R function named centralvar which takes as an argument n and implements the following.
#• Draw 10,000 random samples of n observations each from the N(0, 1) distribution.
#• For each sample compute the mean, median and midrange. Compute the variance of the computed
#means, medians and midranges.

#Your function should return a vector of length 3 which contains the variance of the computed means, medians
#and midranges.

?var
centralvar=function(n){
  samplesize=1e4
  data=matrix(rnorm(samplesize*n),ncol=n) #创造n行数据，每一行是sample size的数据量
  stats=apply(data,1,function(x) c(mean=mean(x),median=median(x),  #根据行，apply 
                                   midrange=(min(x)+max(x))/2))    #输出结果为n行
  apply(stats,1,var) #compute the variance of stats
}

#(b) Use the function you have just written to compute the variance of the means, medians and midranges for
#10,000 samples of size 100.
centralvar(100)


#task 5
#(a) Define the function disp.index which takes as input a data frame x including only numeric columns
#and output the index of dispersion for each column of x. Your function cannot use loops
library(datasets)
data(iris)
str(iris)

#step 1
f=function(x){var(x)/mean(x)}

#step 2
f(iris$Sepal.Length)
apply(iris[,1:4],2,f)

#step 3
disp.index=function(x){apply(x,2,f)} #利用apply和自定义函数组合

#(b) The iris dataset is freely available from R (just type iris). iris gives the measurements in centimeters
#of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3
#species of iris. The species of iris are setosa, versicolor, and virginica.
str(iris)
by(iris[,1:4],iris$Species,disp.index) #不能手动选择species
#by species，如果是对于变量的处理，用by
#对于横竖的处理，才是用apply
#也可以by里面嵌套一个apply



#task 6
#(a) Species not abundant in a certain subdivisions are encoded using “NA”. In order to be able to analyse
#this data further, replace all “NA”’s by “0”.
str(ices)
ices[is.na(ices)]=0

#(b) Determine the total catch for each subdivision.
total.catch.per.sub=apply(ices,1,sum)

#(c) Remove the subdivisions in which the total catch is 0.
ices=subset(ices,apply(ices,1,sum)!=0)
#or
ices=ices[apply(ices,1,sum)!=0]
#or
remove=total.catch.per.sub==0 # Identify regions with no fish caught
ices=ices[!remove,] 
total.catch.per.sub <- total.catch.per.sub[!remove]# Remove these from the data matrix

#(d) Determine the total catch for each species. Which three species are the most abundant?
#most问题
total.catch.per.species=apply(ices,2,sum) #列求和
sort(total.catch.per.sub,decreasing=T)[1:3] # Print the three most abundant species
#先sort排序，然后出第一个到第三个


#(e) To analyse the types of fishery operating in different subdivisions it is useful to convert the table of total
#catches to a table that contains the composition of the catches in percent. This can be done by dividing each
#row of the data frame by its sum. Hint: Use your result from part (c) and the function sweep or scale
ices.relative <- sweep(ices, 1, total.catch.per.sub, "/")
length(total.catch.per.sub)
length(ices)


#(f) For each subdivision determine the species that is most abundant. Hint: The function which.max might
#be of use.
most.ab.index=apply(ices.relative,1,which.max) #每行都要找到哪个最大的index，共有n行个
most.ab=colnames(ices.relative)[most.ab.index] # Turn index into species name
names(most.ab)=rownames(ices.relative) # Set the subdivision labels


#(g) The data frame ices.areas contains for each subdivision the name of the (larger) area it is part of. For
#each area compute the species that is most abundant.
View(ices)
unique.areas <- levels(ices.areas$area) # Find the different areas
ices.by.area <- matrix(ncol=ncol(ices), nrow=length(unique.areas))
rownames(ices.by.area) <- unique.areas # Create matrix to store result and set up col/row names
colnames(ices.by.area) <- colnames(ices)
for (i in unique.areas) { # For each area ... 选中文字
  subdivisions <- subset(ices.areas, area==i)$subdivision #起手一个subset来选中的想要的数据
  subset <- subset(ices, rownames(ices)%in%subdivisions) # Extract observations ...
  ices.by.area[i,] <- apply(subset,2,sum) # ... and add them up.
  # We need drop=FALSE as we might only select one row
}
most.abundant.index <- unlist(apply(ices.by.area, 1, which.max)) # Get index of most abundant species
most.abundant <- colnames(ices.by.area)[most.abundant.index] # Turn index into species name
names(most.abundant) <- rownames(ices.by.area) # Set the subdivision labels

#example
i=unique.areas[1]
subdivisions <- subset(ices.areas, area==i)$subdivision #起手一个subset来选中的想要的数据
subset <- subset(ices, rownames(ices)%in%subdivisions)# Extract observations ...
subset[,1:5]
ices.by.area[i,] <- apply(subset,2,sum) # ... and add them up.


apply(ices.by.area, 1, which.max)



