#regression_example_10
### Hills example

#Load libraries
library(gridExtra)
library(MASS)
library(GGally)
data(hills)

#plot data
p1=ggplot(hills,aes(x=climb,y=time,label=rownames(hills)))+
  geom_point()+geom_text(size=3,hjust=1)
p2=ggplot(hills,aes(x=dist,y=time,label=rownames(hills)))+
  geom_point()+geom_text(size=3,hjust=1)
grid.arrange(p1,p2,ncol=2)

#fit model with climb and dist
model=lm(time~climb+dist,data=hills)
summary(model)

#Look at predicted values
pred1<-predict(model,newdata=data.frame("climb"=seq(0,10000,length=100),"dist"=rep(mean(hills$dist),100)))
df1<-data.frame("climbb"=seq(0,10000,length=100),"pred"=pred1)
pred2<-predict(model,newdata=data.frame("dist"=seq(0,40,length=100),"climb"=rep(mean(hills$climb),100)))
df2<-data.frame("distt"=seq(0,40,length=100),"pred"=pred2)

p1<-ggplot(hills,aes(x=climb,y=time)) +
  geom_point()  +
  geom_line(aes(x=climbb,y=pred),df1,size=2)
p2<-ggplot(hills,aes(x=dist,y=time)) +
  geom_point()  +
  geom_line(aes(x=distt,y=pred),df2,size=2)
grid.arrange(p1,p2,ncol=2) 

#check assumptions
library(ggfortify)
autoplot(model,1:2)

#remove potential outliers
hills1=hills[!rownames(hills) %in% c("Bens of Jura","Knock Hill"),]
model1=lm(time~climb+dist,data=hills1)
summary(model1)

#Look at predict values removing outliers (red line)
pred1<-predict(model1,newdata=data.frame("climb"=seq(0,10000,length=100),"dist"=rep(mean(hills1$dist),100)))
df11<-data.frame("climbb"=seq(0,10000,length=100),"pred"=pred1)
pred2<-predict(model1,newdata=data.frame("dist"=seq(0,40,length=100),"climb"=rep(mean(hills1$climb),100)))
df22<-data.frame("distt"=seq(0,40,length=100),"pred"=pred2)
p1<-ggplot(hills,aes(x=climb,y=time)) +
  geom_point()  +
  geom_line(aes(x=climbb,y=pred),df1,size=2)+
  geom_line(aes(x=climbb,y=pred),df11,color="red",size=2)
p2<-ggplot(hills,aes(x=dist,y=time)) +
  geom_point()  +
  geom_line(aes(x=distt,y=pred),df2,size=2)+
  geom_line(aes(x=distt,y=pred),df22,color="red",size=2)
grid.arrange(p1,p2,ncol=2) 

#Check assumptions
autoplot(model1,1:2)

#Let's now consider possible transformation
p1<-ggplot(hills,aes(x=climb)) +
  geom_histogram() 
p2<-ggplot(hills,aes(x=dist)) +
  geom_histogram() 
grid.arrange(p1,p2,ncol=2)

#Log transform climb and distance
#不用transform
hills$lclimb=log(hills$climb)
hills$ldist=log(hills$dist)

p1=ggplot(hills,aes(x=lclimb))+
  geom_histogram()
p2=ggplot(hills,aes(x=ldist))+
  geom_histogram()
grid.arrange(p1,p2,ncol=2)

#Fit model with log climb and log dist
model2<-lm(time~lclimb+ldist,data=hills)
summary(model2)

#Look at predicted values
pred1<-predict(model2,newdata=data.frame("lclimb"=seq(5,9,length=100),
                                         "ldist"=rep(mean(hills$ldist),100)))
df1<-data.frame("climbb"=seq(5,9,length=100),"pred"=pred1)
pred2<-predict(model2,newdata=data.frame("ldist"=seq(0,3.5,length=100),
                                         "lclimb"=rep(mean(hills$lclimb),100)))
df2<-data.frame("distt"=seq(0,3.5,length=100),"pred"=pred2)

p1<-ggplot(hills,aes(x=lclimb,y=time)) +
  geom_point()  +
  geom_line(aes(x=climbb,y=pred),df1,size=2)
p2<-ggplot(hills,aes(x=ldist,y=time)) +
  geom_point()  +
  geom_line(aes(x=distt,y=pred),df2,size=2)
grid.arrange(p1,p2,ncol=2) 

#Check assumptions
autoplot(model2,1:2)

#possible quadratic relationships
model3=lm(time~poly(lclimb,2)+poly(ldist,2),data=hills)
summary(model3)$coefficients

#look at fitted values
pred1<-predict(model3,newdata=data.frame("lclimb"=seq(5,9,length=100),"ldist"=rep(mean(hills$ldist),100)))
df1<-data.frame("climbb"=seq(5,9,length=100),"pred"=pred1)
pred2<-predict(model3,newdata=data.frame("ldist"=seq(0,3.5,length=100),"lclimb"=rep(mean(hills$lclimb),100)))
df2<-data.frame("distt"=seq(0,3.5,length=100),"pred"=pred2)

p1<-ggplot(hills,aes(x=lclimb,y=time)) +
  geom_point()  +
  geom_line(aes(x=climbb,y=pred),df1,size=2)
p2<-ggplot(hills,aes(x=ldist,y=time)) +
  geom_point()  +
  geom_line(aes(x=distt,y=pred),df2,size=2)
grid.arrange(p1,p2,ncol=2) 

#Check assumptions
autoplot(model3,1:2)

#Now let's revisit possible outliers - Cook's distance and leverage
cd_cont_pos=function(leverage,level,model){
  sqrt(level*length(coef(model))*(1-leverage)/leverage)
}
cd_cont_neg <- function(leverage, level, model) 
{-cd_cont_pos(leverage, level, model)}

autoplot(model3, which = 5) +
  stat_function(fun = cd_cont_pos, args = list(level = 0.5, model = model), 
                xlim = c(0, 0.7), lty = 2, colour = "red") +
  stat_function(fun = cd_cont_neg, args = list(level = 0.5, model = model), 
                xlim = c(0, 0.7), lty = 2, colour = "red") +
  stat_function(fun = cd_cont_pos, args = list(level = 1, model = model), 
                xlim = c(0, 0.7), lty = 3, colour = "red") +
  stat_function(fun = cd_cont_neg, args = list(level = 1, model = model), 
                xlim = c(0, 0.7), lty = 3, colour = "red") +
  scale_y_continuous(limits = c(-5, 3.5))
?autoplot
?stat_function

#outlier test
library(car)
outlierTest(model3)

#remove knock hill
hills1<-hills[!rownames(hills) %in% c("Knock Hill"),]

?%in%

model4<-lm(time~poly(lclimb,2)+poly(ldist,2),data=hills1)
summary(model4)$coeff
  
#Look at new predicted values (red curves)
pred1<-predict(model3,newdata=data.frame("lclimb"=seq(5,9,length=100),"ldist"=rep(mean(hills$ldist),100)))
df1<-data.frame("climbb"=seq(5,9,length=100),"pred"=pred1)
pred2<-predict(model3,newdata=data.frame("ldist"=seq(0,3.5,length=100),"lclimb"=rep(mean(hills$lclimb),100)))
df2<-data.frame("distt"=seq(0,3.5,length=100),"pred"=pred2)

p1<-ggplot(hills,aes(x=lclimb,y=time)) +
  geom_point()  +
  geom_line(aes(x=climbb,y=pred),df1,size=2)+
  geom_line(aes(x=climbb,y=pred),df11,color="red",size=2)
p2<-ggplot(hills,aes(x=ldist,y=time)) +
  geom_point()  +
  geom_line(aes(x=distt,y=pred),df2,size=2)+
  geom_line(aes(x=distt,y=pred),color="red",df22,size=2)
grid.arrange(p1,p2,ncol=2) 


#Check assumptions
autoplot(model4,1:2)


#Check again for outliers
cd_cont_pos <- function(leverage, level, model) 
{sqrt(level*length(coef(model))*(1-leverage)/leverage)}
cd_cont_neg <- function(leverage, level, model) 
{-cd_cont_pos(leverage, level, model)}

autoplot(model4, which = 5) +
  stat_function(fun = cd_cont_pos, args = list(level = 0.5, model = model), 
                xlim = c(0, 0.7), lty = 2, colour = "red") +
  stat_function(fun = cd_cont_neg, args = list(level = 0.5, model = model), 
                xlim = c(0, 0.7), lty = 2, colour = "red") +
  stat_function(fun = cd_cont_pos, args = list(level = 1, model = model), 
                xlim = c(0, 0.7), lty = 3, colour = "red") +
  stat_function(fun = cd_cont_neg, args = list(level = 1, model = model), 
                xlim = c(0, 0.7), lty = 3, colour = "red") +
  scale_y_continuous(limits = c(-5, 5))


#Remove Bens of Jura too
hills1<-hills[!rownames(hills) %in% c("Knock Hill","Bens of Jura"),]
model5<-lm(time~poly(lclimb,2)+poly(ldist,2),data=hills1)
summary(model5)

#Check fitted values (blue curves)
pred1<-predict(model3,newdata=data.frame("lclimb"=seq(5,9,length=100),"ldist"=rep(mean(hills$ldist),100)))
df1<-data.frame("climbb"=seq(5,9,length=100),"pred"=pred1)
pred2<-predict(model3,newdata=data.frame("ldist"=seq(0,3.5,length=100),"lclimb"=rep(mean(hills$lclimb),100)))
df2<-data.frame("distt"=seq(0,3.5,length=100),"pred"=pred2)

pred11<-predict(model4,newdata=data.frame("lclimb"=seq(5,9,length=100),"ldist"=rep(mean(hills1$ldist),100)))
df11<-data.frame("climbb"=seq(5,9,length=100),"pred"=pred11)
pred22<-predict(model4,newdata=data.frame("ldist"=seq(0,3.5,length=100),"lclimb"=rep(mean(hills1$lclimb),100)))
df22<-data.frame("distt"=seq(0,3.5,length=100),"pred"=pred22)


pred111<-predict(model5,newdata=data.frame("lclimb"=seq(5,9,length=100),"ldist"=rep(mean(hills1$ldist),100)))
df111<-data.frame("climbb"=seq(5,9,length=100),"pred"=pred111)
pred222<-predict(model5,newdata=data.frame("ldist"=seq(0,3.5,length=100),"lclimb"=rep(mean(hills1$lclimb),100)))
df222<-data.frame("distt"=seq(0,3.5,length=100),"pred"=pred222)

p1<-ggplot(hills,aes(x=lclimb,y=time)) +
  geom_point()  +
  geom_line(aes(x=climbb,y=pred),df1,size=2)+
  geom_line(aes(x=climbb,y=pred),df11,color="red",size=2)+
  geom_line(aes(x=climbb,y=pred),df111,color="blue",size=2)
p2<-ggplot(hills,aes(x=ldist,y=time)) +
  geom_point()  +
  geom_line(aes(x=distt,y=pred),df2,size=2)+
  geom_line(aes(x=distt,y=pred),color="red",df22,size=2)+
  geom_line(aes(x=distt,y=pred),color="blue",df222,size=2)
grid.arrange(p1,p2,ncol=2) 



#Check assumptions
autoplot(model5,1:2)




#Check for outliers
cd_cont_pos <- function(leverage, level, model) 
{sqrt(level*length(coef(model))*(1-leverage)/leverage)}
cd_cont_neg <- function(leverage, level, model) 
{-cd_cont_pos(leverage, level, model)}

autoplot(model5, which = 5) +
  stat_function(fun = cd_cont_pos, args = list(level = 0.5, model = model), 
                xlim = c(0, 0.7), lty = 2, colour = "red") +
  stat_function(fun = cd_cont_neg, args = list(level = 0.5, model = model), 
                xlim = c(0, 0.7), lty = 2, colour = "red") +
  stat_function(fun = cd_cont_pos, args = list(level = 1, model = model), 
                xlim = c(0, 0.7), lty = 3, colour = "red") +
  stat_function(fun = cd_cont_neg, args = list(level = 1, model = model), 
                xlim = c(0, 0.7), lty = 3, colour = "red") +
  scale_y_continuous(limits = c(-5, 5))


### Hills
library(car)
outlierTest(model5)
#We could remove another observation: Two Breweries
hills1<-hills[!rownames(hills) %in% c("Knock Hill","Bens of Jura","Two Breweries"),]
model6<-lm(time~poly(lclimb,2)+poly(ldist,2),data=hills1)

summary(model6)

#Look at predicted values (orange curves)
pred1<-predict(model3,newdata=data.frame("lclimb"=seq(5,9,length=100),"ldist"=rep(mean(hills$ldist),100)))
df1<-data.frame("climbb"=seq(5,9,length=100),"pred"=pred1)
pred2<-predict(model3,newdata=data.frame("ldist"=seq(0,3.5,length=100),"lclimb"=rep(mean(hills$lclimb),100)))
df2<-data.frame("distt"=seq(0,3.5,length=100),"pred"=pred2)

pred11<-predict(model4,newdata=data.frame("lclimb"=seq(5,9,length=100),"ldist"=rep(mean(hills1$ldist),100)))
df11<-data.frame("climbb"=seq(5,9,length=100),"pred"=pred11)
pred22<-predict(model4,newdata=data.frame("ldist"=seq(0,3.5,length=100),"lclimb"=rep(mean(hills1$lclimb),100)))
df22<-data.frame("distt"=seq(0,3.5,length=100),"pred"=pred22)


pred111<-predict(model5,newdata=data.frame("lclimb"=seq(5,9,length=100),"ldist"=rep(mean(hills1$ldist),100)))
df111<-data.frame("climbb"=seq(5,9,length=100),"pred"=pred111)
pred222<-predict(model5,newdata=data.frame("ldist"=seq(0,3.5,length=100),"lclimb"=rep(mean(hills1$lclimb),100)))
df222<-data.frame("distt"=seq(0,3.5,length=100),"pred"=pred222)

pred1111<-predict(model6,newdata=data.frame("lclimb"=seq(5,9,length=100),"ldist"=rep(mean(hills1$ldist),100)))
df1111<-data.frame("climbb"=seq(5,9,length=100),"pred"=pred1111)
pred2222<-predict(model6,newdata=data.frame("ldist"=seq(0,3.5,length=100),"lclimb"=rep(mean(hills1$lclimb),100)))
df2222<-data.frame("distt"=seq(0,3.5,length=100),"pred"=pred2222)

p1<-ggplot(hills,aes(x=lclimb,y=time)) +
  geom_point()  +
  geom_line(aes(x=climbb,y=pred),df1,size=2)+
  geom_line(aes(x=climbb,y=pred),df11,color="red",size=2)+
  geom_line(aes(x=climbb,y=pred),df111,color="blue",size=2)+
  geom_line(aes(x=climbb,y=pred),df1111,color="orange",size=2)
p2<-ggplot(hills,aes(x=ldist,y=time)) +
  geom_point()  +
  geom_line(aes(x=distt,y=pred),df2,size=2)+
  geom_line(aes(x=distt,y=pred),color="red",df22,size=2)+
  geom_line(aes(x=distt,y=pred),color="blue",df222,size=2)+
  geom_line(aes(x=distt,y=pred),color="orange",df2222,size=2)
grid.arrange(p1,p2,ncol=2) 



#Check assumptions
autoplot(model6,1:2)


#Instead, lets try a transformation of time
hills$ltime<-log(hills$time)
model7<-lm(ltime~lclimb+ldist,data=hills)
summary(model7)



#Look at predicted values
pred1<-predict(model7,newdata=data.frame("lclimb"=seq(5,9,length=100),"ldist"=rep(mean(hills$ldist),100)))
df1<-data.frame("climbb"=seq(5,9,length=100),"pred"=pred1)
pred2<-predict(model7,newdata=data.frame("ldist"=seq(0,3.5,length=100),"lclimb"=rep(mean(hills$lclimb),100)))
df2<-data.frame("distt"=seq(0,3.5,length=100),"pred"=pred2)

p1<-ggplot(hills,aes(x=lclimb,y=ltime)) +
  geom_point()  +
  geom_line(aes(x=climbb,y=pred),df1,size=2)
p2<-ggplot(hills,aes(x=ldist,y=ltime)) +
  geom_point()  +
  geom_line(aes(x=distt,y=pred),df2,size=2)
grid.arrange(p1,p2,ncol=2) 


#Check assumptions
autoplot(model7,1:2)

#Remove Knock hill

#Remove potential outliers
hills1<-hills[!rownames(hills) %in% c("Knock Hill"),]
model8<-lm(ltime~lclimb+ldist,data=hills1)
summary(model8)

#Look at predict values removing outliers (red line)
pred1<-predict(model8,newdata=data.frame("lclimb"=seq(5,9,length=100),"ldist"=rep(mean(hills1$ldist),100)))
df11<-data.frame("climbb"=seq(5,9,length=100),"pred"=pred1)
pred2<-predict(model8,newdata=data.frame("ldist"=seq(0,3.5,length=100),"lclimb"=rep(mean(hills1$lclimb),100)))
df22<-data.frame("distt"=seq(0,3.5,length=100),"pred"=pred2)

p1<-ggplot(hills,aes(x=lclimb,y=ltime)) +
  geom_point()  +
  geom_line(aes(x=climbb,y=pred),df1,size=2)+
  geom_line(aes(x=climbb,y=pred),df11,color="red",size=2)
p2<-ggplot(hills,aes(x=ldist,y=ltime)) +
  geom_point()  +
  geom_line(aes(x=distt,y=pred),df2,size=2)+
  geom_line(aes(x=distt,y=pred),df22,color="red",size=2)
grid.arrange(p1,p2,ncol=2) 


#Check assumptions
autoplot(model8,1:2)


#Remove potential outliers
hills1<-hills[!rownames(hills) %in% c("Knock Hill","Black Hill"),]
model9<-lm(ltime~lclimb+ldist,data=hills1)
summary(model9)

#Look at predict values removing outliers (red line)

pred1<-predict(model9,newdata=data.frame("lclimb"=seq(5,9,length=100),"ldist"=rep(mean(hills1$ldist),100)))
df111<-data.frame("climbb"=seq(5,9,length=100),"pred"=pred1)
pred2<-predict(model9,newdata=data.frame("ldist"=seq(0,3.5,length=100),"lclimb"=rep(mean(hills1$lclimb),100)))
df222<-data.frame("distt"=seq(0,3.5,length=100),"pred"=pred2)


p1<-ggplot(hills,aes(x=lclimb,y=ltime)) +
  geom_point()  +
  geom_line(aes(x=climbb,y=pred),df1,size=2)+
  geom_line(aes(x=climbb,y=pred),df11,color="red",size=2)+
  geom_line(aes(x=climbb,y=pred),df111,color="blue",size=2)
p2<-ggplot(hills,aes(x=ldist,y=ltime)) +
  geom_point()  +
  geom_line(aes(x=distt,y=pred),df2,size=2)+
  geom_line(aes(x=distt,y=pred),df22,color="red",size=2)+
  geom_line(aes(x=distt,y=pred),df222,color="blue",size=2)
grid.arrange(p1,p2,ncol=2) 


#Check assumptions
autoplot(model9,1:2)

###problem 2
library(lasso2)
data(Prostate)
head(Prostate)

#p1
#p2
ggplot(Prostate,aes(y=lcavol,x=lpsa))+geom_point()+
  labs(y="log.size(volume)",x='log.antigen')

#3
model=lm(lcavol~lpsa,Prostate)
par(mfrow=c(1,2))
autoplot(model,which=1:2)











