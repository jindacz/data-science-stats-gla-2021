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

#look at predicted values
pred1=predict(model,newdata=data.frame("climb"=seq(0,10000,length=100),
                                       "dist"=rep(mean(hills$dist),100)))