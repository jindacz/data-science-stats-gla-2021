setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/regreesion\ code/Data\ sets-20210205")
phys=read.csv("phys1.csv")
names(phys)
View(phys)
plot(Power1~Weight,data=phys)
pl