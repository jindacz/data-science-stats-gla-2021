
Soar<- read.csv("SiteSoar.csv", header=T)
dim(Soar)
fix(Soar)
library(NADA)

##Soar<-Soar[Soar$Ammonia< "2" ,] ## to remove the outlier
   
   pctCen(Soar$Ammonia,Soar$censored)    ## percent of censored data
   censummary(Soar$Ammonia,Soar$censored)## like summary cmd but for censored data

   ROS <-cenros(Soar$Ammonia,Soar$censored) ## constructs an object of class c("ros", "lm")
        plot(ROS)   ## probability plot
        plot(ROS, plot.censored= TRUE)## plots the modelled censored observations as well
        summary(ROS)## more info about the ROS regression
        print(ROS)  ## prints a simple summary of the ROS model.
 
   KM  <-cenfit(Soar$Ammonia,Soar$censored)  ## constructs a Kaplan-Meier model
        plot(KM)   ## survival function plot
        summary(KM)
        print(KM)
        
   MLE <-cenmle(Soar$Ammonia,Soar$censored) ## constructs a Maximum Likelihood model
        plot(MLE)
        summary(MLE)
        print(MLE)
       
   ROS
   KM
   MLE   
   
 ## impute values using ROS output ##
    
mu<- mean(ROS) 
stdev<-sd(ROS)

Soar$imputed <-0
n <- nrow(Soar)

for(s in 1:n)
    { if (Soar[s,"censored"]==FALSE )   { Soar[s,"imputed"]  <- Soar[s,"Ammonia"]  }
      else {  aux <- rnorm(1,mean= mu, sd= stdev )
              while((aux > Soar[s,"Ammonia"])|| (aux<0)) 
                    { aux <- rnorm(1,mean= mu, sd= stdev )} 
              Soar[s,"imputed"] <- aux     
            }
    }
    

fix(Soar)

plot(Soar$doy, Soar$Ammonia )
plot(Soar$doy, Soar$imputed )


## or

year.day <- Soar[ , "year"] + Soar[ ,"doy"] / 366

plot(year.day, Soar$Ammonia, col=2 )
plot(year.day, Soar$imputed,pch=1+as.numeric(Soar$censored ),col=1+Soar$censored)
