####################################
#### CLASS TEST 1 SOLUTIONS MSc ####
####################################


# Question 1

#1. 1 mark
# some people may have used na for 1s - that's ok!
houseprices <- read.csv("houseprices.csv")


#2. 1 mark
mean(subset(houseprices,Month == 8)$Price)


#3.  2 marks (1 for subset correctly - 1 for nrow)
houseprices.summer <- subset(houseprices, (Month == 7 & Day >= 15) | (Month == 8 & Day <= 15 ))
nrow(houseprices.summer)


#4. 1 mark 
### some people may have found the minimum for houseprices.summer - that's ok!
houseprices[which.min(houseprices$Price),]


#5. 1 mark
houseprices$Lon <- houseprices$Lon* pi /180
houseprices$Lat <- houseprices$Lat* pi /180


#6. 4 marks (1 for lambda1/phi1 - 1 for deltalambda/deltaphi - 1 for alpha - 1 for Dist2Uni)
Lambda1 <- -4.2886 / 180 * pi
Phi1 <- 55.8711  / 180 * pi
DeltaLambda <- houseprices$Lon- Lambda1
DeltaPhi <- houseprices$Lat - Phi1
alpha <- sin(DeltaPhi/2)^2 + cos(Phi1)*cos(houseprices$Lat)*sin(DeltaLambda/2)^2
Dist2University <- 12742 * atan2(sqrt(alpha),sqrt(1-alpha))


#7. 1 mark
mean(subset(houseprices,Dist2Uni<= 1)$Price)

#8. 2 marks (1 for plot(density()) - 1 for appropriate title/axis)
plot(density(subset(houseprices,Price < 1000000)$Price),main="Density of Price", xlab="Price")



# Question 2

# 1.  1 mark
hearth <- read.table("hearth.txt",header = TRUE, na.strings = ";")

# 2.  1 mark
hearth <- na.omit(hearth)

# 3. 2 marks (1 for each variable)
hearth$difference <- hearth$MF - hearth$SV
hearth$mean <- (hearth$MF + hearth$SV)/2

# 4. 2 marks (1 for plot - 1 for labelling)
plot(hearth$mean,hearth$difference, xlab="Mean of measurements", ylab = "Difference of measurements")

# 5. 2 marks (1 for mean line - 1 for the sd lines)
abline(h=mean(hearth$difference))
abline(h=mean(hearth$difference)+sd(hearth$difference),lty=2)
abline(h=mean(hearth$difference)-sd(hearth$difference),lty=2)



# Question 3

#1.  1 mark 
potus <- read.table("potus.txt",header=TRUE, sep = ",")


#2.  2 marks (1 for mean - 1 for right subset)
mean(subset(potus,VotesTrump>=3*VotesClinton)$HIncome)


#3. 1 mark
sum(subset(potus,State=="California")$VotesClinton)


#4. 1 mark
potus$Hillary.Wins <- potus$VotesClinton > potus$VotesTrump 


#5. (and 6)   4 marks (2 for plots - 1 for right coloring - 1 for legend) )
mycols <- c("blue", "red")
plot(HIncome~PercWhite, data=potus,col=mycols[unclass(as.factor(Hillary.Wins))],pch=19)
legend("topleft", pch=19, col=c("red", "blue"), legend = c("Clinton", "Trump"))
