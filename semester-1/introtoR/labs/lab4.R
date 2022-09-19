#R lab 4
#task 1
setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/introtoR/W4data")
iris=read.csv("iris.csv")
class(iris$Species)
class(iris$Sepal.Length)

c=as.factor(iris$Species)
plot(Sepal.Width~Sepal.Length,data=iris,
     col=unclass(c),xlab="Speal.Length",ylab="Speal.Width",pch=unclass(c),
     lwd=2,main="Fisher's iris data")

legend("topleft",pch=1:nlevels(c),col=1:nlevels(c),
       legend=levels(c),cex=0.7)
#use cex to change size

#task 2
#1 Create boxplots of the variable FL, RW, CL, CW and BD
par(mfcol=c(2,2))
crab=read.csv("crab.csv")
crab <- transform(crab, sex = factor(sex), sp = factor(sp))
boxplot(crab[,4:8])

#2 Create two vectors CW.orange and CW.blue containing the measurements of the carapace width for the
#two species, respectively. Finally, create boxplots of both variables. To make comparing the box plots
#easier, it is best to place them in the same figure.
?subset
a=subset(crab,crab$sp=="O")
b=subset(crab,crab$sp=="B")
CW.orange=c(a$CW)
CW.orange
CW.Blue=c(b$CW)
CW.Blue
boxplot(CW.orange,CW.Blue,names=c('Orange','Blue'))

#3 Create the same boxplots as in part 2 without creating the vectors CW.orange and CW.blue
boxplot(subset(crab,crab$sp=="O")$CW,subset(crab,crab$sp=="B")$CW,
        names=c('Orange','Blue'),
        border=c('orange','blue'))
###or
boxplot(CW~sp, data=crab,col=c("blue","orange"))

#4 Create boxplots of the other variables for each of the two species. Based on the boxplots, does it seem
#possible to tell the two species apart?
boxplot(subset(crab,crab$sp=="O")$FL,subset(crab,crab$sp=="B")$FL,
        names=c('Orange','Blue'))
boxplot(subset(crab,crab$sp=="O")$RW,subset(crab,crab$sp=="B")$RW,
        names=c('Orange','Blue'))
boxplot(subset(crab,crab$sp=="O")$CL,subset(crab,crab$sp=="B")$CL,
        names=c('Orange','Blue'))
boxplot(subset(crab,crab$sp=="O")$BD,subset(crab,crab$sp=="B")$BD,
        names=c('Orange','Blue'))
###or
boxplot(FL~sp, data=crab,col=c("blue","orange"))
boxplot(RW~sp, data=crab,col=c("blue","orange"))
boxplot(CL~sp, data=crab,col=c("blue","orange"))
boxplot(BD~sp, data=crab,col=c("blue","orange"))

#yes, Orange's mean is larger than Blue

#5 Create a scatter plot of the columns CW (carapace width) and BD (body depth)
plot(BD~CW,data=crab)
  
  
#6 It is usually a good idea to both have self-explanatory axis labels and to give the units of measurement.
#Change your command from part 5 so that the axis label on the horizontal axis is Carapace width
#(in mm) and that the label on the vertical axis is Body depth (in mm). The title of the plot should
#be Two Species of Leptograpsus.
plot(BD~CW,data=crab,xlab="s Carapace width(in mm)",ylab="Body depth (in mm)",
     main="Body depth (in mm)")



#7 Change your plotting command from the previous task such that the plotting symbol reflects the sex
#and that the colour reflects the species.
c=factor(crab$sp)
d=factor(crab$sex)
plot(BD~CW,data=crab,xlab="Carapace width(in mm)",ylab="Body depth (in mm)",
     main="Two Species of Leptograpsus.",col=1+unclass(c),pch=unclass(d))
  
#8 The two species are actually called blue and orange. Change your plotting command so that it uses
# these two colours
my.cols=c('blue','orange')
plot(BD~CW,data=crab,xlab="Carapace width(in mm)",ylab="Body depth (in mm)",
     main="Two Species of Leptograpsus.",col=my.cols[unclass(factor(crab$sp))],
     pch=unclass(d))

class(crab$sp)


#9 Add a legend to your plot, which explains both the plotting symbols and the colours used, as in the
#plot below.
c=factor(crab$sp)

legend("topleft",pch=1:nlevels(c),col=c("blue","blue","orange","orange"),
       legend=c("blue female","blue male","orange female","orange male"),cex=1)

#10 What happens when you run the commands rug(crabs$CW) and rug(crabs$BD, side=2) after you
#have created the scatter plot?
rug(crab$CW)
rug(crab$BD,side=2)


#11 Use the function pairs to create a scatter plot of all measurements in the data set. Just like in the
#previous task, the plotting symbol should reflect the sex and the colour should reflect the species.
crab$sp.col <- ifelse(crab$sp=="O", "orange", "blue")
pairs(crab[,c("FL", "RW", "CL","CW","BD")], col=crab$sp.col)
###or
pairs(crab[,4:8],col=my.cols[unclass(factor(crab$sp))],
      pch=unclass(factor(crab$sex)))

#12
#Most of the variability in the plots comes down to the overall size of the crab, 
#which is probably mostly related to age. The variables CW and BD allow for some separation between the species if they are
#used concurrently.

#13
ratio=crab$CW/crab$BD
boxplot(ratio~crab$sp)
#the ratio seems to be very good at tellng the two species apart

#Task3
x=seq(0,2*pi,length.out=100)
y=sin(x)
plot(x,y,type="l",lwd=2,xlab="x",ylab="sin(x)")
polygon(x,y,col="grey",border=NA)
lines(x,y,lwd=2)

#task 4 The function locator(n, type) reads the coordinates of the mouse when the left-mouse button is pressed.
#locator reads in total n clicks unless the user aborts by pressing the Esc key or by clicking the right mouse
#button. The argument type controls whether (and how) the points are plotted. type="n" (default) plots
#nothing, type="p" plots the points (see ?locator for more details). locator returns a list containing the
#coordinates of the points as x and y

#1Set up an empty plotting area with range (−1, 1) × (−1, 1) (you can use the option NULL to do this).
#Use the function locator to read 15 in points from mouse clicks.
plot(NULL,xlim=c(-1,1),ylim=c(-1,1),xlab='x',ylab='y')
points=locator(15,type='p')


#4.2 After having read the coordinates, colour the points above the bisector y = x in blue and the other
#points in red.

plot(points, xlim=c(-1,1), ylim=c(-1,1), xlab="x", ylab="y")
colors=rep("blue",15)
colors[points$x>points$y]='red'
points(points,col=colors)

#4.3 Use the function abline to draw the bisector y = x as a dotted line.
abline(0,1,lty=3,col="red") 
















  