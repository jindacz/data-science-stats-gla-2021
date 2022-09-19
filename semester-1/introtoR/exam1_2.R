setwd("/Users/kurisuuu/Downloads/Assignment1Data")
starwars=read.csv('starwars.csv')

#1
str(starwars)


missing=colSums(is.na(starwars));missing
#wrong，average missing value可还行
missing = colMeans(is.na(starwars));missing

#3 Update the data frame starwars by removing the rows from starwars where the 
#height or mass of the character is missing. The updated data frame should be 
#called starwars.
starwars=subset(starwars,!is.na(starwars$height)&!is.na(starwars$mass))

#4 Create a vector of length 2 that contains 
#the average height of females and males.
male=subset(starwars, starwars$gender=="male")
female=subset(starwars, starwars$gender=="female")
average_male = mean(male$height)
average_female = mean(female$height)
q4=c(average_male,average_female)
names(q4)=c("average_male","average_female")
q4

#5 In Episode IV (“A New Hope”), Luke Skywalker is a farmer on Tatooine living 
#with his uncle and aunt. Tatooine is a harsh desert world, where humans and 
#droids coexist in relative harmony. Based on the starwars data frame, 
#how many humans live in Tatooine?
Tatooine=subset(starwars,starwars$homeworld=="Tatooine")
Tatooine_human=subset(Tatooine,Tatooine$species=="Human")
humans_live_in_Tatooine=nrow(Tatooine_human)
humans_live_in_Tatooine
  
#6 In Episode IV (“A New Hope”) Chewbacca (Chewie) is a Wookie that servers as 
#the co-pilot of the Millennium Falcon starship. Wookies are very tall individuals 
#and are very concerned about their body mass index (BMI). Create a new data frame 
#called chewie containing only characters from the same homeworld as Chewbacca. 
#Add a new column to the chewie data frame called BMI , which contains the BMI 
#defined as m/h2 , where m is mass in kg, and h is height in meters.
#height	integer	Height (cm)
#mass	double	Weight (kg)
which_place=starwars[starwars$name=="Chewbacca",]$homeworld
which_place
chewie=subset(starwars,starwars$homeworld=="Kashyyyk")
chewie=transform(chewie,BMI=mass/(height/100)^2)
  
  
  
  
  