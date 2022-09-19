setwd("/Users/kurisuuu/Downloads/Assignment1Data")
spotify_songs=read.table("spotify_songs.txt",header=TRUE)

#8 Update the data frame spotify_songs by removing the rows from spotify_songs 
#where the track artist is missing. The updated data frame should be called 
#spotify_songs.
spotify_songs=subset(spotify_songs,!is.na(spotify_songs$track_artist))

#9  What artist has the most energetic song?
spotify_songs[which.max(spotify_songs$energy),]$track_artist

#10 How many latin songs have a danceability level above 0.6 and popularity of 
#70 or more?
q10_1=subset(spotify_songs,(danceability>0.6)&(track_popularity>=70))
q10_2=subset(q10_1,q10_1$playlist_genre=="latin")
nrow(q10_2)

#11 
q11_1=c(0,0.4,0.6,0.8,1.0)
q11_2=cut(spotify_songs$energy,breaks=q11_1,right=T,
          labels=c("low","medium-low","medium-high","high")) 
spotify_songs=transform(spotify_songs,energyGroup=q11_2)

q11_3=sum(spotify_songs$energyGroup=="low")
q11_4=sum(spotify_songs$energyGroup=="medium-low")
q11_5=sum(spotify_songs$energyGroup=="medium-high")
q11_6=sum(spotify_songs$energyGroup=="high")
n_artist=c(q11_3,q11_4,q11_5,q11_6)
names(n_artist)=c("low","medium-low","medium-high","high")
n_artist

#12 Create a new column called duration_min that contains 
#the track duration in minutes. 
spotify_songs=transform(spotify_songs,duration_min=duration_ms/(60000))

#13 Sort the spotify_songs data frame in ascending order according to the energy level.
spotify_songs=spotify_songs[order(spotify_songs$energy),]

#14
x=spotify_songs$energy
y=spotify_songs$danceability
X=cbind(1,x,x^2,x^3,x^4)

#15
beta=solve(t(X)%*%X,t(X)%*%y)
y.hat=X%*%beta

#16
reg=list(list(x,y),X,y.hat)
names(reg)=c("data","designM","yhat")
reg

#17
#p1
duration=data.frame(runif(10000,72080,513440),runif(10000,72080,513440))  

#p2
names(duration)=c("d_1","d_2")
  
#p3
duration=transform(duration,d_12avg=(d_1+d_2)/2)

#p4
N1=subset(spotify_songs,track_id=='26416')$duration_ms
N2=2*N1
p=nrow(subset(duration,duration$d_12avg>=N2))/nrow(duration)
p  


