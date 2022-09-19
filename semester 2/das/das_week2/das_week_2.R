#das_week_2
#for internet connect problem please use singapore 3 route
install.packages("robotstxt")
install.packages("rvest")
library(robotstxt)
library(rvest)


paths_allowed("http://www.imdb.com")
paths_allowed("http://www.facebook.com")

page <- read_html("https://www.imdb.com/chart/top/")
page
typeof(page)
class(page)

#use selector gadge, scrape the nodes
page %>%
  html_nodes(".titleColumn a")

#get the text out of the nodes
titles=page %>%
  html_nodes(".titleColumn a")%>%
  html_text()
titles

#scrape years movies were made and save as years
page%>%
  html_nodes(".secondaryInfo")%>% #only needs text
  html_text() #get rid of html coding arozund

#stringr package
install.packages("stringr")
library("stringr")
str_remove(string="jello",pattern="el")

str_replace(string="jello",pattern="j",replacement="h")

#clearn up the text
years=page%>%
  html_nodes(".secondaryInfo")%>%
  html_text()%>%
  str_remove("\\(")%>%
  str_remove("\\)")%>%
  #remove(, //( means maybe there is special meaning, whatever 
  #the meaning is just remove (
  as.numeric()
years

#scrape the nodes
ratings=page%>%
  html_nodes("strong") %>%
  html_text%>%
  as.numeric()
ratings

imdb_top_250=tibble(
  title=titles,
  year=years,
  rating=ratings #varibles names=contents of the variable
)
imdb_top_250

#clean up/enhance
glimpse(imdb_top_250)
#add a variable for rank
imdb_top_250=imdb_top_250%>%
  mutate(rank=1:nrow(imdb_top_250))%>% #add new varibale rank
  relocate(rank) #move rank to first variable

#WCHI YEARS HAVE THE MOST movies on the list
imdb_top_250%>%
  count(year,sort=TRUE)

#which 1995 movies made the list
imdb_top_250%>%
  filter(year==1995)%>%
  print(n=8)

#visualize the average yearly rating for movies that made it on the top 250 list over time
imdb_top_250%>%
  group_by(year)%>%
  summarise(avg_score=mean(rating))%>%
  ggplot(aes(y=avg_score,x=year))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE+
                labs(x="Year",y="Average score")
              
