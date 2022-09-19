#das_hw_1
library(rvest)
library(tidyverse)
page1 <- read_html("https://www.animenewsnetwork.com/encyclopedia/ratings-anime.php?top50=best_bayesian&n=250")
page1
typeof(page)
class(page)

titles=page1 %>%
  html_nodes(".t a")%>%
  html_text()
titles

ratings=page1%>%
  html_nodes(".t+ .r") %>%
  html_text%>%
  as.numeric()
ratings=ratings[-1]

nb.votes=page1%>%
  html_nodes(".r+ .r") %>%
  html_text%>%
  as.numeric()
nb.votes=nb.votes[-1]

anit250=tibble(
  title=titles,
  rating=ratings,
  nb.vote=nb.votes
)
View(anit250)

anit250=anit250%>%
  mutate(rank=1:nrow(anit250))%>% #add new varibale rank
  relocate(rank)

str(anit250)


mydestfile <- "/Users/kurisuuu/Downloads/output.xls " # change the path and file name as per your system
download.file(anit250, mydestfile, mode="wb")


write_excel_csv(anit250, mydestfile, na = "NA", append =
                  FALSE)

