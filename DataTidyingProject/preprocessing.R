library(RCurl)
library(tidyverse)
library(factoextra)
file_name <- getURL("https://raw.githubusercontent.com/dazunalejandro/Data_tidying_Assignment/master/DataTidyingProject/movies.csv")
movies <- read.csv(text = file_name)

movies <- as.tibble(movies)

###############################
###############Only consider movies from 2010-2016
###############################
movies_1016 <- movies%>%
  filter(year %in% c("2005","2006","2007","2008","2009",
                     "2010","2011","2012","2013","2014","2015","2016")) %>%
          filter(budget != 0)


###############################
###############Add continent column 
###############################
American_countries <- c("Argentina","Colombia","Chile","Mexico","Panama","USA",
                        "Canada")
European_countries <- c("Austria","Belgium","Czech Republic","Denmark",
                        "Finland","France","Germany","Hungary","Norway",
                        "Spain","Switzerland","Malta","UK","Ireland")
Asian_countries <- c("Hong Kong","India","Indonesia","Iran","Israel","Russia",
                     "Taiwan","China","Japan","South Korea")
Oceania_countries <- c("New Zealand","Australia")
African_countries <- c("South Africa","Kenya")

movies_1016$continent <- NA
movies_1016$continent[movies_1016$country %in% American_countries] <- "America"
movies_1016$continent[movies_1016$country %in% European_countries] <- "Europe"
movies_1016$continent[movies_1016$country %in% Asian_countries] <- "Asia"
movies_1016$continent[movies_1016$country %in% Oceania_countries] <- "Oceania"
movies_1016$continent[movies_1016$country %in% African_countries] <- "Africa"

###############################
###############Convert score to a factor of three levels
###############################
movies_1016 <- movies_1016 %>% 
  mutate(scoreFactor=cut(score, breaks=c(-Inf, 5, 7, Inf), labels=c("Bad","Good","Excellent")))

###############################
###############Convert score to a factor of three levels
###############################
movies_1016 <- movies_1016 %>% 
  mutate(votesFactor=cut(votes, breaks=c(-Inf, 73864,158515, Inf), labels=c("Unpopular","Average","Popular")))


###############################
###############Reduce the number of levels of rating factor
###############################
movies_1016$rating[movies_1016$rating=="UNRATED"] <- "G"
movies_1016$rating[movies_1016$rating=="Not specified"] <- "G"
movies_1016$rating[movies_1016$rating=="NOT RATED"] <- "G"
movies_1016$rating[movies_1016$rating=="TV-MA"] <- "NC-17"
movies_1016$rating[movies_1016$rating=="B"] <- "PG-13"
movies_1016$rating[movies_1016$rating=="B15"] <- "PG-13"
movies_1016$rating[movies_1016$rating=="TV-14"] <- "PG-13"


###############################
###############Convert runtime to factor of three levels
###############################
movies_1016 <- movies_1016 %>% 
  mutate(runtime=cut(runtime, breaks=c(-Inf, 120, 140, Inf), labels=c("Short RT","Average RT","Long RT")))


###############################
###############Create a new variable "gross per genre"
###############################
movies_1016 <- movies_1016 %>%
  group_by(year,genre) %>%
  mutate(grossPergenre=(sum(gross)))



###############################
###############Rescale budget, grossPergenre, and votes
###############################
movies_1016$budget <- log(movies_1016$budget)
movies_1016$grossPergenre <- log(movies_1016$grossPergenre)
movies_1016$votes <- log(movies_1016$votes)
movies_1016$gross <- log(movies_1016$gross)
# 
# names(movies_1016)
# data <- movies_1016[, c(1, 6)]
# clusters <- kmeans(data, 2)
# pclusters$size/
# Index_cluster1=which(kmeans(data, 2)$cluster==1)
# # Index_cluster2=which(kmeans(data, 2)$cluster==2)
# # Index_cluster3=which(kmeans(data, 2)$cluster==3)
# # Index_cluster4=which(kmeans(data, 2)$cluster==4)
# # Index_cluster5=which(kmeans(data, 2)$cluster==5)
# # Index_cluster6=which(kmeans(data, 2)$cluster==6)
# # Index_cluster7=which(kmeans(data, 2)$cluster==7)
# # Index_cluster8=which(kmeans(data, 2)$cluster==7)
# # Index_cluster1
# data_count <- movies_1016 %>%
#   filter(row_number() %in% Index_cluster1) %>%
#   group_by(genre) %>%
#   tally() %>%
#   mutate(freq = n/sum(n))
# 
# which
# c <- (data_count$genre[which.max(data_count$n)])
# data_count
# tab <- which.max(table(movies_1016[Index_cluster1,]$genre))
# tab
# tab <- which.max(table(movies_1016[Index_cluster1,]$genre))
# tab
# tab<-max(table(movies_1016[Index_cluster1,]$genre))
# tab
