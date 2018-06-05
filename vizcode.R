
library(dplyr)
library(tidyverse)
# setwd("C:/Users/yasit/Desktop/Blog")
min <- read.csv(file ="usg.csv", sep = ',', header = T)



#Table of Percent per rd by player

#%percent points = points scored by player/ totoal points by both teams per round

#sum(points by player) group by round
#sum(points by rd)

percent <- read.csv('PLAYOFFSDB.csv', header = TRUE )
head(percent)


#player_scorepercent(x,r) = player_score(x,r)/ sum(team_score(r))
#Points

sumscore <- percent%>% 
  group_by(RD) %>% 
  summarise(sum(PTS))

playerscore <- percent %>% 
  group_by(Player, RD)%>% 
  summarise(sum(PTS))

Scorepercent <-full_join(sumscore, playerscore, by= "RD")
colnames(Scorepercent) <- c("Round", "TotalPoints", "Player", "PlayerPoints")

for(i in 1:nrow(Scorepercent)){
  Scorepercent$percentage[i] <- Scorepercent$PlayerPoints[i] / Scorepercent$TotalPoints[i]
}

Scorepercent %>%
  arrange(desc(percentage))


#Github improvement is to use map()
# percent <- function(percentof){
# Scorepercent$PlayerPoints(percentof) / Scorepercent$TotalPoints(percentof)
# }
# result <- mutate(as.character(Scorepercent$Player), result = map(Scorepercent$Player, percent))


#Assits 
sumassits <- percent%>% 
  group_by(RD) %>% 
  summarise(sum(AST))

playerassits <- percent %>% 
  group_by(Player, RD)%>% 
  summarise(sum(AST))

assistspercent <-full_join(sumassits, playerassits, by= "RD")
colnames(assistspercent) <- c("Round", "Totalassits", "Player", "Playerassits")

for(i in 1:nrow(assistspercent)){
  assistspercent$percentage[i] <- assistspercent$Playerassits[i] / assistspercent$Totalassits[i]
}

assistspercent %>%
  arrange(desc(percentage))


#Rebounds
```{r}
sumrebs <- percent%>% 
  group_by(RD) %>% 
  summarise(sum(TRB))

playerreb <- percent %>% 
  group_by(Player, RD)%>% 
  summarise(sum(TRB))

rebpercent <-full_join(sumrebs, playerreb, by= "RD")
colnames(rebpercent) <- c("Round", "Totalreb", "Player", "Playerreb")

for(i in 1:nrow(rebpercent)){
  rebpercent$percentage[i] <- rebpercent$Playerreb[i] / rebpercent$Totalreb[i]
}

rebpercent %>%
  arrange(desc(percentage))

#Merging sets
df <-merge( Scorepercent,
            merge(assistspercent, rebpercent, by.x = (c("Round", "Player")), by.y = c("Round", "Player")),
            by.x = c("Round", "Player"), by.y = c("Round", "Player")
)

# df1 <-
df %>%
  select("Round", "Player", "percentage", "percentage.x", "percentage.y")  %>% #x =assits, #y = rebounds
  mutate(average = mean(c(percentage, percentage.x, percentage.y))) %>%
  rowwise() %>% 
  ungroup() %>%
  arrange((average))



