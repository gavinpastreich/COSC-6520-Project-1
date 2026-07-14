# Gavin Pastreich project 1
# COSC 6520
# 
# An analysis on identifying "Breakout" players in the NPB


rm(list=ls())

#Libraries

library(sqldf)
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(caret)
library(gains)
library(pROC)
library(rpart)
library(rpart.plot)
library(forecast)
library(randomForest)
library(adabag)


#Data Loading


BattingData <- read.csv("C:/Users/gavin/OneDrive/Desktop/2026 Spring Courses/COSC 6520/Project 1/BattingData.csv")

#AtBtas will be needed later

AtBats <- read.csv("C:/Users/gavin/OneDrive/Desktop/2026 Spring Courses/COSC 6520/NPB Data/AtBats.csv")



head(BattingData)


#Now that the data is loaded, have to clean

#First is making column names easier to understand

names(BattingData)[names(BattingData) == "Game.Type"] <- "GameType"

names(BattingData)[names(BattingData) == "X."] <- "Number"

names(BattingData)[names(BattingData) == "X1B"] <- "Single"

names(BattingData)[names(BattingData) == "X2B"] <- "Double"

names(BattingData)[names(BattingData) == "X3B"] <- "Triple"

names(BattingData)[names(BattingData) == "DB"] <- "HBP"


#removing playoff statistics, only regular season
#With my background, SQLdf is easier and quicker for rapid transformations

#Playoffs not necessary, also not counted to season statistics. 
ggplot(BattingData, aes(x = GameType, fill = GameType)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
labs(title = "Types of Games", x = "Type of Game", y = "Count") +
  theme_minimal()


BattingData <- sqldf("Select * from BattingData where GameType == 'Regular Season'")


#Need to fix Season 1936-1938 cause of spring and fall. Then update the column to a integer. 
#Currently Season is a character due to 1936, 1937, and 1938 having spring and fall seasons
#Updating them for now, and making season a integer

BattingData$Season[BattingData$Season == '1936 Spring'] <- 1936

BattingData$Season[BattingData$Season == '1936 Fall'] <- 1936

BattingData$Season[BattingData$Season == '1937 Spring'] <- 1937

BattingData$Season[BattingData$Season == '1937 Fall'] <- 1937

BattingData$Season[BattingData$Season == '1938 Spring'] <- 1938

BattingData$Season[BattingData$Season == '1938 Fall'] <- 1938

BattingData$Season <- as.integer(BattingData$Season)



#Next thing is removing pitchers. I really don't care about pitchers, offense isn't their job.

#Summating data in this join, and joining based on season, name, and playerID will
#Account for a players whole season even if they were traded and played for two teams.
#All necessary columns to summate are num or INT

ggplot(BattingData, aes(x = Position, fill = Position)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)+
labs(title = "Position Count", x = "Position", y = "Count") +
  theme_minimal()

#Pitchers are a overwhelming part of the dataset.
#Pitchers are never Offensive producers unless it's Shohei Ohtani who is classified as a DH here anyways.

#Removing Pitchers 
#Also grouping players into their individual rows. If they were traded during season they would have two records
BattingData <- sqldf("
select Season, Name, PlayerID,
sum(G) G, sum(PA) PA, sum(AB) AB, sum(BA) BA, sum(OBP) OBP, sum(SLG) SLG, sum(OPS) OPS,
sum(R) R, sum(RBI) RBI, sum(H) H, sum(Single) Single, Sum(Double) Double, sum(Triple) Triple, sum(HR) HR,
sum(TB) TB, sum(SB) SB, sum(CS) CS, sum(BB) BB, sum(IBB) IBB, sum(K) K, sum(DP) DP, sum(HBP) HBP, sum(SH) SH, sum(SF) SF
from BattingData
where position != 'P'
group by Season, Name, PlayerID
order by name asc
      ")

#Age is not in the dataset, which is quite unfortunate. What I can do though

#Apply a variable called "ServiceTime", to all players based on the first year they
 # Appear in the dataset, and each additional time is +1 to servicetime. 

BattingData <- BattingData %>%
  group_by(PlayerID) %>%
  arrange(Season) %>% #Making sure servicetime is arranged by season so it starts at oldest instance of a player
  mutate(ServiceTime = row_number()) %>%
  ungroup()

ggplot(BattingData, aes(x = ServiceTime, fill = ServiceTime)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)+
  labs(title = "NPB ServiceTime Instances from 1936-2025", x = "Years in league", y = "Count") +
  theme_minimal()


#I am also removing all data before 1984 for now. It removes every duplicate and NA
#Also, what's the point of having data back from the 70s, sports have evolved drastically since. 

BattingData <- sqldf("select * from BattingData where Season > 1984")

summary(BattingData)

#Zero NAs
sum(is.na(BattingData))



#There are players who played games yet had no accrued data at all. They are not necessary

ZeroStats <- sqldf("select * from BattingData 
      where PA = 0 and AB = 0 and BA = 0 and OBP = 0 and SLG = 0 and OPS = 0 and R = 0
            and RBI = 0 and H = 0 and Single = 0 and Double = 0 and Triple = 0 and HR = 0 and TB = 0 and SB = 0
            and CS = 0 and BB = 0 and IBB = 0 and K = 0 and DP = 0 and HBP = 0 and SH = 0 and SF = 0")

#174 people who didn't have any stats. Remove them. 


BattingData <- sqldf("select * from BattingData
      EXCEPT
      select * from BattingData 
      where PA = 0 and AB = 0 and BA = 0 and OBP = 0 and SLG = 0 and OPS = 0 and R = 0
            and RBI = 0 and H = 0 and Single = 0 and Double = 0 and Triple = 0 and HR = 0 and TB = 0 and SB = 0
            and CS = 0 and BB = 0 and IBB = 0 and K = 0 and DP = 0 and HBP = 0 and SH = 0 and SF = 0")



#Dataset has been shrunk a fair bit.
#With the summating earlier, there are statistics incorrect due to them being calculations
#BA, OBP, SLG, and OPS are now off. Have to fix those.

#BA = H / AB 

BattingData$BA <- round((BattingData$H / BattingData$AB),3)

summary(BattingData$BA)

BattingData$BA[is.na(BattingData$BA) == TRUE] <- 0

#OBP = (H + BB + HBP) / (AB + BB + HBP + SF)

BattingData$OBP <- round((BattingData$H + BattingData$BB + BattingData$HBP) / (BattingData$AB + BattingData$BB + BattingData$HBP + BattingData$SF),3)

summary(BattingData$OBP)

BattingData$OBP[is.na(BattingData$OBP) == TRUE] <- 0

#SLG = (Single + (2*double) + (3* triple) + (4*Home run)) / AB

BattingData$SLG <- round(((BattingData$Single + (2*BattingData$Double) + (3*BattingData$Triple) + (4*BattingData$HR))/BattingData$AB),3)

summary(BattingData$SLG)

BattingData$SLG[is.na(BattingData$SLG) == TRUE] <- 0

#OPS = OBP + SLG
BattingData$OPS <- round((BattingData$OBP + BattingData$SLG),3)

#Now that BattingData is fully loaded and clean, time to load the other dataset


#####

#AtBats
#wRC is a metric that isn't easily available. Using play by play I will be able to apply a
# League standardized value to the rest of the league. 

#####

#AtBats <- read.csv("C:/Users/gavin/OneDrive/Desktop/2026 Spring Courses/COSC 6520/NPB Data/AtBats.csv")


#Updating Column Names

names(AtBats)[names(AtBats) == "Index"] <- "PlayIndicator" #Index would break things later

AtBats$X <- NULL

names(AtBats)[names(AtBats) == "X.1"] <- "Inning"

AtBats$X.2 <- NULL

AtBats$Pitcher.1 <- NULL

AtBats$X.3 <- NULL

names(AtBats)[names(AtBats) == "Player.1"] <- "Batter"

AtBats$Player <- NULL

names(AtBats)[names(AtBats) == "On.Base"] <- "Situation"

AtBats$Count <- NULL

#The Result column has a lot of needed information 
#Such as the Runs, and the type of play

sqldf("select count(distinct result) from AtBats") #345 different outcomes

#Creating new column, Runs (Correspondaing to the amount of runs scored on a given play)

AtBats["Runs"] <- NA


#Looking at each result case to identify amount of runs scored
AtBats <- AtBats %>%
  mutate(
    Runs = case_when(
      str_detect(Result, regex("1", ignore_case = TRUE)) ~ "1",
      str_detect(Result, regex("2", ignore_case = TRUE)) ~ "2",
      str_detect(Result, regex("3", ignore_case = TRUE)) ~ "3",
      str_detect(Result, regex("4", ignore_case = TRUE)) ~ "4",
      str_detect(Result, regex("Successful stolen base attempt on home", ignore_case = FALSE)) ~ "1",
      
      TRUE ~ "0"
    )
  )

AtBats$Runs <- as.numeric(AtBats$Runs)


unique(AtBats$Situation) #This is who is on base at any given time, needed for Run Expectancy

#Updating the "Situation" column into something more readable is needed

AtBats$Situation <- ifelse(is.na(AtBats$Situation), "0-0-0", AtBats$Situation)

AtBats$Situation <- ifelse(AtBats$Situation == "", "0-0-0", AtBats$Situation)

AtBats$Situation <- ifelse(AtBats$Situation == "1・2", "1-2-0", AtBats$Situation)

AtBats$Situation <- ifelse(AtBats$Situation == "1・3", "1-0-3", AtBats$Situation)

AtBats$Situation <- ifelse(AtBats$Situation == "2・3", "0-2-3", AtBats$Situation)

AtBats$Situation <- ifelse(AtBats$Situation == "1・2・3", "1-2-3", AtBats$Situation)

AtBats$Situation <- ifelse(AtBats$Situation == "1", "1-0-0", AtBats$Situation)

AtBats$Situation <- ifelse(AtBats$Situation == "2", "0-2-0", AtBats$Situation)

AtBats$Situation <- ifelse(AtBats$Situation == "3", "0-0-3", AtBats$Situation)



#Removing a duplicate GameID record

AtBats <- sqldf("select distinct * from AtBats")




#Creating way to identify type of play "result" is
AtBats["PlayCode"] <- NA


#The metrics I need to group together are outs (scaling), Sacrifices (outs that don't count to the batter)
#singles (1b), doubles (2b), triples(3b), home runs (HR),
# Unintentional walks (BB), and hit by pitches (HBP)


#testing
AtBats <- AtBats %>%
  mutate(
    PlayCode = case_when(
      #Outs
      str_detect(Result, regex("Groundout", ignore_case = TRUE)) ~ "BatterOut",
      str_detect(Result, regex("Line-out", ignore_case = TRUE)) ~ "BatterOut",
      str_detect(Result, regex("Struck out", ignore_case = TRUE)) ~ "BatterOut",
      str_detect(Result, regex("Strikeout", ignore_case = TRUE)) ~ "BatterOut",
      str_detect(Result, regex("Fly ball", ignore_case = TRUE)) ~ "BatterOut",
      str_detect(Result, regex("Foul fly", ignore_case = TRUE)) ~ "BatterOut",
      
      #Sacrifice Outs do not count as plate appearances, necessary for later calculation.
      str_detect(Result, regex("Sacrifice", ignore_case = TRUE)) ~ "Sacrifice",
      
      #Plays that change the batting situation
      str_detect(Result, regex("Successful stolen base", ignore_case = TRUE)) ~ "AlternativePlay",
      str_detect(Result, regex("No result - ", ignore_case = TRUE)) ~ "AlternativePlay",
      str_detect(Result, regex("Passed ball", ignore_case = TRUE)) ~ "AlternativePlay",
      str_detect(Result, regex("Batter interference", ignore_case = TRUE)) ~ "AlternativePlay",
      str_detect(Result, regex("Base running interference", ignore_case = TRUE)) ~ "AlternativePlay",
      
      str_detect(Result, regex("Failed stolen base", ignore_case = TRUE)) ~ "AlternativeOut",
      str_detect(Result, regex("picked off", ignore_case = TRUE)) ~ "AlternativeOut",
      str_detect(Result, regex("out", ignore_case = TRUE)) ~ "AlternativeOut",
      
      
      #Singles
      str_detect(Result, regex("Timely base hit", ignore_case = FALSE)) ~ "1B",
      str_detect(Result, regex("Base hit", ignore_case = FALSE)) ~ "1B",
      str_detect(Result, regex("Bunt hit", ignore_case = FALSE)) ~ "1B",
      
      #Doubles      
      str_detect(Result, regex("Timely two base hit", ignore_case = FALSE)) ~ "2B",
      str_detect(Result, regex("Two base hit", ignore_case = FALSE)) ~ "2B",
      str_detect(Result, regex("Ground rule double", ignore_case = FALSE)) ~ "2B",
      
      #Triples
      str_detect(Result, regex("Timely three base hit", ignore_case = FALSE)) ~ "3B",
      str_detect(Result, regex("Three base hit", ignore_case = FALSE)) ~ "3B",
      #Homers
      str_detect(Result, regex("home run", ignore_case = TRUE)) ~ "HR",
      #BBs (walks)
      str_detect(Result, regex("Base on balls", ignore_case = FALSE)) ~ "BB",
      #DB or HBP, hit by pitch
      str_detect(Result, regex("Dead ball", ignore_case = FALSE)) ~ "HBP",
      #Intentional walks (IBB)
      str_detect(Result, regex("Intentional base on balls", ignore_case = FALSE)) ~ "IBB",
      
      
      TRUE ~ NA
    )
  )


#The first step of calculating wRC is a run expectancy matris

#Columns that will value each play using a run expectancy matrix
AtBats["PreSituation"] <- NA #Situation at start of play

AtBats["PostSituation"] <- NA #Situation at end of play

AtBats["PreOuts"] <- NA #Outs at start of play

AtBats["PostOuts"] <- NA #Outs after the play

AtBats["PreRE"] <- NULL #Run Expectancy before the play

AtBats["PostRE"] <- NULL #RUn Expectancy after the play 

AtBats["RunValue"] <- NULL #PostRE - PreRE. Able to see how many runs a play created for a team. 



#For each game, grab the situation before and after for each play
#lead grabs the future / next row
AtBats <- AtBats %>%
  arrange(GameID, Inning, PlayIndicator) %>%
  group_by(GameID, Inning) %>%
  mutate(
    PreSituation = Situation,
    PostSituation = lead(Situation, order_by = PlayIndicator),
    PostSituation = if_else(is.na(PostSituation), "EndHalfInning", PostSituation),
    PreOuts = Outs,
    PostOuts = case_when(
      PostSituation == "EndHalfInning" ~ 3,
      TRUE ~ lead(Outs, order_by = PlayIndicator)
    )
  ) %>%
  ungroup()


#Since I want data to be accurate by year, adding in season

AtBats['Season'] <- NA

head(AtBats$GameID, 1) #Can be seen that they have the date. 

#GameID has season in each one. Will allow me to apply final wRC values to the right season
AtBats <- AtBats %>%
  mutate(
    Season = case_when(
      str_detect(GameID, regex("2016", ignore_case = TRUE)) ~ "2016",
      str_detect(GameID, regex("2017", ignore_case = TRUE)) ~ "2017",
      str_detect(GameID, regex("2018", ignore_case = TRUE)) ~ "2018",
      str_detect(GameID, regex("2019", ignore_case = TRUE)) ~ "2019",
      str_detect(GameID, regex("2020", ignore_case = TRUE)) ~ "2020",
      str_detect(GameID, regex("2021", ignore_case = TRUE)) ~ "2021",
      str_detect(GameID, regex("2022", ignore_case = TRUE)) ~ "2022",
      str_detect(GameID, regex("2023", ignore_case = TRUE)) ~ "2023",
      str_detect(GameID, regex("2024", ignore_case = TRUE)) ~ "2024",
      str_detect(GameID, regex("2025", ignore_case = TRUE)) ~ "2025",
      
      
      TRUE ~ "0"
    )
  )


#Making it match with preOuts
AtBats$PostOuts <- as.integer(AtBats$PostOuts)


#Play by Play data is now loaded, time to do the math part to get my analysis started

#####


#Run Expectancy matrix time. 

#A run expectancy matrix is an 8x3 representation on the likelihood that a run will score
# Given a baserunning and out situation. 8 possible base setups, 3 out possibilities

#I want to do this season by season so

seasons <- unique(AtBats$Season) #2016-2025, something to fix for BattingData


RunExpectancy <- AtBats %>%
  arrange(GameID, PlayIndicator) %>% #PlayByPlay Ordering
  group_by(Season, GameID, Inning) %>% #Each specific inning per game
  mutate(RunsFromPoint = sum(Runs) - (cumsum(Runs) - Runs)) %>% #Runs to end of inning from a situation
  ungroup() %>%
  group_by(Season, Situation, PreOuts) %>% #Grouping situations by year together
  summarise(RE = mean(RunsFromPoint)) %>% #Count of runs divided by situation count
  ungroup()



RunExpectancy #good matrix, 24 rows x 10 years = 240
#I hate how this is named
names(RunExpectancy)[names(RunExpectancy) == "PreOuts"] <- "Outs"

#240 observations makes sense, 10 years. 4 variables, Season, and each Outs category

#This takes the run expectancy matrix and calculates the runs gained or lost per play

AtBats <- AtBats %>%
  left_join(RunExpectancy, by = c("Season", "PreSituation" = "Situation", "PreOuts" = "Outs")) %>%
  rename(PreRE = RE) %>%
  left_join(RunExpectancy, by = c("Season", "PostSituation" = "Situation", "PostOuts" = "Outs")) %>%
  rename(PostRE = RE) %>%
  mutate(
    PostRE = ifelse(is.na(PostRE), 0, PostRE),
    RunValue = PostRE - PreRE + Runs
  )


#Now that I have run expectancy per play, have to apply to the type of play
#For wOBA which is used in wRC
#Getting the average run value per offensive play. 

#For the wOBA equation, need unintentional walks, HBP, Singles, Doubles, Triples, Homers
#And have to do Outs as a standardized, which will be added to the weights

#Steps
#1. Calculate yearly league OBP to have
#2. Calculate yearly wOBA weights AND SCALE by outs

unique(AtBats$PlayCode)
#Linear weights are used in wOBA
LinearWeightPlays <- c("BatterOut", "BB", "1B", "2B", "IBB", "HR", "HBP", "3B")

LinearWeights <- AtBats %>%
  filter(PlayCode %in% LinearWeightPlays) %>%
  group_by(Season, PlayCode) %>%
  summarise(LinearWeights = mean(RunValue)) %>%
  ungroup() %>%
  group_by(Season) %>%
  mutate(
    OutValue = LinearWeights[PlayCode == "BatterOut"],
    AdjustedLinearWeight = LinearWeights - OutValue
  ) %>%
  ungroup() %>%
  select(Season, PlayCode, AdjustedLinearWeight) %>%
  pivot_wider(names_from = PlayCode, 
              values_from = AdjustedLinearWeight,
              names_prefix = "lw_")
  


#Now I have the linear weights I need for calculating the other stuff. Time to apply to season stats

#Need to put season back as a character to match the new weights

BattingData$Season <- as.character(BattingData$Season)

#League wOBA has to be the sum of all values in a season multiplied by the 
#Associated AdjustedLinearWeight. Example cause I can see it in preview on side
# Ex: 2016 Singles has adjustedlinearweight of .65, 10972 singles. #10972*.65

LeaguewOBA <- BattingData %>%
  group_by(Season) %>%
  summarise(
    BB = sum(BB), IBB = sum(IBB), HBP = sum(HBP),
    Single = sum(Single), Double = sum(Double),
    Triple = sum(Triple), HR = sum(HR),
    PA = sum(PA)
  ) %>%
  left_join(LinearWeights, by = "Season") %>%
  mutate(
    LwOBA = (BB * lw_BB + IBB * lw_IBB + HBP * lw_HBP +
                 Single * lw_1B + Double * lw_2B +
                 Triple * lw_3B + HR * lw_HR) / PA
  )


#The seasons I want have the wOBA applied. Keeping extra data right now in case needed
#Calculating LeagueOBP for scaling
LeageOBP <- BattingData %>%
  group_by(Season) %>%
  summarise(
    LOBP = sum(Single + Double + Triple + HR + BB + IBB + HBP) / sum(PA)
  )

#Because wOBA is meant to look like OBP, a scale is needed. 

# wOBA scale
wOBAScale <- LeaguewOBA %>%
  left_join(LeageOBP, by = "Season") %>%
  mutate(LwOBA_scale = LOBP / LwOBA)



# Player wOBA
PlayerwOBA <- BattingData %>%
  left_join(wOBAScale %>% select(Season, LwOBA_scale), by = "Season") %>%
  left_join(LinearWeights, by = "Season") %>%
  mutate(
    PwOBA = (BB * lw_BB + IBB * lw_IBB + HBP * lw_HBP +
              Single * lw_1B + Double * lw_2B +
              Triple * lw_3B + HR * lw_HR) / PA * LwOBA_scale
  )


#Time for wRC calculation

#wRC = (((wOBA-League wOBA)/wOBA Scale)+(League R/PA))*PA


# League R per PA
RPA <- BattingData %>%
  group_by(Season) %>%
  summarise(RPA = sum(R) / sum(PA))

wRC <- PlayerwOBA %>%
  left_join(LeaguewOBA %>% select(Season, LwOBA), by = "Season") %>%
  left_join(RPA, by = "Season") %>%
  mutate(
    wRC = (((PwOBA - LwOBA) / LwOBA_scale) + RPA) * PA
  )

#Putting it back to BattingData to begin breakout identification analysis



BattingData <- BattingData %>%
  left_join(wRC %>% select(Season, PlayerID, wRC), by = c("Season", "PlayerID"))


summary(BattingData)

#A lot of NA wRC, because the play by play data only went back to 2016

plot(BattingData$Season, BattingData$wRC)

#Remove everything 2015 and earlier. 

BattingData <- sqldf("select * from BattingData where Season > 2015")

#Now the question is what is a quantifiable breakout? 

#Clear positive trend in games to wRC

ggplot(BattingData, aes(x = G, y = wRC)) +
  geom_point(alpha = 0.6, size = 2) +
 labs(title = "wRC compared to Games played") +
  theme_minimal()

#It is visibly clear that there is a left skew to the data as well
#This makes sense, as star players are rare, and star players should play more.

ggplot(BattingData, aes(x = ServiceTime, y = wRC)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(title = "wRC compared to Games played") +
  theme_minimal()


#Longer careers are rarer obviously.
#wRC progressively increases through first 5 years of career then dips over time.
#Taking into account the competitive nature and lower average job length. 
mean(BattingData$ServiceTime) #6 years


#Since I'm hunting "Breakout" players, I need to first identify when breakouts occur

#Then I need to mark the season prior as a "Next Season Breakout".

#Showing the previous years wRC and the difference from the previous season

BattingData <- BattingData %>% 
  arrange(PlayerID, Season) %>%
  group_by(PlayerID) %>%
  mutate(prev_wRC = lag(wRC),
         wRC_diff = wRC - lag(wRC)) %>%
  ungroup()


#If a breakout doesn't occur odds are they won't have a job past year 6 (mean career length)
#First 5 years is probably bold but with a small dataset in NPB trying to find anything.

BattingData <- sqldf("select * from BattingData where ServiceTime < 6")

#Breakouts can happen in years 2, 3, 4, and 5. 
#Prediction servicetimes will be year 1, 2, 3, and 4.
#Issue with wRC data only being 2016-2025 is I am unable to get breakouts for second years in 2016


#Players who only appear in dataset 1 time. No multiple seasons, unnecessary when looking over multiple seasons
PlayerIDs <- sqldf("select PlayerID, count(*) from BattingData
      group by PlayerID having count(*) = 1")
#211 players

#These guys are rookies, need to keep them for prediction. 
sqldf("select * from BattingData where Season = 2025 and PlayerID in (select PlayerID from PlayerIDs)")
#53 players


RemovePlayerIDs <- sqldf("select PlayerID from BattingData where Season != 2025 and PlayerID in (select PlayerID from PlayerIDs)")
#158 players 

BattingData <- sqldf("select * from BattingData where PlayerID not in (select PlayerID from RemovePlayerIDs)")
#158 players removed


#Ok so dataset is finally ready to go. Lets identify breakouts and the prior season

summary(BattingData$wRC_diff)


#Still going to be a touch messy cause of guys that go to and from sometimes, but whatever. Late bloomers!
#I'm going off of wRC_diff cause wRC is standardized to the whole league. 


sqldf("select * from BattingData where wRC_diff > 100") #Problem here is their prev_wRC was so low, assume games were low

#Breakouts should only happen once. Lets see what these guys are up to
sqldf("select Name, PlayerID, count(*)  from BattingData where wRC_diff > 40 group by PlayerID having count(*) > 1")

sqldf("select * from BattingData where PlayerID = 21425139") #2018 is def his breakout year at 80 wrc diff

sqldf("select * from BattingData where PlayerID = 73375134") # good bad good again, not sure if really a breakout

sqldf("select * from BattingData where PlayerID = 73375151") #Injuries and a former big leaguer. 

#Manually refining the conditions for a valid wRC_diff. GOing to add a previous game count


sqldf("select * from BattingData where PlayerID = 11215151") #yeah guess this guy counts lol

#Setting a season wRC threshold will help remove some noise


#Final threshold
sqldf("select * from BattingData where wRC_diff >= 60 and wRC >= 80")

hist(BattingData$wRC)


BattingData <- BattingData %>% 
  arrange(PlayerID, Season) %>%
  group_by(PlayerID) %>%
  mutate(
    Breakout = ifelse(wRC_diff >= 60 & wRC >= 80 & ServiceTime >= 2 & ServiceTime <= 5, 1, 0),
    PreBreakout = lead(Breakout)
  ) %>%
  ungroup()



sqldf("select Season, sum(Breakout) from BattingData group by Season")

sqldf("select Season, PreBreakout, count(*) from BattingData group by Season, PreBreakout")

sqldf("select Season, Breakout, PreBreakout, count(*) from BattingData group by Season, Breakout, PreBreakout")


#Happy with this. There is one more cleaning before I begin modeling

#First is figuring out what the deal is with the NAs in PreBreakout. 

sqldf("select Season, ServiceTime, PreBreakout, count(*) from BattingData where PreBreakout is null
      group by Season, PreBreakout")


#The NAs are because of players who had no additional data afterward

BattingData <- BattingData %>%
  mutate(PreBreakout = ifelse(is.na(PreBreakout), 0, PreBreakout))


summary(BattingData) #So I don't care about all of these NAs because it was just a way to identify

#Breakout having some NAs is concerning. Time to look

sqldf("select * from BattingData where Breakout is null")

#Ok so it's because of 2016 or not having anything before.
#Doesn't matter cause I'm not looking on what is a breakout, I'm looking at what can predict a breakout.

sqldf("select * from BattingData where PlayerID = 1305137") #sup Shohei

#There are probably players in the data that are noise and affecting accuracy, but just gonna move on

#Dependent Variable is PreBreakout. 
#The Independent Variables are from G to ServiceTime. wRC was PURELY to standardize values


ShrinkData <- sqldf("select Name, Season, G, PA, AB, BA, OBP, SLG, OPS, R, RBI, H, Single, Double, Triple, HR,
              TB, SB, CS, BB, IBB, K, DP, HBP, SH, SF, ServiceTime, PreBreakout from BattingData")

sum(ShrinkData$PreBreakout) #Have all 28 which is good

#Something I alwayas do is look for any high levels of correlation before PCA. 

#Even though PCA will make non-correlated PCs, I still will have some variables that are so correlated
#That it makes sense to remove anyways

sum(is.na(ShrinkData))

library(corrplot)

cor <- cor(ShrinkData[,3:27])

corrplot(cor, method = 'color', type = "lower", number.cex = .5)

cor

#So Games, Plate Appearances, and At Bats are insanely correlated. PA essentially is 100% with AB
#This makes sense, more you play, more of these you will accrue. 

#I'll remove G and AB. Plate Appearances are the best indicator of offensive opportunity
ShrinkData$AB <- NULL
ShrinkData$G <- NULL

#I also will be removing total bases, as that is a full addition of singles, doubles, triples, and homers
ShrinkData$TB <- NULL

#Same with Hits
ShrinkData$H <- NULL

#And since OPS is a summation of SLG and OBP, I will remove OPS
ShrinkData$OPS <- NULL

ShrinkData$PreBreakout <- as.factor((ShrinkData$PreBreakout))



summary(ShrinkData)

#Servicetime of 5 cancnot be a breakout. Final thing in data cleaning!

ShrinkData <- sqldf("select * from ShrinkData where ServiceTime != 5")

write.csv(ShrinkData, "C:/Users/gavin/OneDrive/Desktop/2026 Spring Courses/COSC 6520/Project 1/ShrinkData.csv")

