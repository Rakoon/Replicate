#===============================================================================
# R codes for extracting NBA data from basketball.sqlite
# Coder: Rak
# Updated: 2023-08-19
# wyattowalsh's Comprehensive NBA Basketball SQLite Database (formerly on Kaggle)
# https://github.com/wyattowalsh/nba-db
#===============================================================================

install.packages("RSQLite")
library("RSQLite")
setwd("Your directory")

# Tutorial code: https://stackoverflow.com/questions/9802680/how-to-import-from-sqlite-database
con <- dbConnect(drv=RSQLite::SQLite(), dbname="basketball.sqlite") # connect to db
tables <- dbListTables(con) # list all tables
tables <- tables[tables != "sqlite_sequence"] # (exclude sqlite_sequence containing table information)
lDataFrames <- vector("list", length=length(tables)) # Create a list for the dataframes that we want

### Pull data.frames from our list
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}

### Export stuff into separate data frames
PlayerDraftData = as.data.frame(lDataFrames[[1]]) # Draft information: from which school to which team (1949-2020)
PlayerBioData = as.data.frame(lDataFrames[[2]]) # Basic player information (2001-2021)
TeamGameData = as.data.frame(lDataFrames[[3]]) # Team total statistics for each game (1946-2021)
PlayerGameData = as.data.frame(lDataFrames[[4]]) # Roster of players who appeared in each game (1946-2021)
RefGameData = as.data.frame(lDataFrames[[5]]) # Who ref'd which games
#NewsData = as.data.frame(lDataFrames[[6]]) # News stories; VERY SLOW to load and irrelevant to my current project.
# DF[[7]] is also about news scraping results. Irrelevant.
# DF[[8]] is a list of first and last names.
# DF[[9]] is a roughly combined player bio and performance stats
PayTradeData = as.data.frame(lDataFrames[[10]]) # Trades and Salaries data (1985-2026 contracts)
# DF[[11]] contains activity status (e.g., first & last seasons; how many seasons in the NBA)
ContractData = as.data.frame(lDataFrames[[12]]) # Contract info (e.g., Guaranteed vs. NonGuaranteed; Salary Amount; On the Roster Dummy)
# DF[[13]] has team info like [[14]] but with less data.
TeamBioData = as.data.frame(lDataFrames[[14]]) # Team names, abbreviations, cities, founding dates, head coach & GM names
# DF[[15]] has team info like [[14]] but also with defunct franchises.
TeamPayData = as.data.frame(lDataFrames[[16]]) # Team-level salaries

#===============================================================================
### Export select data to csv
write.csv(TeamGameData, na = "", "NBA_GameLevelData1946-2021.csv", row.names = F) # Team total statistics for each game (1946-2021)
