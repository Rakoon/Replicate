#=========================================================================================
# NBA Individual- and Team-Level Data Scrape
# Coder: Rak
# Updated: 2022-07-31
#=========================================================================================
### Load packages
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2) # expand max connection size; otherwise error occurs. I arbitrarily chose a large number that worked.
# library(devtools)
# devtools::install_github("abresler/nbastatR")
# See details at: https://github.com/abresler/nbastatR
library(nbastatR) # Still works as of 2022/07/31. Need to first install "devtools" from line 10.
library(dplyr)

### Scrape PLAYER level data for 1951-2022
bref.player = bref_players_stats(seasons = 1951:2022, tables = c("advanced", "totals"))
unique(bref.player$yearSeason) # Check if we got the correct years
# Note that Player Efficiency Rating was introduced in the 1952 season, so all PER values = 0 for the first year (1951) of our data. We need to change them to missing.

#----------------------------------------------------------------------------------------------------------
### Scrape TEAM level data for 1951-2022
# We use for loop because manually writing 70+ lines of code is silly. The assign() function allows us to dynamically assign scraped object names. Otherwise, newly scraped objects will overwrite old ones.
for (i in 1951:2022) {
  TeamDB = list() # Make an empty list, then save the scraped data
  assign(paste0("TeamDB", i), data.frame(bref_teams_stats(seasons = i)))
}
# Our scraped TEAM data are shown on the Global Environment as TeamDB####. Each object is 1 obs. of 2 variables. Cell (TeamDB####)[[2]][[1]] has the TEAM data table that we need. And so, we must convert the scraped TEAM level data from a multi-layered object into a data frame.
df_names <- ls(pattern="^TeamDB\\d+") # Create a list of the object names that we're manipulating
TeamDB_list <- lapply(mget(df_names), function(DF){
  DF <- DF[[2]][[1]]
  }) # Create the function to replace each multi-layered object with the table within, then put them all in a single list (TeamDB_list)
list2env(TeamDB_list,envir=.GlobalEnv) # Extract the stuff from within the list. Now all the TEAM data table are immediately accessible instead of being hidden in (TeamDB####)[[2]][[1]]

# Append all of the scraped objects. We get all the objects in our Global Environment using dplyr + pattern matching. 
# Otherwise, we would be manually coding: bref.team = rbind(TeamDB1951, TeamDB1952, ..., TeamDB2022), which is an inefficient approach.
NBATeamDB = mget(ls(pattern="^TeamDB\\d+")) %>% bind_rows()  # mget() searches for all objects with "^TeamDB" at the start of the string, followed by 1 or more digits (\\d+).

#=========================================================================================
### Some data cleaning
NBATeamDB = NBATeamDB %>% relocate(nameTeam, slugTeamBREF, .before = slugSeason) # Reorder columns
NBATeamDB = select(NBATeamDB, -c(isSeasonCurrent, urlSeasonBREF, rankConferenceTeam, urlBREFTeamData, urlThumbnailTeam, nameDivision, 
                                 gamesBehind1ConferenceStandingsConf, ptsOppPerGameStandingsDiv, 
                                 ptsTeamPerGameStandingsDiv, ratingStrengthOfScheduleStandingsDiv)) # Remove a few unnecessary columns

# Export scraped data to csv
setwd("Your directory")
write.csv(bref.player,"NBA_PlayerDB1951-2022.csv", row.names = F) # Combined Player data
write.csv(dataBREFPlayerTotals,"NBA_PlayerTotalDB.csv", row.names = F) # Total Stats Player data
write.csv(dataBREFPlayerAdvanced,"NBA_PlayerAdvancedDB.csv", row.names = F) # Advanced Stats Player data
write.csv(NBATeamDB,"NBA_TeamDB1951-2022.csv", row.names = F) # Team data

# For multi-teams players
NBAMultiTeam = select(bref.player, c(yearSeason, slugPlayerSeason, slugPlayerBREF, namePlayer, slugTeamsBREF))
write.csv(NBAMultiTeam, "NBA_MultiTeam_Players.csv", row.names = F)
