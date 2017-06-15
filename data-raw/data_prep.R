# Prep seasonAVG data set.
seasonAVG <- read.csv("~/Downloads/seasonAVG.csv")
# These data come from Fangraphs
# http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=8&season=2015&month=0&season1=1901&ind=0&team=0,ss&rost=0&age=0&filter=&players=0

# Check to make sure new columns line up with the old. May need to adjust colnames if they don't match.
colnames(seasonAVG) <- c("yearID", "tot_G", "tot_PA", "tot_HR", "tot_R", "tot_RBI", "tot_SB", "avg_BB",
                         "avg_K", "avg_ISO", "avg_BABIP", "avg_BA", "avg_OBP", "avg_SLG", "avg_wOBA",
                         "avg_wRC", "avg_BsR", "off", "def", "avg_WAR")
names(seasonAVG)
devtools::use_data(seasonAVG, overwrite = TRUE)
rm(seasonAVG)


# Get example Batting, Pitching, and Fielding tables to be used in package testing to avoid long test times.
library(baseballDBR)
library(dplyr)

get_bbdb("Batting")
Batting2016 <- subset(Batting, yearID == "2016")
devtools::use_data(Batting2016, overwrite = TRUE)
rm(Batting2016)
rm(Batting)

get_bbdb("Pitching")
Pitching2016 <- subset(Pitching, yearID == "2016")
devtools::use_data(Pitching2016, overwrite = TRUE)
rm(Pitching2016)
rm(Pitching)

get_bbdb("Fielding")
Fielding2016 <- subset(Fielding, yearID == "2016")
devtools::use_data(Fielding2016, overwrite = TRUE)
rm(Fielding2016)
rm(Fielding)
