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
