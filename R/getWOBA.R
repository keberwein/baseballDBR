library(dplyr)
library(Lahman)

# Find primary positions
fielding <- Lahman::Fielding

PrimPos <- subset(fielding, select=c("playerID", "yearID", "teamID", "G", "POS")) %>%
    mutate(POS = ifelse(POS=="RF" | POS=="LF", "OF", POS)) %>%
    group_by(playerID, yearID, teamID, POS) %>%
    summarise(G = sum(G))

# Find a run environment for each season, excluding pitches thrown by non-pitchers.
pitching <- Lahman::Pitching
# Make sure to exclude anyone who's not a full-time pitcher.
pitchersPos <- subset(PrimPos, POS=="P", select=("playerID")) %>% distinct
pitching <- subset(pitching, playerID %in% pitchersPos$playerID)

LeagueRunsPerOut <- subset(pitching, select=c("yearID","R", "IPouts")) %>%
    # Set NA to 0 so the sums will work.
    mutate(IPouts=ifelse(is.na(IPouts),0,IPouts)) %>%
    group_by(yearID) %>%
    summarise(R=sum(R), IPouts=sum(IPouts)) %>%
    mutate(RperOut=R/IPouts) %>%
    rename(totR=R, totOuts=IPouts)

# Calculate the Run Values for each event using Tom Tango's linear weights.
# More info from Tango can be found here:
# http://www.insidethebook.com/ee/index.php/site/comments/woba_year_by_year_calculations/
# Note that HR and SB are static values. Tango admits this isn't perfect but is close.
RunValues <- subset(LeagueRunsPerOut, select=c("yearID", "RperOut")) %>%
    mutate(runBB=RperOut+0.14) %>%
    mutate(runHBP=runBB+0.025) %>%
    mutate(run1B=runBB+0.155) %>%
    mutate(run2B=run1B+0.3) %>%
    mutate(run3B=run2B+0.27) %>%
    mutate(runHR=1.4) %>% mutate(runSB=0.2) %>%
    mutate(runCS=(2*RperOut)+0.075)

# Use Batting table to find the runsPlus and runsMinus values to use in the wOBA multiplier.
batting <- Lahman::Batting
# We only need the position players from our PrimPos data frame.
batting <- inner_join(PrimPos, batting, by=c("playerID", "yearID", "teamID"))
# Replace NA with 0, otherwise it will not sum by year correctly. Normally this would be a no-no,
# but its the fastest solution.
batting[is.na(batting)] <- 0
# Summarize values by year.
yearbatting <- subset(batting, select=c("yearID", "AB", "R", "H", "X2B", "X3B", "HR",
                                        "SB", "CS", "BB", "SO", "IBB", "HBP", "SF")) %>%
    group_by(yearID) %>%
    summarise(AB=sum(AB), R=sum(R), H=sum(H), X2B=sum(X2B), X3B=sum(X3B), HR=sum(HR),
              SB=sum(SB), CS=sum(CS), BB=sum(BB), SO=sum(SO), IBB=sum(IBB), HBP=sum(HBP),
              SF=sum(SF))
# Join yearly aggregates with the RunValues modifiers.
runsBatting <- left_join(yearbatting, RunValues, by="yearID") %>%
    mutate(runsMinus = ((runBB*(BB-IBB)) + (runHBP*HBP) + (run1B*(H-X2B-X3B-HR)) + (run2B*X2B) + (run3B*X3B) + (1.4*HR) +
               ((runSB*SB) - (runCS*CS)) /
               (AB-H+SF)))




SUM(runBB*(BB-COALESCE(IBB,0))+runHB*COALESCE(HBP,0)+run1B*(H-X2B-X3b-HR)+run2B*X2B+run3B*X3B+1.4*HR+runSB*COALESCE(SB,0)-runCS*COALESCE(CS,0))/
    SUM(AB-H+COALESCE(SF,0)) AS runMinus


, SUM(runBB*(BB-COALESCE(IBB,0))+runHB*COALESCE(HBP,0)+run1B*(H-X2b-X3b-HR)+run2B*X2B+run3B*X3B+1.4*HR+runSB*COALESCE(SB,0)-runCS*COALESCE(CS,0))/
    SUM(BB-COALESCE(IBB,0)+COALESCE(HBP,0)+H) AS runPlus
, SUM(H+BB-COALESCE(IBB,0)+COALESCE(HBP,0))/
    SUM(AB+BB-COALESCE(IBB,0)+COALESCE(HBP,0)+COALESCE(SF,0)) AS wOBA











tbat <- subset(batting, yearID == 1973)













