#' @title Return wOBA values
#' @description Get wOBA values for each year in your database. This calculation requires all fields of
#' the Pitching, Fielding and Batting tables from the Lahman package.
#' @keywords woba, wOBA, on base average
#' @import Lahman, dplyr
#' @export wOBA
#' @examples
#' \dontrun{
#' woba_df <- wOBA()
#' woba_df
#' }
#'

wOBA <- function(){
    # Find primary positions
    fielding <- Lahman::Fielding
    # The "postf" field below is to filter out Natl. League players who may have
    # played as DH in inter-leauge games, and may have multiple entries at diff. positions.
    PrimPos <- mutate(fielding, postf=ifelse(POS=="OF" & yearID>1995, 1,0)) %>%
        subset(postf==0,
               select=c("playerID", "yearID", "teamID", "G", "POS")) %>%
        group_by(playerID, yearID, teamID, POS) %>%
        summarise(G = sum(G))

    # Find a run environment for each season, including pitchers.
    pitching <- Lahman::Pitching %>%  subset(select=c("yearID", "playerID","R", "IPouts"))
    pitchersPOS <- subset(PrimPos, POS=="P")

    pitchingLRPO <- inner_join(pitchersPOS, pitching, by=c("yearID", "playerID"))

    LeagueRunsPerOut <- subset(pitchingLRPO, select=c("yearID", "R", "IPouts")) %>%
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

    # Use Position Players table to find the runsPlus and runsMinus values to use in the wOBA multiplier.
    batting <- Lahman::Batting[, !names(Batting) %in% c("G")]
    batting <- inner_join(batting, PrimPos, by=c("playerID", "yearID"))
    # We only need the position players from our PrimPos data frame.
    #batting <- inner_join(PrimPos, batting, by=c("playerID", "yearID", "teamID"))
    # Replace NA with 0, otherwise our runsMinus and runsPlus calculations will thow NA.
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
        group_by(yearID, RperOut, runBB, runHBP, run1B, run2B, run3B, runHR, runSB, runCS) %>%
        # Calculate modifiers for wOBA events and wOBA scale.
        mutate(runMinus = ((runBB*(BB-IBB)) + (runHBP*HBP) + (run1B*(H-X2B-X3B-HR)) +
                               (run2B*X2B) + (run3B*X3B) + (1.4*HR) + (runSB*SB) - (runCS*CS)) / (AB-H+SF)) %>%
        # Calculate modifier for wOBA scale.
        mutate(runPlus = ((runBB*(BB-IBB)) + (runHBP*HBP) + (run1B*(H-X2B-X3B-HR)) +
                              (run2B*X2B) + (run3B*X3B) + (1.4*HR) + (runSB*SB) - (runCS*CS)) / (BB-IBB+HBP+H)) %>%
        # Calculate league wOBA.
        mutate(wOBA = (H+BB+IBB+HBP) / (AB+BB-IBB+HBP+SF)) %>%
        # Calculate wOBA scale.
        mutate(wOBAscale = 1/(runPlus+runMinus)) %>%
        # wOBA hit-event modifiers.
        mutate(wobaBB = (runBB+runMinus)*wOBAscale) %>%
        mutate(wobaHBP = (runHBP+runMinus)*wOBAscale) %>%
        mutate(woba1B = (run1B+runMinus)*wOBAscale) %>%
        mutate(woba2B = (run2B+runMinus)*wOBAscale) %>%
        mutate(woba3B = (run3B+runMinus)*wOBAscale) %>%
        mutate(wobaHR = (runHR+runMinus)*wOBAscale) %>%
        mutate(wobaSB = runSB*wOBAscale) %>%
        mutate(wobaCS = runCS*wOBAscale)
    return(runsBatting)

}
