#' @title Return wOBA values per season
#' @description Get wOBA values for each year in your database. This calculation requires all fields of
#' the Pitching, Fielding and Batting tables from the Lahman package, or a comparable data set. The function uses
#' a version of Tom Tango's wOBA formula by default, but can also return Fangraphs wOBA values.
#' @param BattingTable A full batting table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recommended.
#' @param PitchingTable A full pitching table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recommended.
#' @param FieldingTable A full batting table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recommended.
#' @param Sep.Leagues If TRUE, this will split the calculation and return unique wOBA values for the various leagues. This can be
#' helpful in handling Designated Hitters and National League pitchers. It also isolates the park factors to their respective leagues.
#' @param Fangraphs if TRUE the function will return the Fangraphs wOBA values. By default the function uses a method adapted from
#' Tom Tango. These values are often very close to Fangraphs, but are not the same due to Fangraphs using a different algorithm.
#' This can not be used in conjunction with the \code{Sep.Leagues} argument because Fangraphs does not separate FIP constants by league.
#' @keywords woba, wOBA, on base average, fangraphs
#' @importFrom rvest html_node html_table
#' @importFrom xml2 read_html
#' @importFrom stats setNames
#' @import dplyr
#' @export wOBA_values
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#' data("Pitching2016")
#' head(Pitching2016)
#' data("Fielding2016")
#' head(Fielding2016)
#'
#' woba_df <- wOBA_values(Batting2016, Pitching2016, Fielding2016, Sep.Leagues=FALSE, Fangraphs=FALSE)
#'

wOBA_values <- function(BattingTable, PitchingTable, FieldingTable, Sep.Leagues=FALSE, Fangraphs=FALSE){
    # Declare values for Rcheck so it won't throw a note.
    POS=yearID=postf=playerID=teamID=lgID=G=IPouts=R=RperOut=runBB=run1B=run2B=runHBP=run3B=
        runHR=runSB=runCS=AB=H=X2B=X3B=HR=SB=CS=BB=SO=IBB=HBP=SF=runPlus=runMinus=wOBAscale=NULL
    # Make sure users don't contradict themselves.
    if(isTRUE(Sep.Leagues) & isTRUE(Fangraphs)){
        print("The Fangraphs Guts table does not sperate wOBA by league. Applying the default calculation...")
        Fangraphs=FALSE
    }

    if(isTRUE(Fangraphs)){
        # If user wants to use Fangraphs, grab it from the website.
        runsBatting <- xml2::read_html("http://www.fangraphs.com/guts.aspx?type=cn") %>%
            html_node(xpath = '//*[(@id = "GutsBoard1_dg1_ctl00")]') %>%
            html_table() %>%
            setNames(c("yearID", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B",
                              "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP"))
    }

    if(!isTRUE(Fangraphs)){
        # Find primary positions
        fielding <- FieldingTable
        # The "postf" field below is to filter out Natl. League players who may have
        # played as DH in inter-leauge games, and may have multiple entries at diff. positions.
        PrimPos <- dplyr::mutate(fielding, postf=ifelse(POS=="OF" & yearID>1995, 1,0)) %>%
            subset(postf==0,
                   select=c("playerID", "yearID", "teamID", "lgID","G", "POS")) %>%
            group_by(playerID, yearID, teamID, lgID, POS) %>%
            summarise(G = sum(G))

        # Find a run environment for each season, including pitchers.
        pitching <- PitchingTable %>%  subset(select=c("yearID", "playerID", "lgID","R", "IPouts"))

        pitchersPOS <- subset(PrimPos, POS=="P")

        pitchingLRPO <- inner_join(pitchersPOS, pitching, by=c("yearID", "playerID", "lgID"))


        LeagueRunsPerOut <- subset(pitchingLRPO, select=c("yearID", "lgID", "R", "IPouts")) %>%
            # Set NA to 0 so the sums will work.
            mutate(IPouts=ifelse(is.na(IPouts),0,IPouts))

        # Check to see if user wants the AL and NL split.
        if(isTRUE(Sep.Leagues)){
            LeagueRunsPerOut <-  group_by(LeagueRunsPerOut, yearID, lgID) %>%
                summarise(R=sum(R), IPouts=sum(IPouts)) %>%
                mutate(RperOut=R/IPouts) %>%
                rename(totR=R, totOuts=IPouts)

            RunValues <- subset(LeagueRunsPerOut, select=c("yearID", "lgID", "RperOut")) %>%
                group_by(yearID, lgID) %>%
                mutate(runBB=RperOut+0.14, runHBP=runBB+0.025, run1B=runBB+0.155, run2B=run1B+0.3,
                       run3B=run2B+0.27, runHR=1.4, runSB=0.2, runCS=(2*RperOut)+0.075) %>%
                group_by(yearID, lgID ,RperOut, runBB, runHBP, run1B, run2B, run3B, runHR, runSB, runCS)
        } else {
            LeagueRunsPerOut <-  group_by(LeagueRunsPerOut, yearID) %>%
                summarise(R=sum(R), IPouts=sum(IPouts)) %>%
                mutate(RperOut=R/IPouts) %>%
                rename(totR=R, totOuts=IPouts)

            # Calculate the Run Values for each event using Tom Tango's linear weights.
            # More info from Tango can be found here:
            # http://www.insidethebook.com/ee/index.php/site/comments/woba_year_by_year_calculations/
            # Note that HR and SB are static values. Tango admits this isn't perfect but is close.
            RunValues <- subset(LeagueRunsPerOut, select=c("yearID", "RperOut")) %>% group_by(yearID) %>%
                mutate(runBB=RperOut+0.14, runHBP=runBB+0.025, run1B=runBB+0.155, run2B=run1B+0.3,
                       run3B=run2B+0.27, runHR=1.4, runSB=0.2, runCS=(2*RperOut)+0.075) %>%
                group_by(yearID, RperOut, runBB, runHBP, run1B, run2B, run3B, runHR, runSB, runCS)
        }


        # Use Position Players table to find the runsPlus and runsMinus values to use in the wOBA multiplier.
        batting <- BattingTable
        batting <- batting[, !names(batting) %in% c("G")]
        batting <- inner_join(batting, PrimPos, by=c("playerID", "yearID", "lgID"))
        # Replace NA with 0, otherwise our runsMinus and runsPlus calculations will thow NA.
        batting[is.na(batting)] <- 0

        if(isTRUE(Sep.Leagues)){
            # Summarize values by year.
            yearbatting <- subset(batting, select=c("yearID", "lgID", "AB", "R", "H", "X2B", "X3B", "HR",
                                                    "SB", "CS", "BB", "SO", "IBB", "HBP", "SF")) %>%
                group_by(yearID, lgID) %>%
                summarise(AB=sum(AB), R=sum(R), H=sum(H), X2B=sum(X2B), X3B=sum(X3B), HR=sum(HR),
                                 SB=sum(SB), CS=sum(CS), BB=sum(BB), SO=sum(SO), IBB=sum(IBB), HBP=sum(HBP),
                                 SF=sum(SF))

            # Join yearly aggregates with the RunValues modifiers.
            runsBatting <- left_join(yearbatting, RunValues, by= c("yearID", "lgID")) %>%
                group_by(yearID, RperOut, runBB, runHBP, run1B, run2B, run3B, runHR, runSB, runCS) %>%
                # Calculate modifiers for wOBA events and wOBA scale.
                mutate(runMinus = ((runBB*(BB-IBB)) + (runHBP*HBP) + (run1B*(H-X2B-X3B-HR)) +
                                              (run2B*X2B) + (run3B*X3B) + (1.4*HR) + (runSB*SB) - (runCS*CS)) / (AB-H+SF)) %>%
                # Calculate modifier for wOBA scale.
                mutate(runPlus = ((runBB*(BB-IBB)) + (runHBP*HBP) + (run1B*(H-X2B-X3B-HR)) +
                                             (run2B*X2B) + (run3B*X3B) + (1.4*HR) + (runSB*SB) - (runCS*CS)) / (BB-IBB+HBP+H)) %>%
                # Calculate league wOBA.
                mutate(lg_woba = (H+BB+IBB+HBP) / (AB+BB-IBB+HBP+SF)) %>%
                # Calculate wOBA scale.
                mutate(woba_scale = 1/(runPlus+runMinus)) %>%
                # wOBA hit-event modifiers.
                mutate(wBB = (runBB+runMinus)*woba_scale, wHBP = (runHBP+runMinus)*woba_scale,
                              w1B = (run1B+runMinus)*woba_scale, w2B = (run2B+runMinus)*woba_scale,
                              w3B = (run3B+runMinus)*woba_scale, wHR = (runHR+runMinus)*woba_scale,
                              wSB = runSB*woba_scale, wCS = runCS*woba_scale)
        } else {
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
                mutate(lg_woba = (H+BB+IBB+HBP) / (AB+BB-IBB+HBP+SF)) %>%
                # Calculate wOBA scale.
                mutate(woba_scale = 1/(runPlus+runMinus)) %>%
                # wOBA hit-event modifiers.
                mutate(wBB = (runBB+runMinus)*woba_scale, wHBP = (runHBP+runMinus)*woba_scale,
                       w1B = (run1B+runMinus)*woba_scale, w2B = (run2B+runMinus)*woba_scale,
                       w3B = (run3B+runMinus)*woba_scale, wHR = (runHR+runMinus)*woba_scale,
                       wSB = runSB*woba_scale, wCS = runCS*woba_scale)
        }
    }
    return(runsBatting)
}
