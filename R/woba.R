#' @title Return wOBA values per season
#' @description Get wOBA values for each year in your database. This calculation requires all fields of
#' the Pitching, Fielding and Batting tables from the Lahman package, or a comprable data set. The function uses
#' a version of Tom Tango's wOBA formula by default, but can also return Fangraphs wOBA values.
#' @param Sep.Leagues If TRUE, this will split the calculation and return unique wOBA values for the various leagues. This can be
#' helpful in handling Designated Hitters and National League pitchers. It also isolates the park factors to their respective leagues.
#' @param Fangraphs if TRUE the function will return the Fangraphs wOBA values. By default the function uses a method adapted from
#' Tom Tango. These values are often very close to Fangraphs, but are not the same due to Fangraphs using a different algorithum.
#' This can not be used in conjuction with the \code{Sep.Leagues} argument because Fangraphs does not seperate FIP constants by league.
#' @keywords woba, wOBA, on base average, fangraphs
#' @importFrom rvest html_node
#' @importFrom xml2 read_html
#' @importFrom stats setNames
#' @import dplyr
#' @import Lahman
#' @export wOBA_values
#' @examples
#' \dontrun{
#' woba_df <- wOBA_values(Sep.Leagues=FALSE, Fangraphs=FALSE)
#' head(woba_df)
#'}
#'

wOBA_values <- function(Sep.Leagues=FALSE, Fangraphs=FALSE){
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
        rvest::html_node(xpath = '//*[(@id = "GutsBoard1_dg1_ctl00")]') %>%
        rvest::html_table() %>%
        stats::setNames(c("yearID", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B",
                   "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP"))
    }

    if(!isTRUE(Fangraphs)){
    # Find primary positions
    fielding <- Lahman::Fielding
    # The "postf" field below is to filter out Natl. League players who may have
    # played as DH in inter-leauge games, and may have multiple entries at diff. positions.
    PrimPos <- dplyr::mutate(fielding, postf=ifelse(POS=="OF" & yearID>1995, 1,0)) %>%
        subset(postf==0,
               select=c("playerID", "yearID", "teamID", "lgID","G", "POS")) %>%
        dplyr::group_by(playerID, yearID, teamID, lgID, POS) %>%
        dplyr::summarise(G = sum(G))

    # Find a run environment for each season, including pitchers.
    pitching <- Lahman::Pitching %>%  subset(select=c("yearID", "playerID", "lgID","R", "IPouts"))

    pitchersPOS <- subset(PrimPos, POS=="P")

    pitchingLRPO <- dplyr::inner_join(pitchersPOS, pitching, by=c("yearID", "playerID", "lgID"))


    LeagueRunsPerOut <- subset(pitchingLRPO, select=c("yearID", "lgID", "R", "IPouts")) %>%
        # Set NA to 0 so the sums will work.
        dplyr::mutate(IPouts=ifelse(is.na(IPouts),0,IPouts))

    # Check to see if user wants the AL and NL split.
    if(isTRUE(Sep.Leagues)){
        LeagueRunsPerOut <-  dplyr::group_by(LeagueRunsPerOut, yearID, lgID) %>%
        dplyr::summarise(R=sum(R), IPouts=sum(IPouts)) %>%
        dplyr::mutate(RperOut=R/IPouts) %>%
        dplyr::rename(totR=R, totOuts=IPouts)

        RunValues <- subset(LeagueRunsPerOut, select=c("yearID", "lgID", "RperOut")) %>%
            dplyr::group_by(yearID, lgID) %>%
            dplyr::mutate(runBB=RperOut+0.14) %>%
            dplyr::mutate(runHBP=runBB+0.025) %>%
            dplyr::mutate(run1B=runBB+0.155) %>%
            dplyr::mutate(run2B=run1B+0.3) %>%
            dplyr::mutate(run3B=run2B+0.27) %>%
            dplyr::mutate(runHR=1.4) %>% mutate(runSB=0.2) %>%
            dplyr::mutate(runCS=(2*RperOut)+0.075) %>%
            dplyr::group_by(yearID, lgID ,RperOut, runBB, runHBP, run1B, run2B, run3B, runHR, runSB, runCS)
    } else {
        LeagueRunsPerOut <-  dplyr::group_by(LeagueRunsPerOut, yearID) %>%
            dplyr::summarise(R=sum(R), IPouts=sum(IPouts)) %>%
            dplyr::mutate(RperOut=R/IPouts) %>%
            dplyr::rename(totR=R, totOuts=IPouts)

        # Calculate the Run Values for each event using Tom Tango's linear weights.
        # More info from Tango can be found here:
        # http://www.insidethebook.com/ee/index.php/site/comments/woba_year_by_year_calculations/
        # Note that HR and SB are static values. Tango admits this isn't perfect but is close.
        RunValues <- subset(LeagueRunsPerOut, select=c("yearID", "RperOut")) %>%
            dplyr::group_by(yearID) %>%
            dplyr::mutate(runBB=RperOut+0.14) %>%
            dplyr::mutate(runHBP=runBB+0.025) %>%
            dplyr::mutate(run1B=runBB+0.155) %>%
            dplyr::mutate(run2B=run1B+0.3) %>%
            dplyr::mutate(run3B=run2B+0.27) %>%
            dplyr::mutate(runHR=1.4) %>% mutate(runSB=0.2) %>%
            dplyr::mutate(runCS=(2*RperOut)+0.075) %>%
            dplyr::group_by(yearID, RperOut, runBB, runHBP, run1B, run2B, run3B, runHR, runSB, runCS)
    }


    # Use Position Players table to find the runsPlus and runsMinus values to use in the wOBA multiplier.
    batting <- Lahman::Batting
    batting <- batting[, !names(batting) %in% c("G")]
    batting <- dplyr::inner_join(batting, PrimPos, by=c("playerID", "yearID", "lgID"))
    # Replace NA with 0, otherwise our runsMinus and runsPlus calculations will thow NA.
    batting[is.na(batting)] <- 0

    if(isTRUE(Sep.Leagues)){
        # Summarize values by year.
        yearbatting <- subset(batting, select=c("yearID", "lgID", "AB", "R", "H", "X2B", "X3B", "HR",
                                                "SB", "CS", "BB", "SO", "IBB", "HBP", "SF")) %>%
            dplyr::group_by(yearID, lgID) %>%
            dplyr::summarise(AB=sum(AB), R=sum(R), H=sum(H), X2B=sum(X2B), X3B=sum(X3B), HR=sum(HR),
                      SB=sum(SB), CS=sum(CS), BB=sum(BB), SO=sum(SO), IBB=sum(IBB), HBP=sum(HBP),
                      SF=sum(SF))

        # Join yearly aggregates with the RunValues modifiers.
        runsBatting <- dplyr::left_join(yearbatting, RunValues, by= c("yearID", "lgID")) %>%
            dplyr::group_by(yearID, RperOut, runBB, runHBP, run1B, run2B, run3B, runHR, runSB, runCS) %>%
            # Calculate modifiers for wOBA events and wOBA scale.
            dplyr::mutate(runMinus = ((runBB*(BB-IBB)) + (runHBP*HBP) + (run1B*(H-X2B-X3B-HR)) +
                                   (run2B*X2B) + (run3B*X3B) + (1.4*HR) + (runSB*SB) - (runCS*CS)) / (AB-H+SF)) %>%
            # Calculate modifier for wOBA scale.
            dplyr::mutate(runPlus = ((runBB*(BB-IBB)) + (runHBP*HBP) + (run1B*(H-X2B-X3B-HR)) +
                                  (run2B*X2B) + (run3B*X3B) + (1.4*HR) + (runSB*SB) - (runCS*CS)) / (BB-IBB+HBP+H)) %>%
            # Calculate league wOBA.
            dplyr::mutate(wOBA = (H+BB+IBB+HBP) / (AB+BB-IBB+HBP+SF)) %>%
            # Calculate wOBA scale.
            dplyr::mutate(wOBAscale = 1/(runPlus+runMinus)) %>%
            # wOBA hit-event modifiers.
            dplyr::mutate(wBB = (runBB+runMinus)*wOBAscale) %>%
            dplyr::mutate(wHBP = (runHBP+runMinus)*wOBAscale) %>%
            dplyr::mutate(w1B = (run1B+runMinus)*wOBAscale) %>%
            dplyr::mutate(w2B = (run2B+runMinus)*wOBAscale) %>%
            dplyr::mutate(w3B = (run3B+runMinus)*wOBAscale) %>%
            dplyr::mutate(wHR = (runHR+runMinus)*wOBAscale) %>%
            dplyr::mutate(wSB = runSB*wOBAscale) %>%
            dplyr::mutate(wCS = runCS*wOBAscale)
    } else {
        # Summarize values by year.
        yearbatting <- subset(batting, select=c("yearID", "AB", "R", "H", "X2B", "X3B", "HR",
                                                "SB", "CS", "BB", "SO", "IBB", "HBP", "SF")) %>%
            dplyr::group_by(yearID) %>%
            dplyr::summarise(AB=sum(AB), R=sum(R), H=sum(H), X2B=sum(X2B), X3B=sum(X3B), HR=sum(HR),
                      SB=sum(SB), CS=sum(CS), BB=sum(BB), SO=sum(SO), IBB=sum(IBB), HBP=sum(HBP),
                      SF=sum(SF))

        # Join yearly aggregates with the RunValues modifiers.
        runsBatting <- dplyr::left_join(yearbatting, RunValues, by="yearID") %>%
            dplyr::group_by(yearID, RperOut, runBB, runHBP, run1B, run2B, run3B, runHR, runSB, runCS) %>%
            # Calculate modifiers for wOBA events and wOBA scale.
            dplyr::mutate(runMinus = ((runBB*(BB-IBB)) + (runHBP*HBP) + (run1B*(H-X2B-X3B-HR)) +
                                   (run2B*X2B) + (run3B*X3B) + (1.4*HR) + (runSB*SB) - (runCS*CS)) / (AB-H+SF)) %>%
            # Calculate modifier for wOBA scale.
            dplyr::mutate(runPlus = ((runBB*(BB-IBB)) + (runHBP*HBP) + (run1B*(H-X2B-X3B-HR)) +
                                  (run2B*X2B) + (run3B*X3B) + (1.4*HR) + (runSB*SB) - (runCS*CS)) / (BB-IBB+HBP+H)) %>%
            # Calculate league wOBA.
            dplyr::mutate(wOBA = (H+BB+IBB+HBP) / (AB+BB-IBB+HBP+SF)) %>%
            # Calculate wOBA scale.
            dplyr::mutate(wOBAscale = 1/(runPlus+runMinus)) %>%
            # wOBA hit-event modifiers.
            dplyr::mutate(wBB = (runBB+runMinus)*wOBAscale) %>%
            dplyr::mutate(wHBP = (runHBP+runMinus)*wOBAscale) %>%
            dplyr::mutate(w1B = (run1B+runMinus)*wOBAscale) %>%
            dplyr::mutate(w2B = (run2B+runMinus)*wOBAscale) %>%
            dplyr::mutate(w3B = (run3B+runMinus)*wOBAscale) %>%
            dplyr::mutate(wHR = (runHR+runMinus)*wOBAscale) %>%
            dplyr::mutate(wSB = runSB*wOBAscale) %>%
            dplyr::mutate(wCS = runCS*wOBAscale)

    }
    }

    return(runsBatting)
}


woba_df <- wOBA_values(Sep.Leagues=FALSE, Fangraphs=FALSE)
