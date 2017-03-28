#' @title Return FIP constants per season
#' @description Get fip constants for each season. By default the function uses a method adapted from
#' Tom Tango and used by Fangraphs. The function returns FIP constants based on ERA \code{FIP_ERA} as well as constants based on RA \code{FIP_RA}.
#' Both the Tango and Frangraphs formulas use ERA for their FIP constants.
#' @param PitchingTable A full pitching table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recomended.
#' @param Sep.Leagues If TRUE, this will split the calculation and return unique FIP constants for the various leagues. This can be
#' helpful in handling Designated Hitters and National League pitchers. It also isolates the park factors to their respective leagues.
#' @param Fangraphs If TRUE the function will return the Fangraphs FIP constants. This can not be used in conjuction with the
#' \code{Sep.Leagues} argument because Fangraphs does not seperate FIP constants by league.
#' @keywords woba, wOBA, on base average, fangraphs
#' @importFrom rvest html_node
#' @importFrom xml2 read_html
#' @importFrom stats setNames
#' @import dplyr
#' @export fip_values
#' @examples
#' \dontrun{
#' fip_df <- fip_values(Fangraphs=FALSE)
#' head(fip_df)
#'}
#'

fip_values <- function(PitchingTable=NULL, Sep.Leagues=FALSE, Fangraphs=FALSE){
    # Declare values for Rcheck so it won't throw a note.
    yearID=lgID=G=IPouts=H=HR=BB=SO=IBB=HBP=R=SF=W=L=GS=CG=SHO=SV=ER=WP=BK=BFP=GF=SH=GIDP=IP=lgERA=lgRA=NULL
    if(isTRUE(Sep.Leagues) & isTRUE(Fangraphs)){
        print("The Fangraphs Guts table does not sperate wOBA by league. Applying the default calculation...")
    }

    if(isTRUE(Fangraphs)){
        # If user wants to use Fangraphs, grab it from the website.
        runsBatting <- xml2::read_html("http://www.fangraphs.com/guts.aspx?type=cn") %>%
            rvest::html_node(xpath = '//*[(@id = "GutsBoard1_dg1_ctl00")]') %>%
            rvest::html_table %>%
            stats::setNames(c("yearID", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B",
                              "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP"))
    }

    if(!isTRUE(Fangraphs)){
        pitching <- PitchingTable
        pitching <-  pitching[, !names(pitching) %in% c("playerID", "teamID", "stint", "BAOpp", "ERA")]
        # Replace NA with 0, otherwise our runsMinus and runsPlus calculations will thow NA.
        pitching[is.na(pitching)] <- 0

        if(isTRUE(Sep.Leagues)){
            pitching %<>% dplyr::group_by(yearID, lgID)
        } else {
            pitching %<>% dplyr::group_by(yearID)
        }

        pitching %<>%
            #dplyr::group_by(yearID, lgID) %>%
            dplyr::summarise(W=sum(W), L=sum(L), G=sum(G), GS=sum(GS), CG=sum(CG), SHO=sum(SHO), SV=sum(SV),
                             IPouts=sum(IPouts), H=sum(H), ER=sum(ER), HR=sum(HR), BB=sum(BB), SO=sum(SO), IBB=sum(IBB),
                             WP=sum(WP), HBP=sum(HBP), BK=sum(BK), BFP=sum(BFP), GF=sum(GF), R=sum(R), SH=sum(SH),
                             SF=sum(SF), GIDP=sum(GIDP)) %>%
            dplyr::mutate(IP=IPouts/3) %>%
            dplyr::mutate(lgERA=ER / IP*9) %>%
            dplyr::mutate(lgRA=R / IP*9) %>%
            dplyr::mutate(FIP_ERA=lgERA - ((HR*13) + ((BB + HBP - IBB)*3) - (SO*2)) / IP) %>%
            dplyr::mutate(FIP_RA=lgRA - ((HR*13) + ((BB + HBP - IBB)*3) - (SO*2)) / IP)
    }

}

