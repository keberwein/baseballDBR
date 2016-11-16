#' @title Return fip values per season
#' @description Get fip values for each season.
#' @param Fangraphs if TRUE the function will return the Fangraphs wOBA values. By default the function uses a method adapted from
#' Tom Tango. These values are often very close to Fangraphs, but aren't the same.
#' @keywords woba, wOBA, on base average, fangraphs
#' @importFrom rvest html_node
#' @importFrom xml2 read_html
#' @importFrom stats setNames
#' @import dplyr
#' @import Lahman
#' @export fip_values
#' @examples
#' \dontrun{
#' fip_df <- fip_values(Fangraphs=FALSE)
#' head(fip_df)
#'}
#'

fip_values <- function(Fangraphs=FALSE){
    # Declare values for Rcheck so it won't throw a note.
    yearID=lgID=G=IPouts=H=HR=BB=SO=IBB=HBP=R=SF=W=L=GS=CG=SHO=SV=ER=WP=BK=BFP=GF=SH=GIDP=IP=NULL

    if(isTRUE(Fangraphs)){
        # If user wants to use Fangraphs, grab it from the website.
        runsBatting <- xml2::read_html("http://www.fangraphs.com/guts.aspx?type=cn") %>%
            rvest::html_node(xpath = '//*[(@id = "GutsBoard1_dg1_ctl00")]') %>%
            rvest::html_table %>%
            stats::setNames(c("yearID", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B",
                              "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP"))
    }

    if(!isTRUE(Fangraphs)){
        pitching <- Lahman::Pitching
        pitching <-  pitching[, !names(pitching) %in% c("playerID", "teamID", "stint", "BAOpp", "ERA")]
        # Replace NA with 0, otherwise our runsMinus and runsPlus calculations will thow NA.
        pitching[is.na(pitching)] <- 0
        pitching %<>%
            dplyr::group_by(yearID, lgID) %>%
            dplyr::summarise(W=sum(W), L=sum(L), G=sum(G), GS=sum(GS), CG=sum(CG), SHO=sum(SHO), SV=sum(SV),
                             IPouts=sum(IPouts), H=sum(H), ER=sum(ER), HR=sum(HR), BB=sum(BB), SO=sum(SO), IBB=sum(IBB),
                             WP=sum(WP), HBP=sum(HBP), BK=sum(BK), BFP=sum(BFP), GF=sum(GF), R=sum(R), SH=sum(SH),
                             SF=sum(SF), GIDP=sum(GIDP)) %>%
            dplyr::mutate(IP=IPouts/3) %>%
            dplyr::mutate(lgERA=ER / IP*9) %>%
            dplyr::mutate(lgRA=R / IP*9) %>%
            dplyr::mutate(FIPERscale=(ER / (IPouts/3) *9) - (((HR*13) + (BB+HBP+IBB)*3 - (SO*2)) / IP))

            # Find innings pitched per leauge.


    }

}
