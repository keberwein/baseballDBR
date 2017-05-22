
#' @title Calculate walks per nine innings
#' @description Find batting average walks per nine innings for pitchers with more one or more inning pitched.
#' Required fields from the Pitching table are; "IPouts", and "BB."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords BB BB_9 BB9 bb/9
#' @export BB_9
#' @examples
#' \dontrun{
#' get_bbdb("Pitching")
#' new_df <- BB_9(Pitching)
#' new_df
#' }
#'
BB_9 <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }
    if (any(!isTRUE(c("IPouts", "BB") %in% names(dat)))){
        ifelse(dat$IPouts > 2,
               dat$BB_9 <- round((dat$BB*9 / (dat$IPouts / 3)), 3), NA)
    }
    if (any(isTRUE(c("BB", "IPouts") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'BB', and 'IPouts'")
    }
    return(dat)
}

#' @title Fielding Independent Pitching (FIP)
#' @description Find the FIP for all pitchers with one or strike outs in a particular season.
#' Required fields from the Pitching table are "BB", "HBP", "SO", and "IPouts."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @param Sep.Leagues If TRUE the algorithum will calculate different run enviornments for the National and American leagues. Grouping
#' the leauges can solve problems introduced by the designated hitter and hitting pitchers. It also serves to further isolate for
#' park factors between the American and National leauges. The default for this argument is FALSE.
#' @param NA_to_zero If TRUE this will replace NAs with 0 for years that certain stats weren't counted. For example, sacrafice hits
#' weren't a counted statistic until 1954, therefore we are technically unable to calculate wOBA for any player prior to 1954.
#' The default is set to TRUE. Even though this is bad practice mathematically, many in the sabermetrics community accept the practice.
#' If FALSE, the wOBA calculation will return NaN for years with missing data.
#' @param Fangraphs If TRUE the function will download wOBA values from Fangraphs. If FALSE the function will use the internal
#' formula adapted from Tom Tango's original wOBA formula. Note, the internal formula is typically identical to Fangraphs and
#' does not require an external download. If not specified, the default is set to FALSE.
#' @keywords FIP fielding independent pitching
#' @export FIP
#' @examples
#' \dontrun{
#' get_bbdb("Pitching")
#' new_df <- FIP(Pitching, Fangraphs=FALSE, NA_to_zero=TRUE, Sep.Leagues=FALSE)
#' new_df
#' }
#'
FIP <- function (dat=NULL, Fangraphs=FALSE, NA_to_zero=TRUE, Sep.Leagues=FALSE){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }

    if(isTRUE(Sep.Leagues) & isTRUE(Fangraphs)){
        print("The Fangraphs Guts table does not sperate FIP by league. Applying the default calculation...")
        Fangraphs=FALSE
    }

    fip <- fip_values(dat=dat, Fangraphs=Fangraphs, Sep.Leagues=Sep.Leagues)

    if (any(!isTRUE(c("BB", "HBP", "SO", "IPouts") %in% names(dat)))){
        if (isTRUE(NA_to_zero)){
            dat <- dplyr::mutate(dat, HBP=ifelse(is.na(HBP),0,HBP))
        }

        if(isTRUE(Sep.Leagues)){
            fip <- fip[, c("yearID", "lgID", "cFIP")]
            dat <- dplyr::left_join(dat, fip, by=c("yearID", "lgID"))
        } else {
            fip <- fip[, c("yearID", "cFIP")]
            dat <- dplyr::left_join(dat, fip, by="yearID")
        }

        ifelse(dat$SO > 0,
               dat$fip <- (((dat$HR*13) + ((dat$BB + dat$IBB + dat$HBP - dat$IBB)*3) - (dat$SO*2)) / (dat$IPouts/3) + dat$cFIP), NA)
    }
    if (any(isTRUE(c("BB", "HBP", "SO", "IPouts") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'BB', 'HBP', 'K', and 'IPouts'")
    }

    dat <- dat[, !names(dat) %in% c("cFIP")]

    return(dat)
}


#' @title Calculate Hits per Nine innings
#' @description Find the number of hits a pitcher throws per nine innings pitched.
#' Required fields from the Pitching table are; "H", "BB", and "IPouts."
#' @param dat A data frame you would wish to calculate.  The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords hits per nine innings
#' @export H_9
#' @examples
#' \dontrun{
#' get_bbdb("Pitching")
#' new_df <- H_9(Pitching)
#' new_df
#' }
#'
H_9 <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }
    if (any(!isTRUE(c("H", "BB", "IPouts") %in% names(dat)))){
        ifelse(dat$IPouts > 2,
               dat$H_9 <- round((dat$H*9) / (dat$IPouts/3), 3), NA)
    }
    if (any(isTRUE(c("H", "BB", "IPouts") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'H', and 'IPouts'")
    }
    return(dat)
}

#' @title Calculate Home Runs per Nine innings
#' @description Find the number of home runs a pitcher allows per nine innings pitched.
#' Required fields from the Pitching table are; "H" and "IPouts."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords hits per nine innings
#' @export HR_9
#' @examples
#' \dontrun{
#' get_bbdb("Pitching")
#' new_df <- HR_9(Pitching)
#' new_df
#' }
#'
HR_9 <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }
    if (any(!isTRUE(c("HR", "IPouts") %in% names(dat)))){
        ifelse(dat$IPouts > 2,
               dat$HR <- round((dat$HR*9) / (dat$IPouts/3), 3), NA)
    }
    if (any(isTRUE(c("Hr", "IPouts") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'Hr', and 'IPouts'")
    }
    return(dat)
}

#' @title Calculate the innings pitched
#' @description Find the number of innings a player has ptiched for a season.
#' Required fields from the Pitching table are; "IPouts."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords innings pitched
#' @export IP
#' @examples
#' \dontrun{
#' get_bbdb("Pitching")
#' new_df <- IP(Pitching)
#' new_df
#' }
#'
IP <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }
    if (any(isTRUE(c("IPouts") %in% names(dat)))){
        ifelse(dat$IPouts > 2,
               dat$IP <- round(dat$IPouts/3, 3), NA)
    }
    if (any(!isTRUE(c("IPouts") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'IPouts'")
    }
    return(dat)
}

#' @title Calculate Strikes per Nine innings
#' @description Find the number of strikes a pitcher throws per nine innings pitched.
#' Required fields from the Pitching table are; "H", "BB", "IPouts", and "SO."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords strikes per nine innings
#' @export K_9
#' @examples
#' \dontrun{
#' get_bbdb("Pitching")
#' new_df <- K_9(Pitching)
#' new_df
#' }
#'
K_9 <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }
    if (any(!isTRUE(c("H", "BB", "IPouts", "SO") %in% names(dat)))){
        ifelse(dat$IPouts > 2,
               dat$K_9 <- round((dat$SO*9) / (dat$IPouts/3), 3), NA)
    }
    if (any(isTRUE(c("H", "BB", "IPouts", "SO") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'H', 'BB', 'SO', and 'IPouts'")
    }
    return(dat)
}

#' @title Calculate the left on base percentage
#' @description Find the percentaqge of base runners that a pitcher leaves on base of the course of a season.
#' Required fields from the Pitching table are; "H", "BB", "HBP", "R", and "HR."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords LOB_pct LOB LOB percentage
#' @export LOB_pct
#' @examples
#' \dontrun{
#' get_bbdb("Pitching")
#' new_df <- LOB_pct(pitching_df)
#' new_df
#' }
#'
LOB_pct <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }
    if (any(!isTRUE(c("H", "BB", "HBP", "R", "HR") %in% names(dat)))){
        ifelse(dat$IPouts > 2,
               dat$LOB_pct <- round(
                   (dat$H+dat$BB+dat$HBP-dat$R) / (dat$H+dat$BB+dat$HBP-(1.4*dat$HR))
                   , 3), NA)
    }
    if (any(isTRUE(c("H", "BB", "HBP", "R", "HR") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'BB', 'IBB', 'HBP', 'SF', and 'SH'")
    }
    return(dat)
}

#' @title Calculate Walks plus Hits per Innings Pitched
#' @description Find the number of walks plus hits a pitcher allows per inning pitched.
#' Required fields from the Pitching table are; "H", "BB", and "IPouts."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords Walks plus Hits per Innings Pitched WHIP
#' @export WHIP
#' @examples
#' \dontrun{
#' get_bbdb("Pitching")
#' new_df <- WHIP(pitching_df)
#' new_df
#' }
#'
WHIP <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }
    if (any(!isTRUE(c("H", "BB", "IPouts") %in% names(dat)))){
        ifelse(dat$IPouts > 2,
               dat$WHIP <- round((dat$BB+dat$H) / (dat$IPouts/3), 3), NA)
    }
    if (any(isTRUE(c("H", "BB", "IPouts") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'H', 'BB', and 'IPouts'")
    }
    return(dat)
}




