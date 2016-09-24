# Metrics not in current Lahman. There is no "pitchingStats table in R Lahman"

# wOBA

# War: The fWar calculation can be found here. http://www.fangraphs.com/library/calculating-position-player-war-a-complete-example/

#' @title Calculate base on ball percentage
#' @description Find base on ball percentage for batters with more than zero at bats.
#' Required fields from the Batting table are; "AB", "SO", "BB", "HBP", "SF", and "SH."
#' Intentional base on balls (IBB) is added for the years that metric is available.
#' @param dat A data frame you would wish to calculate. If NULL, it will use the appropriate table from
#' the Lahman package. However, functions will accept custom data frames as well.
#' @keywords BBpct base on ball percentage bb
#' @export BBpct
#' @examples
#' \dontrun{
#' batting_df <- Lahman::Batting
#' new_df <- BBpct(batting_df)
#' new_df
#' }
#'
BBpct <- function (dat=NULL){
    if (is.null(dat)){
        dat = Lahman::Batting
    }
    if (any(!isTRUE(c("AB", "BB", "HBP", "SF", "SH", "IBB") %in% names(dat)))){
        ifelse(dat$AB > 0,
               dat$BBpct <- round((dat$BB+dat$IBB/(dat$AB+dat$BB+dat$HBP+dat$SF+dat$SH)), 3), NA)
    }
    if (any(isTRUE(c("AB", "BB", "HBP", "SF", "SH") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'BB', 'IBB', 'HBP', 'SF', and 'SH'")
    }
    return(dat)
}

#' @title Calculate a batter's contact rate
#' @description Find the contact rate for batters.
#' Required fields from the batting table are "AB" and "SO."
#' @param dat A data frame you would wish to calculate. If NULL, it will use the appropriate table from
#' the Lahman package. However, functions will accept custom data frames as well.
#' @keywords CTpct contact rate
#' @export CTpct
#' @examples
#' \dontrun{
#' batting_df <- Lahman::Batting
#' new_df <- CTpct(batting_df)
#' new_df
#' }
#'
CTpct <- function (dat=NULL){
    if (is.null(dat)){
        dat = Lahman::Batting
    }
    if (any(!isTRUE(c("AB", "SO") %in% names(dat)))){
        ifelse(dat$H > 0,
               dat$CTpct <- round(((dat$AB-dat$SO)/dat$AB), 3), NA)
    }
    if (any(isTRUE(c("AB", "SO") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB' and 'SO'")
    }
    return(dat)
}

#' @title Calculate home run percentage
#' @description Find home run percentage for batters with more than zero at bats.
#' Required fields from the Batting table are "AB" and "HR."
#' @param dat A data frame you would wish to calculate. If NULL, it will use the appropriate table from
#' the Lahman package. However, functions will accept custom data frames as well.
#' @keywords HRpct home run percentage
#' @export HRpct
#' @examples
#' \dontrun{
#' batting_df <- Lahman::Batting
#' new_df <- HRpct(batting_df)
#' new_df
#' }
#'
HRpct <- function (dat=NULL){
    if (is.null(dat)){
        dat = Lahman::Batting
    }
    if (any(!isTRUE(c("AB", "HR") %in% names(dat)))){
        ifelse(dat$IPouts > 0 & dat$BB > 0,
               dat$HRpct <- round((dat$AB/(dat$HR)), 3), NA)
    }
    if (any(isTRUE(c("AB", "HR") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'HR'")
    }
    return(dat)
}

#' @title Calculate ISO for batters
#' @description Find isolated power (ISO) for batters with more than zero at bats.
#' Required fields from the batting table are "H", "X2B", "X3B", "HR"."
#' @param dat A data frame you would wish to calculate. If NULL, it will use the appropriate table from
#' the Lahman package. However, functions will accept custom data frames as well.
#' @keywords ISO isolated power
#' @export ISO
#' @examples
#' \dontrun{
#' batting_df <- Lahman::Batting
#' new_df <- ISO(batting_df)
#' new_df
#' }
#'
ISO <- function (dat=NULL){
    if (is.null(dat)){
        dat = Lahman::Batting
    }
    if (any(!isTRUE(c("AB", "X2B", "X3B", "HR") %in% names(dat)))){
        ifelse(dat$AB > 0,
               dat$XBHpct <- round(((dat$X2B+(2*dat$X3B)+(3*dat$X3B)/dat$AB)), 3), NA)
    }
    if (any(isTRUE(c("AB", "X2B", "X3B", "HR") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'X2B', 'X3B' and 'HR'")
    }
    return(dat)
}

#' @title Calculate strikeout percentage
#' @description Find strikeout percentage for batters with more than zero at bats.
#' Required fields from the Batting table are; "AB", "SO", "BB", "HBP", "SF", and "SH."
#' @param dat A data frame you would wish to calculate. If NULL, it will use the appropriate table from
#' the Lahman package. However, functions will accept custom data frames as well.
#' @keywords Kpct strikeout percentage
#' @export Kpct
#' @examples
#' \dontrun{
#' batting_df <- Lahman::Batting
#' new_df <- Kpct(batting_df)
#' new_df
#' }
#'
Kpct <- function (dat=NULL){
    if (is.null(dat)){
        dat = Lahman::Batting
    }
    if (any(!isTRUE(c("AB", "SO", "BB", "HBP", "SF", "SH") %in% names(dat)))){
        ifelse(dat$IPouts > 0 & dat$SO > 0,
               dat$Kpct <- round((dat$SO / (dat$AB + dat$BB + dat$HBP + dat$SF + dat$SH)), 3), NA)
    }
    if (any(isTRUE(c("AB", "SO", "BB", "HBP", "SF", "SH") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'SO', 'BB', 'HBP', 'SF', and 'SH'")
    }
    return(dat)
}

#' @title Calculate on base percentage (OBP)
#' @description Find the OBP for batters with more than zero hits.
#' Required fields from the batting table are "H", "X2B", "X3B", "HR"."
#' @param dat A data frame you would wish to calculate. If NULL, it will use the appropriate table from
#' the Lahman package. However, functions will accept custom data frames as well.
#' @keywords OBP on base percentage
#' @export OBP
#' @examples
#' \dontrun{
#' batting_df <- Lahman::Batting
#' new_df <- OBP(batting_df)
#' new_df
#' }
#'
OBP <- function (dat=NULL){
    if (is.null(dat)){
        dat = Lahman::Batting
    }
    if (any(!isTRUE(c("H", "BB", "HBP", "AB", "SF") %in% names(dat)))){
        ifelse(dat$H > 0,
               dat$OBP <- round((dat$H+dat$BB+dat$HBP)/(dat$AB+dat$BB+dat$HBP+dat$SF), 3), NA)
    }
    if (any(isTRUE(c("H", "BB", "HBP", "AB", "SF") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'H', 'AB', 'BB', 'HBP' and 'SF'")
    }
    return(dat)
}

#' @title Calculate on base percentage plus slugging (OPS)
#' @description Find the OPS for batters with more than zero hits.
#' Required fields from the batting table are "H", "X2B", "X3B", "HR", "BB", "HBP", "AB" and "SF."
#' @param dat A data frame you would wish to calculate. If NULL, it will use the appropriate table from
#' the Lahman package. However, functions will accept custom data frames as well.
#' @keywords OPS on base percentage
#' @export OPS
#' @examples
#' \dontrun{
#' batting_df <- Lahman::Batting
#' new_df <- OPS(batting_df)
#' new_df
#' }
#'
OPS <- function (dat=NULL){
    if (is.null(dat)){
        dat = Lahman::Batting
    }
    if (any(!isTRUE(c("H", "BB", "HBP", "AB", "SF", "X2B", "X3B", "HR", "AB") %in% names(dat)))){
        ifelse(dat$H > 0,
               dat$OPS <- round((dat$H+dat$BB+dat$HBP)/
                                    (dat$AB+dat$BB+dat$HBP+dat$SF)+
                                    ((dat$H+dat$X2B+dat$X3B+dat$HR)/dat$AB), 3), NA)
    }
    if (any(isTRUE(c("H", "BB", "HBP", "AB", "SF", "X2B", "X3B", "HR", "AB") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'H', 'AB', 'BB', 'SF', 'X2B', 'X3B', and 'HR'")
    }
    return(dat)
}

#' @title Calculate slugging percentage (SLG)
#' @description Find the SLG for batters with more than zero hits.
#' Required fields from the batting table are "H", "X2B", "X3B", "HR"."
#' @param dat A data frame you would wish to calculate. If NULL, it will use the appropriate table from
#' the Lahman package. However, functions will accept custom data frames as well.
#' @keywords SLG on base percentage
#' @export SLG
#' @examples
#' \dontrun{
#' batting_df <- Lahman::Batting
#' new_df <- SLG(batting_df)
#' new_df
#' }
#'
SLG <- function (dat=NULL){
    if (is.null(dat)){
        dat = Lahman::Batting
    }
    if (any(!isTRUE(c("H", "X2B", "X3B", "AB", "HR") %in% names(dat)))){
        ifelse(dat$H > 0,
               dat$OBP <- round((dat$H+dat$X2B+dat$X3B+dat$HR)/dat$AB, 3), NA)
    }
    if (any(isTRUE(c("H", "X2B", "X3B", "AB", "HR") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'H', 'AB', 'X2B', 'X3B' and 'HR'")
    }
    return(dat)
}

#' @title Calculate a batter's total bases
#' @description Find total bases.
#' Required fields from the batting table are "H", "X2B", "X3B" and "HR."
#' @param dat A data frame you would wish to calculate. If NULL, it will use the appropriate table from
#' the Lahman package. However, functions will accept custom data frames as well.
#' @keywords TBs total bases
#' @export TBs
#' @examples
#' \dontrun{
#' batting_df <- Lahman::Batting
#' new_df <- TBs(batting_df)
#' new_df
#' }
#'
TBs <- function (dat=NULL){
    if (is.null(dat)){
        dat = Lahman::Batting
    }
    if (any(!isTRUE(c("H", "X2B", "X3B", "HR") %in% names(dat)))){
        ifelse(dat$H > 0,
               dat$TBs <- round(((dat$H)+(2*dat$X2B)+(3*dat$X3B)+(4*dat$HR)), 3), NA)
    }
    if (any(isTRUE(c("H", "X2B", "X3B", "HR") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'H', 'X2B', 'X3B' and 'HR'")
    }
    return(dat)
}

#' @title Calculate extra base percentage
#' @description Find extra base percentage for batters with more than zero at bats.
#' Required fields from the batting table are "AB", "BB", "HBP", "SF", "SH", "X2B", "X3B", "HR"."
#' @param dat A data frame you would wish to calculate. If NULL, it will use the appropriate table from
#' the Lahman package. However, functions will accept custom data frames as well.
#' @keywords XBHpct extra base percentage
#' @export XBHpct
#' @examples
#' \dontrun{
#' batting_df <- Lahman::Batting
#' new_df <- XBHpct(batting_df)
#' new_df
#' }
#'
XBHpct <- function (dat=NULL){
    if (is.null(dat)){
        dat = Lahman::Batting
    }
    if (any(!isTRUE(c("AB", "BB", "HBP", "SF", "SH", "X2B", "X3B", "HR") %in% names(dat)))){
        ifelse(dat$AB > 0,
               dat$XBHpct <- round(((dat$X2B+dat$X3B+dat$HR)/(dat$AB + dat$BB + dat$HBP + dat$SF + dat$SH)), 3), NA)
    }
    if (any(isTRUE(c("AB", "BB", "HBP", "SF", "SH", "X2B", "X3B", "HR") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'BB', 'HBP', 'SF', 'SH', 'X2B', 'X2B' and 'HR'")
    }
    return(dat)
}

#' @title Calculate extra base per hit
#' @description Find the average extra bases per hit for batters with more than zero hits.
#' Required fields from the batting table are "H", "X2B", "X3B", "HR"."
#' @param dat A data frame you would wish to calculate. If NULL, it will use the appropriate table from
#' the Lahman package. However, functions will accept custom data frames as well.
#' @keywords XBperH extra base per hit
#' @export XBperH
#' @examples
#' \dontrun{
#' batting_df <- Lahman::Batting
#' new_df <- XBperH(batting_df)
#' new_df
#' }
#'
XBperH <- function (dat=NULL){
    if (is.null(dat)){
        dat = Lahman::Batting
    }
    if (any(!isTRUE(c("H", "X2B", "X3B", "HR") %in% names(dat)))){
        ifelse(dat$H > 0,
               dat$XBHpct <- round(((dat$X2B+dat$X3B+dat$HR)/(dat$H)), 3), NA)
    }
    if (any(isTRUE(c("H", "X2B", "X3B", "HR") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'H', 'X2B', 'X3B' and 'HR'")
    }

    return(dat)
}



# OPS+
# Baseball refreence has its own method. Try to find a couple.

# OFF (offensive runs above average)
# http://www.fangraphs.com/library/offense/off/

# RC

# http://www.fangraphs.com/library/offense/wrc/
# wRC (weighted runs created)
# wRC = (((wOBA-League wOBA)/wOBA Scale)+(League R/PA))*PA

# wRC+ this one uses park factors and leauge adjustments.
# wRC+ = (((wRAA/PA + League R/PA) + (League R/PA – Park Factor* League R/PA))/
# (AL or NL wRC/PA excluding pitchers))*100

# RAA (runs above average)

# WAA (wins above average)

# wRAA (weighted runs above average)
# http://www.fangraphs.com/library/offense/wraa/
# wRAA = ((wOBA – league wOBA) / wOBA scale) × PA


