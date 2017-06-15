
#' @title Batting: Calculate batting average
#' @description Find batting average for batters with more than zero at bats.
#' Required fields from the Batting table are; "AB", and "H."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords BA base on ball percentage bb
#' @family Batting functions
#' @export BA
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#'
#' Batting2016$BA <- BA(Batting2016)
#'
BA <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if(!all(c("AB", "H") %in% colnames(dat))) {
        message("Not enough data to calculate. Please make sure your data inclueds 'H', and 'AB'")
    }
    ifelse(dat$AB > 0, BA <- round((dat$H/dat$AB), 3), NA)
    return(BA)
}

#' @title Batting: Calculate batting average on balls in play (BABIP)
#' @description Find BABIP for batters with more than zero at bats.
#' Required fields from the Batting table are; "AB", "BB", "H", "HBP", "SF", "SH", "HR"  and "SO."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords BABIP base on ball percentage bb
#' @family Batting functions
#' @export BABIP
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#'
#' Batting2016$BABIP <- BABIP(Batting2016)
#'
BABIP <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if(!all(c("AB", "SO", "H", "SF") %in% colnames(dat))) {
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'SO', 'H' and 'SF'")
    }

    ifelse(dat$AB > 0, BABIP <- round(((dat$H-dat$HR)/(dat$AB-dat$SO-dat$HR+dat$SF)), 3), NA)

    return(BABIP)
}

#' @title Batting: Calculate base on ball percentage
#' @description Find base on ball percentage for batters with more than zero at bats.
#' Required fields from the Batting table are; "AB", "SO", "BB", "HBP", "SF", and "SH."
#' Intentional base on balls (IBB) is added for the years that metric is available.
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords BBpct base on ball percentage bb
#' @family Batting functions
#' @export BBpct
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#'
#' Batting2016$BBpct <- BBpct(Batting2016)
#'
BBpct <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if (!all(c("AB", "BB", "HBP", "SF", "SH") %in% names(dat))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'BB', 'IBB', 'HBP', 'SF', and 'SH'")
    }

    ifelse(dat$AB > 0, BBpct <- round((dat$BB/(dat$AB+dat$BB+dat$HBP+dat$SF+dat$SH)) * 100, 3) , NA)
    return(BBpct)
}

#' @title Batting: Calculate a batter's contact rate
#' @description Find the contact rate for batters.
#' Required fields from the batting table are "AB" and "SO."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords CTpct contact rate
#' @family Batting functions
#' @export CTpct
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#'
#' Batting2016$CTpct <- CTpct(Batting2016)
#'
CTpct <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if (!all(c("AB", "SO") %in% names(dat))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB' and 'SO'")
    }

    ifelse(dat$AB > 0, CTpct <- round(((dat$AB-dat$SO)/dat$AB) * 100, 3), NA)
    return(CTpct)
}

#' @title Batting: Calculate home run percentage
#' @description Find home run percentage for batters with more than zero at bats.
#' Required fields from the Batting table are "AB" and "HR."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords HRpct home run percentage
#' @family Batting functions
#' @export HRpct
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#'
#' Batting2016$HRpct <- HRpct(Batting2016)
#'
HRpct <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if (!all(c("AB", "HR") %in% names(dat))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'HR'")
    }

    ifelse(dat$HR > 0, HRpct <- round(dat$HR/dat$AB * 100, 3), NA)
    return(HRpct)
}

#' @title Batting: Calculate ISO for batters
#' @description Find isolated power (ISO) for batters with more than zero at bats.
#' Required fields from the batting table are "H", "X2B", "X3B", "HR"."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords ISO isolated power
#' @family Batting functions
#' @export ISO
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#'
#' Batting2016$ISO <- ISO(Batting2016)
#'
ISO <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if (!all(c("AB", "X2B", "X3B", "HR") %in% names(dat))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'X2B', 'X3B' and 'HR'")
    }

    ifelse(dat$AB > 0,
               ISO <- round((((dat$H-dat$X2B-dat$X3B-dat$HR) + (dat$X2B*2) + (dat$X3B*3) + (dat$HR*4))/dat$AB)-dat$H/dat$AB, 3), NA)
    return(ISO)
}

#' @title Batting: Calculate strikeout percentage
#' @description Find strikeout percentage for batters with more than zero at bats.
#' Required fields from the Batting table are; "AB", "SO", "BB", "HBP", "SF", and "SH."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords Kpct strikeout percentage
#' @family Batting functions
#' @export Kpct
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#'
#' Batting2016$Kpct <- Kpct(Batting2016)
#'
Kpct <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if (!all(c("AB", "SO", "BB", "HBP", "SF", "SH") %in% names(dat))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'SO', 'BB', 'HBP', 'SF', and 'SH'")
    }

    ifelse(dat$SO > 0,
               Kpct <- round((dat$SO / (dat$AB + dat$BB + dat$HBP + dat$SF + dat$SH)) * 100, 3), NA)
    return(Kpct)
}

#' @title Batting: Calculate on base percentage (OBP)
#' @description Find the OBP for batters with more than zero hits.
#' Required fields from the batting table are "H", "X2B", "X3B", "HR"."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords OBP on base percentage
#' @family Batting functions
#' @export OBP
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#'
#' Batting2016$OBP <- OBP(Batting2016)
#'
OBP <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if (!all(c("H", "BB", "HBP", "AB", "SF") %in% names(dat))){
        message("Not enough data to calculate. Please make sure your data inclueds 'H', 'AB', 'BB', 'HBP' and 'SF'")
    }

   ifelse(dat$H > 0,
               OBP <- round((dat$H+dat$BB+dat$HBP)/(dat$AB+dat$BB+dat$HBP+dat$SF), 3), NA)
    return(OBP)
}

#' @title Batting: Calculate on base percentage plus slugging (OPS)
#' @description Find the OPS for batters with more than zero hits.
#' Required fields from the batting table are "H", "X2B", "X3B", "HR", "BB", "HBP", "AB" and "SF."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords OPS on base percentage
#' @family Batting functions
#' @export OPS
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#'
#' Batting2016$OPS <- OPS(Batting2016)
#'
OPS <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if (!all(c("H", "BB", "HBP", "AB", "SF", "X2B", "X3B", "HR", "AB") %in% names(dat))){
        message("Not enough data to calculate. Please make sure your data inclueds 'H', 'AB', 'BB', 'SF', 'X2B', 'X3B', and 'HR'")
    }

    ifelse(dat$H > 0,
               OPS <- round((dat$H+dat$BB+dat$HBP) / (dat$AB+dat$BB+dat$HBP+dat$SF) +
                                    ((dat$H-dat$X2B-dat$X3B-dat$HR) + (dat$X2B*2) + (dat$X3B*3) + (dat$HR*4))/dat$AB, 3), NA)
    return(OPS)
}

#' @title Batting: Calculate plate appearances for batters
#' @description Find the plate appearances (PA) for batters.
#' Required fields from the batting table are "AB", "BB", "HBP", "SH", and "SF."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords PA on base percentage
#' @family Batting functions
#' @export PA
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#'
#' Batting2016$PA <- PA(Batting2016)
#'
PA <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if (!all(c("AB", "BB", "HBP", "SF") %in% names(dat))){
        message("Not enough data to calculate. Please make sure your data inclueds AB', 'BB', 'HBP', and 'SF'")
    }

    ifelse(dat$AB >= 0,
               PA <- dat$AB+dat$BB+dat$HBP+dat$SF)
    return(PA)
}

#' @title Batting: Calculate extra base percentage
#' @description Find extra base percentage for batters with more than zero at bats.
#' Required fields from the batting table are "AB", "BB", "HBP", "SF", "SH", "X2B", "X3B", "HR"."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords XBHpct extra base percentage
#' @family Batting functions
#' @export XBHpct
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#'
#' Batting2016$XBHpct <- XBHpct(Batting2016)
#'
XBHpct <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if (!all(c("AB", "BB", "HBP", "SF", "SH", "X2B", "X3B", "HR") %in% names(dat))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'BB', 'HBP', 'SF', 'SH', 'X2B', 'X2B' and 'HR'")
    }

    ifelse(dat$AB > 0,
               XBHpct <- round(((dat$X2B+dat$X3B+dat$HR)/(dat$AB + dat$BB + dat$HBP + dat$SF + dat$SH)) * 100, 3), NA)
    return(XBHpct)
}

#' @title Batting: Calculate extra base per hit
#' @description Find the average extra bases per hit for batters with more than zero hits.
#' Required fields from the batting table are "H", "X2B", "X3B", "HR"."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords XBperH extra base per hit
#' @family Batting functions
#' @export XBperH
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#'
#' Batting2016$XBperH <- XBperH(Batting2016)
#'
XBperH <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if (!all(c("H", "X2B", "X3B", "HR") %in% names(dat))){
        message("Not enough data to calculate. Please make sure your data inclueds 'H', 'X2B', 'X3B' and 'HR'")
    }

    ifelse(dat$H > 0,
           XBperH <- round(((dat$X2B+dat$X3B+dat$HR)/(dat$H)) * 100, 3), NA)

    return(XBperH)
}

#' @title Batting: Calculate Runs Created using the basic formula.
#' @description Find the runs created using the basic formula presented by Bill James in the late 1970s.
#' Required fields from the batting table are "AB", "H", "BB", "X2B", "X3B", and "HR."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords RCbasic extra base per hit
#' @family Batting functions
#' @export RCbasic
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#'
#' Batting2016$RCbasic <- RCbasic(Batting2016)
#'
RCbasic <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if (!all(c("AB", "H", "BB", "X2B", "X3B", "HR") %in% names(dat))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'H', 'BB', 'X2B', 'X3B', and 'HR.'")
    }

    ifelse(dat$AB > 0,
               RCbasic <- ((dat$H+dat$BB)*(dat$H+2*dat$X2B+3*dat$X3B+4*dat$HR)/(dat$AB+dat$BB)), NA)
    return(RCbasic)
}

#' @title Batting: Calculate Runs Created using the technical formula.
#' @description The "Technical Version" is the most well-known formula for RC. It adds several factors to the
#' basic formula such as sacrifice hits, stolen bases and intentional base on balls.
#' Required fields from the batting table are "AB", "H", "BB", "X2B", "X3B", "HR", "GIDP", "HBP", "SB", "CS",
#' "SF" and "SH," and "IBB."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords RCtech extra base per hit
#' @family Batting functions
#' @export RCtech
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#'
#' Batting2016$RCtech <- RCtech(Batting2016)
#'
RCtech <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if (!all(c("AB", "H", "BB", "X2B", "X3B", "HR", "GIDP", "HBP",
                          "SB", "CS", "SF", "SH", "IBB") %in% names(dat))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'H', 'BB', 'X2B', 'X3B',\n
                'HR', 'GIDP', 'HBP', 'SB', 'CS', 'SF', 'SH', and 'IBB.'")
    }
    X1B <- dat$H-dat$X2B-dat$X3B-dat$HR
    TB <- X1B + 2*dat$X2B + 3*dat$X3B + 4*dat$HR
    ifelse(dat$AB > 0,
           RCtech <- (((dat$H+dat$BB-dat$CS+dat$HBP-dat$GIDP)*
                                        (TB+(.26*(dat$BB-dat$IBB+dat$HBP))) + (.52*(dat$SH+dat$SF+dat$SB)))/
                                        (dat$AB+dat$BB+dat$HBP+dat$SH+dat$SF)), NA)
    return(RCtech)
}

#' @title Batting: Calculate Runs Created using the updated 2002 formula.
#' @description The "2002 Version" is an updated version of the "Technical Version" by Bill James.
#' The 2002 RC uses the same counting stats as the Technical Version but applies weights to many of the raw stats.
#' Required fields from the batting table are "AB", "H", "BB", "X2B", "X3B", "HR", "GIDP", "HBP", "SB", "CS",
#' "SF" and "SH," "SO", and "IBB."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords RC2002 extra base per hit
#' @family Batting functions
#' @export RC2002
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#'
#' Batting2016$RC2002 <- RC2002(Batting2016)
#'
RC2002 <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if (!all(c("AB", "H", "BB", "X2B", "X3B", "HR", "GIDP", "HBP",
               "SB", "CS", "SF", "SH", "IBB", "SO") %in% names(dat))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'H', 'BB', 'X2B', 'X3B',\n
                'HR', 'GIDP', 'HBP', 'SB', 'CS', 'SF', 'SH', 'SO', and 'IBB.'")
    }

    X1B <- dat$H-dat$X2B-dat$X3B-dat$HR
    OnBaseFact <- dat$H+dat$BB-dat$CS+dat$HBP-dat$GIDP
    AdvanceFact <- (1.25*X1B)+(1.69*dat$X2B)+(3.02*dat$X3B)+(3.73*dat$HR)+0.29*(dat$BB-dat$IBB+dat$HBP)+
        0.492*(dat$SH+dat$SF+dat$SB)-(0.04*dat$SO)
    OpportunityFact <- dat$AB+dat$BB+dat$HBP+dat$SH+dat$SF
    ifelse(dat$AB > 0,
           RC2002 <- (((((2.4*OpportunityFact)+OnBaseFact)*((3*OpportunityFact)+AdvanceFact))/
                                  (9*OpportunityFact))-(0.9*OpportunityFact)), NA)
    return(RC2002)
}

#' @title Batting: Calculate slugging percentage (SLG)
#' @description Find the SLG for batters with more than zero hits.
#' Required fields from the batting table are "H", "X2B", "X3B", "HR"."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords SLG on base percentage
#' @family Batting functions
#' @export SLG
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#'
#' Batting2016$SLG <- SLG(Batting2016)
#'
SLG <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if (!all(c("H", "X2B", "X3B", "AB", "HR") %in% names(dat))){
        message("Not enough data to calculate. Please make sure your data inclueds 'H', 'AB', 'X2B', 'X3B' and 'HR'")
    }

    ifelse(dat$H > 0,
               SLG <- round(((dat$H-dat$X2B-dat$X3B-dat$HR) + (dat$X2B*2) + (dat$X3B*3) + (dat$HR*4))/dat$AB, 3), NA)
    return(SLG)
}

#' @title Batting: Calculate a batter's total bases
#' @description Find total bases.
#' Required fields from the batting table are "AB","H", "X2B", "X3B" and "HR."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords TBs total bases
#' @family Batting functions
#' @export TBs
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#'
#' Batting2016$TBs <- TBs(Batting2016)
#'
TBs <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if (!all(c("H", "X2B", "X3B", "HR") %in% names(dat))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB','H', 'X2B', 'X3B' and 'HR'")
    }

    ifelse(dat$AB > 0,
               TBs <- round(((dat$H)+(2*dat$X2B)+(3*dat$X3B)+(4*dat$HR)), 3), NA)
    return(TBs)
}

#' @title Batting: Calculate Weighted On-Base Average (wOBA)
#' @description Find the wOBA for all players with one or more hits for a particular season.
#' Required fields from the batting table are "AB", "H", "BB", "X2B", "X3B", "HR", "HBP", "SF", "IBB."
#' @param BattingTable A full batting table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recommended.
#' @param PitchingTable A full pitching table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recommended.
#' @param FieldingTable A full batting table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recommended.
#' @param Sep.Leagues If TRUE the algorithm will calculate different run environments for the National and American leagues. Grouping
#' the leagues can solve problems introduced by the designated hitter and hitting pitchers. It also serves to further isolate for
#' park factors between the American and National leagues. The default for this argument is FALSE.
#' @param NA_to_zero If TRUE this will replace NAs with 0 for years that certain stats were not counted. For example, sacrifice hits
#' were not a counted statistic until 1954, therefore we are technically unable to calculate wOBA for any player prior to 1954.
#' The default is set to TRUE. Even though this is bad practice mathematically, many in the sabermetrics community accept the practice.
#' If FALSE, the wOBA calculation will return NaN for years with missing data.
#' @param Fangraphs If TRUE the function will download wOBA values from Fangraphs. If FALSE the function will use the internal
#' formula adapted from Tom Tango's original wOBA formula. Note, the internal formula is typically identical to Fangraphs and
#' does not require an external download. If not specified, the default is set to FALSE.
#' @keywords wOBA Weighted On-Base Average
#' @family Batting functions
#' @import dplyr
#' @export wOBA
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#' data("Pitching2016")
#' head(Pitching2016)
#' data("Fielding2016")
#' head(Fielding2016)
#'
#' Batting2016$wOBA <- wOBA(Batting2016, Pitching2016, Fielding2016, Fangraphs=FALSE,
#'                           NA_to_zero=TRUE, Sep.Leagues=FALSE)
#'
wOBA <- function (BattingTable=NULL, PitchingTable=NULL, FieldingTable=NULL, Fangraphs=FALSE, NA_to_zero=TRUE, Sep.Leagues=FALSE){
    if(isTRUE(Sep.Leagues) & isTRUE(Fangraphs)){
        print("The Fangraphs Guts table does not sperate wOBA by league. Applying the default calculation...")
        Fangraphs=FALSE
    }

    dat <- BattingTable
    wOBA_values <- wOBA_values(BattingTable, PitchingTable, FieldingTable, Fangraphs=Fangraphs, Sep.Leagues=Sep.Leagues)

    if (!all(c("AB", "H", "BB", "X2B", "X3B", "HR", "HBP", "SF", "IBB") %in% names(dat))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'H', 'BB', 'X2B', 'X3B',\n
                'HR', 'HBP', 'SF', and 'IBB.'")
    }

    if (isTRUE(NA_to_zero)){
            dat <- mutate(dat, SF=ifelse(is.na(SF),0,SF), IBB=ifelse(is.na(IBB),0,IBB), HBP=ifelse(is.na(HBP),0,HBP))
    }

    if(isTRUE(Sep.Leagues)){
            wOBA_values <- wOBA_values[, c("yearID", "lgID", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR")]
            dat <- left_join(dat, wOBA_values, by=c("yearID", "lgID"))

        } else {
            wOBA_values <- wOBA_values[, c("yearID", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR")]
            dat <- left_join(dat, wOBA_values, by="yearID")
        }

        ifelse(dat$H > 0,
               wOBA <- (dat$wBB*(dat$BB-dat$IBB) + dat$wHBP*dat$HBP + dat$w1B*(dat$H-dat$X2B-dat$X3B-dat$HR) +
                                dat$w2B*dat$X2B + dat$w3B*dat$X3B + dat$wHR*dat$HR)/
                                  (dat$AB+(dat$BB-dat$IBB)+dat$SF+dat$HBP) , NA)
    return(wOBA)
}


#' @title Batting: Calculate Weighted Runs Above Average (wRAA)
#' @description Find the wRAA for all players with one or more hits for a particular season.
#' Required fields from the batting table are "AB", "H", "BB", "X2B", "X3B", "HR", "HBP", "SF", "IBB."
#' @param BattingTable A full batting table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recommended.
#' @param PitchingTable A full pitching table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recommended.
#' @param FieldingTable A full batting table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recommended.
#' @param Sep.Leagues If TRUE the algorithm will calculate different run environments for the National and American leagues. Grouping
#' the leagues can solve problems introduced by the designated hitter and hitting pitchers. It also serves to further isolate for
#' park factors between the American and National leagues. The default for this argument is FALSE.
#' @param NA_to_zero If TRUE this will replace NAs with 0 for years that certain stats were not counted. For example, sacrifice hits
#' were not a counted statistic until 1954, therefore we are technically unable to calculate wRAA for any player prior to 1954.
#' The default is set to TRUE. Even though this is bad practice mathematically, many in the sabermetrics community accept the practice.
#' If FALSE, the wRAA calculation will return NaN for years with missing data.
#' @param Fangraphs If TRUE the function will download wOBA values from Fangraphs. Both wOBA scale and league wOBA are used in the wRAA
#' calculation. If FALSE the function will use the internal wOBA algorithm, which is adapted from Tom Tango's original wOBA formula.
#' This algorithm produces a slightly different wOBA scale than the Fangraphs wOBA scale, so variations in wRAA should be expected.
#' The default internal method does not require an external download from Fangraphs. If not specified, the default is set to FALSE.
#' @keywords wRAA Weighted Runs Above Average
#' @family Batting functions
#' @import dplyr
#' @export wRAA
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#' data("Pitching2016")
#' head(Pitching2016)
#' data("Fielding2016")
#' head(Fielding2016)
#'
#' Batting2016$wRAA <- wRAA(Batting2016, Pitching2016, Fielding2016, Fangraphs=FALSE,
#'                           NA_to_zero=TRUE, Sep.Leagues=FALSE)
#'
wRAA <- function (BattingTable=NULL, PitchingTable=NULL, FieldingTable=NULL, Fangraphs=FALSE, NA_to_zero=TRUE, Sep.Leagues=FALSE){
    if(isTRUE(Sep.Leagues) & isTRUE(Fangraphs)){
        print("The Fangraphs Guts table does not sperate wOBA by league. Applying the default calculation...")
        Fangraphs=FALSE
    }
    dat <- BattingTable

    if (!all(c("AB", "H", "BB", "X2B", "X3B", "HR", "HBP", "SF", "IBB") %in% names(dat))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'H', 'BB', 'X2B', 'X3B',\n
                'HR', 'HBP', 'SF', and 'IBB.'")
    }

    wOBA_values <- wOBA_values(BattingTable, PitchingTable, FieldingTable, Fangraphs=Fangraphs, Sep.Leagues=Sep.Leagues)

    if (isTRUE(NA_to_zero)){
            dat <- mutate(dat, SF=ifelse(is.na(SF),0,SF), IBB=ifelse(is.na(IBB),0,IBB), HBP=ifelse(is.na(HBP),0,HBP))
        }
    if(isTRUE(Sep.Leagues)){
            wOBA_values <- wOBA_values[, c("yearID", "lgID", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "woba_scale", "lg_woba")]
            dat <- left_join(dat, wOBA_values, by=c("yearID", "lgID"))
        } else {
            wOBA_values <- wOBA_values[, c("yearID", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "woba_scale", "lg_woba")]
            dat <- left_join(dat, wOBA_values, by="yearID")
        }

        ifelse(dat$H > 0,
               dat$wOBA <- (dat$wBB*(dat$BB-dat$IBB) + dat$wHBP*dat$HBP + dat$w1B*(dat$H-dat$X2B-dat$X3B-dat$HR) +
                                dat$w2B*dat$X2B + dat$w3B*dat$X3B + dat$wHR*dat$HR)/
                   (dat$AB+(dat$BB-dat$IBB)+dat$SF+dat$HBP), NA)

        ifelse(dat$H > 0,
               wRAA <- ((dat$wOBA-dat$lg_woba) / dat$woba_scale * (dat$AB+dat$BB+dat$HBP+dat$SF)), NA)
    return(wRAA)
}


#' @title Batting: Calculate Weighted Runs Created (wRC)
#' @description Find the wRC for all players with one or more hits for a particular season.
#' Required fields from the batting table are "AB", "H", "BB", "X2B", "X3B", "HR", "HBP", "SF", "IBB."
#' @param BattingTable A full batting table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recommended.
#' @param PitchingTable A full pitching table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recommended.
#' @param FieldingTable A full fielding table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recommended.
#' @param Sep.Leagues If TRUE the algorithm will calculate different run environments for the National and American leagues. Grouping
#' the leagues can solve problems introduced by the designated hitter and hitting pitchers. It also serves to further isolate for
#' park factors between the American and National leagues. The default for this argument is FALSE.
#' @param NA_to_zero If TRUE this will replace NAs with 0 for years that certain stats were not counted. For example, sacrifice hits
#' were not a counted statistic until 1954, therefore we are technically unable to calculate wRC for any player prior to 1954.
#' The default is set to TRUE. Even though this is bad practice mathematically, many in the sabermetrics community accept the practice.
#' If FALSE, the wRC calculation will return NaN for years with missing data.
#' @param Fangraphs If TRUE the function will download wOBA values from Fangraphs. Both wOBA scale and league wOBA are used in the wRC
#' calculation. If FALSE the function will use the internal wOBA algorithm, which is adapted from Tom Tango's original wOBA formula.
#' This algorithm produces a slightly different wOBA scale than the Fangraphs wOBA scale, so variations in wRC should be expected.
#' The default internal method does not require an external download from Fangraphs. If not specified, the default is set to FALSE.
#' @keywords wRC Weighted Runs Above Average
#' @family Batting functions
#' @import dplyr
#' @export wRC
#' @examples
#'
#' data("Batting2016")
#' head(Batting2016)
#' data("Pitching2016")
#' head(Pitching2016)
#' data("Fielding2016")
#' head(Fielding2016)
#'
#' Batting2016$wRC <- wRC(Batting2016, Pitching2016, Fielding2016, Fangraphs=FALSE,
#'                          NA_to_zero=TRUE, Sep.Leagues=FALSE)
#'
wRC <- function (BattingTable=NULL, PitchingTable=NULL, FieldingTable=NULL, Fangraphs=FALSE, NA_to_zero=TRUE, Sep.Leagues=FALSE){
    if(isTRUE(Sep.Leagues) & isTRUE(Fangraphs)){
        print("The Fangraphs Guts table does not sperate wOBA by league. Applying the default calculation...")
        Fangraphs=FALSE
    }
    dat <- BattingTable

    if (!all(c("AB", "H", "BB", "X2B", "X3B", "HR", "HBP", "SF", "IBB") %in% names(dat))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'H', 'BB', 'X2B', 'X3B',\n
                'HR', 'HBP', 'SF', and 'IBB.'")
    }

    wOBA_values <- wOBA_values(BattingTable, PitchingTable, FieldingTable, Fangraphs=Fangraphs, Sep.Leagues=Sep.Leagues)

    if (!isTRUE(Fangraphs)) {
            wOBA_values$lg_r_pa <- wOBA_values$R / (wOBA_values$AB+wOBA_values$BB+wOBA_values$HBP+wOBA_values$SF)
        }
    if (isTRUE(NA_to_zero)){
            dat <- mutate(dat, SF=ifelse(is.na(SF),0,SF), IBB=ifelse(is.na(IBB),0,IBB), HBP=ifelse(is.na(HBP),0,HBP))
        }
    if(isTRUE(Sep.Leagues)){
            wOBA_values <- wOBA_values[, c("yearID", "lgID", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "woba_scale", "lg_woba", "lg_r_pa")]
            dat <- left_join(dat, wOBA_values, by=c("yearID", "lgID"))
        } else {
            wOBA_values <- wOBA_values[, c("yearID", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "woba_scale", "lg_woba", "lg_r_pa")]
            dat <- left_join(dat, wOBA_values, by="yearID")
        }

        ifelse(dat$H > 0,
               dat$wOBA <- (dat$wBB*(dat$BB-dat$IBB) + dat$wHBP*dat$HBP + dat$w1B*(dat$H-dat$X2B-dat$X3B-dat$HR) +
                                dat$w2B*dat$X2B + dat$w3B*dat$X3B + dat$wHR*dat$HR)/
                   (dat$AB+(dat$BB-dat$IBB)+dat$SF+dat$HBP), NA)

        ifelse(dat$H > 0,
               wRC <- ((((dat$wOBA-dat$lg_woba) / dat$woba_scale) + dat$lg_r_pa) * (dat$AB+dat$BB+dat$HBP+dat$SF)), NA)
    return(wRC)
    }




