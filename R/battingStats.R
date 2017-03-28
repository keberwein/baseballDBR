
#' @title Calculate batting average
#' @description Find batting average for batters with more than zero at bats.
#' Required fields from the Batting table are; "AB", and "H."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords BA base on ball percentage bb
#' @export BA
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- BA(Batting)
#' new_df
#' }
#'
BA <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }
    if (any(!isTRUE(c("AB", "H") %in% names(dat)))){
        ifelse(dat$BA > 0,
               dat$BA <- round((dat$H/dat$AB), 3), NA)
    }
    if (any(isTRUE(c("AB", "H") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'BB', 'IBB', 'HBP', 'SF', and 'SH'")
    }
    return(dat)
}

#' @title Calculate batting average on balls in play (BABIP)
#' @description Find BABIP for batters with more than zero at bats.
#' Required fields from the Batting table are; "AB", "BB", "H", "HBP", "SF", "SH", "HR"  and "SO."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords BABIP base on ball percentage bb
#' @export BABIP
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- BABIP(batting_df)
#' new_df
#' }
#'
BABIP <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }
    if (any(!isTRUE(c("AB", "BB", "H", "HBP", "SF", "SH", "HR", "SO") %in% names(dat)))){
        ifelse(dat$AB > 0,
               dat$BABIP <- round(((dat$H-dat$HR)/((dat$AB+dat$BB+dat$HBP+dat$SF+dat$SH)-dat$SO-dat$BB-dat$HR)), 3), NA)
    }
    if (any(isTRUE(c("AB", "BB", "H", "HBP", "SF", "SH", "HR", "SO") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'BB', 'H', 'HBP', 'SF', 'SH', 'HR', and 'SO.'")
    }
    return(dat)
}

#' @title Calculate base on ball percentage
#' @description Find base on ball percentage for batters with more than zero at bats.
#' Required fields from the Batting table are; "AB", "SO", "BB", "HBP", "SF", and "SH."
#' Intentional base on balls (IBB) is added for the years that metric is available.
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords BBpct base on ball percentage bb
#' @export BBpct
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- BBpct(Batting)
#' new_df
#' }
#'
BBpct <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
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
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords CTpct contact rate
#' @export CTpct
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- CTpct(Batting)
#' new_df
#' }
#'
CTpct <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }
    if (any(!isTRUE(c("AB", "SO") %in% names(dat)))){
        ifelse(dat$AB > 0,
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
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords HRpct home run percentage
#' @export HRpct
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- HRpct(Batting)
#' new_df
#' }
#'
HRpct <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }
    if (any(!isTRUE(c("AB", "HR") %in% names(dat)))){
        ifelse(dat$HR > 0,
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
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords ISO isolated power
#' @export ISO
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- ISO(Batting)
#' new_df
#' }
#'
ISO <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
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
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords Kpct strikeout percentage
#' @export Kpct
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- Kpct(Batting)
#' new_df
#' }
#'
Kpct <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }
    if (any(!isTRUE(c("AB", "SO", "BB", "HBP", "SF", "SH") %in% names(dat)))){
        ifelse(dat$SO > 0,
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
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords OBP on base percentage
#' @export OBP
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- OBP(Batting)
#' new_df
#' }
#'
OBP <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
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
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords OPS on base percentage
#' @export OPS
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- OPS(Batting)
#' new_df
#' }
#'
OPS <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
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

#' @title Calculate plate appearances for batters
#' @description Find the plate appearances (PA) for batters.
#' Required fields from the batting table are "AB", "BB", "HBP", "SH", and "SF."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords PA on base percentage
#' @export PA
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- PA(Batting)
#' new_df
#' }
#'
PA <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }
    if (any(!isTRUE(c("AB", "BB", "HBP", "SF", "SH") %in% names(dat)))){
        ifelse(dat$AB >= 0,
               dat$PA <- dat$AB+dat$BB+dat$HBP+dat$SF+dat$SH)
    }
    if (any(isTRUE(c("AB", "BB", "HBP", "SF", "SH") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds AB', 'BB', 'HBP', 'SF', and 'SH'")
    }
    return(dat)
}

#' @title Calculate slugging percentage (SLG)
#' @description Find the SLG for batters with more than zero hits.
#' Required fields from the batting table are "H", "X2B", "X3B", "HR"."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords SLG on base percentage
#' @export SLG
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- SLG(Batting)
#' new_df
#' }
#'
SLG <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
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
#' Required fields from the batting table are "AB","H", "X2B", "X3B" and "HR."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords TBs total bases
#' @export TBs
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- TBs(Batting)
#' new_df
#' }
#'
TBs <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }
    if (any(!isTRUE(c("AB", "H", "X2B", "X3B", "HR") %in% names(dat)))){
        ifelse(dat$AB > 0,
               dat$TBs <- round(((dat$H)+(2*dat$X2B)+(3*dat$X3B)+(4*dat$HR)), 3), NA)
    }
    if (any(isTRUE(c("H", "X2B", "X3B", "HR") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB','H', 'X2B', 'X3B' and 'HR'")
    }
    return(dat)
}

#' @title Calculate extra base percentage
#' @description Find extra base percentage for batters with more than zero at bats.
#' Required fields from the batting table are "AB", "BB", "HBP", "SF", "SH", "X2B", "X3B", "HR"."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords XBHpct extra base percentage
#' @export XBHpct
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- XBHpct(Batting)
#' new_df
#' }
#'
XBHpct <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
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
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords XBperH extra base per hit
#' @export XBperH
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- XBperH(Batting)
#' new_df
#' }
#'
XBperH <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
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

#' @title Calculate Runs Created using the basic formula.
#' @description Find the runs created using the basic formula presented by Bill James in the late 1970s.
#' Required fields from the batting table are "AB", "H", "BB", "X2B", "X3B", and "HR."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords RCbasic extra base per hit
#' @export RCbasic
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- RCbasic(Batting)
#' new_df
#' }
#'
RCbasic <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }
    if (any(!isTRUE(c("AB", "H", "BB", "X2B", "X3B", "HR") %in% names(dat)))){
        ifelse(dat$AB > 0,
               dat$RCbasic <- ((dat$H+dat$BB)*(dat$H+2*dat$X2B+3*dat$X3B+4*dat$HR)/(dat$AB+dat$BB)), NA)
    }
    if (any(isTRUE(c("AB", "H", "BB", "X2B", "X3B", "HR") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'H', 'BB', 'X2B', 'X3B', and 'HR.'")
    }

    return(dat)
}

#' @title Calculate Runs Created using the technical formula.
#' @description The "Technical Version" is the most well-known formula for RC. It adds several factors to the
#' basic formula such as sacrifice hits, stolen bases and intentional base on balls.
#' Required fields from the batting table are "AB", "H", "BB", "X2B", "X3B", "HR", "GIDP", "HBP", "SB", "CS",
#' "SF" and "SH," and "IBB."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords RCtech extra base per hit
#' @export RCtech
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- RCtech(Batting)
#' new_df
#' }
#'
RCtech <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }
    if (any(!isTRUE(c("AB", "H", "BB", "X2B", "X3B", "HR", "GIDP", "HBP",
                          "SB", "CS", "SF", "SH", "IBB") %in% names(dat)))){
    X2B=X3B=HR=NULL
    X1B <- dat$H-dat$X2B-dat$X3B-dat$HR
    TB <- X1B + 2*X2B + 3*X3B + 4*HR
    ifelse(dat$AB > 0,
           dat$RCtech <- (((dat$H+dat$BB-dat$CS+dat$HBP-dat$GIDP)*
                                        (TB+(.26*(dat$BB-dat$IBB+dat$HBP))) + (.52*(dat$SH+dat$SF+dat$SB)))/
                                        (dat$AB+dat$BB+dat$HBP+dat$SH+dat$SF)), NA)
    }
    if (any(isTRUE(c("AB", "H", "BB", "X2B", "X3B", "HR", "GIDP", "HBP",
                     "SB", "CS", "SF", "SH", "IBB") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'H', 'BB', 'X2B', 'X3B',\n
                'HR', 'GIDP', 'HBP', 'SB', 'CS', 'SF', 'SH', and 'IBB.'")
    }

    return(dat)
}

#' @title Calculate Runs Created using the updated 2002 formula.
#' @description The "2002 Version" is an updated version of the "Technical Version" by Bill James.
#' The 2002 RC uses the same counting stats as the Technical Version but applies weights to many of the raw stats.
#' Required fields from the batting table are "AB", "H", "BB", "X2B", "X3B", "HR", "GIDP", "HBP", "SB", "CS",
#' "SF" and "SH," "SO", and "IBB."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' For a list of column names, use the \code{Lahman_names()} function.
#' @keywords RC2002 extra base per hit
#' @export RC2002
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- RC2002(Batting)
#' new_df
#' }
#'
RC2002 <- function (dat=NULL){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }
    if (any(!isTRUE(c("AB", "H", "BB", "X2B", "X3B", "HR", "GIDP", "HBP",
                      "SB", "CS", "SF", "SH", "IBB", "SO") %in% names(dat)))){
    X1B <- dat$H-dat$X2B-dat$X3B-dat$HR
    OnBaseFact <- dat$H+dat$BB-dat$CS+dat$HBP-dat$GIDP
    AdvanceFact <- (1.25*X1B)+(1.69*dat$X2B)+(3.02*dat$X3B)+(3.73*dat$HR)+0.29*(dat$BB-dat$IBB+dat$HBP)+
        0.492*(dat$SH+dat$SF+dat$SB)-(0.04*dat$SO)
    OpportunityFact <- dat$AB+dat$BB+dat$HBP+dat$SH+dat$SF
    ifelse(dat$AB > 0,
           dat$RC2002 <- (((((2.4*OpportunityFact)+OnBaseFact)*((3*OpportunityFact)+AdvanceFact))/
                                  (9*OpportunityFact))-(0.9*OpportunityFact)), NA)
    }
    if (any(isTRUE(c("AB", "H", "BB", "X2B", "X3B", "HR", "GIDP", "HBP",
                     "SB", "CS", "SF", "SH", "IBB", "SO") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'H', 'BB', 'X2B', 'X3B',\n
                'HR', 'GIDP', 'HBP', 'SB', 'CS', 'SF', 'SH', 'SO', and 'IBB.'")
    }

    return(dat)
}


#' @title Calculate Weighted On-Base Average (wOBA)
#' @description Find the wOBA for all players with one or more hits for a particular season.
#' Required fields from the batting table are "AB", "H", "BB", "X2B", "X3B", "HR", "HBP", "SF", "IBB."
#' @param BattingTable A full batting table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recomended.
#' @param PitchingTable A full pitching table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recomended.
#' @param FieldingTable A full batting table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recomended.
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
#' @keywords wOBA Weighted On-Base Average
#' @export wOBA
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- wOBA(Batting, Fangraphs=FALSE, NA_to_zero=TRUE, Sep.Leagues=FALSE)
#' new_df
#' }
#'
wOBA <- function (BattingTable, PitchingTable, FieldingTable, Fangraphs=FALSE, NA_to_zero=TRUE, Sep.Leagues=FALSE){
    if (is.null(BattingTable) | is.null(PitchingTable) | is.null(FieldingTable)){
        print("Please supply source Batting, Pitching, and Fielding data frames.")
    }

    if(isTRUE(Sep.Leagues) & isTRUE(Fangraphs)){
        print("The Fangraphs Guts table does not sperate wOBA by league. Applying the default calculation...")
        Fangraphs=FALSE
    }

    dat <- BattingTable
    wOBA_values <- wOBA_values(BattingTable, PitchingTable, FieldingTable, Fangraphs=Fangraphs, Sep.Leagues=Sep.Leagues)

    if (any(!isTRUE(c("AB", "H", "BB", "X2B", "X3B", "HR", "HBP", "SF", "IBB") %in% names(dat)))){
        if (isTRUE(NA_to_zero)){
            dat <- dplyr::mutate(dat, SF=ifelse(is.na(SF),0,SF))
            dat <- dplyr::mutate(dat, IBB=ifelse(is.na(IBB),0,IBB))
            dat <- dplyr::mutate(dat, HBP=ifelse(is.na(HBP),0,HBP))
        }
        if(isTRUE(Sep.Leagues)){
            wOBA_values <- wOBA_values[, c("yearID", "lgID", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR")]
            dat <- dplyr::left_join(dat, wOBA_values, by=c("yearID", "lgID"))
        } else {
            wOBA_values <- wOBA_values[, c("yearID", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR")]
            dat <- dplyr::left_join(dat, wOBA_values, by="yearID")
        }

        ifelse(dat$H > 0,
               dat$wOBA <- (dat$wBB*(dat$BB-dat$IBB) + dat$wHBP*dat$HBP + dat$w1B*(dat$H-dat$X2B-dat$X3B-dat$HR) +
                                dat$w2B*dat$X2B + dat$w3B*dat$X3B + dat$wHR*dat$HR)/
                                  (dat$AB+(dat$BB-dat$IBB)+dat$SF+dat$HBP) , NA)
    }
    if (any(isTRUE(c("AB", "H", "BB", "X2B", "X3B", "HR", "HBP", "SF", "IBB") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'H', 'BB', 'X2B', 'X3B',\n
                'HR', 'HBP', 'SF', and 'IBB.'")
    }

    dat <- dat[, !names(dat) %in% c("wBB", "wHBP", "w1B", "w2B", "w3B", "wHR")]

    return(dat)
}


#' @title Calculate Weighted Runs Above Average (wRAA)
#' @description Find the wRAA for all players with one or more hits for a particular season.
#' Required fields from the batting table are "AB", "H", "BB", "X2B", "X3B", "HR", "HBP", "SF", "IBB."
#' @param BattingTable A full batting table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recomended.
#' @param PitchingTable A full pitching table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recomended.
#' @param FieldingTable A full batting table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recomended.
#' @param Sep.Leagues If TRUE the algorithum will calculate different run enviornments for the National and American leagues. Grouping
#' the leauges can solve problems introduced by the designated hitter and hitting pitchers. It also serves to further isolate for
#' park factors between the American and National leauges. The default for this argument is FALSE.
#' @param NA_to_zero If TRUE this will replace NAs with 0 for years that certain stats weren't counted. For example, sacrafice hits
#' weren't a counted statistic until 1954, therefore we are technically unable to calculate wRAA for any player prior to 1954.
#' The default is set to TRUE. Even though this is bad practice mathematically, many in the sabermetrics community accept the practice.
#' If FALSE, the wRAA calculation will return NaN for years with missing data.
#' @param Fangraphs If TRUE the function will download wOBA values from Fangraphs. Both wOBA scale and leauge wOBA are used in the wRAA
#' calculation. If FALSE the function will use the internal wOBA algorithum, which is adapted from Tom Tango's original wOBA formula.
#' This algorithum produces a slightly different wOBA scale than the Fangraphs wOBA scale, so variations in wRAA should be expected.
#' The default internal method does not require an external download from Fangraphs. If not specified, the default is set to FALSE.
#' @keywords wRAA Weighted Runs Above Average
#' @export wRAA
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- wRAA(Batting, Fangraphs=FALSE, NA_to_zero=TRUE, Sep.Leagues=FALSE)
#' new_df
#' }
#'
wRAA <- function (BattingTable=NULL, PitchingTable=NULL, FieldingTable=NULL, Fangraphs=FALSE, NA_to_zero=TRUE, Sep.Leagues=FALSE){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }

    if(isTRUE(Sep.Leagues) & isTRUE(Fangraphs)){
        print("The Fangraphs Guts table does not sperate wOBA by league. Applying the default calculation...")
        Fangraphs=FALSE
    }
    dat <- BattingTable
    wOBA_values <- wOBA_values(BattingTable, PitchingTable, FieldingTable, Fangraphs=Fangraphs, Sep.Leagues=Sep.Leagues)

    if (any(!isTRUE(c("AB", "H", "BB", "X2B", "X3B", "HR", "HBP", "SF", "IBB") %in% names(dat)))){
        if (isTRUE(NA_to_zero)){
            dat <- dplyr::mutate(dat, SF=ifelse(is.na(SF),0,SF))
            dat <- dplyr::mutate(dat, IBB=ifelse(is.na(IBB),0,IBB))
            dat <- dplyr::mutate(dat, HBP=ifelse(is.na(HBP),0,HBP))
        }

        if(isTRUE(Sep.Leagues)){
            woba <- woba[, c("yearID", "lgID", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "woba_scale", "lg_woba")]
            dat <- dplyr::left_join(dat, woba, by=c("yearID", "lgID"))
        } else {
            woba <- woba[, c("yearID", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "woba_scale", "lg_woba")]
            dat <- dplyr::left_join(dat, woba, by="yearID")
        }

        ifelse(dat$H > 0,
               dat$wOBA <- (dat$wBB*(dat$BB-dat$IBB) + dat$wHBP*dat$HBP + dat$w1B*(dat$H-dat$X2B-dat$X3B-dat$HR) +
                                dat$w2B*dat$X2B + dat$w3B*dat$X3B + dat$wHR*dat$HR)/
                   (dat$AB+(dat$BB-dat$IBB)+dat$SF+dat$HBP), NA)

        ifelse(dat$H > 0,
               dat$wRAA <- ((dat$wOBA-dat$lg_woba) / dat$woba_scale * (dat$AB+dat$BB+dat$HBP+dat$SF)), NA)

    }
    if (any(isTRUE(c("AB", "H", "BB", "X2B", "X3B", "HR", "HBP", "SF", "IBB") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'H', 'BB', 'X2B', 'X3B',\n
                'HR', 'HBP', 'SF', and 'IBB.'")
    }

    dat <- dat[, !names(dat) %in% c("wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "woba_scale", "lg_woba")]

    return(dat)
}


#' @title Calculate Weighted Runs Created (wRC)
#' @description Find the wRC for all players with one or more hits for a particular season.
#' Required fields from the batting table are "AB", "H", "BB", "X2B", "X3B", "HR", "HBP", "SF", "IBB."
#' @param BattingTable A full batting table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recomended.
#' @param PitchingTable A full pitching table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recomended.
#' @param FieldingTable A full batting table from the \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' Any subsetting or removal of players will affect your results. All players for each year are recomended.
#' @param Sep.Leagues If TRUE the algorithum will calculate different run enviornments for the National and American leagues. Grouping
#' the leauges can solve problems introduced by the designated hitter and hitting pitchers. It also serves to further isolate for
#' park factors between the American and National leauges. The default for this argument is FALSE.
#' @param NA_to_zero If TRUE this will replace NAs with 0 for years that certain stats weren't counted. For example, sacrafice hits
#' weren't a counted statistic until 1954, therefore we are technically unable to calculate wRC for any player prior to 1954.
#' The default is set to TRUE. Even though this is bad practice mathematically, many in the sabermetrics community accept the practice.
#' If FALSE, the wRC calculation will return NaN for years with missing data.
#' @param Fangraphs If TRUE the function will download wOBA values from Fangraphs. Both wOBA scale and leauge wOBA are used in the wRC
#' calculation. If FALSE the function will use the internal wOBA algorithum, which is adapted from Tom Tango's original wOBA formula.
#' This algorithum produces a slightly different wOBA scale than the Fangraphs wOBA scale, so variations in wRC should be expected.
#' The default internal method does not require an external download from Fangraphs. If not specified, the default is set to FALSE.
#' @keywords wRC Weighted Runs Above Average
#' @export wRC
#' @examples
#' \dontrun{
#' get_bbdb("Batting")
#' new_df <- wRC(Batting, Fangraphs=FALSE, NA_to_zero=TRUE, Sep.Leagues=FALSE)
#' new_df
#' }
#'
wRC <- function (BattingTable=NULL, PitchingTable=NULL, FieldingTable=NULL, Fangraphs=FALSE, NA_to_zero=TRUE, Sep.Leagues=FALSE){
    if (is.null(dat)){
        print("Please supply a source data frame. See the get_bbdb() function for help.")
    }

    if(isTRUE(Sep.Leagues) & isTRUE(Fangraphs)){
        print("The Fangraphs Guts table does not sperate wOBA by league. Applying the default calculation...")
        Fangraphs=FALSE
    }
    dat <- BattingTable
    wOBA_values <- wOBA_values(BattingTable, PitchingTable, FieldingTable, Fangraphs=Fangraphs, Sep.Leagues=Sep.Leagues)

    if (any(!isTRUE(c("AB", "H", "BB", "X2B", "X3B", "HR", "HBP", "SF", "IBB") %in% names(dat)))){
        if (!isTRUE(Fangraphs)) {
            woba$lg_r_pa <- woba$R / (woba$AB+woba$BB+woba$HBP+woba$SF)
        }

        if (isTRUE(NA_to_zero)){
            dat <- dplyr::mutate(dat, SF=ifelse(is.na(SF),0,SF))
            dat <- dplyr::mutate(dat, IBB=ifelse(is.na(IBB),0,IBB))
            dat <- dplyr::mutate(dat, HBP=ifelse(is.na(HBP),0,HBP))
        }

        if(isTRUE(Sep.Leagues)){
            woba <- woba[, c("yearID", "lg_r_pa", "lgID", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "woba_scale", "lg_woba")]
            dat <- dplyr::left_join(dat, woba, by=c("yearID", "lgID"))
        } else {
            woba <- woba[, c("yearID", "lg_r_pa", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "woba_scale", "lg_woba")]
            dat <- dplyr::left_join(dat, woba, by="yearID")
        }

        ifelse(dat$H > 0,
               dat$wOBA <- (dat$wBB*(dat$BB-dat$IBB) + dat$wHBP*dat$HBP + dat$w1B*(dat$H-dat$X2B-dat$X3B-dat$HR) +
                                dat$w2B*dat$X2B + dat$w3B*dat$X3B + dat$wHR*dat$HR)/
                   (dat$AB+(dat$BB-dat$IBB)+dat$SF+dat$HBP), NA)

        ifelse(dat$H > 0,
               dat$wRC <- ((((dat$wOBA-dat$lg_woba) / dat$woba_scale) + dat$lg_r_pa) * (dat$AB+dat$BB+dat$HBP+dat$SF)), NA)

    }
    if (any(isTRUE(c("AB", "H", "BB", "X2B", "X3B", "HR", "HBP", "SF", "IBB") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'H', 'BB', 'X2B', 'X3B',\n
                'HR', 'HBP', 'SF', and 'IBB.'")
    }

    dat <- dat[, !names(dat) %in% c("wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "woba_scale", "lg_woba")]

    return(dat)
    }




