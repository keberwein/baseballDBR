
#' @title Fielding: Calculate defensive chances
#' @description The number of chances a player had to make a defensive play.
#' Required fields from the Fielding table are; "PO", "A", and "E."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords Ch Defensive Chances
#' @family Fielding functions
#' @export Ch
#' @examples
#'
#' data("Fielding2016")
#' head(Fielding2016)
#'
#' Fielding2016$Ch <- Ch(Fielding2016)
#'
Ch <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if(!all(c("PO", "A", "E") %in% colnames(dat))) {
        message("Not enough data to calculate. Please make sure your data inclueds 'PO', 'A' and 'E'")
    }
    Ch <- dat$A + dat$PO + dat$E
    return(Ch)
}



#' @title Fielding: Calculate batting average
#' @description Find batting average for batters with more than zero at bats.
#' Required fields from the Fielding table are; "PO", "A", and "E."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords Fld_pct fielding percentage
#' @family Fielding functions
#' @export Fld_pct
#' @examples
#'
#' data("Fielding2016")
#' head(Fielding2016)
#'
#' Fielding2016$Fld_pct <- Fld_pct(Fielding2016)
#'
Fld_pct <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if(!all(c("PO", "A", "E") %in% colnames(dat))) {
        message("Not enough data to calculate. Please make sure your data inclueds 'PO', 'A' and 'E'")
    }
    ifelse(dat$PO+dat$A+dat$E > 0, Fld_pct <- round(((dat$PO + dat$A) / (dat$PO + dat$A + dat$E)), 3), NA)
    return(Fld_pct)
}
