
#' @title Calculate defensive chances
#' @description The number of chances a player had to make a defensive play.
#' Required fields from the Fielding table are; "PO", "A", and "E."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords Ch Defensive Chances
#' @export Ch
#' @examples
#' \dontrun{
#' get_bbdb("Fielding")
#' Fielding$Ch <- Ch(Fielding)
#'
#' }
#'
Ch <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if(!all(c("PO", "A", "E") %in% colnames(dat))) {
        message("Not enough data to calculate. Please make sure your data inclueds 'PO', 'A' and 'E'")
    }
    Ch <- dat$A + dat$PO + dat$E
    return(Ch)
}



#' @title Calculate batting average
#' @description Find batting average for batters with more than zero at bats.
#' Required fields from the Fielding table are; "PO", "A", and "E."
#' @param dat A data frame you would wish to calculate. The data frame must have the same column names found in
#' The \code{Lahman} package or the Chadwick Bureau GitHub repository.
#' @keywords Fld_pct fielding percentage
#' @export Fld_pct
#' @examples
#' \dontrun{
#' get_bbdb("Fielding")
#' Fielding$Fld_pct <- Fld_pct(Fielding)
#'
#' }
#'
Fld_pct <- function (dat=NULL){
    ifelse(is.null(dat), message("Please supply a valid data frame."), dat <- dat)

    if(!all(c("PO", "A", "E") %in% colnames(dat))) {
        message("Not enough data to calculate. Please make sure your data inclueds 'PO', 'A' and 'E'")
    }
    ifelse(dat$PO+dat$A+dat$E > 0, Fld_pct <- round(((dat$PO + dat$A) / (dat$PO + dat$A + dat$E)), 3), NA)
    return(Fld_pct)
}
