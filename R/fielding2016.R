#' A sample subset of the Fielding table from the Baseball Databank for the year 2016.
#'
#' A dataset containing Fielding statistics in 2016.
#'
#' @format A data frame with 1953 rows and 18 variables:
#' \describe{
#'   \item{playerID}{database key for unique player}
#'   \item{yearID}{year}
#'   \item{stint}{number of times played on team in a single year}
#'   \item{teamID}{database key for unique team}
#'   \item{lgID}{database key for unique league}
#'   \item{POS}{primary position}
#'   \item{G}{number of games played}
#'   \item{GS}{number of games started}
#'   \item{InnOuts}{number of outs played in field}
#'   \item{PO}{number of putouts}
#'   \item{A}{number of assists}
#'   \item{E}{number of home errors}
#'   \item{DP}{number of double plays}
#'   \item{PB}{number of passed balls by catchers}
#'   \item{WP}{number of wild pitches by catchers}
#'   \item{SB}{opponent stolen bases by catchers}
#'   \item{CS}{opponents caught stealing by catchers}
#'   \item{ZR}{zone rating}

#' }
#' @docType data
#' @keywords internal
#' @usage data(Fielding2016)
#' @note Last updated 2016-06-15
"Fielding2016"
