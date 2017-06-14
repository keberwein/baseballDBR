#' League averages and aggregates from 1901 to present.
#'
#' A dataset containing combined aggregates and averages for all of the
#' MLB. These stats are primarily used in calculated advanced player stats.
#'
#' @format A data frame with 115 rows and 20 variables:
#' \describe{
#'   \item{yearID}{year}
#'   \item{tot_G}{total games played}
#'   \item{tot_PA}{total plate appearances}
#'   \item{tot_HR}{total home runs}
#'   \item{tot_R}{total runs scored}
#'   \item{tot_RBI}{total runs batted in}
#'   \item{tot_SB}{total stolen bases}
#'   \item{avg_BB}{mean base on ball percentage}
#'   \item{avg_K}{mean strikeout percentage}
#'   \item{avg_ISO}{mean isolated power}
#'   \item{avg_BABIP}{mean batting average on balls in play}
#'   \item{avg_BA}{mean batting average}
#'   \item{avg_OBP}{mean on base percentage}
#'   \item{avg_SLG}{mean slugging percentage}
#'   \item{avg_wOBA}{mean weighted on base average}
#'   \item{avg_wRC}{mean weighted runs created}
#'   \item{avg_BsR}{mean base running average}
#'   \item{off}{offense}
#'   \item{def}{defense}
#'   \item{avg_WAR}{mean wins above replacement}
#' }
#' @docType data
#' @keywords internal
#' @usage data(seasonAVG)
#' @note Last updated 2016-09-21
"seasonAVG"
