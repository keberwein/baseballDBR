#' @title Get an up to date copy of the Baseball Databank.
#' @description Download the newest version of the Baseball Databank from the Chadwick Bureau GitHub repository. This is the soruce of
#' Sean Lahman's baseball database and is always under development. This function will read the .csv files and return them as data frames.
#' There is also an option to download the entire directory.
#' @keywords woba, wOBA, on base average, fangraphs
#' @export get_bbdb
#' @examples
#' \dontrun{
#'
#'}
#'

get_bbdb <- function(Sep.Leagues=FALSE, Fangraphs=FALSE){
    baseURL <- "https://raw.githubusercontent.com/chadwickbureau/baseballdatabank/master/core/"

    z <- read.csv(paste0(baseURL, "Batting", ".csv"), stringsAsFactors = F)

}
