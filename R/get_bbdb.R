#' @title Get an up to date copy of the Baseball Databank.
#' @description Download the newest version of the Baseball Databank from the Chadwick Bureau GitHub repository. This is the soruce of
#' Sean Lahman's baseball database and is always under development. This function will read the .csv files and return them as data frames.
#' There is also an option to download the entire directory.
#' @param table The tables you would like to download. Uses Lahman table names in lower case. Ex. "batting", "master", "allstarfull", etc...
#' @keywords woba, wOBA, on base average, fangraphs
#' @export get_bbdb
#' @examples
#' \dontrun{
#'
#'}
#'

# Todo: Figure out a way to load multiple tables c("batting", "pitching")
# This function could replace the dependency on the Lahman package.
get_bbdb <- function(table=NULL){
    if (is.null(table)){
        download.file("https://github.com/chadwickbureau/register/archive/master.zip", zipdir)
    }
    if (!is.null(table())) {
        baseURL <- "https://raw.githubusercontent.com/chadwickbureau/baseballdatabank/master/core/"

        table <- read.csv(paste0(baseURL, table, ".csv"), stringsAsFactors = F)
    }






}
