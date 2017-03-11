#' @title Get an up to date copy of the Baseball Databank.
#' @description Download the newest version of the Baseball Databank from the Chadwick Bureau GitHub repository. This is the soruce of
#' Sean Lahman's baseball database and is always under development. This function will read the .csv files and return them as data frames.
#' There is also an option to download the entire directory.
#' @param table The tables you would like to download. Uses Lahman table names Ex. "Batting", "Master", "AllstarFull", etc...
#' If this argument is left as NULL, the function will download all twenty-seven tables.
#' @param downlaodZip If ture, this will download a zip file of all twenty-seven tables in .csv format to your working directory.
#' @keywords database, data frame
#' @export get_bbdb
#' @examples
#' \dontrun{
#' get_bbdb(table = c("Batting", "Pitching"))
#'}
#'
#'\dontrun{
#' get_bbdb(downloadZip = TRUE)
#'}

# Todo: Clean up code block with ifelse()
# Todo: Need to add TryCatch
# This function could replace the dependency on the Lahman package.
get_bbdb <- function(table=NULL, downloadZip=FALSE){
    if (isTRUE(downloadZip)) {
        download.file("https://github.com/chadwickbureau/baseballdatabank/archive/master.zip", "master.zip")
    }
    if (!is.null(table)) {
        baseURL <- "https://raw.githubusercontent.com/chadwickbureau/baseballdatabank/master/core/"
        urlList <- list()

        for (i in 1:length(table)) {
            urlList[[i]] <- paste0(baseURL, table[i], ".csv")
        }
        list2env(lapply(setNames(urlList, make.names(gsub("*.csv$", "", table))), read.csv, stringsAsFactors=FALSE), envir = .GlobalEnv)
    }
    if (is.null(table)) {
        download.file("https://github.com/chadwickbureau/baseballdatabank/archive/master.zip", "master.zip")
        # It would be better to read the file names of the unzipped directory in case table names change.
        table <- c("AllstarFull", "Appearances", "AwardsManagers", "AwardsPlayers", "AwardsShareManagers", "AwardsSharePlayers", "Batting", "BattingPost", "CollegePlaying",
                   "Fielding", "FieldingOF", "FieldingOFsplit", "FieldingPost", "HallOfFame", "HomeGames", "Managers", "ManagersHalf", "Master", "Parks", "Pitching", "PitchingPost",
                   "Salaries", "Schools", "SeriesPost", "Teams", "TeamsFranchises", "TeamsHalf")
        unzip("master.zip")
        baseURL <- "baseballdatabank-master/core/"
        urlList <- list()
        for (i in 1:length(table)) {
            urlList[[i]] <- paste0(baseURL, table[i], ".csv")
        }
        list2env(lapply(setNames(urlList, make.names(gsub("*.csv$", "", table))), read.csv, stringsAsFactors=FALSE), envir = .GlobalEnv)
        rm("master.zip")
        unlink("baseballdatabank-master", recursive=T)
    }
}


