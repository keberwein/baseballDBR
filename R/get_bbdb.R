#' @title Get an up to date copy of the Baseball Databank.
#' @description Download the newest version of the Baseball Databank from the Chadwick Bureau GitHub repository. This is the source of
#' Sean Lahman's baseball database and is always under development. This function will read the .csv files and return them as data frames.
#' There is also an option to download the entire directory.
#' @param table The tables you would like to download. Uses Lahman table names Ex. "Batting", "Master", "AllstarFull", etc...
#' If this argument is left as NULL, the function will download all twenty-seven tables.
#' @param downloadZip If true, this will download a zip file of all twenty-seven tables in .csv format to your working directory.
#' @param AllTables If true, this will download all the tables in the database. The default is set to false.
#' @keywords database, data frame
#' @import utils
#' @export get_bbdb
#' @examples
#'
#' get_bbdb(table = "Batting")
#'
#' \dontrun{
#' get_bbdb(table = c("Batting", "Pitching"))
#'}
#'
#'\dontrun{
#' get_bbdb(downloadZip = TRUE)
#'}

get_bbdb <- function(table=NULL, downloadZip=FALSE, AllTables=FALSE){
    if (isTRUE(downloadZip)) {
        # Try to ping the Chadwick Bureau repository. If that fails to connect, try the backup repo.
        if (isTRUE(baseballDBR::urlExists("https://github.com/chadwickbureau/baseballdatabank/archive/master.zip"))){
            download.file("https://github.com/chadwickbureau/baseballdatabank/archive/master.zip", "master.zip")
        }
        else {
            print(print("Chadwick Bureau failed to connect, trying backup."))
            if (!isTRUE(baseballDBR::urlExists("https://github.com/chadwickbureau/baseballdatabank/archive/master.zip"))){
                download.file("https://github.com/keberwein/baseballdatabank/archive/master.zip", "master.zip")
            }
        }
    }
    if (!is.null(table)) {
        # Try to ping the Chadwick Bureau repository. If that fails to connect, try the backup repo.
        if (isTRUE(baseballDBR::urlExists("https://github.com/chadwickbureau/baseballdatabank/tree/master/core"))){
            baseURL <- "https://raw.githubusercontent.com/chadwickbureau/baseballdatabank/master/core/"
        }
        else {
            print(print("Chadwick Bureau failed to connect, trying backup."))
            if (!isTRUE(baseballDBR::urlExists("https://github.com/chadwickbureau/baseballdatabank/archive/master.zip"))){
                baseURL <- "https://raw.githubusercontent.com/keberwein/baseballdatabank/master/core/"
            }
        }

        urlList <- list()

        for (i in 1:length(table)) {
            urlList[[i]] <- paste0(baseURL, table[i], ".csv")
        }

        list2env(lapply(setNames(urlList, make.names(gsub("*.csv$", "", table))), read.csv, stringsAsFactors=FALSE), envir = .GlobalEnv)
    }

    if (is.null(table) & isTRUE(AllTables)) {
        # Try to ping the Chadwick Bureau repository. If that fails to connect, try the backup repo.
        if (isTRUE(baseballDBR::urlExists("https://github.com/chadwickbureau/baseballdatabank/tree/master/core"))){
            download.file("https://github.com/chadwickbureau/baseballdatabank/archive/master.zip", "master.zip")
        }
        else {
            print(print("Chadwick Bureau failed to connect, trying backup."))
            if (!isTRUE(baseballDBR::urlExists("https://github.com/chadwickbureau/baseballdatabank/archive/master.zip"))){
                download.file("https://github.com/keberwein/baseballdatabank/archive/master.zip", "master.zip")
            }
        }

        unzip("master.zip")
        baseDIR <- "baseballdatabank-master/core/"
        fileList <- list.files(path = baseDIR, pattern = "*.csv")
        urlList <- list()
        for (i in 1:length(fileList)) {
            urlList[[i]] <- paste0(baseDIR, fileList[i])
        }
        list2env(lapply(setNames(urlList, make.names(gsub("*.csv$", "", fileList))), read.csv, stringsAsFactors=FALSE), envir = .GlobalEnv)
        if (!isTRUE(downloadZip)) {
            unlink("master.zip")
        }
        unlink("baseballdatabank-master", recursive=T)
    }
}
