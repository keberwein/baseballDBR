#' @title Return a table of yearly constants to calculate several advanced offensive statistics.
#' @description This function scrapes data from the the guts table found on fangraphs.com.
#' These data include annual wOBA and FIPS constants. Constants include: woba, wOBA, woba scale, wbb, 
#' whbp, w2b, w3b, 2hr, runsb, runcs, r/pa, r/w, and cfip.
#' @keywords woba, wOBA, woba scale, wbb, whbp, w2b, w3b, 2hr, runsb, runcs, r/pa, r/w, cfip
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @export get_guts
#' @seealso \url{http://www.fangraphs.com/guts.aspx/}
#' @examples
#' 
#' \dontrun{
#' }
get_guts <- function(){
    url <- xml2::read_html("http://www.fangraphs.com/guts.aspx")
    guts <- url %>%
        rvest::html_nodes(xpath='//*[@id="GutsBoard1_dg1_ctl00"]') %>%
        rvest::html_table()
    guts <- guts[[1]]
}


# Park Factors
# TODO build url by year
base_url <- "http://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season="
year = 2015

base_url <- paste0(base_url, year)
url <- xml2::read_html(base_url)
pf <- url %>%
    rvest::html_nodes(xpath='//*[@id="GutsBoard1_dg1_ctl00"]') %>%
    rvest::html_table()
pf <- pf[[1]]


# Park Factors by handiness
# TODO build url by year
base_url <- "http://www.fangraphs.com/guts.aspx?type=pfh&teamid=0&season="
year = 2015

base_url <- paste0(base_url, year)
url <- xml2::read_html(base_url)
pfh <- url %>%
    rvest::html_nodes(xpath='//*[@id="GutsBoard1_dg1_ctl00"]') %>%
    rvest::html_table()
pfh <- pfh[[1]]