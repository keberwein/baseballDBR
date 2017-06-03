# Check external urls to make sure they have not changed.

library(testthat)

# Set up function to do a tryCatch on URL.
urlExists <- function(target) {
    tryCatch({
        con <- url(target)
        a  <- capture.output(suppressWarnings(readLines(con)))
        close(con)
        TRUE;
    },
    error = function(err) {
        occur <- grep("cannot open the connection", capture.output(err));
        if(length(occur) > 0) FALSE;
    }
    )
}

# Check Chadwick Bureau Git repo.
testthat::expect_true(urlExists("https://raw.githubusercontent.com/chadwickbureau/baseballdatabank/master/core/AllstarFull.csv"))
# Check my personal fork, backup.
testthat::expect_true(urlExists("https://raw.githubusercontent.com/keberwein/baseballdatabank/master/core/AllstarFull.csv"))
# Check Fangraph guts page.
testthat::expect_true(urlExists("http://www.fangraphs.com/guts.aspx?type=cn"))


