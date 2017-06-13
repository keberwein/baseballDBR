BaseballDBR
===========

[![Build Status](https://travis-ci.org/keberwein/baseballDBR.png?branch=master)](https://travis-ci.org/keberwein/baseballDBR) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/baseballDBR)](http://www.r-pkg.org/badges/version/baseballDBR) [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

Install
=======

-   The latest development version from GitHub:

``` r
devtools::install_github("keberwein/baseballDBR")
```

Gathering Data
==============

The `baseballDBR` package requires data that is formatted similar to the [Baseball Databank](https://github.com/chadwickbureau/baseballdatabank) or Sean Lahman's [Baseball Database](http://www.seanlahman.com/baseball-archive/statistics/). The package also contains the `get_bbdb()` function, which allows us to download the most up-to-date tables directly from the Chadwick Bureau's GitHub repository. For example, we can easily load the "Batting" table into our R environment.

``` r
library(baseballDBR)

get_bbdb(table = "Batting")
head(Batting)
#>    playerID yearID stint teamID lgID  G  AB  R  H X2B X3B HR RBI SB CS BB
#> 1 abercda01   1871     1    TRO <NA>  1   4  0  0   0   0  0   0  0  0  0
#> 2  addybo01   1871     1    RC1 <NA> 25 118 30 32   6   0  0  13  8  1  4
#> 3 allisar01   1871     1    CL1 <NA> 29 137 28 40   4   5  0  19  3  1  2
#> 4 allisdo01   1871     1    WS3 <NA> 27 133 28 44  10   2  2  27  1  1  0
#> 5 ansonca01   1871     1    RC1 <NA> 25 120 29 39  11   3  0  16  6  2  2
#> 6 armstbo01   1871     1    FW1 <NA> 12  49  9 11   2   1  0   5  0  1  0
#>   SO IBB HBP SH SF GIDP
#> 1  0  NA  NA NA NA   NA
#> 2  0  NA  NA NA NA   NA
#> 3  5  NA  NA NA NA   NA
#> 4  2  NA  NA NA NA   NA
#> 5  1  NA  NA NA NA   NA
#> 6  1  NA  NA NA NA   NA
```

### Use with the Lahman Package

``` r
library(Lahman)
library(baseballDBR)

Batting <- Lahman::Batting
head(Batting)
#>    playerID yearID stint teamID lgID  G  AB  R  H X2B X3B HR RBI SB CS BB
#> 1 abercda01   1871     1    TRO   NA  1   4  0  0   0   0  0   0  0  0  0
#> 2  addybo01   1871     1    RC1   NA 25 118 30 32   6   0  0  13  8  1  4
#> 3 allisar01   1871     1    CL1   NA 29 137 28 40   4   5  0  19  3  1  2
#> 4 allisdo01   1871     1    WS3   NA 27 133 28 44  10   2  2  27  1  1  0
#> 5 ansonca01   1871     1    RC1   NA 25 120 29 39  11   3  0  16  6  2  2
#> 6 armstbo01   1871     1    FW1   NA 12  49  9 11   2   1  0   5  0  1  0
#>   SO IBB HBP SH SF GIDP
#> 1  0  NA  NA NA NA   NA
#> 2  0  NA  NA NA NA   NA
#> 3  5  NA  NA NA NA   NA
#> 4  2  NA  NA NA NA   NA
#> 5  1  NA  NA NA NA   NA
#> 6  1  NA  NA NA NA   NA
```

Adding Basic Metrics
====================

Simple batting metrics can be easily added to any batting data frame. For example, we can add slugging percentage, on-base percentage and on-base plus slugging. Note that OPS and OBP appears as "NA" for the years before IBB was tracked.

``` r
library(baseballDBR)

Batting$SLG <- SLG(Batting)

Batting$OBP <- OBP(Batting)

head(Batting, 3)
#>    playerID yearID stint teamID lgID  G  AB  R  H X2B X3B HR RBI SB CS BB
#> 1 abercda01   1871     1    TRO   NA  1   4  0  0   0   0  0   0  0  0  0
#> 2  addybo01   1871     1    RC1   NA 25 118 30 32   6   0  0  13  8  1  4
#> 3 allisar01   1871     1    CL1   NA 29 137 28 40   4   5  0  19  3  1  2
#>   SO IBB HBP SH SF GIDP   SLG OBP
#> 1  0  NA  NA NA NA   NA 0.000  NA
#> 2  0  NA  NA NA NA   NA 0.322  NA
#> 3  5  NA  NA NA NA   NA 0.394  NA
```

Advanced Metrics
================

The package includes a suite of advanced metrics such as wOBA, RAA, and FIP, among others. Many of the advanced metrics require multiple tables. For example, the wOBA metric requires the Batting, Pitching, and Fielding tables in order to establish a player's regular defensive position.

``` r
library(baseballDBR)

get_bbdb(table = c("Batting", "Pitching", "Fielding"))

Batting$wOBA <- wOBA(Batting, Pitching, Fielding, Fangraphs = T)
head(Batting, 3)
#>    playerID yearID stint teamID lgID  G  AB  R  H X2B X3B HR RBI SB CS BB
#> 1 abercda01   1871     1    TRO <NA>  1   4  0  0   0   0  0   0  0  0  0
#> 2  addybo01   1871     1    RC1 <NA> 25 118 30 32   6   0  0  13  8  1  4
#> 3 allisar01   1871     1    CL1 <NA> 29 137 28 40   4   5  0  19  3  1  2
#>   SO IBB HBP SH SF GIDP      wOBA
#> 1  0  NA  NA NA NA   NA 0.0000000
#> 2  0  NA  NA NA NA   NA 0.2855902
#> 3  5  NA  NA NA NA   NA 0.3078849
```

The code above uses [Fangraphs](http://www.fangraphs.com/guts.aspx?type=cn) wOBA values. The default behavior is to uses Tom Tango's adapted [SQL formula](http://www.insidethebook.com/ee/index.php/site/article/woba_year_by_year_calculations/). Other options include `Sep.Leagues`, which may act as a buffer to any bias created by the designated hitter.

``` r
library(baseballDBR)

get_bbdb(table = c("Batting", "Pitching", "Fielding"))

Batting$wOBA <- wOBA(Batting, Pitching, Fielding, Fangraphs = F, Sep.Leagues = T)
head(Batting, 3)
#>    playerID yearID stint teamID lgID  G  AB  R  H X2B X3B HR RBI SB CS BB
#> 1 abercda01   1871     1    TRO <NA>  1   4  0  0   0   0  0   0  0  0  0
#> 2  addybo01   1871     1    RC1 <NA> 25 118 30 32   6   0  0  13  8  1  4
#> 3 allisar01   1871     1    CL1 <NA> 29 137 28 40   4   5  0  19  3  1  2
#>   SO IBB HBP SH SF GIDP wOBA
#> 1  0  NA  NA NA NA   NA   NA
#> 2  0  NA  NA NA NA   NA   NA
#> 3  5  NA  NA NA NA   NA   NA
```

We can also produce a data frame that only shows the wOBA multipliers. Notice the Fangraphs wOBA multipliers slightly differ from the Tango multipliers.

``` r
library(baseballDBR)

get_bbdb(table = c("Batting", "Pitching", "Fielding"))

fangraphs_woba <- wOBA_values(Batting, Pitching, Fielding, Fangraphs=T)
head(fangraphs_woba, 3)
#>   yearID lg_woba woba_scale   wBB  wHBP   w1B   w2B   w3B   wHR runSB
#> 1   2017   0.319      1.197 0.692 0.722 0.878 1.237 1.560 1.994   0.2
#> 2   2016   0.318      1.212 0.691 0.721 0.878 1.242 1.569 2.015   0.2
#> 3   2015   0.313      1.251 0.687 0.718 0.881 1.256 1.594 2.065   0.2
#>    runCS lg_r_pa lg_r_w  cFIP
#> 1 -0.419   0.121  9.967 3.109
#> 2 -0.410   0.118  9.778 3.147
#> 3 -0.392   0.113  9.421 3.134

tango_woba <- wOBA_values(Batting, Pitching, Fielding, Fangraphs=F)
head(tango_woba, 3)
#> # A tibble: 3 x 35
#> # Groups:   yearID, RperOut, runBB, runHBP, run1B, run2B, run3B, runHR,
#> #   runSB, runCS [3]
#>   yearID    AB     R     H   X2B   X3B    HR    SB    CS    BB    SO   IBB
#>    <int> <int> <int> <int> <int> <int> <int> <dbl> <dbl> <int> <dbl> <dbl>
#> 1   1871 23179  5659  6616   950   495   101   948   270   817   371     0
#> 2   1872 34755  7487 10003  1212   293    88   536   264   477   532     0
#> 3   1873 40346  8487 11832  1308   472   102   395   253   747   552     0
#> # ... with 23 more variables: HBP <dbl>, SF <dbl>, RperOut <dbl>,
#> #   runBB <dbl>, runHBP <dbl>, run1B <dbl>, run2B <dbl>, run3B <dbl>,
#> #   runHR <dbl>, runSB <dbl>, runCS <dbl>, runMinus <dbl>, runPlus <dbl>,
#> #   lg_woba <dbl>, woba_scale <dbl>, wBB <dbl>, wHBP <dbl>, w1B <dbl>,
#> #   w2B <dbl>, w3B <dbl>, wHR <dbl>, wSB <dbl>, wCS <dbl>
```

Create Local Database
=====================

A relational database is not needed to work with these data. However, we may want to store the data to be called more quickly at a later time. We can download all of the tables at once with the `get_bbdb()` function and then write them to an empty schema in our favorite database. The example uses a newly created PostgreSQL instance, but other database tools can be used assuming an appropriate R package exists.

``` r
library(baseballDBR)
library(RPostgreSQL)

# Load all tables into the Global Environment.
get_bbdb(AllTables = TRUE)

# Make a list of all data frames.
dbTables <- names(Filter(isTRUE, eapply(.GlobalEnv, is.data.frame)))

# Load data base drivers and load all data frames in a loop.
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host= "localhost", dbname= "lahman", user= "YOUR_USERNAME", password = "YOUR_PASSWORD")

for (i in 1:length(dbTables)) { 
    if (dbExistsTable(con, dbTables[i])) {
        dbRemoveTable(dbTables[i])
    }
    dbWriteTable(con, name =  dbTables[i], value = get0(dbTables[i])) 
}

# Disconnect from database.
dbDisconnect(con)
rm(con, drv)
```
