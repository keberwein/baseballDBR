<!-- README.md is generated from README.Rmd. Please edit that file -->
BaseballDBR
===========

[![Build Status](https://travis-ci.org/keberwein/baseballDBR.png?branch=master)](https://travis-ci.org/keberwein/baseballDBR) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/baseballDBR)](http://www.r-pkg.org/badges/version/baseballDBR) [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/concept.svg)](http://www.repostatus.org/#concept)

Install
=======

-   The latest development version from GitHub:

``` r
devtools::install_github("keberwein/baseballDBR")
```

Gathering Data
==============

The `baseballDBR` package requires data that is formatted similar to the [Baseball Databank](https://github.com/chadwickbureau/baseballdatabank) or Sean Lahman's [Baseball Database](http://www.seanlahman.com/baseball-archive/statistics/). The `Lahman` package is great source for these data. The package also contains the `get_bbdb()` function, which allows us to download the most up-to-date tables directly from the Chadwick Bureau's GitHub repository. For example, we can easily load the "Batting" table into our R environment.

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

Adding Basic Metrics
====================

Simple batting metrics can be easily added to any batting data frame. For example, we can add slugging percentage, on-base percentage and on-base plus slugging. Note that OPS and OBP appears as "NA" for the years before IBB was tracked.

``` r
library(baseballDBR)

Batting <- SLG(Batting) %>% OBP() %>% OPS() %>% ISO()
head(Batting, 3)
#>    playerID yearID stint teamID lgID  G  AB  R  H X2B X3B HR RBI SB CS BB
#> 1 abercda01   1871     1    TRO <NA>  1   4  0  0   0   0  0   0  0  0  0
#> 2  addybo01   1871     1    RC1 <NA> 25 118 30 32   6   0  0  13  8  1  4
#> 3 allisar01   1871     1    CL1 <NA> 29 137 28 40   4   5  0  19  3  1  2
#>   SO IBB HBP SH SF GIDP   SLG OBP OPS   ISO
#> 1  0  NA  NA NA NA   NA 0.000  NA  NA 0.000
#> 2  0  NA  NA NA NA   NA 0.322  NA  NA 0.051
#> 3  5  NA  NA NA NA   NA 0.394  NA  NA 0.102
```

Advanced Metrics
================

The package includes a suite of advanced metrics such as wOBA, RAA, and FIP among others. Many of the advanced metrics require multiple tables. For example, the wOBA metric requires the Batting, Pitching, and Fielding tables in order to establish a player's regular defensive position.

``` r
library(baseballDBR)

get_bbdb(table = c("Batting", "Pitching", "Fielding"))

Batting <- wOBA(Batting, Pitching, Fielding, Fangraphs = T)
head(Batting, 3)
#>    playerID yearID stint teamID lgID  G  AB  R  H X2B X3B HR RBI SB CS BB
#> 1 abercda01   1871     1    TRO <NA>  1   4  0  0   0   0  0   0  0  0  0
#> 2  addybo01   1871     1    RC1 <NA> 25 118 30 32   6   0  0  13  8  1  4
#> 3 allisar01   1871     1    CL1 <NA> 29 137 28 40   4   5  0  19  3  1  2
#>   SO IBB HBP SH SF GIDP      wOBA
#> 1  0   0   0 NA  0   NA 0.0000000
#> 2  0   0   0 NA  0   NA 0.2855902
#> 3  5   0   0 NA  0   NA 0.3078849
```

The code above uses [Fangraphs](http://www.fangraphs.com/guts.aspx?type=cn) wOBA values. The default behavior is to uses [Tom Tango's formula](http://www.insidethebook.com/ee/index.php/site/article/woba_year_by_year_calculations/). Other options include `Sep.Leagues`, which may act as a buffer to any bias created by the designated hitter.

``` r
library(baseballDBR)

get_bbdb(table = c("Batting", "Pitching", "Fielding"))

Batting <- wOBA(Batting, Pitching, Fielding, Fangraphs = F, Sep.Leagues = T)
head(Batting, 3)
#>    playerID yearID stint teamID lgID  G  AB  R  H X2B X3B HR RBI SB CS BB
#> 1 abercda01   1871     1    TRO <NA>  1   4  0  0   0   0  0   0  0  0  0
#> 2  addybo01   1871     1    RC1 <NA> 25 118 30 32   6   0  0  13  8  1  4
#> 3 allisar01   1871     1    CL1 <NA> 29 137 28 40   4   5  0  19  3  1  2
#>   SO IBB HBP SH SF GIDP wOBA
#> 1  0   0   0 NA  0   NA   NA
#> 2  0   0   0 NA  0   NA   NA
#> 3  5   0   0 NA  0   NA   NA
```

We can also produce a data frame that only shows the wOBA multipliers. Notice the Fangraphs wOBA multipliers slightly differ from the Tango multipliers.

``` r
library(baseballDBR)

get_bbdb(table = c("Batting", "Pitching", "Fielding"))

fangraphs_woba <- wOBA_values(Batting, Pitching, Fielding, Fangraphs=T)
head(fangraphs_woba, 3)
#>   yearID lg_woba woba_scale   wBB  wHBP   w1B   w2B   w3B   wHR runSB
#> 1   2017   0.318      1.208 0.693 0.723 0.880 1.243 1.569 2.010   0.2
#> 2   2016   0.318      1.212 0.691 0.721 0.878 1.242 1.569 2.015   0.2
#> 3   2015   0.313      1.251 0.687 0.718 0.881 1.256 1.594 2.065   0.2
#>    runCS lg_r_pa lg_r_w  cFIP
#> 1 -0.415   0.120  9.887 3.034
#> 2 -0.410   0.118  9.778 3.147
#> 3 -0.392   0.113  9.421 3.134

tango_woba <- wOBA_values(Batting, Pitching, Fielding, Fangraphs=F)
head(tango_woba, 3)
#> Source: local data frame [3 x 35]
#> Groups: yearID, RperOut, runBB, runHBP, run1B, run2B, run3B, runHR, runSB, runCS [3]
#> 
#> # A tibble: 3 x 35
#>   yearID    AB     R     H   X2B   X3B    HR    SB    CS    BB    SO
#>    <int> <int> <int> <int> <int> <int> <int> <dbl> <dbl> <int> <dbl>
#> 1   1871 23179  5659  6616   950   495   101   948   270   817   371
#> 2   1872 34755  7487 10003  1212   293    88   536   264   477   532
#> 3   1873 40346  8487 11832  1308   472   102   395   253   747   552
#> # ... with 24 more variables: IBB <dbl>, HBP <dbl>, SF <dbl>,
#> #   RperOut <dbl>, runBB <dbl>, runHBP <dbl>, run1B <dbl>, run2B <dbl>,
#> #   run3B <dbl>, runHR <dbl>, runSB <dbl>, runCS <dbl>, runMinus <dbl>,
#> #   runPlus <dbl>, lg_woba <dbl>, woba_scale <dbl>, wBB <dbl>, wHBP <dbl>,
#> #   w1B <dbl>, w2B <dbl>, w3B <dbl>, wHR <dbl>, wSB <dbl>, wCS <dbl>
```

Create Local Database
=====================

A relational database is not needed to work with these data. However, we may want to store the data to be called more quickly at a later time. We can download all of the tables at once with the `get_bbdb()` function and then write them to an empty schema in our favorite database. The example uses a newly created PostgreSQL instance, but other database tools can be used assuming an appropriate R package exists.

``` r
library(baseballDBR)
get_bbdb(AllTables = TRUE)

dbTables <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))

#for (i in 1:length(dbTables)) { dbWriteTable(con, name =  dbTables[i], value = dbTables[i], overwrite=T) }

library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host= "localhost", dbname, user, password)
dbWriteTable(con, "Batting", Batting)
dbDisconnect(con)
rm(con, drv)
```
