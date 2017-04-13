<!-- README.md is generated from README.Rmd. Please edit that file -->
BaseballDBR
===========

[![Build Status](https://travis-ci.org/keberwein/baseballDBR.png?branch=master)](https://travis-ci.org/keberwein/baseballDBR) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/baseballDBR)](http://www.r-pkg.org/badges/version/baseballDBR) [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/concept.svg)](http://www.repostatus.org/#concept)

### This is a concept to apply advanced metrics to the baseball databank (the Lahman database.) It is currently under devlopmment. Additions will be frequent until it is submitted to CRAN.

Install
=======

-   The latest development version from GitHub:

``` r
devtools::install_github("keberwein/baseballDBR")
```

Gathering Data
==============

The `baseballDBR` package requires data that is formatted similar to teh [Baseball Databank](https://github.com/chadwickbureau/baseballdatabank) or Sean Lahman's [Baseball Database](http://www.seanlahman.com/baseball-archive/statistics/). The `Lahman` package is great source for these data. The package also contains the `get_bbdb()` function, which allows us to download the most up-to-date tables directly from the Chadwick Bureau's GitHub repository. For example, we can easily load the "Batting" table into our R enviornment.

``` r
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
