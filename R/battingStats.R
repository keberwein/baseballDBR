# Metrics not in current Lahman. There is no "pitchingStats table in R Lahman"

# wOBA

#' @title Calculate strikeout percentage
#' @description Find strikeout percentage for batters or pitchers with more than zero at bats or innings pitched.
#' Required fields for batters are; "AB", "SO", "BB", "HBP", "SF", and "SH." Required fields for pitchers are;
#' "SO" and "IPouts"
#' @param dat The data you would wish to calculate.
#' @param position This is a required argument that designates either a "batter" or "pitcher." The argument should
#' be spelled out and within quotes.
#' @keywords Kpct strikeout percentage
#' @import Lahman
#' @export Kpct
#' @examples
#' \dontrun{
#' batting_df <- Lahman::Batting
#' new_df <- Kpct(batting_df, position = "batter")
#' new_df
#' }
#'
Kpct <- function (dat, position){

    if (any(!isTRUE(c("SO", "IPouts") %in% names(dat))) & position == "pitcher"){
        ifelse(dat$IPouts > 0 & dat$SO > 0,
               dat$Kpct <- round(((dat$SO * 9) / (dat$IPouts / 3)), 3), NA)
    }

    if (any(!isTRUE(c("AB", "SO", "BB", "HBP", "SF", "SH") %in% names(dat))) & position == "batter"){
        ifelse(dat$IPouts > 0 & dat$SO > 0,
               dat$Kpct <- round((dat$SO / (dat$AB + dat$BB + dat$HBP + dat$SF + dat$SH)), 3), NA)
    }

    if (position == "batter" & any(isTRUE(c("AB", "SO", "BB", "HBP", "SF", "SH") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'SO', 'BB', 'HBP', 'SF', and 'SH'")
    }

    if (position == "pitcher" & any(isTRUE(c("SO", "IPouts") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'SO' and 'IPouts'")
    }

    return(dat)
}


#' @title Calculate base on ball percentage
#' @description Find base on ball percentage for batters or pitchers with more than zero at bats or innings pitched.
#' Required fields for batters are; "AB", "SO", "BB", "HBP", "SF", and "SH." Required fields for pitchers are;
#' "SO" and "IPouts"
#' @param dat The data you would wish to calculate.
#' @param position This is a required argument that designates either a "batter" or "pitcher." The argument should
#' be spelled out and within quotes.
#' @keywords BBpct base on ball percentage bb
#' @import Lahman
#' @export BBpct
#' @examples
#' \dontrun{
#' batting_df <- Lahman::Batting
#' new_df <- BBpct(batting_df, position = "batter")
#' new_df
#' }
#'
BBpct <- function (dat, position){

    if (any(!isTRUE(c("BB", "IPouts") %in% names(dat))) & position == "pitcher"){
        ifelse(dat$IPouts > 0 & dat$BB > 0,
               dat$BBpct <- round(((dat$BB * 9) / (dat$IPouts / 3)), 3), NA)
    }

    if (any(!isTRUE(c("AB", "BB", "HBP", "SF", "SH") %in% names(dat))) & position == "batter"){
        ifelse(dat$IPouts > 0 & dat$BB > 0,
               dat$BBpct <- round((dat$BB / (dat$AB + dat$BB + dat$HBP + dat$SF + dat$SH)), 3), NA)
    }

    if (position == "batter" & any(isTRUE(c("AB", "BB", "HBP", "SF", "SH") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'BB', 'HBP', 'SF', and 'SH'")
    }

    if (position == "pitcher" & any(isTRUE(c("BB", "IPouts") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'BB' and 'IPouts'")
    }

    return(dat)
}



# hr_pct
# xbh_pct (extra base hit percentage)
# (2b + 3b +hr / pa)
# x_h_pct (extra base per hit)
# (2b + 3b +hr / h)

# Contact Rate
# b$ContactRate = round(((b$AB-b$SO)/b$AB), 3) #Batter contact rate

# Total bases
# h + 2*2b + 3*3b + 4*hr

# RC

# bip_pct (balls in play percentage)
# (ab - so - hr + sf) / pa

# http://www.fangraphs.com/library/offense/wrc/
# wRC (weighted runs created)
# wRC = (((wOBA-League wOBA)/wOBA Scale)+(League R/PA))*PA


# wRC+ this one uses park factors and leauge adjustments.
# wRC+ = (((wRAA/PA + League R/PA) + (League R/PA – Park Factor* League R/PA))/
# (AL or NL wRC/PA excluding pitchers))*100

# RAA (runs above average)

# WAA (wins above average)

# wRAA (weighted runs above average)
# http://www.fangraphs.com/library/offense/wraa/
# wRAA = ((wOBA – league wOBA) / wOBA scale) × PA

# ISO
# iso <- ((X2B + (2 * X3B) + (3 * HR) / AB), 3)

# OPS+
# Baseball refreence has its own method. Try to find a couple.

# OFF (offensive runs above average)
# http://www.fangraphs.com/library/offense/off/
