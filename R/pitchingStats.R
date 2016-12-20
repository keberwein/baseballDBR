
#' @title Calculate walks per nine innings
#' @description Find batting average walks per nine innings for pitchers with more one or more inning pitched.
#' Required fields from the Batting table are; "IPouts", and "BB."
#' @param dat A data frame you would wish to calculate. If NULL, it will use the appropriate table from
#' the Lahman package. However, functions will accept custom data frames as well.
#' @keywords BB BB_9 BB9 bb/9
#' @export BB_9
#' @examples
#' \dontrun{
#' pitching_df <- Lahman::Pitching
#' new_df <- BB_9(pitching_df)
#' new_df
#' }
#'
BB_9 <- function (dat=NULL){
    if (is.null(dat)){
        dat <- Lahman::Pitching
    }
    if (any(!isTRUE(c("IPouts", "H") %in% names(dat)))){
        ifelse(dat$IPouts > 2,
               dat$BB_9 <- round((dat$BB*9 / (dat$IPouts / 3)), 3), NA)
    }
    if (any(isTRUE(c("BB", "IPouts") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'BB', 'IBB', 'HBP', 'SF', and 'SH'")
    }
    return(dat)
}



#' @title Calculate the innings pitched
#' @description Find the number of innings a player has ptiched for a season.
#' Required fields from the Batting table are; "IPouts."
#' @param dat A data frame you would wish to calculate. If NULL, it will use the appropriate table from
#' the Lahman package. However, functions will accept custom data frames as well.
#' @keywords innings pitched
#' @export IP
#' @examples
#' \dontrun{
#' pitching_df <- Lahman::Pitching
#' new_df <- LOB_pct(pitching_df)
#' new_df
#' }
#'
IP <- function (dat=NULL){
    if (is.null(dat)){
        dat <- Lahman::Pitching
    }
    if (any(!isTRUE(c("H", "BB", "HBP", "R", "HR") %in% names(dat)))){
        ifelse(dat$IPouts > 2,
               dat$IP <- round(dat$IPouts/3, 3), NA)
    }
    if (any(isTRUE(c("H", "BB", "HBP", "R", "HR") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'BB', 'IBB', 'HBP', 'SF', and 'SH'")
    }
    return(dat)
}


#' @title Calculate the left on base percentage
#' @description Find the percentaqge of base runners that a pitcher leaves on base of the course of a season.
#' Required fields from the Batting table are; "H", "BB", "HBP", "R", and "HR."
#' @param dat A data frame you would wish to calculate. If NULL, it will use the appropriate table from
#' the Lahman package. However, functions will accept custom data frames as well.
#' @keywords LOB_pct LOB LOB%
#' @export LOB_pct
#' @examples
#' \dontrun{
#' pitching_df <- Lahman::Pitching
#' new_df <- LOB_pct(pitching_df)
#' new_df
#' }
#'
LOB_pct <- function (dat=NULL){
    if (is.null(dat)){
        dat <- Lahman::Pitching
    }
    if (any(!isTRUE(c("H", "BB", "HBP", "R", "HR") %in% names(dat)))){
        ifelse(dat$IPouts > 2,
               dat$LOB_pct <- round(
                   (dat$H+dat$BB+dat$HBP-dat$R) / (dat$H+dat$BB+dat$HBP-(1.4*dat$HR))
                   , 3), NA)
    }
    if (any(isTRUE(c("H", "BB", "HBP", "R", "HR") %in% names(dat)))){
        message("Not enough data to calculate. Please make sure your data inclueds 'AB', 'BB', 'IBB', 'HBP', 'SF', and 'SH'")
    }
    return(dat)
}

############ Pitching

# WHIP
# WHIP = ifelse(IP > 0, round((BB+H) / IP, 3), NA)

# K per 9
#K_9 = ifelse(IP > 0, round((SO*9) / IP, 3), NA)

# ERA+ (do research)

# h9
# hr9
# so9





