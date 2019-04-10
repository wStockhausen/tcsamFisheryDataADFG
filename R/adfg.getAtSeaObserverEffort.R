#'
#' @title Get at-sea observer effort from a csv file
#'
#' @description Function to get at-sea observer effort from a csv file.
#'
#' @param csv - filename to read
#' @param skip - number of lines to skip (default=4)
#'
#' @return tibble with columns:
#' * fishery
#' * area
#' * year - fishery year
#' * summary pots - number of summary pots sampled
#' * measure pots - number of measure pots
#'
#' @details Read a csv file with at-sea observer effort, including sampled pots with no crab.
#' Uses \code{readr::read_csv}, \code{tibble::add_column}, \code{stringr::str_sub}.
#'
#' @import magrittr
#'
#' @export
#'
adfg.getAtSeaObserverEffort<-function(csv="AtSeaCrabObserverEffort.1990-2018.csv",
                                      skip=4){
  tbl <- readr::read_csv(csv,progress=FALSE,skip=skip);
  names(tbl) <- tolower(names(tbl));
  tbl <- tbl[,c("year","fishery","total pots","measured pots")];
  names(tbl)[3:4] <- c("summary pots","measure pots");
  idE <- tbl$fishery=="Tanner E";
  idW <- tbl$fishery=="Tanner W";
  area<-"all EBS";
  tbl <- tbl %>% tibble::add_column(area);
  tbl$area[idE] <- "East 166W";
  tbl$area[idW] <- "West 166W";
  tbl$fishery <- adfg.ConvertFisheryNames(tbl$fishery);
  tbl$year <- as.numeric(stringr::str_sub(tbl$year,1,4));
  tbl <- tbl[,c("fishery","area","year","summary pots","measure pots")];
  return(tbl);
}

