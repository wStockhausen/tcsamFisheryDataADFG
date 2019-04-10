#'
#' @title Get total fishery effort from a csv file
#'
#' @description Function to get total fishery effort from a csv file.
#'
#' @param - the csv file name
#' @param skip - number of lines to skip (default=2)
#'
#' @return a tibble with columns
#' * fishery
#' * area
#' * year
#' * totPots
#'
#' @details Uses \code{readr::read_csv}, \code{stringr::str_sub}, \code{dplyr::mutate},
#' \code{tidyr::gather}, and \code{sqldf::sqldf}.
#'
#' @import magrittr
#'
#' @export
#'
adfg.getTotalFisheryEffort<-function(csv="FishingEffortByFleet.1990-2017.csv",
                                     skip=2){
  tbl <- readr::read_csv(csv,progress=FALSE,skip=skip);
  names(tbl)[1:2]<-c("year","area");
  names(tbl)[3:5]<-stringr::str_sub(names(tbl)[3:5],10,100);
  tbl <- tbl %>% dplyr::mutate(year=as.numeric(stringr::str_sub(year,1,4)));
  tbl <- tbl %>% tidyr::gather(fishery,totPots,Tanner:BBRKC);
  idf<-(tbl$fishery=="Snow")|(tbl$fishery=="BBRKC");
  tbl$area[idf] <- "all EBS";
  tbl$fishery[!idf]<-paste(tbl$fishery[!idf],tbl$area[!idf]);
  ida<-tbl$area=="E";
  tbl$area[ida] <- "East 166W";
  ida<-tbl$area=="W";
  tbl$area[ida] <- "West 166W";
  tbl$fishery <- adfg.ConvertFisheryNames(tbl$fishery);

  qry<-"select fishery,area,year,
          sum(totPots) as totPots
        from tbl
        group by fishery,area,year;";
  tbl<-sqldf::sqldf(qry);

  return(tbl);
}
