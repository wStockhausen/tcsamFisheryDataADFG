#'
#' @title Get annual retained catch abundance and biomass by crab fishery from a csv file
#'
#' @description Function to get annual retained catch abundance and biomass by crab fishery from a csv file.
#'
#' @param csv - the csv file name
#' @param skip - number of lines to skip (default=2)
#'
#' @return a tibble with columns
#' * fishery
#' * area
#' * year
#' * sex
#' * shell condition
#' * abundance
#' * biomass (kg)
#'
#' @details Uses \code{readr::read_csv} and \code{stringr::str_sub}.
#'
#' @export
#'
adfg.getRetainedCatchAB<-function(csv,
                                  skip=2){

  tmp<-readr::read_csv(csv,skip=skip,progress=FALSE);
  names(tmp)<-tolower(names(tmp));
  tmp$year<-as.numeric(stringr::str_sub(tmp$`season`,1,4));
  tmp$area<-"all EBS";
  idx<-tmp$fishery=="Tanner E"; tmp$fishery[idx]<-"TCF"; tmp$area[idx] <- "East 166W";
  idx<-tmp$fishery=="Tanner W"; tmp$fishery[idx]<-"TCF"; tmp$area[idx] <- "West 166W";
  idx<-tmp$fishery=="BBRKC";    tmp$fishery[idx]<-"RKF";
  idx<-tmp$fishery=="snow";     tmp$fishery[idx]<-"SCF";
  tmp$sex     <-"male";
  tmp$shellcon<-"all";

  tbl<-tmp[,c("fishery","area","year","sex","shellcon","abundance","biomass (kg)")];
  names(tbl)[5]<-"shell condition";

  return(tbl);
}
