#'
#' @title Get annual total catch abundance and biomass by crab fishery from a csv file
#'
#' @description Function to get annual total catch abundance and biomass by crab fishery from a csv file.
#'
#' @param csv - the csv file name
#' @param skip - number of lines to skip (default=5)
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
adfg.getTotalCatchAB<-function(csv,
                               skip=5){

  tmp<-readr::read_csv(csv,skip=skip,progress=FALSE);
  tmp<-tmp[tmp$Abundance>=0,]; #REMOVE "-9"s
  names(tmp)<-tolower(names(tmp));
  tmp$year<-as.numeric(stringr::str_sub(tmp$`fishery year`,1,4));
  tmp$area<-"all EBS";
  idx<-tmp$fishery=="Tanner E"; tmp$fishery[idx]<-"TCF"; tmp$area[idx] <- "East 166W";
  idx<-tmp$fishery=="Tanner W"; tmp$fishery[idx]<-"TCF"; tmp$area[idx] <- "West 166W";
  idx<-tmp$fishery=="BBRKC";    tmp$fishery[idx]<-"RKF";
  idx<-tmp$fishery=="snow";     tmp$fishery[idx]<-"SCF";
  tmp$sex     <-tolower(tmp$sex);
  tmp$shellcon<-paste(tolower(tmp$shellcon),"shell");

  tbl<-tmp[,c("fishery","area","year","sex","shellcon","abundance","biomass kg")];
  names(tbl)[5]<-"shell condition";
  names(tbl)[7]<-"biomass (kg)";

  return(tbl);
}
