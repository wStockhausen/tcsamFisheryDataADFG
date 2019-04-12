#'
#' @title Get annual retained catch size compositions by crab fishery from a csv file
#'
#' @description Function to get annual retained catch size compositions by crab fishery from a csv file.
#'
#' @param csv - the csv file name
#' @param skip - number of lines to skip (default=5)
#' @param progress - flag to show progress reading csv file
#'
#' @return a tibble with columns
#' * fishery
#' * area
#' * sex
#' * shell condition
#' * year
#' * size
#' * count
#'
#' @details Uses \code{readr::read_csv} and \code{stringr::str_sub}.
#'
#' @export
#'
adfg.getRetainedCatchZCs<-function(csv,skip=2,progress=FALSE){
  #--read csv
  tmp0<-readr::read_csv(csv,skip=skip,progress=progress);
  names(tmp0)<-tolower(names(tmp0));
  #extract fishery year
  tmp0$year<-as.integer(stringr::str_sub(tmp0$`fishery year`,1,4));
  #--revise fishery names and areas
  tmp0$area<-"all EBS";
  idx<-tmp0$fishery=="BBRKC"; tmp0$fishery[idx]<-"RKF";
  idx<-tmp0$fishery=="snow";  tmp0$fishery[idx]<-"SCF";
  idx<-tmp0$fishery=="Tanner E";  tmp0$fishery[idx]<-"TCF"; tmp0$area[idx]<-"East 166W";
  idx<-tmp0$fishery=="Tanner W";  tmp0$fishery[idx]<-"TCF"; tmp0$area[idx]<-"West 166W";
  tmp0$sex<-"male";
  #return(tmp0);
  #--revise shell condition categories
  tmp1<-reshape2::melt(tmp0,id.vars=c("year","fishery","area","sex","cw"),
                       measure.vars=c("new-shell","old-shell"),
                       value.name="frequency",factorsAsString=TRUE);
  tmp1$variable<-as.character(tmp1$variable);
  names(tmp1)[5]<-"size";

  #--revise shell condition categories
  idx<-tmp1$variable=="new-shell";  tmp1$variable[idx]<-"new shell";
  idx<-tmp1$variable=="old-shell";  tmp1$variable[idx]<-"old shell";
  names(tmp1)[6]<-"shell condition";
  names(tmp1)[7]<-"count";

  #--revise column order
  tbl<-tmp1[,c("fishery","area","sex","shell condition","year","size","count")];
  return(tbl);
}
