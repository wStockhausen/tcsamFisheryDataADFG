#'
#' @title Get annual total catch size compositions by crab fishery from a csv file
#'
#' @description Function to get annual total catch size compositions by crab fishery from a csv file.
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
#' @details The input csv file should be one provided by ADFG.
#' Uses \code{readr::read_csv} and \code{stringr::str_sub}.
#'
#' @importFrom readr read_csv
#' @importFrom stringr str_sub
#'
#' @export
#'
adfg.getTotalCatchZCs<-function(csv,skip=2,progress=FALSE){
  #--read csv
  tmp0<-readr::read_csv(csv,skip=skip,progress=progress);
  names(tmp0)<-tolower(names(tmp0));
  #extract fishery year
  tmp0$year<-as.integer(stringr::str_sub(tmp0$year,1,4));
  #--revise fishery names and areas
  tmp0$area<-"all EBS";
  idx<-tmp0$fishery=="BBRKC"; tmp0$fishery[idx]<-"RKF";
  idx<-tmp0$fishery=="snow";  tmp0$fishery[idx]<-"SCF";
  idx<-tmp0$fishery=="Tanner E";  tmp0$fishery[idx]<-"TCF"; tmp0$area[idx]<-"East 166W";
  idx<-tmp0$fishery=="Tanner W";  tmp0$fishery[idx]<-"TCF"; tmp0$area[idx]<-"West 166W";
  tmp0$sex<-tolower(tmp0$sex);
  #--revise observer categories
  idx<-tmp0$`obs cat`=="Female";    tmp0$`obs cat`[idx]<-"female";
  idx<-tmp0$`obs cat`=="Legal-NR";  tmp0$`obs cat`[idx]<-"LNR";
  idx<-tmp0$`obs cat`=="Legal-Ret"; tmp0$`obs cat`[idx]<-"LR";
  idx<-tmp0$`obs cat`=="Sublegal";  tmp0$`obs cat`[idx]<-"sublegal";
  #--revise shell condition categories
  tmp1<-reshape2::melt(tmp0,id.vars=c("year","fishery","area","sex","obs cat","size"),
                       measure.vars=c("soft","pliable","new","old","very old"),
                       value.name="frequency",factorsAsString=TRUE);
  tmp1$variable<-as.character(tmp1$variable);

  #--revise shell condition categories
  idx<-tmp1$variable=="soft";     tmp1$variable[idx]<-"new shell";
  idx<-tmp1$variable=="pliable";  tmp1$variable[idx]<-"new shell";
  idx<-tmp1$variable=="new";      tmp1$variable[idx]<-"new shell";
  idx<-tmp1$variable=="old";      tmp1$variable[idx]<-"old shell";
  idx<-tmp1$variable=="very old"; tmp1$variable[idx]<-"old shell";
  names(tmp1)[7]<-"shell condition";
  #--sum over observer categorizations
  tmp1a<-reshape2::dcast(tmp1,fishery+area+sex+`shell condition`+year+size~.,
                        value.var="frequency",fun.aggregate=wtsUtilities::Sum);
  names(tmp1a)[7]<-"count";

  return(tmp1a);
}
