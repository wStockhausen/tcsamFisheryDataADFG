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
#' * maturity
#' * shell condition
#' * abundance
#' * biomass (kg)
#'
#' @details Uses \code{readr::read_csv} and \code{stringr::str_sub}.
#'
#' @export
#'
adfgRead_RetainedCatchABs<-function(csv,
                                   skip=2){

  tmp<-readr::read_csv(csv,skip=skip,progress=FALSE);
  names(tmp)<-tolower(names(tmp));

  #column names should be:
  expCols<-c("season","fishery","abundance","biomass (lb)","biomass (kg)");
  #check column names
  if (any(names(tmp)!=expCols)){
    idx<-names(tmp)!=expCols;
    str<-paste0("--Error! Input column names \n\t",  paste(names(tmp)[idx],collapse=", "),
                "\nshould match \n\t",               paste(expCols[idx],   collapse=", "));
    stop(str);
  }

  tmp$year<-as.numeric(stringr::str_sub(tmp$`season`,1,4));
  tmp$area<-"all EBS";
  idx<-tmp$fishery=="Tanner E"; tmp$fishery[idx]<-"TCF"; tmp$area[idx] <- "East 166W";
  idx<-tmp$fishery=="Tanner W"; tmp$fishery[idx]<-"TCF"; tmp$area[idx] <- "West 166W";
  idx<-tmp$fishery=="BBRKC";    tmp$fishery[idx]<-"RKF";
  idx<-tmp$fishery=="snow";     tmp$fishery[idx]<-"SCF";
  tmp$sex     <-"male";
  tmp$maturity         <-"undetermined";
  tmp$`shell condition`<-"undetermined";

  tmp1<-tmp[,c("fishery","area","year","sex","maturity","shell condition","abundance","biomass (kg)")];

  #aggregate over area for TCF
  tmp2<-tmp1[tmp1$fishery=="TCF",];
  qry<-"select
          fishery,'all EBS' as area,year,
          sex,maturity,`shell condition`,
          sum(abundance) as abundance,
          sum(`biomass (kg)`) as `biomass (kg)`
        from tmp2
        group by fishery,year,sex,maturity,`shell condition`;";
  tmp3<-sqldf::sqldf(qry);

  #combine tables
  tbl<-rbind(tmp1,tmp3);
  return(tbl);
}
