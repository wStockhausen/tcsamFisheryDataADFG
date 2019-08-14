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
#' @details Uses \code{readr::read_csv}, \code{stringr::str_sub}, and \code{sqldf::sqldf}.
#'
#' @export
#'
adfgRead_TotalCatchABs<-function(csv,
                                 skip=5){

  tmp<-readr::read_csv(csv,skip=skip,progress=FALSE);
  names(tmp)<-tolower(names(tmp));

  #column names should be:
  expCols<-c("fishery year","fishery","sex",	"shellcon",	"abundance","biomass kg");
  #check column names
  if (any(names(tmp)!=expCols)){
    idx<-names(tmp)!=expCols;
    str<-paste0("--Error! Input column names \n\t",  paste(names(tmp)[idx],collapse=", "),
                "\nshould match \n\t",               paste(expCols[idx],   collapse=", "));
    stop(str);
  }

  tmp<-tmp[tmp$abundance>=0,]; #REMOVE "-9"s
  tmp$year<-as.numeric(stringr::str_sub(tmp$`fishery year`,1,4));
  tmp$area<-"all EBS";
  idx<-tmp$fishery=="Tanner E"; tmp$fishery[idx]<-"TCF"; tmp$area[idx] <- "East 166W";
  idx<-tmp$fishery=="Tanner W"; tmp$fishery[idx]<-"TCF"; tmp$area[idx] <- "West 166W";
  idx<-tmp$fishery=="BBRKC";    tmp$fishery[idx]<-"RKF";
  idx<-tmp$fishery=="snow";     tmp$fishery[idx]<-"SCF";
  tmp$sex     <-tolower(tmp$sex);
  tmp$shellcon<-paste(tolower(tmp$shellcon),"shell");
  tmp$maturity<-"undetermined";

  tmp1<-tmp[,c("fishery","area","year","sex","maturity","shellcon","abundance","biomass kg")];
  names(tmp1)[6]<-"shell condition";
  names(tmp1)[8]<-"biomass (kg)";

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
