#'
#' @param dfr - data.frame from function \code{adfg.extractMPD}
#' @params writeCSV - flag (T/F) to write table to csv file
#'
adfg.calcZCsFromMPD<-function(dfr,
                              writeCSV=FALSE){
  #extract data for size compositions
  Sum <- wtsUtilities::Sum;
  qry<-"select
          fishery,area,year,sex,
          'undetermined' as maturity,
          shell as `shell condition`,
          size,
          Sum(count) as count
        from dfr
        group by fishery,area,year,sex,shell,size
        order by fishery,area,year,sex,shell,size;";
  dfrp2<-sqldf::sqldf(qry);
  ymn <- min(dfrp2$year,na.rm=TRUE);
  ymx <- max(dfrp2$year,na.rm=TRUE);
  if (writeCSV) write.csv(dfrp2,file=paste0(fnZCs,".",ymn,"-",ymx,".csv"),row.names=FALSE);
  return(dfrp2);
}
