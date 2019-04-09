#'
#' @param dfr - data.frame from function \code{adfg.calcZCsFromMPD}
#' @params writeCSV - flag (T/F) to write table to csv file
#'
adfg.calcSSsFromZCs<-function(dfr,
                              writeCSV=FALSE){
  #calculate "final" sample sizes
  qry<-"select year,fishery,area,sex,maturity,`shell condition`,
               sum(count) as ss
        from dfr
        group by year,fishery,area,sex,maturity,`shell condition`
        order by year,fishery,area,sex,maturity,`shell condition`;";
  ss <- sqldf::sqldf(qry);
  if (writeCSV) write.csv(ss,file=ss.selected,row.names=FALSE);
  return(ss);
}
