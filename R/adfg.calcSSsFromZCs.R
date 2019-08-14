#'
#' @title Calculate sample sizes for size compositions
#'
#' @description Function to calculate sample sizes for size compositions
#'
#' @param dfr - data.frame from call to function \code{adfg.ExtractRawZCsFromMPD}
#' @param writeCSV - flag (T/F) to write table to csv file
#'
#' @return a dataframe with columns
#' * fishery
#' * area
#' * year
#' * sex
#' * maturity
#' * shell condition
#' * ss - sample size
#'
#' @details Uses \code{sqldf::sqldf}.
#'
#' @export
#'
adfg.calcSSsFromZCs<-function(dfr,
                              writeCSV=FALSE){
  #calculate "final" sample sizes
  qry<-"select fishery,area,year,sex,maturity,`shell condition`,
               sum(count) as ss
        from dfr
        group by fishery,area,year,sex,maturity,`shell condition`
        order by fishery,area,year,sex,maturity,`shell condition`;";
  ss <- sqldf::sqldf(qry);
  if (is.character(writeCSV)) write.csv(ss,file=writeCSV,row.names=FALSE);
  return(ss);
}
