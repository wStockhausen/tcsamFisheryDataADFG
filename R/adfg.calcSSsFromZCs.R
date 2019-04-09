#'
#' @title Calculate sample sizes for size compositions
#'
#' @description Function to calculate sample sizes for size compositions
#'
#' @param dfr - data.frame from function \code{adfg.calcZCsFromMPD}
#' @param writeCSV - flag (T/F) to write table to csv file
#'
#' @return a dataframe with columns
#' * year
#' * fishery
#' * area
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
  qry<-"select year,fishery,area,sex,maturity,`shell condition`,
               sum(count) as ss
        from dfr
        group by year,fishery,area,sex,maturity,`shell condition`
        order by year,fishery,area,sex,maturity,`shell condition`;";
  ss <- sqldf::sqldf(qry);
  if (writeCSV) write.csv(ss,file=ss.selected,row.names=FALSE);
  return(ss);
}
