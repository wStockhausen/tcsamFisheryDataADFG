#'
#' @title Calculate size compositions from measure pot data
#'
#' @description Function to calculate size compositions from measure pot data.
#'
#' @param dfr   - data.frame from function \code{adfgRead_MPD}
#' @param fn - name of file to write csv to (or NULL not to write)
#'
#' @return a dataframe with columns
#' * year
#' * fishery
#' * area
#' * sex
#' * maturity
#' * shell condition
#' * size
#' * count
#'
#' @details Uses \code{sqldf::sqldf} and \code{wtsUtilities::Sum}.
#'
#' @export
#'
adfgZCs_CalcTotZCsFromMPD<-function(dfr,
                                    fn=NULL){
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
  if (!is.null(fn)) write.csv(dfrp2,file=fn,row.names=FALSE);
  return(dfrp2);
}
