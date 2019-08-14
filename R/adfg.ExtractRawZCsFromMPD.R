#'
#' @title Get annual total catch size compositions by crab fishery from a csv file
#'
#' @description Function to get annual total catch size compositions by crab fishery from a csv file.
#'
#' @param mpd - dataframe from call to \code{adfg.getMPD}
#'
#' @return a tibble with columns
#' * fishery
#' * area
#' * sex
#' * maturity
#' * shell condition
#' * year
#' * size
#' * count
#'
#' @details Uses \code{readr::read_csv} and \code{stringr::str_sub}.
#'
#' @export
#'
adfg.ExtractRawZCsFromMPD<-function(mpd){
  #--copy dataframe with measure pot data
  dfrp1<-mpd;

  #extract data for size compositions
  Sum <- wtsUtilities::Sum;
  qry<-"select
          fishery,area,year,sex,
          'undetermined' as maturity,
          shell as `shell condition`,
          size,
          Sum(count) as count
        from dfrp1
        group by fishery,area,year,sex,shell,size
        order by fishery,area,year,sex,shell,size;";
  dfrp2<-sqldf::sqldf(qry);

  return(dfrp2);
}
