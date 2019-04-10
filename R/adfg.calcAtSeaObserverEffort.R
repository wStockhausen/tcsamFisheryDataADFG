#'
#' @title Calculate at-sea observer sampling effort
#'
#' @description Funcrion to calculate at-sea observer sampling effort.
#'
#' @param tbl - measure pot data table from \code{adfg.extractMPD}
#'
#' @details NOTE: MPD appears not to include sampled pots that DID NOT have Tanner crab.
#' As a consequence, the observer effort calculated here DOES NOT INCLUDE sampled pots with
#' no Tanner crab in them. Uses \code{sqldf::sqldf}.
#'
#' @return dataframe with columns
#' * fishery
#' * area
#' * year
#' * numpots
#'
#' @export
#'
adfg.calcAtSeaObserverEffort<-function(tbl){
  #select unique sampled pots
  qry <- "select distinct
            fishery, area, year, trip, adfg, sampdate, spn
          from tbl;";
  tblUPs <- sqldf::sqldf(qry);

  #calculate number of sampled pots
  qry <- "select
            fishery,area,year,
            count(*) as numpots
          from tblUPs
          group by fishery,area,year;";
  tblEff <- sqldf::sqldf(qry);

  return(tblEff);
}
