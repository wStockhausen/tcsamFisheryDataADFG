#'
#' @title Calculate expanded annual total catch size compositions by crab fishery, area, year, sex and shell condition
#'
#' @description Function to calculate expanded annual total catch size compositions by crab fishery, area, year, sex and shell condition.
#'
#' @param tblTotAB - dataframe with total catch abundance and biomass by fishery, area, year, sex, and shell condition
#' @param tblTotZCsRaw -  data.frame from call to function \code{adfg.ReadMPD}
#' @param cutpts - cutpts for bins
#' @param truncate.low - flag to exclude crab with sizes less than minimum cutpt (default=TRUE)
#' @param truncate.high - flag to exclude crab with sizes greater than maximum cutpt (default=FALSE)
#'
#' @return a dataframe with columns
#' * fishery
#' * area
#' * sex
#' * shell condition
#' * year
#' * size
#' * abundance (in thousands of crab)
#'
#' @details Uses \code{sqldf::sqldf} and \code{reshape2::dcast}. tblTotZCsRaw should have columns
#' "fishery", "area", "year", "sex", "maturity", "shell condition", "size" and "count".
#'
#' @importFrom reshape2 dcast
#' @importFrom sqldf sqldf
#'
#' @export
#'
adfg.calcTotalCatchZCs<-function(tblTotAB,
                                 tblTotZCsRaw,
                                 cutpts=seq(from=5,to=185,by=5),
                                 truncate.low=TRUE,
                                 truncate.high=FALSE){
  #--scale total catch size compositions for aggregation
  mdfrAB <-tblTotAB;
  mdfrZCs<-tblTotZCsRaw;
  #--calculate abundance by aggregating over maturity and shell condition
  mdfrABp<-reshape2::dcast(mdfrAB,fishery+area+year+sex~., fun.aggregate=wtsUtilities::Sum,value.var="abundance");
  names(mdfrABp)[5]<-"abundance";
  #--calculate sample sizes by aggregating over maturity and shell condition
  ssByFAYX <-reshape2::dcast(mdfrZCs,fishery+area+year+sex~., fun.aggregate=wtsUtilities::Sum,value.var="count");
  names(ssByFAYX)[5]<-"ss";
  #--normalize by FAYX over SZ
  qry<-"select z.fishery, z.area, z.sex, z.maturity, z.`shell condition`, z.year, z.size,
               z.count/s.ss as p
        from mdfrZCs as z left join ssByFAYX as s
        on
          z.fishery           = s.fishery and
          z.area              = s.area and
          z.sex               = s.sex and
          z.year              = s.year
        order by z.fishery, z.area, z.sex, z.maturity, z.`shell condition`, z.year, z.size;";
  tblTotZCsNormd<-sqldf::sqldf(qry);
  #--scale normalized size comps by abundance by FAYX
  qry<-"select z.fishery, z.area, z.sex, z.maturity, z.`shell condition`, z.year, z.size,
               n.abundance*z.p as abundance
        from tblTotZCsNormd as z left join mdfrABp as n
        on
          z.fishery           = n.fishery and
          z.area              = n.area and
          z.sex               = n.sex and
          z.year              = n.year
        order by z.fishery, z.area, z.sex, z.maturity, z.`shell condition`, z.year, z.size;";
  tblTotZCsByFAYXS<-sqldf::sqldf(qry);

  return(tblTotZCsByFAYXS);
}
