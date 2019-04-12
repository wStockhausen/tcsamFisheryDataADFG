#'
#' @title Calculate expanded annual total catch size compositions by crab fishery, area, year, sex and shell condition
#'
#' @description Function to calculate expanded annual total catch size compositions by crab fishery, area, year, sex and shell condition.
#'
#' @param tblRetAB - dataframe with total catch abundance and biomass by fishery, area, year, sex, an shell condition
#' @param tblTotZCsRaw - "raw" total catch size compositions (counts) by fishery, area, year, sex, shell condition and size
#'
#' @return a datarame with columns
#' * fishery
#' * area
#' * sex
#' * shell condition
#' * year
#' * size
#' * abundance (in thousands of crab)
#'
#' @details Uses \code{sqldf::sqldf} and \code{reshape2::dcast}.
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
  #--calculate abundance by aggregating over shell condition
  mdfrABp<-reshape2::dcast(mdfrAB,fishery+area+year+sex~., fun.aggregate=wtsUtilities::Sum,value.var="abundance");
  names(mdfrABp)[5]<-"abundance";
  #--calculate sample sizes by aggregating over shell condition
  ssByFAYX <-reshape2::dcast(mdfrZCs,fishery+area+year+sex~., fun.aggregate=wtsUtilities::Sum,value.var="count");
  names(ssByFAYX)[5]<-"ss";
  #--normalize by FAYX over SZ
  qry<-"select z.fishery, z.area, z.sex, z.`shell condition`, z.year, z.size,
               z.count/s.ss as p
        from mdfrZCs as z left join ssByFAYX as s
        on
          z.fishery           = s.fishery and
          z.area              = s.area and
          z.sex               = s.sex and
          z.year              = s.year
        order by z.fishery, z.area, z.sex, z.`shell condition`, z.year, z.size;";
  tblRetZCsNormd<-sqldf::sqldf(qry);
  #--scale normalized size comps by abundance by FAYX
  qry<-"select z.fishery, z.area, z.sex, z.`shell condition`, z.year, z.size,
               n.abundance*z.p as abundance
        from tblRetZCsNormd as z left join mdfrABp as n
        on
          z.fishery           = n.fishery and
          z.area              = n.area and
          z.sex               = n.sex and
          z.year              = n.year
        order by z.fishery, z.area, z.sex, z.`shell condition`, z.year, z.size;";
  tmp<-sqldf::sqldf(qry);

  zs<-wtsUtilities::applyCutPts(tmp)

  tblTotZCsByFAYXS

  return(tblTotZCsByFAYXS);
}
