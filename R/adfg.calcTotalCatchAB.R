#'
#' @title Calculate estimated total catch abundance and biomass by expanding observed abundance
#'
#' @description Function to calculate estimated total catch abundance by expanding observed abundance.
#'
#' @param tblMPD - from call to \code{adfg.extractMPD}
#' @param tblObsEff - from call to \code{adfg.getAtSeaObserverEffort} or \code{adfg.calcAtSeaObserverEffort}
#' @param tblTotEff - from call to \code{adfg.getTotalFisheryEffort}
#'
#' @return tibble with columns
#'  * fishery
#'  * area
#'  * year
#'  * potlifts - number of pots fished (total effort)
#'  * measure pots - number of observed pots (sample effort)
#'  * expFactor - expansion factor
#'  * sex - "male", "female", "undetermined", "hermaphrodite", or NA
#'  * variable - "count", "weight", "abundance", or "biomass"
#'  * value - value of asociated variable
#'  * type - "observed" or "expanded"
#'  * units - "ones", "thousands", "kg" or "t"
#'
#' @details Uses \code{sqldf::sqldf}. Units for 'weight' are kg, for 'abundance' are thousands, and for 'biomass' are t.
#'
#' @export
#'
adfg.calcTotalCatchAB<-function(tblMPD,
                                tblObsEff,
                                tblTotEff){
  #assign maturity based on sex and other classifications
  maturity<-adfg.ConvertMaturityCodes(tblMPD$sex);
  tblMPD$maturity<-maturity;

  #calculate individual weights based on size
  wgt_kg<-tcsamFunctions::calc.WatZ(tblMPD$size,
                                    tblMPD$sex,
                                    tblMPD$maturity)/1000;#function result in g
  tblMPD$wgt_kg  <- wgt_kg;

  #calculate observed catch abundance by sex and size
  qry <- "select fishery,area,year,sex,
            sum(count)  as obsAbund,
            sum(count*wgt_kg) as obsWgt_kg
          from tblMPD
          group by fishery,area,year,sex;";
  tblObsAB <-sqldf::sqldf(qry);

  #calculate expansion factors
  qry <- "select t.fishery,t.area,t.year,
            potlifts,`measure pots`,
            (potlifts/`measure pots`) as expFactor
          from tblTotEff t left join tblObsEff o
          on t.fishery=o.fishery and
             t.area = o.area and
             t.year = o.year;";
  tblExpFacs <-sqldf::sqldf(qry);

  #expand to total catch abundance by sex
  qry <- "select
            t.fishery,t.area,t.year,
            potlifts,`measure pots`,
            expFactor,sex,
            obsAbund, obsWgt_kg,
            expFactor*obsAbund/1000  as totAbund,
            expFactor*obsWgt_kg/1000 as totBio
          from tblExpFacs t left join tblObsAB o
          on t.fishery=o.fishery and
             t.area = o.area and
             t.year = o.year;";
  tmp<-sqldf::sqldf(qry);

  tmp1<-tmp1<-reshape2::melt(tmp,id.vars=c("fishery","area","year","potlifts","measure pots","expFactor","sex"));
  tmp1$variable<-as.character(tmp1$variable);
  tmp1$type<-"observed";
  tmp1$units<-"counts";
  #----
  idx<-tmp1$variable=="obsAbund";
  tmp1$type[idx]    <-"observed";
  tmp1$units[idx]   <-"--";
  tmp1$variable[idx]<-"count";
  #----
  idx<-tmp1$variable=="obsWgt_kg";
  tmp1$type[idx]    <-"observed";
  tmp1$units[idx]   <-"kg";
  tmp1$variable[idx]<-"weight";
  #----
  idx<-tmp1$variable=="totAbund";
  tmp1$type[idx]    <-"expanded";
  tmp1$units[idx]   <-"thousands";
  tmp1$variable[idx]<-"abundance";
  #----
  idx<-tmp1$variable=="totBio";
  tmp1$type[idx]    <-"expanded";
  tmp1$units[idx]   <-"t";
  tmp1$variable[idx]<-"biomass";

  return(tmp1);
}

