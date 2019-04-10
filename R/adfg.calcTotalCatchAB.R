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
#'  * sex
#'  * totPots - number of pots fished (total effort)
#'  * obsPots - number of observed pots (sample effort)
#'  * expFactor - expansion factor
#'  * obsAbund - observed abdundance
#'  * obsWgt_kg - individual weight (kg) calculated using L-W regresssions
#'  * totAbund - estimated total catch abundance (millions)
#'  * totBio - estimated total catch biomass (thousands t)
#'
#' @details Uses \code{sqldf::sqldf}.
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
                                    tblMPD$maturity);
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
            totPots,obsPots,
            (totPots/obsPots) as expFactor
          from tblTotEff t left join tblObsEff o
          on t.fishery=o.fishery and
             t.area = o.area and
             t.year = o.year;";
  tblExpFacs <-sqldf::sqldf(qry);

  #expand to total catch abundance by sex
  qry <- "select
            t.fishery,t.area,t.year,sex,
            totPots,obsPots,expFactor,
            obsAbund, obsWgt_kg,
            (totPots/obsPots)*obsAbund/1000000 as totAbund,
            (totPots/obsPots)*obsWgt_kg/1000000 as totBio
          from tblExpFacs t left join tblObsAB o
          on t.fishery=o.fishery and
             t.area = o.area and
             t.year = o.year;";
  tblTotAB<-sqldf::sqldf(qry);
  return(tblTotAB);
}

