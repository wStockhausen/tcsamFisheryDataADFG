#'
#' @title Calculate the scale factor for size composition input sample sizes
#'
#' @description Function to calculate the scale factor for size composition input sample sizes.
#'
#' @param dfrRC_SSs - dataframe with retained catch sample sizes
#' @param mnYr - minimum year for averaging period (default = 1980)
#' @param mxYr - maximum year for averaging time period (default = 2014)
#'
#' @return scale factor based on average retained crab sample sizes over the averaging period.
#'
#' @details The scale factor is the average number of retained crab measured annually
#' during dockside sampling across a given time period. The default period is used for Tanner crab.
#' The scale factor \code{scale} should be used to determine input sample sizes for size composition data
#' using the formula \cr
#'              inp_ss = min(max_ss,max_ss*(ss/scale)) \cr
#' where max_ss is the maximum allowed input sample size (typically 200) and ss is the original sample size
#' (the total number of crab measured)
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom wtsUtilities Sum
#'
#' @export
#'
adfg.calcScaleForInputSSs<-function(dfrRC_SSs=NULL,
                                    mnYr=1980,
                                    mxYr=2014){
  dfrSS<-dfrRC_SSs %>%
           dplyr::group_by(year) %>%
           dplyr::summarize(ss_tot=wtsUtilities::Sum(ss));
  ss_avg<-(dfrSS %>% subset((mnYr<=year)&(year<=mxYr)) %>% dplyr::group_by() %>% dplyr::summarize(ss=mean(ss_tot)))$ss;
  return(ss_avg);
}
