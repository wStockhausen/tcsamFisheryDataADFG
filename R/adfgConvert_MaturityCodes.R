#'
#' @title Convert ADFG measure pot codes to immature/mature/undetermined
#'
#' @description Function to convert ADFG measure pot codes to immature/mature/undetermined.
#'
#' @param sex - vector of sex codes (after conversion to male/female/unknown via \code{adfg.ConvertSexCodes})
#'
#' @return a vector of maturity states (immature/mature/undetermined)
#'
#' @details All male crab have "undetermined" maturity. For now, all females are regarded as
#' 'mature' for purposes of calculating weight (as per B. Daly pers. comm., 2019-04-09.)
#'
#' @export
#'
adfgConvert_MaturityCodes<-function(sex){
  ms  <- rep("undetermined",length.out=length(sex));
  idx <- sex=="female";
  ms[idx] <- "mature"; #Ben Daly assumes all females are mature
  return(ms);
}
