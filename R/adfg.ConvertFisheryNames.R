#'
#' @title Convert ADFG fishery names to canonical assessment names
#'
#' @description Function to convert ADFG fishery names to canonical assessment names.
#'
#' @param x - vector of ADFG fishery names to convert canonical assessment names
#'
#' @return character vector with canonical names
#'
#' @details
#' * original names: "Tanner crab","Tanner E","Tanner W","snow crab","snow","BBRKC","RKC"
#' * final names:    "TCF",        "TCF",     "TCF",     "SCF",      "SCF", "RKF",  "RKF"
#'
#' @export
#'
adfg.ConvertFisheryNames<-function(x){
  #rename fisheries to canonical forms
  orig<-c("Tanner crab","Tanner E","Tanner W","Snow crab","snow crab","Snow","snow","BBRKC","RKC");
  finl<-c("TCF",        "TCF",     "TCF",     "SCF",      "SCF",      "SCF", "SCF" ,"RKF",  "RKF");
  x <- wtsUtilities::substituteValues(x,orig,finl);
  return(x);
}
