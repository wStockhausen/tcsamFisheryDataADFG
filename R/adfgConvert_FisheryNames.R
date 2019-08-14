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
#' * original names: "Tanner crab","Tanner E","Tanner W","Tanner","snow crab","snow","BBRKC","RKC"
#' * final names:    "TCF",        "TCF",     "TCF",     "TCF",   "SCF",      "SCF", "RKF",  "RKF"
#'
#' @export
#'
adfgConvert_FisheryNames<-function(x){
  #rename fisheries to canonical forms
  orig<-c("Tanner crab","Tanner E","Tanner W","Tanner","Snow crab","snow crab","Snow","snow","BBRKC","RKC");
  finl<-c("TCF",        "TCF",     "TCF",     "TCF",   "SCF",      "SCF",      "SCF", "SCF" ,"RKF",  "RKF");
  x <- wtsUtilities::substituteValues(x,orig,finl);
  return(x);
}
