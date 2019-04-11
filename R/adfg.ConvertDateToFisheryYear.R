#'
#' @title Convert a vector of dates to numeric fishery years
#'
#' @description Function to convert a vector of dates to numeric fishery years
#'
#' @param v - vector of dates as mm-dd-yyyy strings
#'
#' @return numeric vector of same length as v with corresponding fishery years (yyyy of yyyy/yy+1)
#'
#' @details fishery year extend July 1, yyyy to June 30, yyyy+1
#'
#' @export
#'
adfg.ConvertDateToFisheryYear<-function(v){
  nv<-length(v);
  fy<-vector(mode="numeric",length=nv);
  lst<-strsplit(v,"-",fixed=TRUE);
  for (i in 1:nv){
    mn<-as.numeric(lst[[i]][1]);
    yr<-as.numeric(lst[[i]][3]);
    fy[i]<-yr-1;
    if (mn>6) fy[i]<-yr;
  }
  return(fy);
}
