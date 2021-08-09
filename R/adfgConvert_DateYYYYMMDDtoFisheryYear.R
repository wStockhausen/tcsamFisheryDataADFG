#'
#' @title Convert a vector of dates as yyyy-mm-dd to numeric fishery years
#'
#' @description Function to convert a vector of dates as yyyy-mm-dd to numeric fishery years.
#'
#' @param x - vector of date strings in format "yyyy-mm-dd"
#'
#' @return numeric vector of fishery years (e.g., 1990 represents 1990/91)
#'
#' @details The fishery year extends from July 1, yyyy to June 30, yyyy+1,so
#' if mm<7, then year=yyyy-1, otherwise year=yyyy.
#'
#' @export
#'
adfgConvert_DateYYYYMMDDtoFisheryYear<-function(x){
  yr<-as.numeric(substr(x,1,4));
  mn<-as.numeric(substr(x,6,7));
  aj<-ifelse(mn<7,-1,0);
  return(yr+aj);
}

