#'
#' @title Convert ADFG sex codes to character labels
#'
#' @description Function to convert ADFG sex codes to character labels.
#'
#' @param x - vector of sex codes to convert to character labels
#'
#' @return character vector with labels
#'
#' @details
#' * sex codes: 1,2,3,0
#' * labels: "male","female","hermaphrodite","undetermined"
#'
#' @export
#'
adfgConvert_SexCodes<-function(x){
  #unique sex: 1 2 3 0
  sx.codes<-c(1,2,3,0);
  sx.strs <-c("male","female","hermaphrodite","undetermined");
  x <- wtsUtilities::substituteValues(x,orig=sx.codes,finl=sx.strs);
  x[is.na(x)] <- "undetermined";
  return(x);
}




