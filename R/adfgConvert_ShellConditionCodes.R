#'
#' @title Convert ADFG shell condition codes to character labels
#'
#' @description Function to convert ADFG shell condition codes to character labels.
#'
#' @param x - vector of shell condition codes to convert to character labels
#'
#' @return character vector with labels
#'
#' @details
#' * shell condition codes: -9,0,1,2,3,4,5,9
#' * labels: "undetermined","new shell","new shell","new shell","old shell","old shell","old shell","new shell"
#'
#' @export
#'
adfgConvert_ShellConditionCodes<-function(x){
  #unique shell: -9,0,1,2,3,4,5,9
  sc.codes<-c(      -9,           0,         1,          2,         3,           4,          5,           9);
  sc.strs <-c("undetermined","new shell","new shell","new shell","old shell","old shell","old shell","new shell");
  x <- wtsUtilities::substituteValues(x,orig=sc.codes,finl=sc.strs);
  x[is.na(x)] <- "undetermined";
  return(x);
}

