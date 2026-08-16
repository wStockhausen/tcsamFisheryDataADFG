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
#' If x is a character vector, values are converted to numeric and elements with non-NA values
#' are converted using the above substitutions. The elements that generated NA values
#' (i.e. non-numeric text) are not changed. Elements that can be converted to numeric but
#' do not match one of the sex codes (e.g., -999) are converted to "undetermined".
#'
#' @examples
#' # example code
#' x = c("1","female","-999",NA);
#' adfgConvert_SexCodes(x);
#'
#' @export
#'
adfgConvert_SexCodes<-function(x){
  #unique sex: 1 2 3 0
  idx = which(!is.na(as.numeric(x))); #--in case numeric codes are characters
  if (length(idx)>0){
    sx.codes<-c(1,2,3,0);
    sx.strs <-c("male","female","hermaphrodite","undetermined");
    x[idx] <- wtsUtilities::substituteValues(as.numeric(x[idx]),orig=sx.codes,finl=sx.strs);
    idxp = which(!is.na(as.numeric(x[idx])));
    if (length(idxp)>0) x[idx[idxp]] = "undetermined";
  }
  x[is.na(x)] <- "undetermined";
  return(x);
}




