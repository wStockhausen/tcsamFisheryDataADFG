#'
#' @title parse a 4-character ADFG "fishery" code
#'
#' @description Function to parse 4-character ADFG "fishery" codes.
#'
#' @param f
#'
#' @return dataframe with columns:
#' * fishery code - f
#' * fishery
#' * area
#' * code_year - year corresponding to fishery code (NOT! fishery year, apparently)
#'
#' @details Uses \code{stringr::sub}, \code{wtsUtilities::substituteValues} functions.
#'
#' @export
#'
parseFisheryCode<-function(f){
  char12 <- stringr::str_sub(f,start=1,end=2);
  char34 <- stringr::str_sub(f,start=3,end=4);
  dfr.12 <- rbind(data.frame(orig="CK",target="RKC or BKC", area= "CDQ",        stringsAsFactors=FALSE),
                  data.frame(orig="CO",target="snow crab",  area= "CDQ",        stringsAsFactors=FALSE),
                  data.frame(orig="CR",target="RKC",        area= "CDQ",        stringsAsFactors=FALSE),
                  data.frame(orig="EI",target="Tanner crab",area="UNKNOWN",     stringsAsFactors=FALSE),
                  data.frame(orig="EO",target="snow crab",  area="UNKNOWN",     stringsAsFactors=FALSE),
                  data.frame(orig="QO",target="snow crab",  area= "Bering Sea", stringsAsFactors=FALSE),
                  data.frame(orig="QR",target="RKC",        area= "Bering Sea ",stringsAsFactors=FALSE),
                  data.frame(orig="QT",target="Tanner crab",area= "West",       stringsAsFactors=FALSE),
                  data.frame(orig="TR",target="RKC",        area= "Bristol Bay",stringsAsFactors=FALSE),
                  data.frame(orig="TT",target="Tanner crab",area= "East",       stringsAsFactors=FALSE));

  fishery <- wtsUtilities::substituteValues(char12,
                                            dfr.12$orig,
                                            dfr.12$target);
  area <- wtsUtilities::substituteValues(char12,
                                            dfr.12$orig,
                                            dfr.12$area);
  num34 <- as.numeric(char34);
  year <- ifelse(num34>50,1900+num34,2000+num34);
  return(data.frame(list(fishery_code=f,fishery=fishery,area=area,code_year=year),stringsAsFactors=FALSE));
}
