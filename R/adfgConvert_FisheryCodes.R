#'
#' @title parse a 4-character ADFG "fishery" code
#'
#' @description Function to parse 4-character ADFG "fishery" codes.
#'
#' @param f -  vector of fishery codes to parse
#'
#' @return dataframe with columns:
#' * fishery code - \code{f} (input vector of ADFG codes)
#' * fishery - RKC, snow crab, Tanner crab
#' * area - Bering Sea, Bristol Bay, CDQ, East, West, UNKNOWN
#' * code_year - year corresponding to fishery code (NOT! fishery year, apparently)
#' * char12 - first two characters of fishery code
#'
#' @details Uses \code{stringr::str_sub}, \code{wtsUtilities::substituteValues} functions.
#'
#' @importFrom dplyr bind_rows
#' @importFrom stringr str_sub
#' @importFrom tibble tibble
#' @importFrom wtsUtilities substituteValues
#'
#' @export
#'
adfgConvert_FisheryCodes<-function(f){
  char12 <- stringr::str_sub(f,start=1,end=2);
  char34 <- stringr::str_sub(f,start=3,end=4);
  dfr.12 <- dplyr::bind_rows(
              tibble::tibble(orig="CK",target="RKC or BKC", area= "CDQ"),
              tibble::tibble(orig="CO",target="snow crab",  area= "CDQ"),
              tibble::tibble(orig="CR",target="RKC",        area= "CDQ"),
              tibble::tibble(orig="EI",target="Tanner crab",area= "UNKNOWN"),
              tibble::tibble(orig="EO",target="snow crab",  area= "UNKNOWN"),
              tibble::tibble(orig="QB",target="GKC",        area= "Pribilof Islands"),
              tibble::tibble(orig="QO",target="snow crab",  area= "Bering Sea"),
              tibble::tibble(orig="QP",target="BKC",        area= "St. Matthew Island"),
              tibble::tibble(orig="QR",target="RKC",        area= "Bering Sea "),
              tibble::tibble(orig="QT",target="Tanner crab",area= "West"),
              tibble::tibble(orig="SB",target="GKC",        area= "St. Matthew Island"),
              tibble::tibble(orig="TB",target="GKC",        area= "Bristol Bay"),
              tibble::tibble(orig="TR",target="RKC",        area= "Bristol Bay"),
              tibble::tibble(orig="TT",target="Tanner crab",area= "East"),
              tibble::tibble(orig="XR",target="RKC",        area= "East")
            );

  fishery <- wtsUtilities::substituteValues(char12,
                                            dfr.12$orig,
                                            dfr.12$target);
  area <- wtsUtilities::substituteValues(char12,
                                            dfr.12$orig,
                                            dfr.12$area);
  num34 <- as.numeric(char34);
  year <- ifelse(num34>50,1900+num34,2000+num34);
  return(tibble::tibble(fishery_code=f,fishery=fishery,area=area,code_year=year,char12=char12));
}
