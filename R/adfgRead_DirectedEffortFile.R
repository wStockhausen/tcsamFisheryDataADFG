#'
#' @title Read the directed fishery effort from a csv file
#'
#' @description Function to read the directed fishery effort from a csv file.
#'
#' @param csv - the csv file name
#' @param skip - number of lines to skip (default=2)
#' @param verbose - flag (T/F) to print diagnostic info
#'
#' @return a tibble with columns
#' \itemize{
#'   \item{fishery - canonical fishery name}
#'   \item{area - fishery area}
#'   \item{year - crab year}
#'   \item{effort - number of potlifts}
#' }
#'
#' @details Uses \code{readr::read_csv}, \code{stringr::str_sub}, \code{dplyr::mutate},
#' \code{tidyr::gather}, and \code{sqldf::sqldf}.
#'
#' The input csv file should have columns
#' \itemize{
#'   \item{fishery year in crab year format (i.e., YYYY/YY)}
#'   \item{area - "E","W", or "all EBS"}
#'   \item{Directed Tanner - effort (number of pots fished)}
#'   \item{Directed Snow   - effort (number of pots fished)}
#'   \item{Directed BBRKC  - effort (number of pots fished)}
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom readr read_csv
#' @importFrom stringr str_sub
#' @importFrom tidyr gather
#' @importFrom utils str
#' @importFrom utils View
#'
#' @export
#'
adfgRead_DirectedEffortFile<-function(csv="FishingEffortByFleet.1990-2017.csv",
                                      skip=2,
                                      verbose=FALSE){
  if (verbose) cat("Starting adfgRead_DirectedEffortFile\n");
  tbl <- readr::read_csv(csv,progress=FALSE,skip=skip);
  names(tbl)[1:2]<-c("year","area");
  names(tbl)[3:5]<-stringr::str_sub(names(tbl)[3:5],10,100); #strip 'Directed' off fishery names
  tbl <- tbl %>% dplyr::mutate(year=as.numeric(stringr::str_sub(year,1,4)));#convert to yyyy
  tbl <- tbl %>% tidyr::gather(fishery,totPots,Tanner:BBRKC);
  #remove entries with NAs for number of potlifts
  idx <- !is.na(tbl$totPots);
  tbl <- tbl[idx,];
  #relabel area for snow crqb and BBRKC fisheries as "all EBS"
  idf<-(tbl$fishery=="Snow")|(tbl$fishery=="BBRKC");
  tbl$area[idf] <- "all EBS";
#  tbl$fishery[!idf]<-paste(tbl$fishery[!idf],tbl$area[!idf]);
  #relabel E/W areas for Tanner crab
  ida<-tbl$area=="E";
  tbl$area[ida] <- "East 166W";
  ida<-tbl$area=="W";
  tbl$area[ida] <- "West 166W";
  #convert to canonical fishery names
  tbl$fishery <- adfgConvert_FisheryNames(tbl$fishery);
  if (verbose) utils::View(tbl);

  qry<-"select fishery,area,year,
          sum(totPots) as effort
        from tbl
        group by fishery,area,year;";
  tbl1<-sqldf::sqldf(qry);

  qry<-"select fishery,year,
          sum(totPots) as effort
        from tbl
        where fishery='TCF'
        group by fishery,year;";
  tbl2<-sqldf::sqldf(qry);
  utils::str(tbl2);
  if (nrow(tbl2)>0) tbl2$area<-"all EBS";

  tbl3<-rbind(tbl1,tbl2);
  if (verbose) View(tbl3);

  if (verbose) cat("Finished adfgRead_DirectedEffortFile\n");
  return(tbl3);
}

# csv<-file.path("~/StockAssessments-Crab/Data/Fishery.ADFG/2019.07/ExtractedCSVFiles",
#               "item1.FishingEffortByFleet.1990-2018plusHistoricalEffort.csv");
# dfrEff<-adfgRead_DirectedEffortFile(fn);

