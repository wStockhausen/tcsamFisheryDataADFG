#'
#' @title Extract dockside data (DSD) as a tibble from a csv file
#'
#' @description Function to extract dockside data as a tibble from a csv file.
#'
#' @param csv - csv filename with dockside data
#' @param date_format - string ("yyyy-mm-dd" or "mm-dd-yyyy") indicating date format
#'
#' @return a tibble with columns named "fishery","fishery_code","area","year","adfg","sampdate",
#'            "spcode","sex","maturity","shell","size","legal","numcrab"
#'
#' @details Uses functions \code{readr::read_csv}, \code{stringr::str_sub}.
#'
#' @export
#'
adfgRead_DSD<-function(csv="TANNER-1990-2018_dockside.csv",
                       date_format="yyyy-mm-dd"){
  #--read measure pot data file
  dfr <- readr::read_csv(csv);
  #column names should be:
  expCols<-c("fishery","adfg","sampdate","spcode",
             "size","legal","shell","numcrab");
  #check column names
  if (any(names(dfr)!=expCols)){
    idx<-names(dfr)!=expCols;
    str<-paste0("--Error! Input column names \n\t",  paste(names(dfr)[idx],collapse=", "),
                "\nshould match \n\t",               paste(expCols[idx],   collapse=", "));
    stop(str);
  }


  #unique fishery names:
  # CO05	EI90	EI91	EI92
  # QO05o	QO05r	QO06	QO07	QO08	QO09	QO10	QO11	QO12	QO13	QO14	QO15	QO16	QO17	QO18
  # QT05	QT06	QT07	QT08	QT13	QT14	QT15	QT17	QT18	QT93	QT94	QT95	QT96
  # TR06	TR07	TR08	TR09	TR11	TR12	TR13	TR14	TR15
  # TT06	TT07	TT08	TT09	TT13	TT14	TT15

  #unique crab areas (character 1): C (CDQ fisheries), E (???), Q (Bering Sea), T (Bristol Bay)
  #unique targets (character 2): K (red or blue king crab), O (snow crab), R (red king crab), I (Tanner crab), T (Tanner crab)

  #unique area/targets: "CK" (CDQ red or blue king crab), "CO" (CDQ snow crab), "CR" (CDQ red king crab),
  #                     "EI" (?? Tanner crab), "EO" (??? snow crab),
  #                     "QO" (Bering Sea snow crab) "QR" (Bering Sea red king crab) "QT" (Tanner crab West),
  #                     "TR" (BBRKC), "TT" (Tanner crab East)

  #assign sex
  dfr$sex <- "male";

  #assign maturity
  dfr$maturity <- "undetermined";

  #convert shell condition codes to labels
  dfr$shell <- adfgConvert_ShellConditionCodes(dfr$shell);

  #determine fishery year corresponding to sample date
  if (date_format=="yyyy-mm-dd"){
    dfr$year<-adfgConvert_DateYYYYMMDDtoFisheryYear(dfr$sampdate);
  } else if (date_format=="mm-dd-yyyy"){
      dfr$year<-adfgConvert_DateMMDDYYYYtoFisheryYear(dfr$sampdate);
  } else {
    stop("#--ERROR!\n\tUnrecognized date format in adfg.getMPD(...).\n")
  }

  #--parse 4-character fishery codes
  dfr.pf<-adfgConvert_FisheryCodes(dfr$fishery);

  #combine columns and drop some
  dfrp <- cbind(dfr[,2:ncol(dfr)],dfr.pf[,1:3])
  #names(dfrp)
  # [1] "adfg"  "sampdate"  "spcode"  "size"  "legal"  "shell"  "numcrab"  "sex"  "maturity"   "year"  "fishery_code"  "fishery"  "area"
  cols <- c("fishery","fishery_code","area","year","adfg","sampdate",
            "spcode","sex","maturity","shell","size","legal","numcrab");
  dfrp <- dfrp[,cols];
  names(dfrp)[13]<-"count";

  dfrp1 <- dfrp; #select subset

  #assign area designations "all EBS", "East 166W" and "West 166W"
  dfrp1$area <- "all EBS"; #all RKC and snow crab
  #--can't split EI Tanner crab based on statarea (i.e., no EWbySA--see adfgRead_MPD)
  idx <- (stringr::str_sub(dfrp1$fishery_code,1,2)=="EI");
  dfrp1$area[idx] <- "all EBS";#show explicitly
  #--can't split QT Tanner crab prior to 2005 based on statarea (i.e., no EWbySA--see adfgRead_MPD)
  idx <- (stringr::str_sub(dfrp1$fishery_code,1,2)=="QT")&(dfrp1$year<2005);
  dfrp1$area[idx] <- "all EBS";#show explicitly
  #--All Tanner crab in QT after 2004 are West 166W
  idx <- (stringr::str_sub(dfrp1$fishery_code,1,2)=="QT")&(dfrp1$year>2004);
  dfrp1$area[idx] <- "West 166W";
  #--All Tanner crab in TT after 2004 are East 166W
  idx <- (stringr::str_sub(dfrp1$fishery_code,1,2)=="TT")&(dfrp1$year>2004);
  dfrp1$area[idx] <- "East 166W";

  #rename fisheries to canonical forms
  dfrp1$fishery <- adfgConvert_FisheryNames(dfrp1$fishery);

  return(dfrp1);
}

# csv<-file.path("~/StockAssessments-Crab/Data/Fishery.ADFG/2019.07/ObserverDataFiles","TANNER-1990-2018_dockside.csv");
# dfr<-adfgRead_DSD(csv);

