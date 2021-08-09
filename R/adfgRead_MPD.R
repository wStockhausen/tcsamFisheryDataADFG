#'
#' @title Extract measure pot data as a tibble from a csv file
#'
#' @description Function to extract measure pot data as a tibble from a csv file.
#'
#' @param csv - csv filename with measure pot data
#' @param date_format - string ("yyyy-mm-dd" or "mm-dd-yyyy") indicating date format
#'
#' @return a tibble with columns "fishery","area","EWbySA","EWbyLon","year","fishery_code",
#' "code_year","trip","adfg","sampdate","spn","statarea","mi_lon","mi_lat",
#' "spcode","sex","shell","size","legal","count"
#'
#' @details Uses functions \code{readr::read_csv}, \code{stringr::str_sub}.
#'
#' @note The 'year' values are 'crab year' based on the sample date. The 'code_year' is the
#' ADFG fishery year based on the fishery code. Prior to rationalization, there may be differences
#' in these two values.
#'
#' @note 'count' is the number of measured crab associated with the rest of the row categories.
#' Unlike the dockside data, this  is 1 for all rows (i.e., one row for each measured crab).
#'
#' @importFrom readr read_csv
#' @importFrom stringr str_sub
#'
#' @export
#'
adfgRead_MPD<-function(csv="crab_dump-931-v17.csv",
                       date_format="yyyy-mm-dd"){
  #--read measure pot data file
  dfr <- readr::read_csv(csv,guess_max=1000000);
  #column names should be:
  expCols<-c("fishery","trip","adfg","sampdate","spn","statarea",
             "mi_lon","mi_lat","spcode","sex","size","legal",
             "shell","clutch","eggdev","clutchcon");
  #check column names
  if (any(names(dfr)!=expCols)){
    idx<-names(dfr)!=expCols;
    str<-paste0("Input column names \n\t",  paste(names(dfr)[idx],collapse=", "),
                "\nshould match \n\t",      paste(expCols[idx],   collapse=", "));
    if (any(!is.na(expCols[idx]))) {
      stop(str);
    } else {
      warning(str,immediate.=TRUE);#--will continue without extra (unmatched) columns
    }
  }


  #unique fishery names:
  # [1] "CK98" "CO00" "CO01" "CO02" "CO03" "CO04" "CO05" "CO98" "CO99" "CR00" "CR01" "CR02" "CR03" "CR04" "CR98" "CR99" "EI91" "EI92" "EO91" "EO92" "EO93" "QO00" "QO01"
  #[24] "QO02" "QO03" "QO04" "QO05" "QO06" "QO07" "QO08" "QO09" "QO10" "QO11" "QO12" "QO13" "QO14" "QO15" "QO16" "QO17" "QO94" "QO95" "QO96" "QO97" "QO98" "QO99" "QR93"
  #[47] "QR95" "QT05" "QT06" "QT07" "QT08" "QT13" "QT14" "QT15" "QT17" "QT93" "QT94" "QT95" "QT96" "TR00" "TR01" "TR02" "TR03" "TR04" "TR05" "TR06" "TR07" "TR08" "TR09"
  #[70] "TR10" "TR11" "TR12" "TR13" "TR14" "TR15" "TR16" "TR17" "TR90" "TR91" "TR92" "TR93" "TR96" "TR97" "TR98" "TR99" "TT06" "TT07" "TT08" "TT09" "TT13" "TT14" "TT15"
  #unique crab areas (character 1): C (CDQ fisheries), E (???), Q (Bering Sea), T (Bristol Bay)

  #unique targets (character 2): K (red or blue king crab), O (snow crab), R (red king crab), I (Tanner crab), T (Tanner crab)

  #unique area/targets: "CK" (CDQ red or blue king crab), "CO" (CDQ snow crab), "CR" (CDQ red ling crab),
  #                     "EI" (?? Tanner crab), "EO" (??? snow crab),
  #                     "QO" (Bering Sea snow crab) "QR" (Bering Sea red king crab) "QT" (Tanner crab West),
  #                     "TR" (BBRKC) "TT", (Tanner crab East)

  #convert sex codes to labels
  dfr$sex <- adfgConvert_SexCodes(dfr$sex);

  #convert shell condition codes to labels
  dfr$shell <- adfgConvert_ShellConditionCodes(dfr$shell);

  #convert clutch fullness codes to labels
  #clutch (clutch fullness):

  #convert egg development codes to labels
  #eggdev (egg development):

  #convert clutch condition codes to labels
  #clutchcon (clutch condition)

  #convert maturity codes
  # maturity:

  #--assign E/W 166W area based on middle longitude of pot string
  dfr$EWbyLon <- ifelse(-166<dfr$mi_lon,"East 166W","West 166W");

  #--assign E/W 166W area based on statarea code (XXYYYY, where XX indicates lon of eastern edge of ADFG stat area)
  dfr$EWbySA  <- ifelse(as.numeric(stringr::str_sub(as.character(dfr$statarea),1,2))>=66,"West 166W","East 166W");

  #--parse 4-character fishery codes
  dfr.pf<-adfgConvert_FisheryCodes(dfr$fishery);

  #combine columns and drop "fishery" column
  dfrp <- cbind(dfr,dfr.pf)
  dfrp <- dfrp[,2:ncol(dfrp)];
  dfrp$count <- 1;#--add 'count': each row represents an observation on 1 crab
  #--determine crab year corresponding to sample date
  if (date_format=="yyyy-mm-dd"){
    dfrp$year<-adfgConvert_DateYYYYMMDDtoFisheryYear(dfrp$sampdate);
  } else if (date_format=="mm-dd-yyyy"){
    dfrp$year<-adfgConvert_DateMMDDYYYYtoFisheryYear(dfrp$sampdate);
  } else {
    stop("#--ERROR!\n\tUnrecognized date format in adfgRead_DSD(...).\n")
  }
  #names(dfrp)
  # [1] "trip"         "adfg"         "sampdate"     "spn"          "statarea"     "mi_lon"       "mi_lat"       "spcode"
  # [9] "sex"          "size"         "legal"        "shell"        "clutch"       "eggdev"       "clutchcon"    "EWbyLon"
  #[17] "EWbySA"       "fishery_code" "fishery"      "area"         "year_code"        "count"    "year"
  cols <- c("fishery","area","EWbySA","EWbyLon","year","fishery_code","code_year","trip","adfg","sampdate","spn","statarea","mi_lon","mi_lat",
            "spcode","sex","shell","size","legal","count");
  dfrp <- dfrp[,cols];

  #fishery_codes to remove
  rmv<-c("CK98",          #Pribs RKC or BKC fisheries   ( 8 crab measured)
         "QR95");         #RKC fisheries west of 166W   (10 crab measured)
  idr <- dfrp$fishery_code %in% rmv;

  #fishery_codes to keep
  keep2<-c("EI","QT","TT",         #Tanner crab fisheries
           "EO","QO","CO",         #snow crab fisheries
           "TR","CR");             #BBRKC fisheries
  idk2 <- stringr::str_sub(dfrp$fishery_code,1,2) %in% keep2;
  keep4<-"QR93";                   #RKC west of 166W but > 1,000 crab measured (but all east of 168W)
  idk4 <- (dfrp$fishery_code %in% keep4) &
          (68>as.numeric(stringr::str_sub(as.character(dfr$statarea),1,2)));#keep RKC only in BB (east of 168W)

  dfrp1 <- dfrp[(!idr)&(idk2|idk4),]; #select subset

  #assign area designations "all EBS", "East 166W" and "West 166W"
  dfrp1$area <- "all EBS"; #all RKC and snow crab
  #--Split EI Tanner crab based on statarea (i.e., EWbySA)
  idx <- (stringr::str_sub(dfrp1$fishery_code,1,2)=="EI");
  dfrp1$area[idx] <- dfrp1$EWbySA[idx];
  #--Split QT Tanner crab prior to 2005 based on statarea (i.e., EWbySA)
  idx <- (stringr::str_sub(dfrp1$fishery_code,1,2)=="QT")&(dfrp1$year<2005);
  dfrp1$area[idx] <- dfrp1$EWbySA[idx];
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
