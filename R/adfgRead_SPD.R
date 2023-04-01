#'
#' @title Extract summary pot data (SPD) as a tibble from a csv file
#'
#' @description Function to extract summary pot data (SPD) as a tibble from a csv file.
#'
#' @param csv - csv filename with summary pot data
#' @param date_format - string ("yyyy-mm-dd" or "mm-dd-yyyy") indicating date format
#' @param returnDropped - flag (T/F) to return *dropped* records, not selected records
#'
#' @return a tibble with columns "fishery","area","EWbySA","EWbyLon","year","fishery_code",
#' "code_year","trip","adfg","sampdate","spn","statarea","longitude","latitude",
#' "female", "sublegal", "legal_ret", "legal_nr", "legal_ur", "tot_legal".
#'
#' @details Uses functions [readr::read_csv()], [stringr::str_sub()].
#'
#' @note The 'year' values are 'crab year' based on the sample date. The 'code_year' is the
#' ADFG fishery year based on the fishery code. Prior to rationalization, there may be differences
#' in these two values.
#'
#' @note value columns represent the number of crab in the category.
#'
#' @importFrom readr read_csv
#' @importFrom stringr str_sub
#'
#' @export
#'
adfgRead_SPD<-function(csv="TANNER-1990-2020_potsum.csv",
                       date_format="mm-dd-yyyy",
                       returnDropped=FALSE){
  #--read summary pot data file for column names
  dfr  = readr::read_csv(csv,n_max=1);
  col_types = paste0(rep("c",ncol(dfr)),collapse="");
  #--read summary pot data file
  dfr  = readr::read_csv(csv,col_types=col_types);
  #column names should be:
  expColNames<-c("fishery","trip","adfg","sampdate","spn","statarea",
                 "latitude", "longitude", "female", "sublegal", "legal_ret",
                 "legal_nr", "legal_ur", "tot_legal", "msr_pot", "biotwine_ok");
  #check column names
  #----check for required names
  if (!all(expColNames %in% names(dfr))) {
    idx = !(expColNames %in% names(dfr));
    str<-paste0("Input column names \n\t",              paste(names(dfr),  collapse=", "),
                "\nare missing the required names \n\t",paste(expCols[idx],collapse=", "));
    stop(str);
  }
  #----check for names of latitude, longitude columns
  llCols = NA;
  if (all(c("mi_lat",  "mi_lon")    %in% names(dfr))) llCols = c("mi_lat",  "mi_lon");    #--nominal names
  if (all(c("latitude","longitude") %in% names(dfr))) llCols = c("latitude","longitude"); #--alternative (new as of 2022) names
  if (any(is.na(llCols))) {
    str<-paste0("Could not identify lat/lon columns in input file.\n",
                "Expected names are 'latitude' and 'longitude' or `mi_lat' and 'mi_lon'.\n",
                "Input column names are \n\t",paste(names(dfr),  collapse=", "));
    stop(str);
  }
  latCol = llCols[1];
  lonCol = llCols[2];
  #----check for extra column
  if (any(!(names(dfr) %in% c(expColNames,llCols)))){
    idx<-!(names(dfr) %in% c(expColNames,llCols));
    str<-paste0("The following unrecognized columns will be skipped \n\t",paste(names(dfr)[idx],collapse=", "));
    warning(str,immediate.=TRUE);#--will continue without extra (unmatched) columns
  }
  dfr$latitude  = dfr[[latCol]];
  dfr$longitude = dfr[[lonCol]];
  #--convert columns to numeric as necessary
  dfr %<>% dplyr::mutate(latitude=as.numeric(latitude),
                         longitude=as.numeric(longitude),
                         female=as.numeric(female),
                         sublegal=as.numeric(sublegal),
                         legal_nr=as.numeric(legal_nr),
                         legal_ur=as.numeric(legal_ur),
                         tot_legal=as.numeric(tot_legal)
                         );


  #unique fishery names (2022):
#  [1] "CK98"  "CO00"  "CO01"  "CO02"  "CO03"  "CO04"  "CO05"  "CO98"  "CO99"  "CR00"  "CR01"  "CR02"  "CR03"  "CR04"  "CR98"
#  [16] "CR99"  "EI91"  "EI92"  "EO91"  "EO92"  "EO93"  "QB01"  "QB02"  "QB03"  "QB04"  "QB05"  "QB10"  "QB11"  "QB17"  "QB18"
#  [31] "QB19"  "QB20"  "QB21"  "QO00"  "QO01"  "QO02"  "QO03"  "QO04"  "QO05o" "QO05r" "QO06"  "QO07"  "QO08"  "QO09"  "QO10"
#  [46] "QO11"  "QO12"  "QO13"  "QO14"  "QO15"  "QO16"  "QO17"  "QO18"  "QO19"  "QO20"  "QO21"  "QO94"  "QO95"  "QO96"  "QO97"
#  [61] "QO98"  "QO99"  "QP09"  "QP10"  "QP11"  "QP12"  "QP14"  "QP15"  "QR93"  "QR95"  "QT05"  "QT06"  "QT07"  "QT08"  "QT13"
#  [76] "QT14"  "QT15"  "QT17"  "QT18"  "QT20"  "QT21"  "QT93"  "QT94"  "QT95"  "QT96"  "SB19"  "TB02"  "TR00"  "TR01"  "TR02"
#  [91] "TR03"  "TR04"  "TR05"  "TR06"  "TR07"  "TR08"  "TR09"  "TR10"  "TR11"  "TR12"  "TR13"  "TR14"  "TR15"  "TR16"  "TR17"
# [106] "TR18"  "TR19"  "TR20"  "TR90"  "TR91"  "TR92"  "TR93"  "TR96"  "TR97"  "TR98"  "TR99"  "TT06"  "TT07"  "TT08"  "TT09"
# [121] "TT13"  "TT14"  "TT15"  "XR06"  "XR07"  "XR09"  "XR13"  "XR14"  "XR15"  "XR16"  "XR17"  "XR18"  "XR19"  "XR20"  "XR21"
  #unique crab areas (character 1)
  #  C: CDQ fisheries
  #  E (???),
  #  Q: Bering Sea
  #  T: Bristol Bay
  #  X: Recovery fishery (principally BBRKC)

  #unique targets (character 2)
  #  B: golden (brown) king crab
  #  K: red or blue king crab
  #  O: snow crab
  #  P: blue king crab
  #  R: red king crab
  #  I: Tanner crab
  #  T: Tanner crab

  #unique area/targets
  #  CK: CDQ red or blue king crab
  #  CO: CDQ snow crab
  #  CR: CDQ red king crab
  #  EI: ?? Tanner crab
  #  EO: ??? snow crab
  #  QB: Pribilof Islands golden king crab
  #  QO: Bering Sea snow crab
  #  QP: Saint Matts BKC
  #  QR: Bering Sea red king crab
  #  QT: Tanner crab West
  #  SB: Saint Matts GKC
  #  TB: BB GKC must have been an exploratory fishery
  #  TR: BBRKC
  #  TT: Tanner crab East
  #  XR: BBRKC cost recovery

  #--assign E/W 166W area based on middle longitude of pot string
  dfr$EWbyLon <- ifelse(-166<dfr$longitude,"East 166W","West 166W");

  #--assign E/W 166W area based on statarea code (XXYYYY, where XX indicates lon of eastern edge of ADFG stat area)
  dfr$EWbySA  <- ifelse(as.numeric(stringr::str_sub(as.character(dfr$statarea),1,2))>=66,"West 166W","East 166W");

  #--parse 4-character fishery codes
  dfr.pf<-adfgConvert_FisheryCodes(dfr$fishery);

  #combine columns and drop "fishery" column
  dfrp <- cbind(dfr,dfr.pf)
  dfrp <- dfrp[,2:ncol(dfrp)];
  #--determine crab year corresponding to sample date
  if (stringr::str_sub(date_format,1,4)=="yyyy"){
    dfrp$year<-adfgConvert_DateYYYYMMDDtoFisheryYear(dfrp$sampdate);
  } else if (stringr::str_sub(date_format,-4,-1)=="yyyy"){
    dfrp$year<-adfgConvert_DateMMDDYYYYtoFisheryYear(dfrp$sampdate);
  } else {
    stop("#--ERROR!\n\tUnrecognized date format in adfgRead_SPD(...).\n")
  }
  #--for testing: cat(paste0('"',names(dfrp),'"',collapse=", "))
  # "trip", "adfg", "sampdate", "spn", "statarea", "latitude", "longitude", "female", "sublegal",
  # "legal_ret", "legal_nr", "legal_ur", "tot_legal", "msr_pot", "biotwine_ok", "EWbyLon", "EWbySA",
  # "fishery_code", "fishery", "area", "code_year", "year"
  cols <- c("fishery","area","EWbySA","EWbyLon","year","fishery_code","code_year","trip","adfg","sampdate","spn","statarea","longitude","latitude",
            "female", "sublegal", "legal_ret", "legal_nr", "legal_ur", "tot_legal");
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
  message("\tOther rows dropped: ",sum(!(idk2|idk4)))
  message("\tdropped ",sum(!((!idr)&(idk2|idk4)))," total rows at stage 1.")

  if (returnDropped){
    dfrp2 = dfrp %>% dplyr::anti_join(dfrp1);
    return(dfrp2);
  }

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
  message("\tfinal number of rows is ",nrow(dfrp1),"\n");

  return(dfrp1);
}
