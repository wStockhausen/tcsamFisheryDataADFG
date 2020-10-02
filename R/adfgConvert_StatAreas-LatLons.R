#'
#' @title Convert ADFG stat areas to lat-lon coordinates
#'
#' @description Function to convert ADFG stat areas to lat-lon coordinates.
#'
#' @param stat_areas - character vector of ADFG stat areas
#'
#' @return a tibble with columns "lat", "lon" representing the center of each stat area.
#'
#' @details ADFG stat areas are coded AABBCC, where\cr 
#' lon = 100+AA W longitude \cr
#' and \cr
#' lat = BB+CC/60 N latitude \cr
#' in decimal degrees. 
#' 
#' AABBCC represents the lower right corner
#' of the stat area.
#'
#' @importFrom tibble tibble
#'
#' @export
#'
adfgConvert_StatAreasToLatLons<-function(stat_areas){
  #get locations of lower left
  lond=-(100+as.numeric(substr(stat_areas,1,2)));#longitude (deg E)
  latd=as.numeric(substr(stat_areas,3,4));       #latitude, whole deg N
  latm=as.numeric(substr(stat_areas,5,6));       #latitude, minutes N
  tbl = tibble(lat=latd+(latm+15)/60,lon=lond-0.5);#shift from corner to center
  return(tbl);
}

#'
#' @title Convert lat-lon coordinates to ADFG stat areas
#'
#' @description Function to convert lat-lon coordinates to ADFG stat areas.
#'
#' @param dfr - dataframe with lat, lon coordinates
#' @param latCol - name of column with latitudes
#' @param lonCol - name of column with longitudes
#'
#' @return a character vector with the ADFG stat area corresponding to each set
#' of lat-lon coordinates.
#'
#' @details ADFG stat areas are coded AABBCC, where\cr 
#' lon = 100+AA W longitude \cr
#' and \cr
#' lat = BB+CC/60 N latitude \cr
#' in decimal degrees. 
#' 
#' AABBCC represents the lower right corner
#' of the stat area.
#'
#' @importFrom wtsUtilities formatZeros
#'
#' @export
#'
adfgConvert_LatLonsToStatAreas<-function(dfr,latCol="lat",lonCol="lon"){
  lats = dfr[[latCol]];
  BB   = as.character(floor(lats));
  CC   = wtsUtilities::formatZeros(30*floor(2*(lats %% 1)));
  lons = abs(dfr[[lonCol]]);
  lons = floor(ifelse(lons>180,360-lons,lons)-100);
  idx<-lons==-100; #--these were NA's at some point 
  lons[idx]<-0;
  AA = wtsUtilities::formatZeros(lons);
  stat_areas<-paste0(AA,BB,CC);
  return(stat_areas);
}

