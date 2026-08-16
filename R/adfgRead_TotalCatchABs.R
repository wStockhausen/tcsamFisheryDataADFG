#'
#' @title Get annual total catch abundance and biomass by crab fishery from a 2026+ csv file
#'
#' @description Function to get annual total catch abundance and biomass by crab fishery from a 2026+ csv file.
#'
#' @param csv - the csv file name
#' @param skip - number of lines to skip (default=0)
#'
#' @return a tibble with columns \cr
#' \itemize{
#'   \item{fishery}
#'   \item{area}
#'   \item{year}
#'   \item{sex}
#'   \item{maturity}
#'   \item{`shell condition`}
#'   \item{abundance - in numbers of crab}
#'   \item{`biomass (kg)` - in kg}
#' }
#'
#' @details Uses \code{readr::read_csv}, \code{stringr::str_sub}, and \code{sqldf::sqldf}.
#'
#' @note The input csv should be in the "total_catch_sex_shell.csv" format provided annually by ADFG
#' for Tanner crab since 2026. These are the "official" estimates of total catch abundance and biomass
#' of Tanner crab in the crab fisheries.
#'
#' `shell_condition` may include "undetermined" values, as well as "new shell" and "old shell" values.
#'
#' @importFrom readr read_csv
#' @importFrom sqldf sqldf
#' @importFrom stringr str_sub
#'
#' @export
#'
adfgRead_TotalCatchABs<-function(csv,
                                 skip=0){

  tmp<-readr::read_csv(csv,skip=skip,progress=FALSE);
  names(tmp)<-tolower(names(tmp));

  #column names should be:
  expCols<-c("crab_year","fishery","target_stock","sex","shell","count","obs_effort","ft_effort","cpue","avg_wt","total_catch_n","total_catch_t");
  #check column names
  if (any(names(tmp)!=expCols)){
    idx<-names(tmp)!=expCols;
    str<-paste0("--Error! Input column names \n\t",  paste(names(tmp)[idx],collapse=", "),
                "\nshould match \n\t",               paste(expCols[idx],   collapse=", "));
    stop(str);
  }

  dfrFCs = tcsamFisheryDataADFG::adfgConvert_FisheryCodes(tmp$fishery) |>
             dplyr::mutate(fishery=tcsamFisheryDataADFG::adfgConvert_FisheryNames(fishery)) |>
             dplyr::mutate(area=ifelse(fishery=="TCF",paste(area,"166W"),"all EBS"));
  tmp1 = dplyr::bind_cols(tmp |> dplyr::rename(year=crab_year) |> dplyr::select(!fishery),
                          dfrFCs |> dplyr::select(fishery,area)) |>
           dplyr::mutate(maturity="undetermined",
                         shell=ifelse(tolower(shell) %in% c("new","old"),paste(shell,"shell"),shell),
                         shell=ifelse(is.na(shell),"undetermined",shell),
                         abundance=total_catch_n,
                         `biomass (kg)`=1000*total_catch_t) |>
           dplyr::select(fishery,area,year,sex,maturity,`shell condition`=shell,abundance,`biomass (kg)`);


  #aggregate over area for TCF
  tmp2<-tmp1[tmp1$fishery=="TCF",];
  qry<-"select
          fishery,'all EBS' as area,year,
          sex,maturity,`shell condition`,
          sum(abundance) as abundance,
          sum(`biomass (kg)`) as `biomass (kg)`
        from tmp2
        group by fishery,year,sex,maturity,`shell condition`;";
  tmp3<-sqldf::sqldf(qry);

  #combine tables
  tbl<-rbind(tmp1,tmp3);
  return(tbl);
}
