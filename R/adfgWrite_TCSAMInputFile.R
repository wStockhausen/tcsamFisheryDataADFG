#'
#' @title Write ADFG data to a TCSAM input file
#'
#' @description Function to write ADFG data to a TCSAM input file.
#'
#' @param fishery - TCSAM fishery name
#' @param fn - output file name  (writes to stdout if NULL or empty)
#' @param closed - vector of years when fishery was closed
#' @param lstRC - input list with retained catch abundance/biomass/size comps information (see Details)
#' @param lstTC - input list with total catch abundance/biomass/size comps information (see Details)
#' @param lstEff - input list with effort data information (see Details)
#'
#' @return Nothing.
#'
#' @details Calls [tcsamFunctions::writeInputFile_FleetData()] with \code{type}="FISHERY" and lstIC and lstDC set to NULL.
#'
#' If not NULL, input \code{lstRC} should be a list for retained catch data with elements
#' \itemize{
#'   \item{lstAbd - NULL, or list as returned by [tcsamFunctions::inputList_AggregateCatchData()] with type "ABUNDANCE"}
#'   \item{lstBio - NULL, or list as returned by [tcsamFunctions::inputList_AggregateCatchData()] with type "BIOMASS"}
#'   \item{lstZCs - NULL, or list as returned by [tcsamFunctions::inputList_SizeCompsData()]}
#' }
#'
#' If not NULL, input \code{lstTC} should be a list for total catch data with elements
#' \itemize{
#'   \item{lstAbd - NULL, or list as returned by [tcsamFunctions::inputList_AggregateCatchData()] with type "ABUNDANCE"}
#'   \item{lstBio - NULL, or list as returned by [tcsamFunctions::inputList_AggregateCatchData()] with type "BIOMASS"}
#'   \item{lstZCs - NULL, or list as returned by [tcsamFunctions::inputList_SizeCompsData()]}
#' }
#'
#' If not NULL, input \code{lstEff} should be a list for effort data as returned by [tcsamFunctions::inputList_EffortData()].
#'
#' @importFrom tcsamFunctions writeInputFile_FleetData
#'
#' @export
#'
adfgWrite_TCSAMInputFile<-function(fishery=NULL,
                                   fn="Data.Fishery.ADFG.inp",
                                   closed=NULL,
                                   lstRC=NULL,
                                   lstTC=NULL,
                                   lstEff=NULL){

  if (is.null(fn)||(fn=="")){
    con = stdout();
  } else {
    if (!file.exists(fn)) {
      res<-file.create(fn);
      if (!res) stop(paste0("Could not create file '",fn,"'.\nAborting...\n"));
    }
    con<-file(fn,open="w");
    on.exit(close(con));
  }

  tcsamFunctions::writeInputFile_FleetData(con=con,
                                           fleet=fishery,
                                           type="FISHERY",
                                           closed=closed,
                                           lstRC=lstRC,
                                           lstTC=lstTC,
                                           lstDC=NULL,
                                           lstIC=NULL,
                                           lstEff=lstEff);
}
