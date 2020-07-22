#'
#' @title Export size compositions in wide format
#'
#' @description Function to export size compositions in wide format.
#'
#' @param dfrSSs - dataframe with sample size information
#' @param dfrZCs - size composition dataframe
#' @param byFacs - character vector of size composition factors to include in ouptut
#' @param sizeCol  - name of column in dfrZCs with sizes
#' @param valCol  -  name of column in dfrZCs with abundance values
#' @param ssCol  -  name of column in dfrSSs with sample sizes
#' @param csvSSs - name of csv file to save sample size to
#' @param csvZCs - name of csv file to save size compositions to
#' @param writeFiles - flag (T/F) to write csv files (default=TRUE)
#' @param verbose - flag (T/F) to print diagnostic information
#'
#' @return A list with named elements
#' * tblZCs - the wide-format size compositions
#' * tblSSs - a dataframe with sample sizes
#'
#' @details Uses \code{sqldf::sqldf}, \code{reshape2::dcast}, and \code{wtsUtilities::Sum}.
#' Input column names should given "as is" and **NOT** be backquoted.
#'
#' @importFrom utils write.csv
#'
#' @export
#'
adfg.ExportWideZCs<-function(dfrSSs,
                             dfrZCs,
                             byFacs=c("fishery","area","sex","shell condition","year"),
                             sizeCol="size",
                             valCol="abundance",
                             ssCol="ss",
                             csvSSs="dfrSSs.csv",
                             csvZCs="dfrZCs.csv",
                             writeFiles=TRUE,
                             verbose=FALSE){
  #--aggregate sample sizes as appropriate
  ssFacs<-names(dfrSSs)[names(dfrSSs) %in% byFacs];
  strFacsSS<-paste0(paste0("s.`",ssFacs,"`"),collapse=",");
  qry<-"select &&facs, sum(s.&&ss) as &&ss
        from dfrSSs as s
        group by &&facs
        order by &&facs;";
  qry<-gsub("&&facs",strFacsSS,qry,fixed=TRUE);
  qry<-gsub("&&ss",ssCol,      qry,fixed=TRUE);

  if (verbose) cat(qry,"\n");
  dfrSSp<-sqldf::sqldf(qry);

  #--aggregate ZCs as appropriate
  strFacs<-paste0(paste0(paste0("z.`",byFacs,"`"),collapse=","),",z.",sizeCol);
  qry<-"select &&facs, sum(z.&&valCol) as &&valCol
        from dfrZCs as z
        group by &&facs
        order by &&facs;";
  qry<-gsub("&&facs",  strFacs,qry,fixed=TRUE);
  qry<-gsub("&&valCol",valCol, qry,fixed=TRUE);

  if (verbose) cat(qry,"\n");
  dfrZCsp<-sqldf::sqldf(qry);

  #--convert ZCs to wide format
  frmla<-paste0(paste0(paste0("`",byFacs,"`"),collapse="+"),"~",sizeCol);
  dfrZCsW<-reshape2::dcast(dfrZCsp,frmla,fun.aggregate=wtsUtilities::Sum,value.var=valCol);

  if (writeFiles){
    write.csv(dfrSSp, file=csvSSs,row.names=FALSE);
    write.csv(dfrZCsW,file=csvZCs,row.names=FALSE);
  }

  return(list(tblZCs=dfrZCsW,tblSSs=dfrSSp));
}
