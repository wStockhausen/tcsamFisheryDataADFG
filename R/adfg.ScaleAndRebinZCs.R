#'
#' @title Scale and rebin size compositions
#'
#' @description Function to scale and rebin size compositions.
#'
#' @param mdfrScl - dataframe with scaling information
#' @param mdfrZCs - "raw" size compositions (counts) to scale and rebin
#' @param aggFacs - character vector of biological factors over which to aggregate (e.g., shell condition)
#' @param scaleCol - name of column in mdfrScl with scale information
#' @param sizeCol  - name of column in mdfrZCs with sizes to rebin
#' @param valCol  -  name of column in mdfrZCs with values to scale
#' @param cutpts - vector of cutpoints
#' @param truncate.low - flag to truncate below first cutpoint (i.e., cutpt[1]<-0)
#' @param truncate.high - flag to truncate above last cutpoint (i.e., cutpt[last]<-Inf)
#' @param returnSampleSizes - flag to return sample sizes as well as size compositions (default=FALSE)
#' @param verbose - flag (T/F) to print diagnostic information
#'
#' @return If returnSampleSizes is FALSE, a dataframe with same columns as mdfrZCs, except that the \code{valCol}
#' column is renamed to that in \code{scaleCol}.
#'
#' If returnSampleSizes is TRUE, then the return value is a list with named elements
#' * tblSS -a dataframe with sample sizes
#' * tblZCs - the scaled, rebinned and expanded size compositions
#'
#' @details Uses \code{sqldf::sqldf}, \code{reshape2::dcast}, \code{wtsUtilities::Sum}, and \code{wtsUtilities::applyCutPts}.
#' Input column names should given "as is" and **NOT** be backquoted.
#'
#' @export
#'
adfg.ScaleAndRebinZCs<-function(mdfrScl,
                                 mdfrZCs,
                                 aggFacs=c("shell condition"),
                                 scaleCol="abundance",
                                 sizeCol="size",
                                 valCol="count",
                                 cutpts=seq(from=5,to=185,by=5),
                                 truncate.low=TRUE,
                                 truncate.high=FALSE,
                                 returnSampleSizes=FALSE,
                                 verbose=FALSE){
  #get names of "factor" column in mdfrZCs
  facs<-names(mdfrZCs)[!names(mdfrZCs) %in% c(aggFacs,sizeCol,valCol)];

  #--calculate scale by aggregating over "factors" (e.g., shell condition) missing from
  frmla<-paste0(paste0(paste0("`",facs,"`"),collapse="+"),"~.");
  mdfrSclp<-reshape2::dcast(mdfrScl,frmla, fun.aggregate=wtsUtilities::Sum,value.var=scaleCol);
  names(mdfrSclp)[ncol(mdfrSclp)]<-scaleCol;

  #--calculate sample sizes by aggregating over shell condition
  mdfrSS <-reshape2::dcast(mdfrZCs,frmla, fun.aggregate=wtsUtilities::Sum,value.var=valCol);
  names(mdfrSS)[ncol(mdfrSS)]<-"ss";

  #--normalize by factors over aggFacs and size columns
  strFacs<-"";
  if ((length(   facs)>0)&&(   facs[1]!=""))
    strFacs<-paste0(paste0(paste0("z.`",facs,"`"),collapse=","),",");
  if ((length(aggFacs)>0)&&(aggFacs[1]!=""))
    strFacs<-paste0(strFacs,paste0(paste0(paste0("z.`",aggFacs,"`"),collapse=","),","));
  if (verbose) cat(strFacs,"\n");

  strOn<-"on \n";
  if ((length(   facs)>0)&&(   facs[1]!=""))
    strOn<-paste0(strOn,paste0(paste0("\tz.`",facs,"`\t=\ts.`",facs,"`"),collapse="\t and \n"));
  if (verbose) cat(strOn,"\n");

  origCols<-names(mdfrZCs)[names(mdfrZCs)!=valCol];
  strOrder<-paste0(paste0("z.`",origCols,"`"),collapse=",");
  if (verbose) cat(strOrder,"\n")

  qry<-"select &&facs z.&&sizeCol,
               z.&&valCol/s.ss as p
        from mdfrZCs as z left join mdfrSS as s
        &&on
        order by &&order;";
  qry<-gsub("&&facs",strFacs,qry,fixed=TRUE);
  qry<-gsub("&&sizeCol",sizeCol,qry,fixed=TRUE);
  qry<-gsub("&&valCol", valCol,qry,fixed=TRUE);
  qry<-gsub("&&on",strOn,qry,fixed=TRUE);
  qry<-gsub("&&order",strOrder,qry,fixed=TRUE);

  if (verbose) cat(qry,"\n");
  mdfrZCsNormd<-sqldf::sqldf(qry);

  #--scale normalized size comps
  qry<-"select &&cols,
               s.&&scaleCol*z.p as &&scaleCol
        from mdfrZCsNormd as z left join mdfrSclp as s
        &&on
        order by &&order;";
  qry<-gsub("&&cols",     strOrder,qry,fixed=TRUE);
  qry<-gsub("&&scaleCol", scaleCol,qry,fixed=TRUE);
  qry<-gsub("&&on",       strOn,   qry,fixed=TRUE);
  qry<-gsub("&&order",    strOrder,qry,fixed=TRUE);

  if (verbose) cat(qry,"\n");
  mdfrZCsScld<-sqldf::sqldf(qry);

  #--rebin sizes to cutpts
  mdfrZCsRbnd<-mdfrZCsScld;
  zs<-wtsUtilities::applyCutPts(mdfrZCsRbnd[[sizeCol]],
                                cutpts=cutpts,
                                truncate.low=truncate.low,
                                truncate.high=truncate.high);
  mdfrZCsRbnd[[sizeCol]]<-zs;
  mdfrZCsRbnd<-mdfrZCsRbnd[!is.na(mdfrZCsRbnd[[sizeCol]]),];

  #--aggregate over bins
  qry<-"select
          &&cols, sum(z.&&scaleCol) as &&scaleCol
        from
          mdfrZCsRbnd as z
        group by
          &&group
        order by
          &&order;"
  qry<-gsub("&&cols",     strOrder,qry,fixed=TRUE);
  qry<-gsub("&&scaleCol", scaleCol,qry,fixed=TRUE);
  qry<-gsub("&&group",    strOrder,qry,fixed=TRUE);
  qry<-gsub("&&order",    strOrder,qry,fixed=TRUE);

  if (verbose) cat(qry,"\n");
  mdfrZCsRbndA<-sqldf::sqldf(qry);

  #--expand size levels to new size bins
  uFacs<-unique(mdfrZCsRbndA[,c(facs,aggFacs)]);
  uZs<-data.frame(size=cutpts[1:(length(cutpts)-1)]);
  names(uZs)<-sizeCol;#rename column to sizeCol
  qry<-"select * from uFacs,uZs;";
  uFZs<-sqldf::sqldf(qry);

  strUFZs<-paste0(paste0("u.`",names(uFZs),"`"),collapse=",");
  if (verbose) cat(strUFZs,"\n");

  strOn<-"on \n";
  strOn<-paste0(strOn,paste0(paste0("\tu.`",names(uFZs),"`\t=\tz.`",names(uFZs),"`"),collapse="\t and \n"));
  if (verbose) cat(strOn,"\n");

  qry<-"select
          &&cols, z.&&scaleCol
        from
          uFZs as u left join mdfrZCsRbndA as z
        &&on
        order by
          &&order;";
  qry<-gsub("&&cols",    strUFZs, qry,fixed=TRUE);
  qry<-gsub("&&scaleCol",scaleCol,qry,fixed=TRUE);
  qry<-gsub("&&on",      strOn,   qry,fixed=TRUE);
  qry<-gsub("&&order",   strUFZs,qry,fixed=TRUE);

  if (verbose) cat(qry,"\n");
  mdfrZCsExpd<-sqldf::sqldf(qry);
  idx<-is.na(mdfrZCsExpd[[scaleCol]]);
  mdfrZCsExpd[[scaleCol]][idx]<-0;

  if (returnSampleSizes) return(list(tblSS=mdfrSS,tblZCs=mdfrZCsExpd));

  return(mdfrZCsExpd);
}
