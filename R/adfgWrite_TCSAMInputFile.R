#'
#' @title Write ADFG data fo a TCSAM input file
#'
#' @description Function to write ADFG data to a TCSAM input file.
#'
#' @param fishery - TCSAM fishery name
#' @param fn - output file name
#' @param closed - vector of years when fishery was closed
#' @param dfrRC_ABs - dataframe with retained catch abundance/biomass data
#' @param dfrRC_ZCs - dataframe with retained catch size composition data
#' @param dfrRC_SSs - dataframe with retained catch sample size information
#' @param dfrTC_ABs - dataframe with total catch abundance/biomass data
#' @param dfrTC_ZCs - dataframe with total catch size composition data
#' @param dfrTC_SSs - dataframe with total catch sample size information
#' @param dfrEffort - dataframe with fishery effort
#' @param rcCutPts - vector with cutpts used for retained catch size comps
#' @param tcCutPts - vector with cutpts used for total catch size comps
#' @param likeRC - likelihood type for retained catch ABs ("NORM2","NORMAL", or "LOGNORMAL")
#' @param cvRC - cv for retained catch above min error (default=0.05)
#' @param minErrRCa - min error in retained catch abundance (default=100)
#' @param minErrRCb - min error in retained catch biomass   (default=100 kg)
#' @param likeTC - likelihood type for total catch ABs ("NORM2","NORMAL", or "LOGNORMAL")
#' @param cvTC - cv for total catch above min error (default=0.20)
#' @param minErrTCa - min error in total catch abundance (default=2000)
#' @param minErrTCb - min error in total catch biomass   (default=2000 kg)
#' @param unitsBiomass - units for output biomass ("THOUSANDS_MT" [default],"MILLIONS_LBS","KG")
#'
#' @return null
#'
#' @details None
#'
#' @export
#'
adfgWrite_TCSAMInputFile<-function(fishery=NULL,
                                   fn="Data.Fishery.ADFG.inp",
                                   closed=NULL,
                                   dfrRC_ABs=NULL,
                                   dfrRC_ZCs=NULL,
                                   dfrRC_SSs=NULL,
                                   dfrTC_ABs=NULL,
                                   dfrTC_ZCs=NULL,
                                   dfrTC_SSs=NULL,
                                   dfrEffort=NULL,
                                   rcCutPts=NULL,
                                   tcCutPts=NULL,
                                   likeRC="NORM2",
                                   cvRC=0.05,
                                   minErrRCa=100,
                                   minErrRCb=100,
                                   likeTC="NORM2",
                                   cvTC=0.20,
                                   minErrTCa=2000,
                                   minErrTCb=2000,
                                   unitsBiomass="THOUSANDS_MT"){

  #--SCALE CONSTANTS
  MILLIONS<-1000000;     #scale to convert to millions
  LBStoKG <- 0.45359237; #multiplicative factor to get kg from lbs
  #--determine scaling for output biomass (input in kg)
  if (unitsBiomass=="THOUSANDS_MT") {
    sclB <- 1.0/MILLIONS;
  } else if (unitsBiomass=="MILLIONS_LBS") {
    sclB <- 1.0/(MILLIONS*LBStoKG);
  } else if (unitsBiomass=="KG") {
    sclB <- 1.0;
  } else {
    msg<-paste0("\nERROR in adfgWrite_TCSAMInputFile.",
                "\ninput value for unitsBiomass ('",unitsBiomass,"') is invalid.",
                "\nValid values are 'THOUSANDS_MT','MILLIONS_LBS','KG'.\n");
    stop(msg);
  }

  #--function to make substitutions for "undetermined"
  subForTCSAM<-function(x,str){
    xp <- ifelse(tolower(x)=="undetermined",str,x);
    xp <- gsub(" ","_",xp,fixed=TRUE);
    return(xp);
    }

  #--column names in dataframes
  yr  <- "year";
  flt <- "fishery"; #--fleet
  are <- "area";
  sx  <- "sex";
  mt  <- "maturity";
  sc  <- "shell condition";
  sz  <- "size"
  abd <-"abundance";
  bio <- "biomass (kg)";
  ss  <- "ss";

  #--write flags for various quantities
  writeRCA<-!is.null(dfrRC_ABs);
  writeRCB<-!is.null(dfrRC_ABs);
  writeRCZ<-!is.null(dfrRC_ZCs);
  writeTCA<-!is.null(dfrTC_ABs);
  writeTCB<-!is.null(dfrTC_ABs);
  writeTCZ<-!is.null(dfrTC_ZCs);
  writeEff<-!is.null(dfrEffort);
  hasEffort<-!is.null(dfrEffort);
  hasRC<-any(writeRCA,writeRCB,writeRCZ);
  hasTC<-any(writeTCA,writeTCB,writeTCZ);

  con<-"";#--write to stdout
  if (fn!=""){
    if (!file.exists(fn)) {
      res<-file.create(fn);
      if (!res) stop(paste0("Could not create file '",fn,"'.\nAborting...\n"));
    }
    con<-file(fn,open="w");
  }

    cat("#---------------------------------------------------------------------------------------\n",file=con);
    cat("#--TCSAM02 model file for abundance, biomass and/or size comp time series               \n",file=con);
    cat("#--for retained and/or total catch and effort in the directed and incidental fisheries. \n",file=con);
    cat("#---------------------------------------------------------------------------------------\n",file=con);
    cat("FISHERY             #required keyword\n",file=con);
    cat(fishery,"       #fishery name\n",file=con,sep='');
    cat("FALSE       #has index catch data?\n",file=con);
    cat(hasRC,"      #has retained catch data?\n",file=con);
    cat("FALSE       #has observed discard catch data\n",file=con);
    cat(hasTC,"      #has observed total catch data\n",file=con);
    cat(hasEffort,"  #has effort data?\n",file=con);
    cat("#------------INDEX CATCH DATA------------	\n",file=con);
    cat("#---none\n",file=con);
    cat("#------------RETAINED CATCH DATA------------#\n",file=con);
    if (!hasRC){
      cat("#---none\n",file=con);
    } else {
      cat("CATCH_DATA     #required keyword\n",file=con);
      cat(writeRCA,"           #has aggregate catch abundance (numbers)\n",file=con);
      cat(writeRCB,"           #has aggregate catch biomass (weight)\n",file=con);
      cat(writeRCZ,"           #has size frequency data\n",file=con);

      #--retained catch abundance
      cat("#------------AGGREGATE CATCH ABUNDANCE (NUMBERS)------------#\n",file=con);
      if (!writeRCA){
        cat("#------------NUMBERS-AT-SIZE DATA-----------\n",file=con);
      } else {
        tmp <- dfrRC_ABs;
        if (!is.null(closed)) tmp <- tmp[!(tmp[[yr]] %in% closed),];
        uYs<-sort(unique(tmp[[yr]]));
        uFCs<-unique(tmp[,c(sx,mt,sc)]);
        cat("AGGREGATE_ABUNDANCE     #required keyword\n",file=con);
        cat("BY_X                    #objective function fitting option\n",file=con);
        cat(likeRC,"                 #likelihood type\n",file=con);
        cat("0.0                     #likelihood weight\n",file=con);
        cat(length(uYs),"            #number of years\n",file=con,sep='');
        cat("ONES                    #units, catch abundance\n",file=con);
        cat(nrow(uFCs),"             #number of factor combinations\n",file=con);
        for (iFC in 1:nrow(uFCs)){
          fc<-uFCs[iFC,];
          cat(toupper(subForTCSAM(fc[[sx]],"ALL_SEX")),
              toupper(subForTCSAM(fc[[mt]],"ALL_MATURITY")),
              toupper(subForTCSAM(fc[[sc]],"ALL_SHELL")),"\n",file=con);
          cat("#year    number    cv\n",file=con);
          for (y in uYs){
            ida<-(tmp[[yr]]==y)&
                  (tmp[[sx]]==fc[[sx]])&
                  (tmp[[mt]]==fc[[mt]])&
                  (tmp[[sc]]==fc[[sc]]);
            val<-tmp[ida,abd,drop=TRUE];#--catch in numbers of crab
            cv<-max(cvRC,minErrRCa/val);#--effective cv
            cat(y,val,cv,"\n",sep="    ",file=con);
          }#--y
        }#--fc
        rm(tmp,uYs,uFCs,fc,y,ida);
      }#--writeA

      #--retained catch biomass
      cat("#------------AGGREGATE CATCH ABUNDANCE (BIOMASS)------------#\n",file=con);
      if (!writeRCB){
        cat("#--none\n",file=con);
      } else {
        tmp <- dfrRC_ABs;
        if (!is.null(closed)) tmp <- tmp[!(tmp[[yr]] %in% closed),];
        uYs<-sort(unique(tmp[[yr]]));
        uFCs<-unique(tmp[,c(sx,mt,sc)]);
        cat("AGGREGATE_BIOMASS       #required keyword\n",file=con);
        cat("BY_X                    #objective function fitting option\n",file=con);
        cat(likeRC,"                 #likelihood type\n",file=con);
        wgt<-1; if (likeRC=="NORM2") wgt<-20.0;;
        cat(wgt,"                    #likelihood weight\n",file=con);
        cat(length(uYs),"                      #number of years\n",file=con,sep='');
        cat(unitsBiomass,"            # units, catch biomass\n",file=con);
        cat(nrow(uFCs),"		#number of factor combinations\n",file=con);
        for (iFC in 1:nrow(uFCs)){
          fc<-uFCs[iFC,];
          cat(toupper(subForTCSAM(fc[[sx]],"ALL_SEX")),
              toupper(subForTCSAM(fc[[mt]],"ALL_MATURITY")),
              toupper(subForTCSAM(fc[[sc]],"ALL_SHELL")),"\n",file=con);
          cat("#year    biomass    cv\n",file=con);
          for (y in uYs){
            idb<-(tmp[[yr]]==y)&
                  (tmp[[sx]]==fc[[sx]])&
                  (tmp[[mt]]==fc[[mt]])&
                  (tmp[[sc]]==fc[[sc]]);
            val<-tmp[idb,bio,drop=TRUE]; #--catch in kg
            cv<-max(cvRC,minErrRCb/val); #--effective cv
            cat(y,sclB*val,cv,"\n",sep="    ",file=con);
          }#--y
        }#--fc
        rm(tmp,uYs,uFCs,fc,y,idb);
      }#--writeB

      #--size compositions
        cat("#------------NUMBERS-AT-SIZE DATA-----------\n",file=con);
      if (!writeRCZ){
        cat("#--none\n",file=con);
      } else {
        tmpZCs <- dfrRC_ZCs;
        if (!is.null(closed)) tmpZCs <- tmpZCs[!(tmpZCs[[yr]] %in% closed),];
        tmpSSs <- dfrRC_SSs;
        if (!is.null(closed)) tmpSSs <- tmpSSs[!(tmpSSs[[yr]] %in% closed),];
        cutpts<-rcCutPts;
        bins<-(cutpts[2:length(cutpts)]+cutpts[1:(length(cutpts)-1)])/2;
        uYs<-sort(unique(tmpZCs[[yr]]));
        uFCs<-unique(tmpZCs[,c(sx,mt,sc)]);
        #cat("uFCs:\n")
        #print(uFCs);
        #cat("dfrSS:\n")
        #print(dfrSS);
        cat("SIZE_FREQUENCY_DATA  #required keyword\n",file=con);
        cat("BY_X                 #objective function fitting option\n",file=con);
        cat("MULTINOMIAL          #likelihood type\n",file=con);
        cat("1.0                  #likelihood weight\n",file=con);
        cat(length(uYs),"       #number of years of data\n",file=con);
        cat("MILLIONS             #units\n",file=con);
        cat(length(cutpts)," #number of size bin cutpoints\n",file=con);
        cat("#size bin cutpts (mm CW)\n",file=con);
        cat(cutpts,"\n",file=con);
        cat("#--------------\n",file=con);
        cat(nrow(uFCs),"    #number of factor combinations\n",file=con);
        for (iFC in 1:nrow(uFCs)){
          fc<-uFCs[iFC,];
          #cat("uFC[",iFC,",]:\n");
          #print(fc);
          cat(toupper(subForTCSAM(fc[[sx]],"ALL_SEX")),
              toupper(subForTCSAM(fc[[mt]],"ALL_MATURITY")),
              toupper(subForTCSAM(fc[[sc]],"ALL_SHELL")),"\n",file=con);
              cat("#year    number    ",bins,"\n",file=con);
              for (y in uYs){
                ids<-(tmpSSs[[yr]]==y)&
                      (tmpSSs[[sx]]==fc[[sx]])&
                      (tmpSSs[[mt]]==fc[[mt]])&
                      (tmpSSs[[sc]]==fc[[sc]]);
                idz<-(tmpZCs[[yr]]==y)&
                      (tmpZCs[[sx]]==fc[[sx]])&
                      (tmpZCs[[mt]]==fc[[mt]])&
                      (tmpZCs[[sc]]==fc[[sc]]);
                rw<-paste(tmpZCs[idz,abd,drop=TRUE]/MILLIONS,collapse=" ");
                cat(y,tmpSSs[ids,ss,drop=TRUE],rw,"\n",sep="    ",file=con);
              }#--y
        }#--iFC
        rm(tmpSSs,tmpZCs,cutpts,bins,iFC,uFCs,fc,y,uYs,ids,idz);
      }#--writeZ
    }

    cat("#------------DISCARD CATCH DATA------------#\n",file=con);
    cat("#---none\n",file=con);

    cat("#------------TOTAL CATCH DATA------------#\n",file=con);
    if (!any(writeTCA,writeTCB,writeTCZ)){
      cat("#---none\n",file=con);
    } else {
      cat("CATCH_DATA     #required keyword\n",file=con);
      cat(writeTCA,"           #has aggregate catch abundance (numbers)\n",file=con);
      cat(writeTCB,"           #has aggregate catch biomass (weight)\n",file=con);
      cat(writeTCZ,"           #has size frequency data\n",file=con);

      #--total catch abundance
      cat("#------------AGGREGATE CATCH ABUNDANCE (NUMBERS)------------#\n",file=con);
      if (!writeTCA){
        cat("#---none\n",file=con);
      } else {
        tmp <- dfrTC_ABs;
        if (!is.null(closed)) tmp <- tmp[!(tmp[[yr]] %in% closed),];
        uYs<-sort(unique(tmp[[yr]]));
        uFCs<-unique(tmp[,c(sx,mt,sc)]);
        cat("AGGREGATE_ABUNDANCE     #required keyword\n",file=con);
        cat("BY_X                    #objective function fitting option\n",file=con);
        cat(likeTC,"                 #likelihood type\n",file=con);
        cat("0.0                     #likelihood weight\n",file=con);
        cat(length(uYs),"            #number of years\n",file=con,sep='');
        cat("MILLIONS   		         #units, catch abundance\n",file=con);
        cat(nrow(uFCs),"             #number of factor combinations\n",file=con);
        for (iFC in 1:nrow(uFCs)){
          fc<-uFCs[iFC,];
          cat(toupper(subForTCSAM(as.character(fc[[sx]]),"ALL_SEX")),
              toupper(subForTCSAM(as.character(fc[[mt]]),"ALL_MATURITY")),
              toupper(subForTCSAM(as.character(fc[[sc]]),"ALL_SHELL")),"\n",file=con);
          cat("#year    number    cv\n",file=con);
          for (y in uYs){
            ida<-(tmp[[yr]]==y)&
                  (tmp[[sx]]==fc[[sx]])&
                  (tmp[[mt]]==fc[[mt]])&
                  (tmp[[sc]]==fc[[sc]]);
            val<-tmp[ida,abd,drop=TRUE]; #--catch in numbers of crab
            cv<-max(cvTC,minErrTCa/val); #--effective cv
            cat(y,val/MILLIONS,cv,"\n",sep="    ",file=con);
          }#--y
        }#--fc
        rm(tmp,uYs,uFCs,fc,y,ida);
      }#--writeA

      #--total catch biomass
      cat("#------------AGGREGATE CATCH ABUNDANCE (BIOMASS)------------#\n",file=con);
      if (!writeTCB){
        cat("#--none\n",file=con);
      } else {
        tmp <- dfrTC_ABs;
        if (!is.null(closed)) tmp <- tmp[!(tmp[[yr]] %in% closed),];
        uYs<-sort(unique(tmp[[yr]]));
        uFCs<-unique(tmp[,c(sx,mt,sc)]);
        cat("AGGREGATE_BIOMASS       #required keyword\n",file=con);
        cat("BY_X                    #objective function fitting option\n",file=con);
        cat(likeTC,"                 #likelihood type\n",file=con);
        wgt<-1; if (likeRC=="NORM2") wgt<-20.0;;
        cat(wgt,"                    #likelihood weight\n",file=con);
        cat(length(uYs),"            #number of years\n",file=con,sep='');
        cat(unitsBiomass,"           #units, catch biomass\n",file=con);
        cat(nrow(uFCs),"		         #number of factor combinations\n",file=con);
        for (iFC in 1:nrow(uFCs)){
          fc<-uFCs[iFC,];
          cat(toupper(subForTCSAM(fc[[sx]],"ALL_SEX")),
              toupper(subForTCSAM(fc[[mt]],"ALL_MATURITY")),
              toupper(subForTCSAM(fc[[sc]],"ALL_SHELL")),"\n",file=con);
          cat("#year    biomass    cv\n",file=con);
          for (y in uYs){
            idb<-(tmp[[yr]]==y)&
                  (tmp[[sx]]==fc[[sx]])&
                  (tmp[[mt]]==fc[[mt]])&
                  (tmp[[sc]]==fc[[sc]]);
            val<-tmp[idb,bio,drop=TRUE]; #--catch in kg
            cv<-max(cvTC,minErrTCb/val); #--effective cv
            cat(y,sclB*val,cv,"\n",sep="    ",file=con);
          }#--y
        }#--fc
        rm(tmp,uYs,uFCs,fc,y,idb);
      }#--writeB

      #--size compositions
        cat("#------------NUMBERS-AT-SIZE DATA-----------\n",file=con);
      if (!writeTCZ){
        cat("#--none\n",file=con);
      } else {
        tmpZCs <- dfrTC_ZCs;
        if (!is.null(closed)) tmpZCs <- tmpZCs[!(tmpZCs[[yr]] %in% closed),];
        tmpSSs <- dfrTC_SSs;
        if (!is.null(closed)) tmpSSs <- tmpSSs[!(tmpSSs[[yr]] %in% closed),];
        cutpts<-tcCutPts;
        bins<-(cutpts[2:length(cutpts)]+cutpts[1:(length(cutpts)-1)])/2;
        uYs<-sort(unique(tmpZCs[[yr]]));
        uFCs<-unique(tmpZCs[,c(sx,mt,sc)]);
        #cat("uFCs:\n")
        #print(uFCs);
        #cat("tmpSSs:\n")
        #print(tmpSSs);
        cat("SIZE_FREQUENCY_DATA  #required keyword\n",file=con);
        cat("BY_X                 #objective function fitting option\n",file=con);
        cat("MULTINOMIAL          #likelihood type\n",file=con);
        cat("1.0                  #likelihood weight\n",file=con);
        cat(length(uYs),"         #number of years of data\n",file=con);
        cat("MILLIONS             #units\n",file=con);
        cat(length(cutpts)," #number of size bin cutpoints\n",file=con);
        cat("#size bin cutpts (mm CW)\n",file=con);
        cat(cutpts,"\n",file=con);
        cat("#--------------\n",file=con);
        cat(nrow(uFCs),"    #number of factor combinations\n",file=con);
        for (iFC in 1:nrow(uFCs)){
          fc<-uFCs[iFC,];
          #cat("uFC[",iFC,",]:\n");
          #print(fc);
          cat(toupper(subForTCSAM(fc[[sx]],"ALL_SEX")),
              toupper(subForTCSAM(fc[[mt]],"ALL_MATURITY")),
              toupper(subForTCSAM(fc[[sc]],"ALL_SHELL")),"\n",file=con);
              cat("#year    ss    ",bins,"\n",file=con);
              for (y in uYs){
                ids<-(tmpSSs[[yr]]==y)&
                      (tmpSSs[[sx]]==fc[[sx]])&
                      (tmpSSs[[mt]]==fc[[mt]])&
                      (tmpSSs[[sc]]==fc[[sc]]);
                idz<-(tmpZCs[[yr]]==y)&
                      (tmpZCs[[sx]]==fc[[sx]])&
                      (tmpZCs[[mt]]==fc[[mt]])&
                      (tmpZCs[[sc]]==fc[[sc]]);
                rw<-paste(tmpZCs[idz,abd,drop=TRUE]/MILLIONS,collapse=" ");
                cat(y,tmpSSs[ids,ss,drop=TRUE],rw,"\n",sep="    ",file=con);
              }#--y
        }#--iFC
        rm(tmpSSs,tmpZCs,cutpts,bins,iFC,uFCs,fc,y,uYs,ids,idz);
      }#--writeZ
    }

    cat("#------------EFFORT DATA------------#\n",file=con);
    if(!writeEff){
      cat("#---none\n",file=con);
    } else {
      tmp<-dfrEffort;
      if (!is.null(closed)) tmp <- tmp[!(tmp[[yr]] %in% closed),];
      uYs<-sort(unique(tmp[[yr]]));
      cat("EFFORT_DATA    #required keyword\n",file=con);
      cat("[1992:-1]  #interval over which to average effort/fishing mortality\n",file=con);
      cat("NORM2      #likelihood type\n",file=con);
      cat("1.0        #likelihood weight\n",file=con);
      cat("ONES       #potlift units\n",file=con);
      cat(paste0(length(uYs),"    #number of years of directed effort data\n"),file=con);
      cat("#year	potlifts\n",file=con);
      for (y in uYs) {
        cat(y,tmp$effort[tmp[[yr]]==y],"\n",file=con);
      }
      rm(tmp,uYs);
    } #-writeEff

    if (con!="") close(con);
    rm(con);

}
