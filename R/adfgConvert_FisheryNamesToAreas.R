#'
#' @title Convert ADFG fishery names to canonical assessment names
#'
#' @description Function to convert ADFG fishery names to canonical assessment names.
#'
#' @param x - vector of ADFG fishery names to convert canonical assessment names
#'
#' @return character vector with canonical names
#'
#' @details The original values are converted to the final values using the following table:
#' \itemize{
#' \item{  original:    final    }
#' \item{WBT:         West 166W}
#' \item{EBT:         East 166W}
#' \item{Tanner crab: all EBS  }
#' \item{ Tanner E:    East 166W}
#' \item{ Tanner W:    West 166W}
#' \item{ Tanner:      all EBS  }
#' \item{ Snow crab:   all EBS  }
#' \item{ snow crab:   all EBS  }
#' \item{ Snow:        all EBS  }
#' \item{ snow:        all EBS  }
#' \item{ BSSC:        all EBS  }
#' \item{ BBRKC:       all EBS  }
#' \item{ RKC:         all EBS  }
#' }
#' @export
#'
adfgConvert_FisheryNamesToAreas<-function(x){
  #rename fisheries to canonical forms
  orig<-c("WBT",      "EBT",      "Tanner crab","Tanner E", "Tanner W", "Tanner", "Snow crab","snow crab","Snow",    "snow",    "BSSC",   "BBRKC",  "RKC");
  finl<-c("West 166W","East 166W","all EBS",    "East 166W","West 166W","all EBS","all EBS",  "all EBS",   "all EBS","all EBS" ,"all EBS","all EBS","all EBS");
  x <- wtsUtilities::substituteValues(x,orig,finl);
  return(x);
}
