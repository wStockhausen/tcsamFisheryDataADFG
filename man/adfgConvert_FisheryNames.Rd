% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adfgConvert_FisheryNames.R
\name{adfgConvert_FisheryNames}
\alias{adfgConvert_FisheryNames}
\title{Convert ADFG fishery names to canonical assessment names}
\usage{
adfgConvert_FisheryNames(x)
}
\arguments{
\item{x}{- vector of ADFG fishery names to convert canonical assessment names}
}
\value{
character vector with canonical names
}
\description{
Function to convert ADFG fishery names to canonical assessment names.
}
\details{
* original names: "Tanner crab","Tanner E","Tanner W","Tanner","snow crab","snow","BBRKC","RKC"
* final names:    "TCF",        "TCF",     "TCF",     "TCF",   "SCF",      "SCF", "RKF",  "RKF"
}
