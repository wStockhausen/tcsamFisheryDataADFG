% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adfg.calcRetainedCatchZCs.R
\name{adfg.calcRetainedCatchZCs}
\alias{adfg.calcRetainedCatchZCs}
\title{Calculate expanded annual retained catch size compositions by crab fishery, area, year, sex and shell condition}
\usage{
adfg.calcRetainedCatchZCs(tblRetAB, tblRetZCsRaw)
}
\arguments{
\item{tblRetAB}{- dataframe with retained catch abundance and biomass by fishery, area, year, and sex}

\item{tblRetZCsRaw}{- "raw" retained catch size compositions (counts)by fishery, area, year, sex, shell condition and size}
}
\value{
a datarame with columns
* fishery
* area
* sex
* shell condition
* year
* size
* abundance (in thousands of crab)
}
\description{
Function to calculate expanded annual retained catch size compositions by crab fishery, area, year, sex and shell condition.
}
\details{
Uses \code{readr::read_csv} and \code{stringr::str_sub}.
}
