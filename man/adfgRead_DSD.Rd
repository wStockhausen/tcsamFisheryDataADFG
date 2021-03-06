% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adfgRead_DSD.R
\name{adfgRead_DSD}
\alias{adfgRead_DSD}
\title{Extract dockside data (DSD) as a tibble from a csv file}
\usage{
adfgRead_DSD(csv = "TANNER-1990-2018_dockside.csv", date_format = "yyyy-mm-dd")
}
\arguments{
\item{csv}{- csv filename with dockside data}

\item{date_format}{- string ("yyyy-mm-dd" or "mm-dd-yyyy") indicating date format}
}
\value{
a tibble with columns named "fishery","fishery_code","area","year","adfg","sampdate",
           "spcode","sex","maturity","shell","size","legal","numcrab"
}
\description{
Function to extract dockside data as a tibble from a csv file.
}
\details{
Uses functions \code{readr::read_csv}, \code{stringr::str_sub}.
}
