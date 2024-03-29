% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adfgConvert_DateYYYYMMDDtoFisheryYear.R
\name{adfgConvert_DateYYYYMMDDtoFisheryYear}
\alias{adfgConvert_DateYYYYMMDDtoFisheryYear}
\title{Convert a vector of dates as yyyy-mm-dd to numeric fishery years}
\usage{
adfgConvert_DateYYYYMMDDtoFisheryYear(x)
}
\arguments{
\item{x}{\itemize{
\item vector of date strings in format "yyyy-mm-dd"
}}
}
\value{
numeric vector of fishery years (e.g., 1990 represents 1990/91)
}
\description{
Function to convert a vector of dates as yyyy-mm-dd to numeric fishery years.
}
\details{
The fishery year extends from July 1, yyyy to June 30, yyyy+1,so
if mm<7, then year=yyyy-1, otherwise year=yyyy.
}
