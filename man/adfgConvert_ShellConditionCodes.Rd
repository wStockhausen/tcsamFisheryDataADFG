% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adfgConvert_ShellConditionCodes.R
\name{adfgConvert_ShellConditionCodes}
\alias{adfgConvert_ShellConditionCodes}
\title{Convert ADFG shell condition codes to character labels}
\usage{
adfgConvert_ShellConditionCodes(x)
}
\arguments{
\item{x}{- vector of shell condition codes to convert to character labels}
}
\value{
character vector with labels
}
\description{
Function to convert ADFG shell condition codes to character labels.
}
\details{
* shell condition codes: -9,0,1,2,3,4,5,9
* labels: "undetermined","new shell","new shell","new shell","old shell","old shell","old shell","new shell"
}
