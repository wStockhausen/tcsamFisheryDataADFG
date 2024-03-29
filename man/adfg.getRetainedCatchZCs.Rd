% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adfg.getRetainedCatchZCs.R
\name{adfg.getRetainedCatchZCs}
\alias{adfg.getRetainedCatchZCs}
\title{Get annual retained catch size compositions by crab fishery from a csv file}
\usage{
adfg.getRetainedCatchZCs(csv, skip = 2, progress = FALSE)
}
\arguments{
\item{csv}{\itemize{
\item the csv file name
}}

\item{skip}{\itemize{
\item number of lines to skip (default=5)
}}

\item{progress}{\itemize{
\item flag to show progress reading csv file
}}
}
\value{
a tibble with columns
\itemize{
\item fishery
\item area
\item sex
\item shell condition
\item year
\item size
\item count
}
}
\description{
Function to get annual retained catch size compositions by crab fishery from a csv file.
}
\details{
Uses \code{readr::read_csv} and \code{stringr::str_sub}.
}
