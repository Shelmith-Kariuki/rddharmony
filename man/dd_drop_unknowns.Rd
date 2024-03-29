% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dd_drop_unknowns.R
\name{dd_drop_unknowns}
\alias{dd_drop_unknowns}
\title{dd_drop_unknowns}
\usage{
dd_drop_unknowns(data)
}
\arguments{
\item{data}{The data to be harmonised}
}
\value{
A data frame without \code{Unknowns} in cases where the value of \code{Unknown} is equal to \verb{reported total - calculated total}.
}
\description{
If reported total - calculated total == "Unknown" , then set Unknown to 0.
}
