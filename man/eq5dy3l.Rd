% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EQ_functions.R
\name{eq5dy3l}
\alias{eq5dy3l}
\title{eq5dy3l}
\usage{
eq5dy3l(x, country = NULL, dim.names = c("mo", "sc", "ua", "pd", "ad"))
}
\arguments{
\item{x}{A vector of 5-digit EQ-5D-Y3L state indexes or a matrix/data.frame with columns corresponding to EQ-5D-Y3L state dimensions.}

\item{country}{String vector indicating country name(s) or country code(s) according to ISO3166 Alpha 2 or Alpha 3.}

\item{dim.names}{A character vector specifying the names of the EQ-5D-Y3L dimensions.  Default is c("mo", "sc", "ua", "pd", "ad").}
}
\value{
A vector of EQ-5D-Y3L values or data.frame with one column for each value set requested.
}
\description{
Get EQ-5D-Y3L index values from individual responses to the five dimensions of the EQ-5D-Y3L.
}
\examples{
eq5dy3l(x = c(11111, 12321, 33333), country = 'SI') # Slovenia -Y3L value set
eq5dy3l(make_all_EQ_states('3L'), c('Germany', 'Spain')) # Germany and Spain -Y3L value sets 
}
