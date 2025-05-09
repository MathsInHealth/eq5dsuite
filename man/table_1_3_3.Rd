% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq5d_devlin.R
\name{table_1_3_3}
\alias{table_1_3_3}
\title{Table 1.3.3: Number of observations in the LFS (Level Frequency Score) according to the EQ-5D values}
\usage{
table_1_3_3(df, names_eq5d = NULL, eq5d_version = NULL, country)
}
\arguments{
\item{df}{Data frame with the EQ-5D columns}

\item{names_eq5d}{Character vector of column names for the EQ-5D dimensions}

\item{eq5d_version}{Version of the EQ-5D instrument}

\item{country}{A character string representing the name of the country. 
This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.}
}
\value{
Summary data frame
}
\description{
Table 1.3.3: Number of observations in the LFS (Level Frequency Score) according to the EQ-5D values
}
\examples{
table_1_3_3(
  example_data, 
  names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
  eq5d_version = "3L",
  country = "UK"
)
}
