% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq5d_devlin.R
\name{table_3_1}
\alias{table_3_1}
\title{Table 3.1: EQ-5D values: by timepoints}
\usage{
table_3_1(
  df,
  names_eq5d = NULL,
  name_fu = NULL,
  levels_fu = NULL,
  eq5d_version = NULL,
  country
)
}
\arguments{
\item{df}{Data frame with the EQ-5D and follow-up columns}

\item{names_eq5d}{Character vector of column names for the EQ-5D dimensions}

\item{name_fu}{Character string for the follow-up column}

\item{levels_fu}{Character vector containing the order of the values in the follow-up column. 
If NULL (default value), the levels will be ordered in the order of appearance in df.}

\item{eq5d_version}{Version of the EQ-5D instrument}

\item{country}{A character string representing the name of the country. 
This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.}
}
\value{
Summary data frame
}
\description{
Table 3.1: EQ-5D values: by timepoints
}
\examples{
table_3_1(
  example_data,
  names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
  name_fu = "time",
  levels_fu = c('Pre-op', 'Post-op'),
  eq5d_version = "3L",
  country = "UK"
)
}
