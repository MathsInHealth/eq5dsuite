% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq5d_aux.R
\name{.summary_table_4_3}
\alias{.summary_table_4_3}
\title{Summary wrapper for Table 4.3}
\usage{
.summary_table_4_3(df, group_by)
}
\arguments{
\item{df}{A data frame.}

\item{group_by}{A character vector of names of variables by which to group the data.}
}
\value{
A data frame with the summary statistics.
}
\description{
This internal function creates a summary of the data frame for Table 4.3. 
It groups the data by the variables specified in `group_by` and calculates various summary statistics.
}
\examples{
df <- data.frame(group = c("A", "A", "B", "B"), 
                 utility = c(0.5, 0.7, 0.8, 0.9))
.summary_table_4_3(df, group_by = "group")
}
