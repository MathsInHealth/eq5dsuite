% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq5d_aux.R
\name{.pchc_plot_by_dim}
\alias{.pchc_plot_by_dim}
\title{Wrapper to generate Paretian Classification of Health Change plot by dimension}
\usage{
.pchc_plot_by_dim(plot_data, ylab, title, cols, text_rotate = FALSE)
}
\arguments{
\item{plot_data}{A data frame containing information to plot, with columns for name (the dimensions to plot), p (the proportion of the total data falling into each dimension), and fu (the follow-up).}

\item{ylab}{The label for the y-axis.}

\item{title}{The plot title.}

\item{cols}{A vector of colors to use for the bars.}

\item{text_rotate}{A logical indicating whether to rotate the text labels for the bars.}
}
\value{
A ggplot object containing the PCHC plot.
}
\description{
This internal function plots Paretian Classification of Health Change (PCHC) by dimension. 
The input is a data frame containing the information to plot, and the plot will contain bars representing 
the proportion of the total data that falls into each dimension, stacked by covariate.
The wrapper is used in Figures 2.2-2.4.
}
\examples{
df <- data.frame(
  name = rep(c("Dim1", "Dim2"), each = 2),
  p = c(0.6, 0.4, 0.7, 0.3),
  groupvar = rep(c("Group A", "Group B"), 2)
)
colors <- c("Group A" = "#1b9e77", "Group B" = "#d95f02")
.pchc_plot_by_dim(df, ylab = "Proportion", title = "Example Plot", cols = colors)
}
