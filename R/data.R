#' An example patient-level dataset
#'
#' A dataset containing patient-level data in a long format. 
#'
#' \itemize{
#'  \item id: patient id
#'  \item mobility, selfcare, usualact, paindisc, anxietyd: the five EQ-5D domains
#'  \item vas: value of the VAS scale measurememnt
#'  \item fu: follow-up (baseline / follow-up)
#'  \item year_range: time period for the follow-up (2009-2010 / 2010-2011 / 2011-2012); could be used as an alternative follow-up variable
#'  \item month: another alternative follow-up variable (1, ..., 15)
#'  \item surgtype: type of surgery (Cataract / Hernia / Hip / Knee / Veins)
#'  \item gender: patient's gender (Female / Male)
#'  \item age: patient's gender in years
#' }
#'
#' @docType data
#' @keywords datasets
#' @name example_data
#' @usage data(example_data)
#' @format A data frame with 6600 rows and 15 variables
NULL

#' Names of countries with available value sets
#'
#' A list consisting of data frames with 2-letter code, full name and short name for every country, for which the relevant value set is available in the, respectively, 3L and 5L value_sets datasets.
#'
#' @docType data
#' @keywords datasets
#' @name country_codes
#' @usage data(country_codes)
#' @format A list with two elements: 3L and 5L. 
NULL

#' Available 3L and 5L value sets
#'
#' A list consisting of two data frames containing available value sets, respectively for 3L and 5L data. Column names correspond to the short names stored in the country_codes datasets.
#'
#' @docType data
#' @keywords datasets
#' @name value_sets
#' @usage data(value_sets)
#' @format A list with two elements: 3L and 5L. 
NULL