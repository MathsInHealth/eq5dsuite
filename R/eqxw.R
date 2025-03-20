#' @title .pstate5t3
#' @description Takes a 15 x 5 matrix with probabilities per level/dimension, and creates an 3125x243 matrix with probabilities per state
#' @param probs 15 x 5 matrix with probabilities per level/dimension, typically saved in .EQxwprob
#' @return An 3125x243 matrix with probabilities per state
.pstate5t3 <-  function(probs = .EQxwprob) {
  
  allst5l <- make_all_EQ_states()
  allst3l <- make_all_EQ_states(version = '3L')
  
  t(Reduce('*', lapply(0:4, function(i)    probs[i*3+allst3l[, i+1],allst5l[,i+1]])))
}



#' @title eqxw
#' @description Get crosswalk values
#' @param x A vector of 5-digit EQ-5D-5L state indexes or a matrix/data.frame with columns corresponding to EQ-5D state dimensions
#' @param country String vector indicating country names or  ISO3166 Alpha 2 / 3 country codes.
#' @param dim.names A vector of dimension names to identify dimension columns
#' @return A vector of reverse crosswalk values or data.frame with one column per reverse crosswalk set requested.
#' @examples 
#' eqxw(c(11111, 12521, 32123, 55555), 'US')
#' eqxw(make_all_EQ_states('5L'), c('DK', 'US'))
#' @export
eqxw <- function(x, country = NULL, dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  do.call(eq5d, c(as.list(match.call())[-1],version = "xw"))
}

#' @title eqxw_NICE
#' @description Crosswalks EQ-5D-5L responses to EQ-5D-3L utilities using NICE's mapping.
#' @param x A vector of 5-digit EQ-5D-5L states (domain scores) or a summary score.
#' @param age A numeric vector or column name indicating respondent age.
#' @param male A numeric vector (1=male, 0=female) or column name indicating gender.
#' @param dim.names A vector of dimension names for EQ-5D states (default: c("mo", "sc", "ua", "pd", "ad")).
#' @param bwidth Numeric. Bandwidth for kernel smoothing when using summary scores.
#' @return A vector or data frame with crosswalked EQ-5D-3L utilities.
#' @examples 
#' eqxw_NICE(c(11111, 12321, 32123, 33333), age = c(30, 40, 55, 70), male = c(1, 0, 1, 0))
#' eqxw_NICE(df, age = "age", male = "male")
#' @export

eqxw_NICE <- function(x, age, male, dim.names = c("mo", "sc", "ua", "pd", "ad"), bwidth = 0) {
 
  # # Load environment
  # pkgenv <- getOption("eq.env")
  # if (is.null(pkgenv$crosswalk_data)) stop("Crosswalk data is missing in `pkgenv`. Please check your system data.")
  # 
  # # Convert `x` to the correct format (either a vector or extracted from a data frame)
  # if (is.data.frame(x)) {
  #   if (!all(dim.names %in% colnames(x))) stop("Provided dimension names are not available in `x`.")
  #   x$Domain <- apply(x[dim.names], 1, paste, collapse = "")
  #   x_input <- x$Domain
  # } else if (is.numeric(x) || is.character(x)) {
  #   x_input <- as.character(x)  # Ensure x is character for domain lookup
  # } else {
  #   stop("`x` must be either a data frame or a vector of EQ-5D states.")
  # }
  # 
  # # Extract age & gender data (supports both vector input and column names)
  # if (is.data.frame(x)) {
  #   if (!(age %in% colnames(x)) || !(male %in% colnames(x))) stop("Age and Male must be valid column names in `x`.")
  #   x$X_age <- as.numeric(x[[age]])
  #   x$X_male <- as.numeric(x[[male]])
  # } else {
  #   x <- data.frame(Domain = x_input, X_age = as.numeric(age), X_male = as.numeric(male))
  # }
  # 
  # # Map age to NICE-defined age bands
  # x$X_age <- cut(x$X_age, breaks = c(1, 18, 35, 45, 55, 65, 100),
  #                labels = c("1", "1", "2", "3", "4", "5"), right = FALSE)
  # x$X_age <- as.numeric(as.character(x$X_age))  # Convert factor to numeric
  # 
  # if (any(is.na(x$X_age))) stop("Age must be between 18 and 100 or an age band (1 to 5).")
  # 
  # # Retrieve crosswalk data
  # crosswalk_data <- pkgenv$crosswalk_data
  # 
  # # Determine if summary scores are provided
  # if (all(nchar(x_input) == 5 & grepl("^[1-5]+$", x_input))) {
  #   # Case 1: Domain-based crosswalk
  #   crosswalk_data <- crosswalk_data[, c("Domain", "Output", "X_age", "X_male")]
  #   x <- merge(x, crosswalk_data, by = c("Domain", "X_age", "X_male"), all.x = TRUE)
  # 
  # } else {
  #   # Case 2: Summary score crosswalk
  #   x$X_U5 <- as.numeric(x_input)
  #   crosswalk_data <- crosswalk_data[, c("X_U5", "Output", "X_age", "X_male")]
  # 
  #   if (bwidth == 0) {
  #     # Exact match
  #     x <- merge(x, crosswalk_data, by = c("X_U5", "X_age", "X_male"), all.x = TRUE)
  #   } else {
  #     # Approximate match using kernel smoothing
  #     x$Output <- mapply(function(age, male, u5) {
  #       subset_data <- crosswalk_data[crosswalk_data$X_age == age & crosswalk_data$X_male == male, ]
  #       weights <- dnorm((subset_data$X_U5 - u5) / bwidth)
  #       if (length(weights) > 0) {
  #         return(weighted.mean(subset_data$Output, weights, na.rm = TRUE))
  #       } else {
  #         return(NA)
  #       }
  #     }, x$X_age, x$X_male, x$X_U5)
  #   }
  # }
  # 
  # return(x$Output)
}
