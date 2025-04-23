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
#' @param age  A numeric vector or column name (if `x` is a data frame). Can be either:
#'   (1) a numeric age between 18 and 100, which will be automatically grouped into NICE-defined age bands (18-35, 35-45, 45-55, 55-65, +65), or
#'   (2) a factor/character/numeric vector already representing the NICE age bands with values 1-5 indicating age bands (18-35, 35-45, 45-55, 55-65, +65).
#' @param male A numeric vector (1=male, 0=female) or column name indicating gender.
#' @param dim.names A vector of dimension names for EQ-5D states (default: c("mo", "sc", "ua", "pd", "ad")).
#' @param bwidth Numeric. Bandwidth for kernel smoothing when using summary scores.
#' @return A vector or data frame with crosswalked EQ-5D-3L utilities.
#' @examples 
#' eqxw_NICE(c(11111, 12345, 32423, 55555), age = c(30, 40, 55, 70), male = c(1, 0, 1, 0))
#' @importFrom stats dnorm weighted.mean
#' @export

eqxw_NICE <- function(x, age, male, dim.names = c("mo", "sc", "ua", "pd", "ad"), bwidth = 0) {
 
  # Load crosswalk data
  pkgenv <- getOption("eq.env")
  eq5d_version <- "5L"
  if (is.null(pkgenv) || is.null(pkgenv$crosswalk_NICE[[eq5d_version]])) {
    stop("Missing NICE crosswalk data in `options(eq.env)`. Please set the environment.")
  }
  crosswalk_data <- pkgenv$crosswalk_NICE[[eq5d_version]]
  
  # Determine input type and construct data
  if (is.data.frame(x)) {
    if (!all(dim.names %in% names(x))) stop("Provided dimension names not found in data.")
    if (!(age %in% names(x)) || !(male %in% names(x))) stop("Age and male must be column names in `x`.")
    
    x$Domain <- apply(x[dim.names], 1, paste, collapse = "")
    x$X_age <- x[[age]]
    x$X_male <- as.numeric(x[[male]])
    
  } else if (is.vector(x)) {
    x <- data.frame(
      Domain = as.character(x),
      X_age = age,
      X_male = male,
      stringsAsFactors = FALSE
    )
  } else {
    stop("`x` must be either a data frame or a character/numeric vector of EQ-5D states.")
  }
  
  # Process age into age bands
  if (is.numeric(x$X_age)) {
    x$X_age <- cut(
      x$X_age,
      breaks = c(1, 18, 35, 45, 55, 65, 100),
      labels = c("1", "1", "2", "3", "4", "5"),
      right = FALSE
    )
    x$X_age <- as.numeric(as.character(x$X_age))
  } else if (is.factor(x$X_age) || is.character(x$X_age)) {
    x$X_age <- as.numeric(as.character(x$X_age))
  }

  if (any(is.na(x$X_age))) stop("Age must be between 18 and 100 or an age band (1 to 5).")
  # Determine type of input: domain or summary score
  if (all(nchar(x$Domain) == 5 & grepl("^[1-5]{5}$", x$Domain))) {
    
    # Case 1: Domain-based
    lookup <- crosswalk_data[, c("Domain", "Output", "X_age", "X_male")]
    x <- merge(x, lookup, by = c("Domain", "X_age", "X_male"), all.x = TRUE)
    
  } else {
    # Case 2: Summary score
    x$X_U5 <- as.numeric(x$Domain)
    lookup <- crosswalk_data[, c("X_U5", "Output", "X_age", "X_male")]
    
    if (bwidth == 0) {
      x <- merge(x, lookup, by = c("X_U5", "X_age", "X_male"), all.x = TRUE)
    } else {
      x$Output <- mapply(function(age, male, u5) {
        subset <- lookup[lookup$X_age == age & lookup$X_male == male, ]
        weights <- dnorm((subset$X_U5 - u5) / bwidth)
        if (length(weights) > 0) {
          weighted.mean(subset$Output, weights, na.rm = TRUE)
        } else {
          NA
        }
      }, x$X_age, x$X_male, x$X_U5)
    }
  }

  return(x$Output)
}
