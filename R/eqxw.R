#' @title .pstate5t3
#' @description Takes a 15 x 5 matrix with probabilities per level/dimension, and creates an 3125x243 matrix with probabilities per state
#' @param .EQxwprob 15 x 5 matrix with probabilities per level/dimension, typically saved in .EQxwprob
#' @return An 3125x243 matrix with probabilities per state
.pstate5t3 <- function(.EQxwprob) {
  
  probs5t3 <- matrix(nrow = 5^5, ncol = 3^5)
  
  for (a1 in 1:5)
    for (b1 in 1:3) {
      p1 <- .EQxwprob[b1, a1]
      for (a2 in 1:5)
        for (b2 in 1:3) {
          p2 <- .EQxwprob[b2 + 3, a2]
          for (a3 in 1:5)
            for (b3 in 1:3) {
              p3 <- .EQxwprob[b3 + 6, a3]
              for (a4 in 1:5)
                for (b4 in 1:3) {
                  p4 <- .EQxwprob[b4 + 9, a4]
                  for (a5 in 1:5)
                    for (b5 in 1:3) {
                      p5 <- .EQxwprob[b5 + 12, a5]
                      i <- a5 + 5*(a4-1) + 25*(a3-1) + 125*(a2-1) + 625*(a1-1)
                      j <- b5 + 3*(b4-1) + 9*(b3-1) + 27*(b2-1) + 81*(b1-1)
                      probs5t3[i, j] <- p1*p2*p3*p4*p5
                    }}}}}
  return(probs5t3)
}

#' @title eqxw
#' @description Get crosswalk values
#' @param x A vector of 5-digit EQ-5D-5L state indexes or a matrix/data.frame with columns corresponding to EQ-5D state dimensions
#' @param country String vector indicating country name(s) or country code(s) according to ISO3166 Alpha 2 or Alpha 3
#' @param dim.names A vector of dimension names to identify dimension columns
#' @return A vector of reverse crosswalk values or data.frame with one column per reverse crosswalk set requested.
#' @examples 
#' eqxw(c(11111, 12521, 32123, 55555), 'US')
#' eqxw(make_all_EQ_states('5L'), c('DK', 'US'))
#' @export
eqxw <- function(x, country = NULL, dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  pkgenv <- getOption("eq.env")
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  if(length(dim(x)) == 2) {
    colnames(x) <- tolower(colnames(x))
    if(is.null(colnames(x))) {
      message("No column names")
      if(NCOL(x) == 5) {
        message("Given 5 columns, will assume dimensions in conventional order: MO, SC, UA, PD, AD.")
        colnames(x) <- dim.names
      } 
    }
    if(!all(dim.names %in% colnames(x))) stop("Provided dimension names not available in matrix/data.frame.")
    x <- toEQ5Dindex(x = x, dim.names = dim.names)
  }
  
  country <- .fixCountries(country, EQvariant = '3L')
  if(any(is.na(country))) {
    isnas <- which(is.na(country))
    for(i in isnas)  warning('Country ', names(country)[i], ' not found. Dropped.')
    country <- country[!is.na(country)]
  }
  
  if(length(country)==0) {
    message('No valid countries listed. These value sets are currently available.')
    eqxwr_display()
    stop('No valid countries listed.')
  }
  
  x <- as.integer(x)
  x[!regexpr("^[1-5]{5}$", x)==1] <- NA
  
  if(length(country)>1) {
    names(country) <- country
    return(do.call(cbind, lapply(country, function(count) eqxw(x, count, dim.names))))
  }
  
  xout <- rep(NA, length(x))
  
  xout[!is.na(x)] <- pkgenv$xwsets[match(x[!is.na(x)], pkgenv$states_5L$state), country]
  xout
  
}