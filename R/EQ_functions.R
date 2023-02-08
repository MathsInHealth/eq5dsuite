#' @title toEQ5DIndex
#' @description Generate EQ-5D state vector from data.frame/matrix or named vector with dimensions.
#' @param x A data.frame, matrix, or vector containing dimension levels. Should have column names corresponding to the dim.names argument.
#' @param dim.names A vector of dimension names in data.frame/matrix/vector x
#' @return A vector of 5-digit EQ-5D health state indexes.
#' @examples 
#' toEQ5Dindex(c(1,2,3,4,5))
#' 
#' example_data <- as.data.frame(matrix(data = c(1, 2, 3, 4, 5, 
#'                                               5, 4, 3, 2, 1, 
#'                                               3, 2, 1, 2, 3), 
#'                                      ncol = 5, 
#'                                      byrow = TRUE, 
#'                                      dimnames = list(NULL, c("mo", "sc", "ua", "pd", "ad"))))
#' example_data$irrelevant <- c(6,5,3)
#' toEQ5Dindex(example_data)
#' @export
toEQ5Dindex <- function(x, dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  # Matrix or data.frame
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
    return(as.integer(as.matrix(x[, dim.names]) %*% (10^(4:0))))
  }
  if(length(x)<5) stop("Too short vector provided. Stopping")
  if(is.null(names(x))) {
    message("No names provided in vector.")
    if(length(x) == 5) {
      message("Vector of 5 provided, assuming dimensions in conventional order: MO, SC, UA, PD, AD.")
      names(x) <- dim.names
    }
  } else {
    names(x) <- tolower(names(x))
  }
  if(!all(dim.names %in% names(x))) stop("Provided dimension names not in vector names. Stopping.")
  as.integer(x[dim.names] %*% (10^(4:0)))
}

#' @title toEQ5Ddims
#' @description Generate dimension vectors based on state index
#' @param x A vector of 5-digit EQ-5D state indexes.
#' @param dim.names A vector of dimension names to be used as names for output columns.
#' @return A data.frame with 5 columns, one for each EQ-5D dimension, with names from dim.names argument.
#' @examples 
#' toEQ5Ddims(c(12345, 54321, 12321))
#' @export
toEQ5Ddims <- function(x, dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  # coerce to integer
  x <- as.integer(x)
  # Remove items outside of bounds
  x[!regexpr("^[1-5]{5}$", x)==1] <- NA
  if(sum(is.na(x))) warning(paste0("Provided vector contained ", sum(is.na(x)), " items not conforming to 5-digit numbers with exclusively digits in the 1-5 range."))
  as.data.frame(outer(X = x, Y = structure(.Data = 10^(4:0), .Names = dim.names), FUN = "%/%") %% 10)
}

#' @title make_all_EQ_states
#' @description Make a data.frame with all health states defined by dimensions
#' @param version Either "3L" or "5L", to signify whether 243 or 3125 states should be generated
#' @param dim.names A vector of dimension names to be used as names for output columns.
#' @param append_index Boolean to indicate whether a column of 5-digit EQ-5D health state indexes should be added to output.
#' @return A data.frame with 5 columns and 243 (-3L) or 3125 (-5L) health states
#' @examples 
#' make_all_EQ_states('3L')
#' @export
make_all_EQ_states <- function(version = "5L", dim.names = c("mo", "sc", "ua", "pd", "ad"), append_index = FALSE) {
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  if(!is.vector(version)) stop("version argument is not a vector of length 1")
  if(length(version)>1) {
      message("version argument provided length of more than 1, first element is used.")
      version <- version[[1]]
    }
  if(!toupper(version) %in% c("5L", "3L")) stop('version argument should be either "3L" or "5L"')
  xout <- do.call(expand.grid, structure(rep(list(1:ifelse(version == "5L", 5, 3)), 5),.Names = dim.names[5:1]))[,5:1]
  if(append_index) xout$state <- toEQ5Dindex(xout, dim.names)
  xout
}

#' @title make_all_EQ_indexes
#' @description Make a vector containing all 5-digit EQ-5D indexes for -3L or -5L version.
#' @param version Either "3L" or "5L", to signify whether 243 or 3125 states should be generated
#' @param dim.names A vector of dimension names to be used as names for output columns.
#' @return A vector with 5-digit state indexes for all 243 (-3L) or 3125 (-5L) EQ-5D health states
#' @examples 
#' make_all_EQ_indexes('3L')
#' @export
make_all_EQ_indexes <- function(version = "5L", dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  toEQ5Dindex(do.call(make_all_EQ_states, as.list(match.call()[-1])))
}

#' @title EQ_dummies
#' @description Make a data.frame of all EQ-5D dummies relevant for e.g. regression modeling. 
#' @param df data.frame containing EQ-5D health states.
#' @param version Either "3L" or "5L", to signify EQ-5D instrument version
#' @param dim.names A vector of dimension names to be used as names for output columns.
#' @param drop_level_1 If set to FALSE, dummies for level 1 will be included. Defaults to TRUE.
#' @param add_intercept If set to TRUE, a column containing 1s will be appended. Defaults to FALSE.
#' @param incremental If set to TRUE, incremental dummies will be produced (e.g. MO = 3 will give mo2 = 1, mo3 = 1). Defaults to FALSE.
#' @param append Optional string to be appended to column names.
#' @param prepend Optional string to be prepended to column names.
#' @param return_df If set to TRUE, data.frame is returned, otherwise matrix. Defaults to TRUE.
#' @return A data.frame of dummy variables 
#' @examples 
#' make_dummies(make_all_EQ_states('3L'), '3L')
#' 
#' make_dummies(df = make_all_EQ_states('3L'), 
#'              version =  '3L', 
#'              incremental = TRUE, 
#'              add_intercept = TRUE, 
#'              prepend = "d_")
#' @export
make_dummies <- function(df, version = "5L", dim.names = c("mo", "sc", "ua", "pd", "ad"), drop_level_1 = TRUE, add_intercept = FALSE, incremental = FALSE, prepend = NULL, append = NULL, return_df = TRUE) {
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  if(!length(dim(df) == 2)) stop("Need to provide matrix or data.frame of 2 dimensions.")

  colnames(df) <- tolower(colnames(df))
  if(is.null(colnames(df))) {
    message("No column names")
    if(NCOL(df) == 5) {
      message("Given 5 columns, will assume dimensions in conventional order: MO, SC, UA, PD, AD.")
      colnames(df) <- dim.names
    } 
  }
  if(!all(dim.names %in% colnames(df))) stop("Provided dimension names not available in matrix/data.frame.")
  # intercept_col <- NULL
  # if(add_intercept) intercept_col <- data.frame(intercept = rep(1, NROW(df)))
  
  vers <- ifelse(version == '5L', 5, 3)
  
  startlevel <- ifelse(drop_level_1, 2, 1)
  if(incremental) {
    tmp <- 1*lower.tri(diag(vers), TRUE)[,startlevel:vers]  
  } else {
    tmp <- diag(vers)[,startlevel:vers]
  }
  
  tmpout <- do.call(cbind, lapply(df[,dim.names], function(x) tmp[x,      ]))
  colnames(tmpout) <- as.vector(t(outer(paste0(prepend, dim.names), paste0(startlevel:vers, append), FUN = paste0)))
  if(add_intercept) tmpout <- cbind(intercept = 1, tmpout)
  if(return_df) tmpout <- as.data.frame(tmpout)
  tmpout
}