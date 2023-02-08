#' @title .pstate3t5
#' @description Takes a N x 25 matrix with probabilities per level/dimension, and creates an N * 3125 matrix with probabilities per state
#' @param PPP N x 25 matrix with probabilities per level/dimension, typically created by .EQrxwprobs
#' @return An N * 3125 matrix with probabilities per state
.pstate3t5 <-function(PPP) {
  vallst <- as.vector(as.matrix(expand.grid(21:25, 16:20, 11:15, 6:10, 1:5))[, 5:1])
  t1l<- lapply(seq_len(dim(PPP)[2]), function(x) PPP[, x])
  do.call(cbind, lapply(seq_len(3125), function(i) {
    t1l[[vallst[[i]]]]*
      t1l[[vallst[[i+3125]]]]*
      t1l[[vallst[[i+6250]]]]*
      t1l[[vallst[[i+9375]]]]*
      t1l[[vallst[[i+12500]]]]
  }))
}

#' @title .EQxwrprob
#' @description Takes a matrix of parameters for reverse crosswalk model, returns 243 x 25 matrix of state/level transition probabilities.
#' @param par Matrix of model parameters 
#' @return An 243 * 25 matrix with probabilities for state level transitions.
.EQxwrprob <- function(par=NULL) {
  if(is.null(par)) stop('No parameters provided.')
  par <- as.matrix(par)
  
  # Logistic function by way of hyperbolic tangent
  lp <- function(eta) 0.5+0.5*tanh(eta*0.5)
  
  # Parameters for transitions between levels 1-->2, 2-->3, 3-->4, and 4-->5, per dimension
  Zx<-t(par[1:4,])            
  # Parameters for adjacent dimensions (10), age, age^2, and gender
  Bx<-par[5:14,]
  
  tmp <- diag(3)[,2:3]
  Y <- - do.call(cbind, lapply(as.list(expand.grid(1:3, 1:3, 1:3, 1:3, 1:3)[,5:1]), function(x) tmp[x,]))
  
  # Model matrix: dummies for EQ-5D-3L (i.e. mo2, mo3, sc2 ...ad2, ad3), age, age^2, gender
  # Y <- - as.matrix(cbind(inp[, 1:10], inp[,11]/10, (inp[,11]/10)^2, inp[,12])) # Note the "-" first; these are negatives
  do.call(cbind, lapply(1:5, function(i) {
    # inverse logit, first the level transitions
    # t(Zx[, rep(i, NROW(inp)])) creates a N * 4 matrix with the corresponding level transition parameters in the four columns
    tmp <- lp(Zx[rep.int(i,243),] + 
                # Y %*% Bx[,i] is a matrix multiplication of the model matrix and the corresponding coefficients
                (Y %*% Bx[,i])[,1])
    # subtract previous level from next, creating a matrix with column 1 representing p for level 1, column 2 for level 2, etc.
    cbind(tmp,1)-cbind(0, tmp)
  }))
}

#' @title eqxwr
#' @description Get reverse crosswalk values
#' @param x A vector of 5-digit EQ-5D state indexes or a matrix/data.frame with columns corresponding to EQ-5D state dimensions
#' @param country String vector indicating country name(s) or country code(s) according to ISO3166 Alpha 2 or Alpha 3
#' @param dim.names A vector of dimension names to identify dimension columns
#' @return A vector of reverse crosswalk values or data.frame with one column per reverse crosswalk set requested.
#' @examples 
#' eqxwr(c(11111, 12321, 32123, 33333), 'US')
#' eqxwr(make_all_EQ_states('3L'), c('DK', 'US'))
#' @export
eqxwr <- function(x, country = NULL, dim.names = c("mo", "sc", "ua", "pd", "ad")) {
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
  
  country <- .fixCountries(country)
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
  x[!regexpr("^[1-3]{5}$", x)==1] <- NA
  
  if(length(country)>1) {
    names(country) <- country
    return(do.call(cbind, lapply(country, function(count) eqxwr(x, count, dim.names))))
  }
  
  xout <- rep(NA, length(x))
  
  xout[!is.na(x)] <- pkgenv$xwrsets[match(x[!is.na(x)], pkgenv$states_3L$state), country]
  xout
  
}

#' @title eqxwr_display
#' @description Display available reverse crosswalk value sets.
#' @param return_df If set to TRUE, the function will return information on the names of the available value sets in a data.frame. Defaults to FALSE
#' @param version Version of the EQ-5D instrument. Can take values 5L (default) or 3L.
#' @return Default NULL, if return_df == TRUE, returns a data.frame with the displayed information.
#' @examples 
#' # Display available value sets.
#' eqxwr_display
#' @export
eqxwr_display <- function(return_df = FALSE, version = "5L") {
  pkgenv <- getOption("eq.env")

  # read off version name
  version <- toupper(version)
  user_defined_str <- paste0("user_defined_", version)
  
  message(str_c("Available national value sets for ", version, " version:"))
  .prettyPrint(pkgenv$country_codes[[version]], 'l')
  if(NROW(pkgenv[[user_defined_str]])) {
    message('User-defined value sets:')    
    .prettyPrint(pkgenv[[user_defined_str]], 'l')
  } else {
    message('No user-defined value sets available.')
  }
  if(return_df) 
    return(rbind(cbind(Type = 'Value set', pkgenv$country_codes[[version]]), if(NROW(pkgenv[[user_defined_str]])) cbind(Type = 'User-defined', pkgenv[[user_defined_str]]) else NULL))
  retval <- NULL
}

#' @title eqxwr_add
#' @description Add user-defined EQ-5D value set to reverse crosswalk options.
#' @param df A data.frame or file name pointing to csv file. The contents of the data.frame or csv file should be exactly two columns: state, containing a list of all 3125 (for 5L) or 243 (for 3L) EQ-5D health state vectors, and a column of corresponding utility values, with a suitable name.
#' @param version Version of the EQ-5D instrument. Can take values 5L (default) or 3L.
#' @param country Optional string. If not NULL, will be used as a country description for the user-defined value set.
#' @param description Optional string. If not NULL, will be used as a descriptive text for the user-defined value set. 
#' @param code2L Optional string. If not NULL, will be used as the two-digit code for the value set. Must be different from any existing national value set code.
#' @return True/False, indicating success or error.
#' @examples 
#' # make nonsense value set
#' new_df <- data.frame(state = make_all_EQ_indexes(), TEST = runif(3125))
#' # Add as value set for Fantasia
#' eqxwr_add(new_df, version = "5L", country = 'Fantasia')
#' @importFrom utils read.csv
#' @export
eqxwr_add <- function(df, version = "5L", country = NULL, description = NULL, code2L = NULL, code3L = NULL, three_letter_code = NULL) {
  pkgenv <- getOption("eq.env")
  if(class(df) == 'character') {
    if(!file.exists(df)) stop('File named ', df, ' does not appear to exist. Exiting.')
    df <- utils::read.csv(file = df, stringsAsFactors = F)
  } 
  if(!NCOL(df) == 2) stop('df should have exactly two columns.')
  class(df[, 1]) <- 'integer'
  
  # read off version-dependent parameters
  j <- if (version == "5L") 5 else 3
  states_str <- paste0("states_", version)
  eq5d_str <- paste0("EQ-5D-", version)
  uservsets_str <- paste0("uservsets", version)
  user_defined_str <- paste0("user_defined_", version)
  
  # check correct number of rows
  n <- j^5
  if(!NROW(df) == n) stop(paste0("df should have exactly ", n, " rows."))
  # message(NROW(pkgenv$states_5L))
  # check all states present
  if(!all(df[,1] %in% pkgenv[[states_str]]$state)) stop(paste0("First column of df should contain all ", eq5d_str, " health state indexes exactly once."))
  df <- df[match(pkgenv[[states_str]]$state, df[,1]),]
  if(!is.null(country)) {
    if(length(country)>1) {
      warning('Length of country argument > 1, first item used.')
      country <- country[1]
    }
  }
  if(!is.null(description)) {
    if(length(description)>1) {
      warning('Length of description argument > 1, first item used.')
      description <- description[1]
    }
  }
  thisName <- colnames(df)[2]
  if(thisName %in% colnames(pkgenv[[uservsets_str]])) {
    warning(paste0("New country name already in user-defined ", eq5d_str, " value set list."))
    return(0)
  }
  
  if(any(is.na(df[,2]*-1.1))) stop("Non-numeric values in second column of df.")
  # No problems
  tmp <- pkgenv[[uservsets_str]]
  tmp[, thisName] <- df[, 2]
  
  assign(x = uservsets_str, value = tmp, envir = pkgenv)
  
  tmp <- data.frame(Name = ifelse(is.null(description), NA, description), 
                    Name_short = ifelse(is.null(country), NA, country), 
                    ISO3166Alpha2 = colnames(df)[2], 
                    ISO3166Alpha3 = NA)
  
  if(user_defined_str %in% names(pkgenv)) tmp <- rbind(pkgenv[[user_defined_str]], tmp)
  
  assign(x = user_defined_str, value = tmp, envir = pkgenv)
  
  # assign(x = 'xwsets', value = pkgenv$probs %*% cbind(as.matrix(.vsets5L[, -1, drop = F]), as.matrix(pkgenv$uservsets5L[,-1, drop = F])), envir = pkgenv)
  
  yesno <- 'n'
  if(file.exists(file.path(pkgenv$cache_path, "cache.Rdta"))) {
    save(list = ls(envir = pkgenv), envir = pkgenv, file = file.path(pkgenv$cache_path, 'cache.Rdta'))
    message('User-defined value set saved.')
  } else {
    message('If you wish for the user-defined reverse value set to be persistent, a file needs to be saved to the hard-drive.')
    message('The file would be saved to: ', pkgenv$cache_path)
    message('Please type "y", "Y", "Yes", or "yes" to allow this file to be saved. All other inputs will be interpreted as "no".')
    yesno = readline(prompt = "Save? (Yes/No) : ")  
    if(tolower(yesno) %in% c("yes", "y")) {
      message('Persistent data will be saved.')
      if(!dir.exists(pkgenv$cache_path)) dir.create2(pkgenv$cache_path, recursive = TRUE)
      save(list = ls(envir = pkgenv), envir = pkgenv, file = file.path(pkgenv$cache_path, 'cache.Rdta'))
      message('User-defined value set saved.')
      
    } else {
      message('Persistent data will not be saved; any user-defined value sets will disappear when this session is closed.')
    }
  }
  
  .fixPkgEnv()
  1
}

#' @title eqxwr_drop
#' @description Drop user-defined EQ-5D value set to reverse crosswalk options.
#' @param version Version of the EQ-5D instrument. Can take values 5L (default) or 3L.
#' @param country Optional string. If NULL, a list of current user-defined value sets will be provided for selection. If set, and matching an existing user-defined value set, a prompt will be given as to whether the value set should be deleted. 
#' @return True/False, indicating success or error.
#' @examples 
#' # make nonsense value set
#' new_df <- data.frame(state = make_all_EQ_indexes(), TEST = runif(3125))
#' # Add as value set for Fantasia
#' eqxwr_add(new_df, version = "5L", country = 'Fantasia')
#' # Drop value set for Fantasia
#' eqxwr_drop('Fantasia')
#' @export
eqxwr_drop <- function(version = "5L", country = NULL) {
  pkgenv <- getOption("eq.env")
  if(length(country)>1) {
    message('Length of country argument larger than 1, only the first element will be used.')
    country <- country[[1]]
  }
  
  # read off version-dependent parameters
  version <- toupper(version)
  user_defined_str <- paste0("user_defined_", version)
  uservsets_str <- paste0("uservsets", version)
  
  states_str <- paste0("states_", version)
  eq5d_str <- paste0("EQ-5D-", version)
  
  
  
  if(!user_defined_str %in% names(pkgenv)) {
    message(paste0("No user-defined value sets exist among the ", version, " datasets. Exiting."))
    return(FALSE)
  } else {
    udc <- pkgenv[[user_defined_str]]
    if(!is.null(country)) {
      tmp <- which(toupper(as.matrix(udc)) == toupper(country), arr.ind = T)
      
      if(nrow(tmp)) {
        country <- udc$ISO3166Alpha2[tmp[1,1]]
        yesno = readline(prompt = paste0('Are you sure you want to delete user-defined country "', country,'" from the ', version, 'value sets? ([Y]es/[N]o) : '))
        if(tolower(yesno) %in% c("yes", "y")) {
          message('Removing ', country, ' from user-defined value sets.')
          udc <- udc[-tmp[1,1],]
          if(NROW(udc)) {
            assign(x = user_defined_str, value = udc, envir = pkgenv)
          } else {
            rm(user_defined_str, envir = pkgenv)
          }
          tmp <- pkgenv[[uservsets_str]]
          tmp[, which(colnames(tmp) == country)] <- NULL
          assign(x = uservsets_str, value = tmp, envir = pkgenv)
          .fixPkgEnv()
          return(TRUE)
        } else {
          message("OK. Exiting.")
          return(FALSE)
        }
      } else {
        message('User-defined value set identified as ', country, ' not found among the ', version, ' value sets.')
      }
    }
  }
  
  udc <- udc[, 3:1]
  colnames(udc) <- c('Code', 'Name', 'Description')
  message('Please enter which value set you wish to drop, or enter any other value to exit.')
  .prettyPrint(udc)
  yesno = trimws(toupper(readline(prompt = paste0('Please enter value set name, description, or code: '))))
  tmp <- which(toupper(as.matrix(udc)) == yesno, arr.ind = T)
  if(NROW(tmp)) return(eqxwr_drop(yesno))
  message('Exiting.')
  return(FALSE)
}