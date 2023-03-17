#' Replace NULL names with default values
#'
#' This function takes in a list of parameters, which would be column names of the input data frame, and checks if they are null. Any nulls are replaced with default values, and the updated list of parameters is returned.
#'
#' @param df a data frame; only used/supplied if levels_fu needs to be defined
#' @param ... a list of parameters consisting of any/all of `names_eq5d`, `name_fu`, `levels_fu`, `eq5d_version`, and `name_vas`.
#' @return a list of parameters with null entries replaced with default values.
#' @examples
#' .get_names(names_eq5d = c("mo", "sc", "ua", "pd", "ad"))
#' .get_names(names_eq5d = NULL, eq5d_version = NULL, name_vas = NULL)
#' .get_names(df = example_data, name_fu = NULL, levels_fu = NULL)
#' @export
#' 
.get_names <- function(df = NULL, ...) {
  
  # read off input parameters and their supplied names
  args <- list(...)
  names_list <- names(args)
  
  # check names_eq5d
  if ("names_eq5d" %in% names_list) {
    names_eq5d <- args$names_eq5d
    if (is.null(names_eq5d)) {
      message("Argument `names_eq5d` not supplied. Default column names will be used: mo, sc, ua, pd, ad")
      names_eq5d <- c("mo", "sc", "ua", "pd", "ad")
    }
    args[["names_eq5d"]] <- names_eq5d
  }
  # check name_fu
  # if name_fu is specified, so must be levels_fu
  if ("name_fu" %in% names_list) {
    name_fu <- args$name_fu
    levels_fu <- args$levels_fu
    if (is.null(name_fu)) {
      message("Argument `name_fu` not supplied. Default column name will be used: fu")
      name_fu <- "fu"
    }
    args[["name_fu"]] <- name_fu
    # check also levels of fu
    if (is.null(levels_fu)) {
      message(str_c("No ordering of time suppled. The time variable will be factorised according to the order in the data frame."))
      levels_fu <- df %>% select(!!sym(name_fu)) %>% unique() %>% pull()
    }
    args[["levels_fu"]] <- levels_fu
  }
  # check eq5d_version
  if ("eq5d_version" %in% names_list) {
    eq5d_version <- args$eq5d_version
    if (is.null(eq5d_version)) {
      message(str_c("No EQ-5D version was provided. 5L version will be used."))
      eq5d_version <- "5L"
    }
    args[["eq5d_version"]] <- eq5d_version
  }
  # check name_vas
  if ("name_vas" %in% names_list) {
    name_vas <- args$name_vas
    if (is.null(name_vas)) {
      message("Argument `name_vas` not supplied. Default column name will be used: vas")
      name_vas <- "vas"
    }
    args[["name_vas"]] <- name_vas
  }
  
  return(args)
}

#' Calculate the Level Frequency Score (LFS)
#'
#' This function calculates the Level Frequency Score (LFS) for a given EQ-5D state and a specified version of EQ-5D.
#' If at least one domain contains a missing entry, the whole LFS is set to be NA.
#'
#' @param s A character vector representing the EQ-5D state, e.g. 11123.
#' @param eq5d_version A character string specifying the version of EQ-5D, i.e. 3L or 5L.
#' @return A character vector representing the calculated LFS.
#' @examples
#' .get_lfs("333", "3L") # returns 003
#' .get_lfs("333", "5L") # returns 00300
#' .get_lfs("12345", "5L") # returns 11111
#' @export
#' 
.get_lfs <- function(s, eq5d_version) {
  
  # for any eq5d version need to count 1s, 2s and 3s
  lfs <- str_c(str_count(s, "1"), str_count(s, "2"), str_count(s, "3"))
  # if 5L, also add count of 4s and 5s
  if (eq5d_version == "5L")
    lfs <- str_c(lfs, str_count(s, "4"), str_count(s, "5"))
  
  return(lfs)
}

#' Add utility values to a data frame
#'
#' This function adds utility values to a data frame based on a specified version of EQ-5D and a country name.
#'
#' @param df A data frame containing the state data. The state must be included in the data frame as a character vector under the column named `state`.
#' @param eq5d_version A character string specifying the version of EQ-5D, i.e. 3L or 5L.
#' @param country A character string representing the name of the country. This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return A data frame with an additional column named `utility` containing the calculated utility values. If the input country name is not found in the country_codes dataset, a list of available codes is printed, and subsequentyl an error message is displayed and the function stops.
#' @examples
#' df <- data.frame(state = c("11111", "11123", "32541"))
#' .add_utility(df, "3L", "GB")
#' \dontrun{
#' .add_utility(df, "5L", "GB")
#' }
#' @export
#' 
.add_utility <- function(df, eq5d_version, country) {
  
  pkgenv <- getOption("eq.env")
  
  # check whether the valuation for this country code exists
  country_code <- .fixCountries(countries = country, EQvariant = eq5d_version)
  if (is.na(country_code)) {
    message('No valid countries listed. These value sets are currently available.')
    eqxwr_display(version = eq5d_version)
    stop('Stopping.')
  }
  
  # country identifiable; proceed to extract the data
  vs <- pkgenv[[paste0("vsets", eq5d_version, "_combined")]] %>%
    select(state, !!(sym(country_code))) %>%
    rename(utility = !!quo_name(country_code))
  
  # merge with df
  df <- merge(df, vs, all.x = TRUE) 
  
  return(df)
}

#' Data checking/preparation: EQ-5D variables
#'
#' This function prepares a data frame for analysis by extracting, processing, and adding columns for EQ-5D variables, including state, LSS (Level Sum Score), LFS (Level Frequency Score) and utility.
#' 
#' @param df a data frame of EQ-5D scores
#' @param names character vector of length 5 with names of EQ-5D variables in the data frame. The variables should be in an integer format.
#' @param add_state logical indicating whether the EQ-5D state should be added
#' @param add_lss logical indicating whether the LSS (Level Sum Score) should be added
#' @param add_lfs logical indicating whether the LFS (Level Frequency Score) should be added
#' @param add_utility logical indicating whether the utility should be added
#' @param eq5d_version character indicating the version of the EQ-5D questionnaire to use (either "3L" or "5L")
#' @param country character indicating the country to retrieve the quality of life score for
#' @return a modified data frame with EQ-5D domain columns renamed to default names, and, if necessary, with added columns for state, LSS, LFS, and/or utility. If any of the checks fail (e.g. EQ-5D columns are not in an integer format), an error message is displayed and the function is stopping.
#' @examples
#' set.seed(1234)
#' df <- data.frame(mo = sample(1:5, 3), sc = sample(1:5, 3), 
#'   ua = sample(1:5, 3), pd = sample(1:5, 3), ad = sample(1:5, 3))
#' .prep_eq5d(df, names = c("mo", "sc", "ua", "pd", "ad"), 
#'   add_state = TRUE, add_lss = TRUE)
#' .prep_eq5d(df, names = c("mo", "sc", "ua", "pd", "ad"),
#'   add_state = TRUE, add_lss = TRUE, add_lfs = TRUE, add_utility = TRUE,
#'   eq5d_version = "5L", country = "Denmark")
#' @export
#' 
.prep_eq5d <- function(df, names,
                       add_state = FALSE,
                       add_lss = FALSE,
                       add_lfs = FALSE,
                       add_utility = FALSE,
                       eq5d_version = NULL,
                       country = NULL) {
  
  # confirm correct length
  if (length(names) != 5)
    stop("Argument dim_names not of length 5. Stopping.")
  
  # confirm numeric format
  df_eq5d <- df %>%
    # leave only required columns
    select(!!!syms(names)) 
  if (sum(sapply(df_eq5d, function(x) all(is.numeric(x)))) != 5)
    stop("All columns must be in a numeric format. Stopping.")
  # confirm integers only
  if (sum(sapply(df_eq5d, function(x) all(floor(x[!is.na(x)]) == x[!is.na(x)]))) != 5)
    stop("Colums can only contain integers or NAs. Stopping.")
  
  # confirm EQ-5D version if required
  if (!is.null(eq5d_version))
    if (!(tolower(eq5d_version) %in% c("3l", "5l")))
      stop("EQ-5D version can only be 3L, 3l, 5L or 5l. Stopping.")
  
  # all checks passed; proceed to the algorithm
  df <- df %>%
    # rename columns
    rename(mo = !!quo_name(names[1]),
           sc = !!quo_name(names[2]),
           ua = !!quo_name(names[3]),
           pd = !!quo_name(names[4]),
           ad = !!quo_name(names[5]))
  
  # add additional columns if required
  if (add_state)
    df <- df %>% mutate(state = str_c(mo, sc, ua, pd, ad))
  if (add_lss)
    df <- df %>% mutate(lss = mo + sc + ua + pd + d)
  if (add_lfs)
    df <- df %>% mutate(lfs = .get_lfs(s = state, eq5d_version = eq5d_version))
  if (add_utility)
    df <- .add_utility(df = df, eq5d_version = eq5d_version, country = country) 
  
  return(df)
}

#' Data checking/preparation: follow-up variable
#' 
#' This function prepares the follow-up (FU) variable for analysis by giving it a default name (`fu`) and factorising
#'
#' @param df A data frame.
#' @param name Column name in the data frame that contains follow-up information.
#' @param levels Levels to factorise the FU variable into.
#' @return A data frame with the follow-up variable renamed as "fu" and factorised.
#' @examples
#' df <- data.frame(id = c(1, 1, 2, 2),
#'   visit = c("baseline", "follow-up", "baseline", "follow-up"))
#' .prep_fu(df = df, name = "visit", levels = c("baseline", "follow-up"))
#' @export
#' 
.prep_fu <- function(df, name = NULL, levels = NULL) {
  
  df <- df %>%
    # rename columns
    rename(fu = !!quo_name(name)) %>%
    # factorise
    mutate(fu = factor(fu, levels = levels))
  
  return(df = df)
}

#' Data checking/preparation: VAS variable
#' 
#' The function prepares the data for VAS (Visual Analogue Scale) analyses. 
#' 
#' @param df A data frame.
#' @param name Column name in the data frame that holds the VAS score. The column can only contain integers or NAs
#' @return A modified data frame with the VAS score renamed to "vas". If any checks fail (e.g. column is not numeric), an error message is displayed and the function is stopping.
#' @examples
#' df <- data.frame(vas_score = c(20, 50, 80, NA, 100))
#' .prep_vas(df = df, name = "vas_score")
#' \dontrun{
#' df <- data.frame(vas_score = c(20.5, 50, 80, NA, 100))
#' .prep_vas(df = df, name = "vas_score")
#' }
#' @export
#' 
.prep_vas <- function(df, name) {
  
  # extract data
  v <- df[, name]
  # remove NAs
  v <- v[!is.na(v)]
  # confirm numeric format
  if (!is.numeric(v))
    stop("VAS column must be in a numeric format. Stopping.")
  # confirm integers only
  if (!all(floor(v) == v))
    stop("VAS column can only contain integers or NAs. Stopping.")
  
  # all checks passed; proceed to the algorithm
  df <- df %>%
    # rename columns
    rename(vas = !!quo_name(name)) 
  
  # return value
  return(df)
}

#' Check the uniqueness of groups
#'
#' This function takes a data frame `df` and a vector of columns `group_by`, and checks whether the combinations of values in the columns specified by `group_by` are unique. If the combinations are not unique, a warning message is printed.
#'
#' @param df A data frame.
#' @param group_by A character vector of column names in `df` that specify the groups to check for uniqueness.
#' @return NULL
#' @examples
#' 
#' df <- data.frame(id = c(1, 1, 1, 1, 2, 2), 
#'                  fu = rep(c("baseline", "follow-up"), 3), 
#'                  value = rnorm(6))
#' .check_uniqueness(df, c("id", "fu"))
#' @export
.check_uniqueness <- function(df, group_by) {
  
  retval <- df %>%
    group_by_at(group_by) %>%
    summarise(n = n(), .groups = "drop") %>%
    select(n) %>%
    unique() %>%
    pull()
  
  if (all.equal(1, retval) != TRUE)
    message(str_c("Warning: there are non-unique ", 
                  str_c(group_by, collapse = "-"),  
                  " combinations."))
}

#' Get the mode of a vector.
#'
#' This function calculates the mode of a numeric or character vector. 
#' If there are multiple modes, the first one is returned. 
#' The code is taken from an \href{https://www.tutorialspoint.com/r/r_mean_median_mode.htm}{R help page}.
#'
#' @param v A numeric or character vector.
#' @return The mode of `v`.
#' @examples
#' .getmode(c(1, 2, 3, 3))
#' .getmode(c("a", "b", "b", "c"))
#' @export
#' 
.getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Wrapper for the repetitive code in function_table_2_1. Data frame summary
#' 
#' This internal function summarises a data frame by grouping it based on the variables specified in the 'group_by' argument and calculates the frequency of each group. The output is used in Table 2.1
#' 
#' @param df A data frame
#' @param group_by A character vector of variables in `df` to group by. Should contain 'eq5d' and 'fu'.
#' @return A summarised data frame with groups defined by `eq5d` and `fu` variables, the count of observations in each group, and the frequency of each group.
#' @examples
#' set.seed(1234)
#' df <- data.frame(eq5d = rep(rnorm(5), 2),
#'                  fu = rep(c(1, 0, 1, 0, 1), 2))
#' .summary_table_2_1(df, c("eq5d", "fu"))
#' @export
#' 
.summary_table_2_1 <- function(df, group_by) {
  
  retval <-  df %>%
    group_by_at(group_by) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(eq5d, fu) %>%
    mutate(freq = n / sum(n))
  
  return(retval)
}

#' Wrapper to determine Paretian Classification of Health Change (PCHC)
#' 
#' This internal function determines Paretian Classification of Health Change (PCHC) for each combination of the variables specified in the `group_by` argument. 
#' It is used in the code for table_2_4-table_2_5 and figure_2_1-figure_2_4. 
#' An EQ-5D health state is deemed to be `better` than another if it is better on at least one dimension and is no worse on any other dimension.
#' An EQ-5D health state is deemed to be `worse` than another if it is worse in at least one dimension and is no better in any other dimension.
#' @param df A data frame with EQ-5D states and follow-up variable. The dataset is assumed to be have been ordered correctly.
#' @param level_fu_1 Value of the first (i.e. earliest) follow-up. Would normally be defined as levels_fu[1].
#' @param add_noprobs Logical value indicating whether to include a separate classification for those without problems (default is FALSE)
#' @return A data frame with PCHC value for each combination of the grouping variables. 
#' If 'add_noprobs' is TRUE, a separate classification for those without problems is also included.
#' @examples
#' df <- data.frame(id = c(1, 1, 2, 2),
#'                  fu = c(1, 2, 1, 2),
#'                  mo = c(1, 1, 1, 1),
#'                  sc = c(1, 1, 5, 1),
#'                  ua = c(1, 1, 4, 3),
#'                  pd = c(1, 1, 1, 3),
#'                  ad = c(1, 1, 1, 1))
#' .pchc(df, level_fu_1 = 1, add_noprobs = TRUE)
#' @export
#' 
.pchc <- function(df, level_fu_1, add_noprobs = FALSE) {
  
  levels_eq5d <- c("mo", "sc", "ua", "pd", "ad")
  
  # initialise positive, negative & zero differences
  df <- df %>%
    mutate(better = 0, worse = 0)
  
  for (dom in levels_eq5d) {
    # new column names
    dom_diff <- str_c(dom, "_diff")
    
    # calculate difference: previous - current
    df <- df %>%
      mutate(!!sym(dom_diff) := lag(!!sym(dom)) - !!sym(dom)) %>%
      # replace entry from 1st follow-up with NA
      mutate(!!sym(dom_diff) := case_when(fu == level_fu_1 ~ NA_real_,
                                          TRUE ~ !!sym(dom_diff))) %>%
      mutate(
        # contribution to positive differences
        better = better + (!!sym(dom_diff) > 0),
        # contribution to negative differences
        worse = worse + (!!sym(dom_diff) < 0))
  }
  
  # classify each combination
  # every change is classified compared to the next line
  df <- df %>%
    mutate(state = 
             case_when(
               # no change
               better == 0 & worse == 0 ~ "No change",
               # at least one dimension better & nothing worse
               better > 0 & worse == 0 ~ "Improve",
               # at least one dimension worse & nothing better
               worse > 0 & better == 0 ~ "Worsen",
               # at least one dimension better & at least one dimension worse
               better > 0 & worse > 0 ~ "Mixed change"
             )) 
  
  # separate classification for those without problems if required
  if (add_noprobs) {
    df <- df %>%
      # no change & 11111 at the second timepoint means 11111 at the first timepoint
      # so enough to check for 11111 at the classifications stage
      mutate(noprobs = 
               (mo == 1 & sc == 1 & ua == 1 & pd == 1 & ad == 1)) %>%
      mutate(state_noprobs = case_when((state == "No change" & noprobs) ~ "No problems",
                                         TRUE ~ state))
  }
  
  return(df)
}

#' Wrapper to summarise a continuous variable by follow-up (FU) 
#' 
#' This function summarizes a continuous variable for each follow-up (FU) and calculates various statistics such as mean, standard deviation, median, mode, kurtosis, skewness, minimum, maximum, range, and number of observations. It also reports the total sample size and the number (and proportion) of missing values for each FU. 
#' The input `df` must contain an ordered FU variable and the continuous variable of interest. 
#' The name of the continuous variable must be specified using `name_v`. 
#' The wrapper is used in Table 3.1 (for VAS) or Table 4.2 (for EQ-5D utility)
#'
#' @param df A data frame containing the FU and continuous variable of interest. The dataset must contain an ordered `fu` variable.
#' @param name_v A character string with the name of the continuous variable in `df` to be summarised.
#' @return Data frame with one row for each statistic and one column for each FU. 
#' @examples
#' df <- data.frame(fu = c(1,1,2,2,3,3), 
#'                  vas = c(7,8,9,NA,7,6))
#' .summary_cts_by_fu(df, name_v = "vas")
#' @export
#' 
.summary_cts_by_fu <- function(df, name_v) {
  
  ### prepare dataset ###
  
  df <- df %>%
    rename(v = !!quo_name(name_v))
  
  # summarise non-NA values
  summary <- df %>%
    filter(!is.na(v)) %>%
    group_by(fu) %>%
    summarise(Mean = mean(v),
              `Standard error` = sd(v) / sqrt(n()),
              Median = median(v),
              Mode = .getmode(v),
              `Standard deviation` = sd(v),
              Kurtosis = moments::kurtosis(v),
              Skewness = moments::skewness(v),
              Minimum = min(v),
              Maximum = max(v),
              Range = max(v) - min(v),
              Observations = n())
  
  # summarise total and NA values
  summary_total_na <- df %>%
    group_by(fu) %>%
    summarise(`Missing (n)` = sum(is.na(v)),
              `Total sample` = n()) %>%
    mutate(`Missing (%)` = `Missing (n)` / `Total sample`)
  
  # combine and tidy up
  retval <- merge(summary, summary_total_na) %>%
    pivot_longer(-fu) %>%
    pivot_wider(id_cols = name, names_from = fu, values_from = value)
  
  return(retval)
}

#' Summary wrapper for Table 4.3
#' 
#' This internal function creates a summary of the data frame for Table 4.3. 
#' It groups the data by the variables specified in `group_by` and calculates various summary statistics.
#' 
#' @param df A data frame.
#' @param group_by A character vector of names of variables by which to group the data.
#' @return A data frame with the summary statistics.
#' @examples
#' df <- data.frame(group = c("A", "A", "B", "B"), 
#'                  utility = c(0.5, 0.7, 0.8, 0.9))
#' .summary_table_4_3(df, group_by = "group")
#' @export
#' 
.summary_table_4_3 <- function(df, group_by) {
  
  retval <- df %>%
    group_by_at(group_by) %>%
    summarise(Mean = mean(utility, na.rm = TRUE),
              `Standard error` = sd(utility, na.rm = TRUE) / sqrt(sum(!is.na(utility))),
              Median = median(utility, na.rm = TRUE),
              `25th` = quantile(utility, probs = 0.25, na.rm = TRUE),
              `75th` = quantile(utility, probs = 0.75, na.rm = TRUE),
              N = sum(!is.na(utility)),
              Missing = sum(is.na(utility)), 
              .groups = "drop")
  
  return(retval)
}

#' Summary wrapper for Table 4.4
#' 
#' This internal function creates a summary of the data frame for Table 4.4. 
#' It groups the data by the variables specified in `group_by` and calculates various summary statistics.
#' 
#' @param df A data frame.
#' @param group_by A character vector of names of variables by which to group the data.
#' @return A data frame with the summary statistics.
#' @examples
#' df <- data.frame(group = c("A", "A", "B", "B"), 
#'                  utility = c(0.5, 0.7, 0.8, 0.9))
#' .summary_table_4_4(df, group_by = "group")
#' @export
#' 
.summary_table_4_4 <- function(df, group_by) {
  
  retval <- df %>%
    group_by_at(group_by) %>%
    summarise(Mean = mean(utility, na.rm = TRUE),
              `Standard error` = sd(utility, na.rm = TRUE) / sqrt(sum(is.na(utility))),
              `25th Percentile` = quantile(utility, probs = 0.25, na.rm = TRUE),
              `50th Percentile (median)` = median(utility, na.rm = TRUE),
              `75th Percentile` = quantile(utility, probs = 0.75, na.rm = TRUE),
              Missing = sum(is.na(utility)),
              .groups = "drop"
    )
  
  return(retval)
}

#' Wrapper to calculate summary mean with 95\% confidence interval
#' 
#' This internal function calculates summary mean and 95\% confidence interval of the utility variable, which can also be grouped.
#' The function is used in Figures 4.2-4.4.
#'
#' @param df A data frame containing a `utility` column.
#' @param group_by A character vector of column names to group by.
#' @return A data frame with the mean, lower bound, and upper bound of the 95% confidence interval of `utility` grouped by the `group_by` variables.
#' examples
#' df <- data.frame(group = c("A", "A", "B", "B"), 
#'                  utility = c(0.5, 0.7, 0.8, 0.9))
#' .summary_mean_ci(df, group_by = "group")
#' @export
#'
.summary_mean_ci <- function(df, group_by) {
  
  retval <- df %>%
    group_by_at(group_by) %>%
    filter(!is.na(utility)) %>%
    summarise(mean = mean(utility), 
              se = sd(utility) / sqrt(n())) %>%
    mutate(ci_lb = mean - 1.96 * se, ci_ub = mean + 1.96 * se) %>%
    select(-se)
  
  return(retval)
}

#' Generate colours for PCHC figures
#'
#' This internal function generates a vector of colours based on the specified base colour. 
#' Currently only green and orange colours are implemented. 
#' The wrapper is used in Figures 2.2-2.4.
#'
#' @param col A character string specifying the base colour. Only "green" or "orange" is accepted.
#' @param n A positive integer specifying the number of colours to generate.
#' @return A vector of colours generated based on the specified base colour and number of colours.
#' @examples
#' # generate 10 colours for base colour "green"
#' .gen_colours("green", 10)
#' # generate 7 colours for base colour "orange"
#' .gen_colours("orange", 7)
#' @export
#'
.gen_colours <- function(col, n) {
  retval <- if (col == "green")
    colorRampPalette(c("#99FF99", "#006600"))(n) else 
      if (col == "orange")
        colorRampPalette(c("#FFCC99", "#663300"))(n)
  
  return(retval)
}

#' Modify ggplot2 theme
#'
#' @param p ggplot2 plot
#' @return ggplot2 plot with modified theme
#' @export
.modify_ggplot_theme <- function(p) {
  # set ggplot2 theme
  p <- p + theme_bw() + theme(
    # remove vertical gridlines
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # remove horisontal minor gridlines
    panel.grid.minor.y = element_blank(),
    # remove x-axis ticks
    axis.ticks.x = element_blank(),
    # centre plot title
    plot.title = element_text(hjust = 0.5),
    # remove legend title
    legend.title = element_blank(),
    # move legend to bottom
    legend.position = "bottom")

return(p)

}

#' Wrapper to generate Paretian Classification of Health Change (PCHC) plot by dimension
#'
#' This internal function plots Paretian Classification of Health Change (PCHC) by dimension. The input is a data frame containing the information to plot, and the plot will contain bars representing the proportion of the total data that falls into each dimension, stacked by covariate.
#' The wrapper is used in Figures 2.2-2.4.
#'
#' @param plot_data A data frame containing information to plot, with columns for name (the dimensions to plot), p (the proportion of the total data falling into each dimension), and fu (the follow-up).
#' @param ylab The label for the y-axis.
#' @param title The plot title.
#' @param cols A vector of colors to use for the bars.
#' @param text_rotate A logical indicating whether to rotate the text labels for the bars.
#' @return A ggplot object containing the PCHC plot.
#' @examples
#' plot_data <- data.frame(name = c("Dimension 1", "Dimension 2"),
#' p = c(0.5, 0.5),
#' fu = c("Covariate A", "Covariate B"))
#' cols <- c("#99FF99", "#006600", "#FFCC99", "#663300")
#' .pchc_plot_by_dim(plot_data, "Proportion", "Example PCHC Plot", cols)
#' @export
#' 
.pchc_plot_by_dim <- function(plot_data, ylab, title, cols, text_rotate = FALSE) {
  
  p <- ggplot(plot_data, aes(x = name, y = p, fill = fu)) + 
    # bar chart
    geom_bar(stat = "identity", position = "dodge") + 
    # manipuilate x-axis
    scale_x_discrete(name = "") + 
    # manipulate y-axis
    scale_y_continuous(name = ylab,
                       expand = expansion(mult = c(0, 0.2)),
                       labels = scales::percent_format()) +
    # title
    ggtitle(title) +
    # manipulate legend
    scale_fill_manual(values = cols)
  
  # add percentages
  if (text_rotate) { 
    p <- p + geom_text(aes(label = scales::percent(p, accuracy = 0.1)), 
                       position = position_dodge(width = 0.9),
                       hjust = -0.1, angle = 90)} else {
                         p <- p + geom_text(aes(label = scales::percent(p, accuracy = 0.1)), 
                                            position = position_dodge(width = 0.9),
                                            vjust = -0.5)
                       }
  return(p)
}