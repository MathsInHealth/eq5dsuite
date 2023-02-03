#' Table 2.1: Frequency of levels by dimension of 'some problems'
#' 
#' @param df Data frame with the EQ-5D and follow-up columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param eq5d_version Version of the EQ-5D instrument
#' @return Summary data frame.
#' @examples
#' table_2_1(df = example_data)
#' table_2_1(df = example_data, name_fu = "month")
#' @export
#' 
table_2_1 <- function(df, 
                      names_eq5d = NULL,
                      name_fu = NULL,
                      levels_fu = NULL,
                      eq5d_version = NULL) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, 
                     names_eq5d = names_eq5d, 
                     name_fu = name_fu, levels_fu = levels_fu,
                     eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d, name_fu)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, eq5d_version = eq5d_version)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  
  ### analysis ###
  
  # reshape into a long format
  df <- df %>%
    pivot_longer(cols = -fu, names_to = "eq5d", values_to = "value")
  
  # complete case dataset: remove NA values
  df_cc <- df %>%
    filter(!is.na(value))
  
  # summary: individual levels
  summary_dim <- .summary_table_2_1(df = df_cc, group_by = c("eq5d", "value", "fu")) %>%
    # tidy up the group category
    rename(level = value) %>%
    mutate(level = as.character(level))
  
  # summary: total
  summary_total <-  .summary_table_2_1(df = df_cc, group_by = c("eq5d", "fu")) %>%
    mutate(level = "Total")
  
  # summary: some problems and change
  summary_problems <- df_cc %>%
    # keep only relevant values
    filter(value != 1) %>%
    group_by(eq5d, fu) %>%
    summarise(n = n(), .groups = "drop")
  
  # merge with summary_total to calculate percentages
  summary_problems <- merge(summary_problems, 
                            summary_total %>% select(eq5d, fu, n), 
                            by = c("eq5d", "fu"),
                            suffix = c("", "_total")) %>%
    mutate(freq = n / n_total) %>%
    # tidy up
    select(-n_total)
  # define the group category for subsequent linking
  suffix <- if (eq5d_version == "3L") "2+3" else "2+3+4+5"
  summary_problems <- summary_problems %>% 
    mutate(level = 
             str_c("Number reporting some problems (levels ", suffix, ")"))
  
  # change in numbers reporting problems since previous time point
  summary_problems_change <- summary_problems %>%
    # remove redundant columns
    select(-freq, -level) %>%
    # impose order on time
    arrange(eq5d, factor(fu, levels = levels_fu)) %>%
    group_by(eq5d) %>%
    # generate lagged n
    mutate(n_prev = lag(n)) %>%
    # calculate change
    mutate(n_change = n - n_prev) %>%
    # add percentage
    mutate(freq = n_change / n_prev) %>%
    # tidy up
    select(-n, -n_prev) %>%
    rename(n = n_change) %>%
    # define the group category for subsequent linking
    mutate(level = "Change in numbers reporting problems")
  
  # summary: rankings
  summary_rank <- summary_problems_change %>%
    select(-n, -level) %>%
    group_by(fu) %>%
    # remove NA entries
    filter(!is.na(freq)) %>%
    # define rank
    mutate(n = rank(freq)) %>%
    # define the group category for subsequent linking
    mutate(level = "Rank of dimensions in terms of % changes")
  
  # summary: missing data
  summary_na <- df %>%
    group_by(eq5d, fu) %>%
    summarise(n = sum(is.na(value)), n_total = n(), .groups = "drop") %>%
    # calculate percentage
    mutate(freq = n / n_total) %>%
    # define the group category for subsequent linking
    mutate(level = "Missing data") %>%
    # tidy up
    select(-n_total)
  
  # combine and tidy up
  levels_eq5d <- c("mobility", "selfcare", "usualact", "paindisc", "anxietyd")
  retval <- bind_rows(
    summary_dim, 
    summary_total, 
    summary_problems, 
    summary_problems_change, 
    summary_rank, 
    summary_na) %>%
    # reshape into a wide format 
    mutate(eq5d = factor(eq5d, levels = levels_eq5d)) %>%
    arrange(eq5d) %>%
    pivot_wider(id_cols = level, 
                names_from = c("eq5d", "fu"), 
                values_from = c("n", "freq"),
                names_glue = "{.value}_{fu}_{eq5d}") %>%
    # arrange rows
    arrange(level = factor(level, 
                           levels = c(sort(unique(summary_dim$level)), 
                                      unique(summary_total$level),
                                      unique(summary_problems$level),
                                      unique(summary_problems_change$level),
                                      unique(summary_rank$level),
                                      unique(summary_na$level)))) %>%
    # arrange columns
    select(level, !!!syms(apply(expand.grid(c("n", "freq"), as.character(levels_fu), levels_eq5d), 
                                1, paste, collapse = "_")))
  
  # return value
  return(retval)
}

#' Table 2.3: Prevalence of the 10 most frequently observed self-reported health states
#' 
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param n Number of most frequently observed states to display (default 10)
#' @return Summary data frame
#' @examples
#' table_2_3(df = example_data)
#' table_2_3(df = example_data, n = 5)
#' table_2_3(df = example_data, eq5d_version = "3L")
#' @export
#'
table_2_3 <- function(df, 
                      names_eq5d = NULL,
                      eq5d_version = NULL,
                      n = 10) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(names_eq5d = names_eq5d, 
                     eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- names_eq5d
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, eq5d_version = eq5d_version, add_profile = TRUE) %>%
    select(profile)
  
  ### analysis ####
  
  # all profiles
  profiles_all <- df %>%
    # non-NA values
    filter(!is.na(profile)) %>%
    # group by profile
    group_by(profile) %>%
    # add count
    summarise(n = n()) %>%
    # add percentage
    mutate(p = n / sum(n)) %>%
    arrange(-n) %>%
    # cumulative percentage
    mutate(cum_p = cumsum(p))
  
  # most frequent n non-NA profiles
  profiles_top <- profiles_all %>%
    filter(!is.na(profile)) %>%
    slice_head(n = n)
  
  # worst profile
  profile_worst <- if (eq5d_version == "5L") "55555" else "33333"
  profiles_worst <- profiles_all %>%
    filter(profile == profile_worst) %>%
    mutate(cum_p = 1)
  
  # missing data
  profile_na <- df %>%
    summarise(n = sum(is.na(profile)), p = sum(is.na(profile)) / n()) %>%
    mutate(profile = "Missing")
  
  # combine and tidy up
  retval <- bind_rows(profiles_top, profiles_worst, profile_na) %>%
    rename(`Health state` = profile,
           Frequency = n,
           Percentage = p,
           `Cumulative percentage` = cum_p)
  
  # return value
  return(retval)
}

#' Table 2.4: Changes in health according to the PCHC (Paretian Classification of Health Change)
#' 
#' @param df Data frame with the EQ-5D, grouping, id and follow-up columns
#' @param name_id Character string for the patient id column
#' @param name_groupvar Character string for the grouping column
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @return Summary data frame
#' @examples
#' table_2_4(df = example_data, name_groupvar = "surgtype", name_id = "id")
#' @export
#'
table_2_4 <- function(df,
                      name_id,
                      name_groupvar,
                      names_eq5d = NULL,
                      name_fu = NULL, 
                      levels_fu = NULL) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, 
                     names_eq5d = names_eq5d, 
                     name_fu = name_fu, levels_fu = levels_fu)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  # check existence of columns 
  names_all <- c(name_groupvar, name_id, names_eq5d, name_fu)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- df %>%
    rename(id = !!quo_name(name_id),
           groupvar = !!quo_name(name_groupvar))
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # sort by id - groupvar - time
  df <- df %>%
    arrange(id, groupvar, fu)
  # check uniqueness of id-groupvar-fu combinations
  .check_uniqueness(df, group_by = c("id", "groupvar", "fu"))
  
  ### analysis ###
  
  # calculate change
  df <- .pchc(df = df, group_by = c("id", "groupvar")) %>%
    filter(!is.na(profile)) %>%
    mutate(profile = 
             factor(profile,
                    levels = c("Improve", "Mixed change", "No change", "Worsen")))
  
  # summarise by groupvar, fu & profile
  summary_dim <- df %>%
    group_by(groupvar, fu, profile) %>%
    summarise(n = n()) %>%
    mutate(p = n / sum(n)) %>%
    ungroup()
  
  # summarise totals
  summary_total <- summary_dim %>%
    group_by(groupvar, fu) %>%
    summarise(n = sum(n), p = sum(p), .groups = "drop") %>%
    # add label
    mutate(profile = "Grand Total") 
  
  # combine & tidy up
  retval <- bind_rows(summary_dim, summary_total) %>%
    # reshape into a long format to subsequently impose order on columns in pivot_wider
    pivot_longer(cols = n:p) %>%
    # finally reshape wider
    pivot_wider(id_cols = profile, 
                names_from = c(groupvar, fu, name), 
                values_from = value,
                # fill NAs with 0
                values_fill = list(value = 0))
  
  # return value
  return(retval)
}

#' Table 2.5: Changes in health according to the PCHC, taking account of those with no problems
#' 
#' @param df Data frame with the EQ-5D, grouping, id and follow-up columns
#' @param name_id Character string for the patient id column
#' @param name_groupvar Character string for the grouping column
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @return Summary data frame
#' @examples
#' table_2_5(df = example_data, name_groupvar = "surgtype", name_id = "id")
#' @export
#'
table_2_5 <- function(df, 
                      name_id,
                      name_groupvar,
                      names_eq5d = NULL,
                      name_fu = NULL,
                      levels_fu = NULL) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, 
                     names_eq5d = names_eq5d, 
                     name_fu = name_fu, levels_fu = levels_fu)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  # check existence of columns 
  names_all <- c(name_groupvar, name_id, names_eq5d, name_fu)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- df %>%
    rename(id = !!quo_name(name_id),
           groupvar = !!quo_name(name_groupvar))
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # sort by id - groupvar - time
  df <- df %>%
    arrange(id, groupvar, fu)
  # check uniqueness of id-groupvar-fu combinations
  .check_uniqueness(df, group_by = c("id", "groupvar", "fu"))
  
  ### analysis ###
  
  # calculate change
  df <- .pchc(df = df, group_by = c("id", "groupvar"), add_noprobs = TRUE) %>%
    filter(!is.na(profile))
  
  # separate out those with problems & calculate percentages
  summary_by_probs_status <- df %>%
    mutate(profile_noprobs = case_when((profile_noprobs == "No problems") ~ "No problems",
                                       TRUE ~ "Total with problems")) %>%
    group_by(groupvar, fu, profile_noprobs) %>%
    summarise(n = n()) %>%
    mutate(p = n / sum(n))
  
  # summarise classes within those with problems
  summary_with_probs <- df %>%
    filter(profile_noprobs != "No problems") %>%
    group_by(groupvar, fu, profile_noprobs) %>%
    summarise(n = n()) %>%
    mutate(p = n / sum(n))
  
  # combine & tidy up
  retval <- bind_rows(summary_with_probs, summary_by_probs_status) %>%
    # impose order
    mutate(profile_noprobs = factor(profile_noprobs, 
                                    levels = c("Improve", "Mixed change", "No change", "Worsen",
                                               "Total with problems", "No problems"))) %>%
    # reshape into a long format to subsequently impose order on columns in pivot_wider
    pivot_longer(cols = n:p) %>%
    # finally reshape into a wide format
    pivot_wider(id_cols = profile_noprobs, 
                names_from = c(groupvar, fu, name), 
                values_from = value,
                # fill NAs with 0
                values_fill = list(value = 0)) %>%
    arrange(profile_noprobs)
  
  # return value
  return(retval)
}

#' Table 2.6: Changes in levels in each dimension, percentages of total and of type of change
#' 
#' @param df Data frame with the EQ-5D, id and follow-up columns
#' @param name_id Character string for the patient id column
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @return Summary data frame
#' @examples
#' table_2_6(df = example_data, name_id = "id")
#' @export
#'
table_2_6 <- function(df, 
                      name_id,
                      names_eq5d = NULL,
                      name_fu = NULL,
                      levels_fu = NULL) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, 
                     names_eq5d = names_eq5d, 
                     name_fu = name_fu, levels_fu = levels_fu)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  # check existence of columns 
  names_all <- c(name_id, names_eq5d, name_fu)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- df %>%
    rename(id = !!quo_name(name_id))
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # sort by id - time
  df <- df %>%
    arrange(id, fu)
  # check uniqueness of id-fu combinations
  .check_uniqueness(df, group_by = c("id", "fu"))
  
  ### analysis ###
  
  # calculate change
  levels_eq5d <- c("mobility", "selfcare", "usualact", "paindisc", "anxietyd")
  
  df_diff <- df %>%
    # reshape into a long format
    pivot_longer(cols = mobility:anxietyd, names_to = "domain") %>%
    select(domain, id, fu, value) %>%
    arrange(domain, id, fu) %>%
    group_by(domain, id) %>%
    # calculate lagged difference
    mutate(diff = lag(value) - value,
           str = str_c(lag(value), "-", value)) %>%
    # remove NAs
    filter(!is.na(diff)) %>%
    # classify the difference
    mutate(diff = case_when(diff > 0 ~ "Better",
                            diff < 0 ~ "Worse",
                            diff == 0 ~ "No change")) %>%
    # order domain column
    mutate(domain = factor(domain, levels = levels_eq5d)) %>%
    arrange(domain)
  
  # % total
  p_total <- df_diff %>%
    group_by(domain, diff, str) %>%
    summarise(n = n()) %>%
    group_by(domain) %>%
    mutate(p = n / sum(n)) %>%
    mutate(type = "% Total")
  
  # % type
  p_type <- df_diff %>%
    group_by(domain, diff, str) %>%
    summarise(n = n()) %>%
    mutate(p = n / sum(n)) %>%
    mutate(type = "% Type")
  
  # combine and tidy up
  retval <- rbind(p_total, p_type) %>%
    select(domain, type, diff, str, p) %>%
    pivot_wider(id_cols = c(diff, str), names_from = c(domain, type), values_from = p,
                values_fill = list(p = 0)) %>%
    # arrange rows
    mutate(diff = factor(diff, levels = c("No change", "Better", "Worse"))) %>%
    arrange(diff, str) %>%
    # arrange rows
    select(diff, str, 
           !!!syms(apply(expand_grid(levels_eq5d, c("% Total", "% Type")), 
                         1, paste, collapse = "_")))
  
  # return value
  return(retval)
}

#' Table 2.7: Summary statistics for the EQ-5D values by all the different LSSs (Level Sum Scores)
#' 
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary data frame
#' @examples
#' table_2_7(df = example_data, country = "United States")
#' table_2_7(df = example_data, eq5d_version = "3L", country = "United States")
#' @export
#'
table_2_7 <- function(df, 
                      names_eq5d = NULL,
                      eq5d_version = NULL,
                      country){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, 
                   add_profile = TRUE, 
                   add_lss = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country) %>%
    # leave relevant columns only
    select(lss, utility)
  
  ### analysis ###
  
  # summarise non-NA values
  summary <- df %>%
    filter(!is.na(lss)) %>%
    group_by(lss) %>%
    summarise(Number = n(),
              Mean = mean(utility),
              `Standard Deviation` = sd(utility),
              Median = median(utility),
              Minimum = min(utility),
              Maximum = max(utility),
              Range = max(utility) - min(utility),
              .groups = "drop")
  
  # missing data
  summary_na <- df %>%
    summarise(Number = sum(is.na(lss))) %>%
    mutate(lss = "Missing")
  
  # combine and tidy up
  retval <- bind_rows(summary %>% mutate(lss = as.character(lss)), summary_na) %>%
    rename(LSS = lss)
  
  # return value
  return(retval)
}

#' Table 2.8: Number of observations in the LFS (Level Frequency Score) according to the EQ-5D values
#' 
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary data frame
#' @examples
#' table_2_8(df = example_data, country = "United States")
#' table_2_8(df = example_data, eq5d_version = "3L", country = "United States")
#' @export
#'
table_2_8 <- function(df, 
                      names_eq5d = NULL,
                      eq5d_version = NULL,
                      country){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, 
                   add_profile = TRUE, 
                   add_lfs = TRUE, eq5d_version = eq5d_version, 
                   add_utility = TRUE, country = country) %>%
    # leave relevant columns only
    select(lfs, utility)
  
  ### analysis ###
  
  # summarise for each utility-LFS combination
  retval <- df %>%
    # exclude NAs
    filter(!is.na(lfs)) %>%
    # summary by each utility-LFS combination
    group_by(utility) %>%
    count(lfs = factor(lfs), .drop = FALSE) %>%
    # arrange by LFS to preserve order when reshaping
    arrange(lfs) %>%
    # reshape into wide format
    pivot_wider(id_cols = utility, names_from = lfs, values_from = n,
                values_fill = list(n = 0)) %>%
    # order rows
    arrange(utility) %>%
    # add row totals
    mutate(Total = sum(c_across())) %>%
    # tidy up
    rename(`EQ-5D Value` = utility)
  
  # return value
  return(retval)
}

#' Table 2.9: Distribution of the EQ-5D profiles by LFS (Level Frequency Score)
#' 
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @return Summary data frame
#' @examples
#' table_2_9(df = example_data)
#' table_2_9(df = example_data, eq5d_version = "3L")
#' @export
#'
table_2_9 <- function(df, 
                      names_eq5d = NULL, 
                      eq5d_version = NULL){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, 
                   add_profile = TRUE, 
                   add_lfs = TRUE, eq5d_version = eq5d_version) %>%
    # leave relevant columns only
    select(lfs)
  
  ### analysis ###
  
  # summarise non-NA values
  summary <- df %>%
    filter(!is.na(lfs)) %>%
    # summary by each utility-lfs combination
    group_by(lfs) %>%
    summarise(n = n()) %>%
    # add percentage
    mutate(p = n / sum(n)) %>%
    arrange(-n) %>%
    # cumulative percentage
    mutate(cum_p = cumsum(p)) %>%
    # tidy up
    rename(LFS = lfs,
           Frequency = n,
           `%` = p,
           `Cum (%)` = cum_p)
  
  # missing data
  summary_na <- df %>%
    summarise(Frequency = sum(is.na(lfs))) %>%
    mutate(LFS = "Missing")
  
  # combine
  retval <- bind_rows(summary, summary_na)
  
  # return value
  return(retval)
}

#' Table 2.10: Summary statistics of EQ-5D values by LFS (Level Frequency Score)
#' 
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary data frame
#' @examples
#' table_2_10(df = example_data, country = "Denmark")
#' table_2_10(df = example_data, eq5d_version = "3L", country = "Denmark")
#' @export
#'
table_2_10 <- function(df, 
                       names_eq5d = NULL,
                       eq5d_version = NULL,
                       country){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, 
                   add_profile = TRUE, 
                   add_lfs = TRUE, eq5d_version = eq5d_version,
                   add_utility = TRUE, country = country) 
  
  ### analysis ###
  
  # summarise non-NA values
  retval <- df %>%
    # remove missing data
    filter(!is.na(utility)) %>%
    group_by(lfs) %>%
    summarise(Mean = mean(utility), 
              Median = median(utility)) %>%
    # tidy up
    arrange(lfs) %>%
    rename(LFS = lfs)
  
  # return value
  return(retval)
}

#' Table 3.1: EQ VAS Score: by timepoints
#' 
#' @param df Data frame with the VAS and the follow-up columns
#' @param name_vas Character string for the VAS column
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' @return Summary data frame
#' @examples
#' table_3_1(df = example_data)
#' @export
#'
table_3_1 <- function(df, 
                      name_vas = NULL,
                      name_fu = NULL,
                      levels_fu = NULL){
  
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, name_fu = name_fu, levels_fu = levels_fu, name_vas = name_vas)
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  name_vas <- temp$name_vas
  # check existence of columns 
  names_all <- c(name_vas, name_fu)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_vas(df = df, name = name_vas)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  
  ### analysis ###
  
  # values calculated in the .summary_cts_by_fu wrapper
  retval <- .summary_cts_by_fu(df = df, name_v = "vas")
  
  # return value
  return(retval)
}

#' Table 3.2: EQ VAS Scores: frequency of mid-points
#' 
#' @param df Data frame with the VAS column
#' @param name_vas Character string for the VAS column
#' @param add_na_total Logical, whether to add summary of the missing, and across the Total, data
#' @return Summary data frame
#' @examples
#' table_3_2(df = example_data)
#' @export
#'
table_3_2 <- function(df, 
                      name_vas = NULL, 
                      add_na_total = TRUE){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(name_vas = name_vas)
  name_vas <- temp$name_vas
  # check existence of columns 
  names_all <- c(name_vas)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_vas(df = df, name = name_vas)
  
  ### analysis ###
  
  # define ranges and midpoints
  retval <- data.frame(
    l = c(0:2, seq(from = 3, to = 93, by = 5), 98:100),
    u = c(0:2, seq(from = 7, to = 97, by = 5), 98:100),
    Midpoint = c(0:2, seq(from = 5, to = 95, by = 5), 98:100)) %>%
    mutate(Frequency = NA)
  
  # populate the table
  # entries for each row
  for (i in 1:nrow(retval)) {
    retval_temp <- retval[i, ]
    # number of entries in df within a given range
    retval[i, "Frequency"] <- nrow(df %>% filter(vas >= retval_temp$l & vas <= retval_temp$u))
  }
  
  # add range column
  retval <- retval %>%
    mutate(Range = case_when(l == u ~ as.character(l),
                             TRUE ~ str_c(l, u, sep = "-"))) %>%
    select(-l, -u) %>%
    select(Range, Midpoint, Frequency)
  
  # add totals & missing data if needed
  if (add_na_total) {
    n_total <- nrow(df)
    n_na <- nrow(df %>% filter(is.na(vas)))
    retval <- bind_rows(retval,
                        data.frame(
                          Range = c("Total observed", "Missing", "Total sample"),
                          Midpoint = rep(NA, 3),
                          Frequency = c(n_total - n_na, n_na, n_total)))
  }
  
  # return value
  return(retval)
}

#' Table 4.2 EQ-5D values: by timepoints
#' 
#' @param df Data frame with the EQ-5D and follow-up columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary data frame
#' @examples
#' table_4_2(df = example_data, country = "United States")
#' table_4_2(df = example_data, eq5d_version = "3L", country = "Denmark")
#' table_4_2(df = example_data, country = "Denmark")
#' @export
#'
table_4_2 <- function(df, 
                      names_eq5d = NULL,
                      name_fu = NULL,
                      levels_fu = NULL,
                      eq5d_version = NULL,
                      country){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, 
                     names_eq5d = names_eq5d, 
                     name_fu = name_fu, levels_fu = levels_fu,
                     eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d, name_fu)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_profile = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  df <- df %>%
    select(fu, utility)
  
  ### analysis ###
  
  # summarise
  retval <- .summary_cts_by_fu(df = df, name_v = "utility")
  
  # return value
  return(retval)
}

#' Table 4.3 EQ-5D values: by groupvar
#' 
#' @param df Data frame with the EQ-5D, follow-up and grouping columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param name_groupvar Character string for the grouping column
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary data frame
#' @examples
#' table_4_3(df = example_data, name_groupvar = "surgtype", country = "United States")
#' @export
#'
table_4_3 <- function(df,
                      names_eq5d = NULL,
                      name_fu = NULL,
                      levels_fu = NULL,
                      name_groupvar,
                      eq5d_version = NULL,
                      country){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, 
                     names_eq5d = names_eq5d, 
                     name_fu = name_fu, levels_fu = levels_fu,
                     eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d, name_fu, name_groupvar)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- df %>%
    rename(groupvar = !!quo_name(name_groupvar))
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_profile = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  df <- df %>%
    select(groupvar, fu, utility)
  
  ### analysis ###
  
  # summary by category
  summary_by <- .summary_table_4_3(df = df, group_by = "groupvar")
  # summary_total
  summary_total <- .summary_table_4_3(df = df, group_by = NULL) %>%
    mutate(groupvar = "All groups")
  
  # combine
  retval <- bind_rows(summary_by, summary_total) %>%
    pivot_longer(-groupvar) %>%
    pivot_wider(id_cols = name, names_from = groupvar, values_from = value)
  
  # return value
  return(retval)
}

#' Table 4.3 EQ-5D values: by age and groupvar
#' 
#' @param df Data frame with the EQ-5D, age, follow-up and grouping columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param name_groupvar Character string for the grouping column
#' @param name_age Character string for the age column
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary data frame
#' @examples
#' table_4_4(df = example_data, name_age = "age", name_groupvar = "surgtype", 
#'   country = "United States")
#' @export
#'
table_4_4 <- function(df,
                      names_eq5d = NULL,
                      name_fu = NULL,
                      levels_fu = NULL,
                      name_groupvar,
                      name_age,
                      eq5d_version = NULL,
                      country){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df,
                     names_eq5d = names_eq5d, 
                     name_fu = name_fu, levels_fu = levels_fu,
                     eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d, name_fu, name_groupvar, name_age)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- df %>%
    rename(groupvar = !!quo_name(name_groupvar),
           age = !!quo_name(name_age))
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_profile = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  df <- df %>%
    select(groupvar, age, fu, utility)
  # split age into categories
  if (!is.numeric(df$age))
    stop("Age column must be in a numeric format. Stopping.")
  age_breaks <- c(0, 18, seq(from = 25, to = 75, by = 10), Inf)
  df <- df %>%
    mutate(age_cat = cut(age, breaks = age_breaks, right = FALSE)) %>%
    mutate(age_cat = factor(age_cat,
                            levels = c("[0,18)", "[18,25)", "[25,35)", "[35,45)", "[45,55)", "[55,65)", "[65,75)", "[75,Inf)"),
                            labels = c("0-17", "18-24", "25-34", "35-44", "45-54","55-64", "65-74", "75+"))) %>%
    select(-age)
  
  ### analysis ###
  
  # summary by category
  summary_by <- .summary_table_4_4(df = df, group_by = c("groupvar", "age_cat"))
  
  # summary of totals by age
  summary_total_age <- .summary_table_4_4(df = df, group_by = c("groupvar")) %>%
    mutate(age_cat = "Total") 
  
  # summary of totals by groupvar
  summary_total_by <- .summary_table_4_4(df = df, group_by = c("age_cat")) %>%
    mutate(groupvar = "Total") 
  
  # summary of totals by groupvar and age
  summary_total_by_age <- .summary_table_4_4(df = df, group_by = NULL) %>%
    mutate(groupvar = "Total", age_cat = "Total")
  
  # combine and tidy up
  retval <- bind_rows(summary_by, summary_total_age, summary_total_by, summary_total_by_age) %>%
    pivot_longer(-(groupvar:age_cat)) %>%
    pivot_wider(id_cols = c(groupvar, name), names_from = age_cat, values_from = value)
  
  # return value
  return(retval)
}

#' Figure 2.1: Paretian Classification of Health Change
#' 
#' @param df Data frame with the EQ-5D, follow-up and patient id columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param name_id Character string for the patient id column
#' @return Summary plot and data used for plotting
#' @examples
#' tmp <- figure_2_1(df = example_data, name_fu = "surgtype", name_id = "id")
#' tmp$p
#' tmp$plot_data
#' @export
#'
figure_2_1 <- function(df, 
                       names_eq5d = NULL,
                       name_fu = NULL,
                       levels_fu = NULL,
                       name_id) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, names_eq5d = names_eq5d, name_fu = name_fu, levels_fu = levels_fu)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  # check existence of columns 
  names_all <- c(name_id, names_eq5d, name_fu)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- df %>%
    rename(id = !!quo_name(name_id))
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # sort by id - time
  df <- df %>%
    arrange(id, fu)
  # check uniqueness of id-fu combinations
  .check_uniqueness(df, group_by = c("id", "fu"))
  
  ### analysis ###
  
  # calculate change
  df <- .pchc(df = df, group_by = c("id"), add_noprobs = TRUE) %>%
    filter(!is.na(profile)) %>%
    mutate(profile_noprobs = 
             factor(profile_noprobs,
                    levels = c("No problems", "No change", "Improve", "Worsen", "Mixed change")))
  
  # summarise by groupvar, & profile
  plot_data <- df %>%
    group_by(fu, profile_noprobs) %>%
    summarise(n = n()) %>%
    mutate(p = n / sum(n)) %>%
    select(-n)
  
  # plot
  p <- ggplot(plot_data, aes(x = profile_noprobs, y = p, fill = fu)) + 
    # bar chart
    geom_bar(stat = "identity", position = "dodge") + 
    # add percentages
    geom_text(aes(label = scales::percent(p, accuracy = 0.1)), 
              position = position_dodge(width = 0.9),
              vjust = -0.5) +
    # manipuilate x-axis
    scale_x_discrete(name = "Pareto classification") + 
    # manipulate y-axis
    scale_y_continuous(name = "Percentage of respondents",
                       expand = expansion(mult = c(0, 0.1)),
                       labels = scales::percent_format()) +
    # title
    ggtitle("PCHC (Paretian Classification of Health Change)") +
    # manipulate legend
    scale_fill_brewer(palette = "Blues")
  
  # tidy up summary
  plot_data <- plot_data %>%
    pivot_wider(id_cols = profile_noprobs, names_from = fu, values_from = p) %>%
    rename(`Change category` = profile_noprobs)
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' Figure 2.2: Percentage of respondents who improved overall by the dimensions (%)
#' 
#' @param df Data frame with the EQ-5D, follow-up and patient id columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param name_id Character string for the patient id column
#' @return Summary plot and data used for plotting
#' @examples
#' tmp <- figure_2_2(df = example_data, name_fu = "year_range", name_id = "id")
#' tmp$p
#' tmp$plot_data
#' @export
#'
figure_2_2 <- function(df, 
                       names_eq5d = NULL,
                       name_fu = NULL,
                       levels_fu = NULL,
                       name_id) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, names_eq5d = names_eq5d, name_fu = name_fu, levels_fu = levels_fu)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  # check existence of columns 
  names_all <- c(name_id, names_eq5d, name_fu)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- df %>%
    rename(id = !!quo_name(name_id))
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # sort by id - time
  df <- df %>%
    arrange(id, fu)
  # check uniqueness of id-fu combinations
  .check_uniqueness(df, group_by = c("id", "fu"))
  
  ### analysis ###
  
  # calculate change
  df <- .pchc(df = df, group_by = c("id")) %>%
    filter(!is.na(profile))
  
  # summarise by name_groupvar & profile
  levels_eq5d <- c("mobility", "selfcare", "usualact", "paindisc", "anxietyd")
  plot_data <- df %>%
    filter(profile == "Improve") %>%
    select(fu, mobility_diff:anxietyd_diff) %>%
    pivot_longer(cols = mobility_diff:anxietyd_diff) %>%
    group_by(fu, name) %>%
    summarise(n = sum(value > 0), n_total = n()) %>%
    mutate(p = n / n_total) %>%
    mutate(name = factor(name, 
                         levels = str_c(levels_eq5d, "_diff"), 
                         labels = c("Mobility", "Self-care", "Usual activities", "Pain & Discomfort", "Anxiety & Depression")))
  
  # plot
  p <- .pchc_plot_by_dim(plot_data = plot_data, 
                         ylab = "Percentage of respondents who improved overall",
                         title = "Percentage of respondents who improved overall \nby the dimensions in which they improved (%)", 
                         cols = .gen_colours("green", length(unique(plot_data$fu))))
  
  # tidy up summary
  plot_data <- plot_data %>%
    pivot_wider(id_cols = name, names_from = fu, values_from = p) %>%
    rename(Values = name)
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' Figure 2.3: Percentage of respondents who worsened overall by the dimensions (%)
#' 
#' @param df Data frame with the EQ-5D, follow-up and patient id columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param name_id Character string for the patient id column
#' @return Summary plot and data used for plotting
#' @examples
#' tmp <- figure_2_3(df = example_data, name_fu = "year_range", name_id = "id")
#' tmp$p
#' tmp$plot_data
#' @export
#'
figure_2_3 <- function(df, 
                       names_eq5d = NULL,
                       name_fu = NULL,
                       levels_fu = NULL,
                       name_id) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, names_eq5d = names_eq5d, name_fu = name_fu, levels_fu = levels_fu)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  # check existence of columns 
  names_all <- c(name_id, names_eq5d, name_fu)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- df %>%
    rename(id = !!quo_name(name_id))
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # sort by id - time
  df <- df %>%
    arrange(id, fu)
  # check uniqueness of id-fu combinations
  .check_uniqueness(df, group_by = c("id", "fu"))
  
  ### analysis ###
  
  # calculate change
  df <- .pchc(df = df, group_by = c("id")) %>%
    filter(!is.na(profile))
  
  # summarise by name_groupvar & profile
  levels_eq5d <- c("mobility", "selfcare", "usualact", "paindisc", "anxietyd")
  plot_data <- df %>%
    filter(profile == "Worsen") %>%
    select(fu, mobility_diff:anxietyd_diff) %>%
    pivot_longer(cols = mobility_diff:anxietyd_diff) %>%
    group_by(fu, name) %>%
    summarise(n = sum(value < 0), n_total = n()) %>%
    mutate(p = n / n_total) %>%
    mutate(name = factor(name, 
                         levels = str_c(levels_eq5d, "_diff"), 
                         labels = c("Mobility", "Self-care", "Usual activities", "Pain & Discomfort", "Anxiety & Depression")))
  
  # plot
  p <- .pchc_plot_by_dim(plot_data = plot_data, 
                         ylab = "Percentage of respondents who worsened overall",
                         title = "Percentage of respondents who worsened overall \nby the dimensions in which they improved (%)", 
                         cols = .gen_colours("orange", length(unique(plot_data$fu))))
  
  # tidy up summary
  plot_data <- plot_data %>%
    pivot_wider(id_cols = name, names_from = fu, values_from = p) %>%
    rename(Values = name)
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' Figure 2.4: Percentage of respondents who had a mixed change by the dimensions in which they improved and worsened (%)
#' 
#' @param df Data frame with the EQ-5D, follow-up and patient id columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param name_id Character string for the patient id column
#' @return Summary plot and data used for plotting
#' @examples
#' tmp <- figure_2_4(df = example_data, name_fu = "year_range", name_id = "id")
#' tmp$p
#' tmp$plot_data
#' @export
#'
figure_2_4 <- function(df, 
                       names_eq5d = NULL,
                       name_fu = NULL,
                       levels_fu = NULL,
                       name_id) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, names_eq5d = names_eq5d, name_fu = name_fu, levels_fu = levels_fu)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  # check existence of columns 
  names_all <- c(name_id, names_eq5d, name_fu)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- df %>%
    rename(id = !!quo_name(name_id))
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # sort by id - time
  df <- df %>%
    arrange(id, fu)
  # check uniqueness of id-fu combinations
  .check_uniqueness(df, group_by = c("id", "fu"))
  
  ### analysis ###
  
  # calculate change
  df <- .pchc(df = df, group_by = c("id")) %>%
    filter(!is.na(profile))
  
  # read fu values
  n_time <- length(levels_fu)
  # fu values interacted with direction of change
  levels_fu_change <- c(str_c("Improve", ": ", levels_fu),
                        str_c("Worsen", ": ", levels_fu))
  
  levels_eq5d <- c("mobility", "selfcare", "usualact", "paindisc", "anxietyd")
  plot_data <- df %>%
    ungroup() %>%
    filter(profile == "Mixed change") %>%
    select(fu, mobility_diff:anxietyd_diff) %>%
    pivot_longer(cols = mobility_diff:anxietyd_diff) %>%
    group_by(fu, name) %>%
    summarise(n_improve = sum(value > 0), n_worsen = sum(value < 0), n_total = n()) %>%
    mutate(Improve = n_improve / n_total, Worsen = n_worsen / n_total) %>%
    mutate(name = factor(name, 
                         levels = str_c(levels_eq5d, "_diff"), 
                         labels = c("Mobility", "Self-care", "Usual activities", "Pain & Discomfort", "Anxiety & Depression"))) %>%
    select(-contains("n_")) %>%
    pivot_longer(cols = Improve:Worsen, names_to = "change", values_to = "p") %>%
    # add direction of change into the fu variable
    mutate(fu = str_c(change, ": ", fu)) %>%
    mutate(fu = factor(fu, levels = levels_fu_change))
  
  # plot
  p <- .pchc_plot_by_dim(plot_data = plot_data, 
                         ylab = "Percentage of respondents who had mixed change",
                         title = "Percentage of respondents who had a mixed change overall \nby the dimensions in which they improved and worsened (%)", 
                         cols = c(.gen_colours("green", n_time), .gen_colours("orange", n_time)),
                         text_rotate = TRUE)
  
  # tidy up summary
  plot_data <- plot_data %>%
    pivot_wider(id_cols = name, names_from = fu, values_from = p) %>%
    rename(Values = name)
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' Figure 2.8: EQ-5D values plotted against LSS
#' 
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary plot and data used for plotting
#' @examples
#' tmp <- figure_2_8(df = example_data, country = "United States")
#' tmp$p
#' tmp$plot_data
#' @export
#'
figure_2_8 <- function(df,
                       names_eq5d = NULL,
                       eq5d_version = NULL,
                       country){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, 
                   add_profile = TRUE, 
                   add_lss = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country) %>%
    # leave relevant columns only
    select(lss, utility)
  
  ### analysis ###
  
  # prepare data for plotting
  plot_data <- df %>%
    select(lss, utility) %>%
    # remove NAs
    filter(!is.na(lss) & !is.na(utility)) %>%
    group_by(lss) %>%
    # summarise
    summarise(median = median(utility),
              min = min(utility),
              max = max(utility),
              .groups = "drop") %>%
    # order
    arrange(lss)
  
  # graphical parameters
  # x-axis variable
  xvar_lab <- "LSS (Level Sum Score)"
  # axis breaks
  x_breaks <- 1:100
  y_breaks <- seq(from = -1, to = 1, by = 0.2)
  
  # plot
  p <- ggplot() +
    # plot median, min and max
    geom_segment(data = plot_data %>%
                   pivot_longer(-lss) %>%
                   mutate(name = factor(name, 
                                        levels = c("median", "min", "max"),
                                        labels = c("Median", "Lowest", "Highest"))), 
                 aes(x = lss - 0.2, xend = lss + 0.2, 
                     y = value, yend = value,
                     colour = name)) +
    # connect values with a line segment
    geom_segment(data = plot_data, aes(x = lss, xend = lss, y = min, yend = max)) +
    # plot title
    ggtitle("EQ-5D values plotted against the LSS (Level Sum Score)") +
    # manipulate x-axis
    scale_x_continuous(name = "Level Sum Score", 
                       breaks = x_breaks,
                       expand = expansion(add = c(0.5, 0.5))) +
    # manipulate y-axis
    scale_y_continuous(name = "EQ-5D value",
                       breaks = y_breaks)
  
  # tidy up summary
  plot_data <- plot_data %>%
    rename(LSS = lss,
           Median = median,
           Minimum = min,
           Maximum = max)
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' Figure 2.10: EQ-5D values plotted against LFS
#' 
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary plot and data used for plotting
#' @examples
#' tmp <- figure_2_10(df = example_data, country = "United States")
#' tmp$p
#' tmp$plot_data
#' @export
#'
figure_2_10 <- function(df,
                        names_eq5d = NULL,
                        eq5d_version = NULL,
                        country){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, 
                   add_profile = TRUE, 
                   add_lfs = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country) %>%
    # leave relevant columns only
    select(lfs, utility)
  
  ### analysis ###
  
  # prepare data for plotting
  plot_data <- df %>%
    select(lfs, utility) %>%
    # remove NAs
    filter(!is.na(lfs) & !is.na(utility)) %>%
    group_by(lfs) %>%
    # summarise
    summarise(median = median(utility),
              min = min(utility),
              max = max(utility),
              .groups = "drop") %>%
    # order
    arrange(-median)
  
  # impose order on the x-axis
  lfs_levels <- plot_data$lfs
  plot_data <- plot_data %>%
    mutate(lfs_f = factor(lfs, levels = lfs_levels))
  
  # axis breaks
  n <- length(lfs_levels)
  i <- seq(from = 1, to = n, by = 3)
  x_breaks <- (1:n)[i]
  x_labels <- lfs_levels[i]
  y_breaks <- seq(from = -1, to = 1, by = 0.2)
  
  # plot
  p <- ggplot() +
    # plot median, min and max
    geom_segment(data = plot_data %>%
                   pivot_longer(-c(lfs, lfs_f)) %>%
                   mutate(name = factor(name, 
                                        levels = c("median", "min", "max"),
                                        labels = c("Median", "Lowest", "Highest"))), 
                 aes(x = as.numeric(lfs_f) - 0.5, xend = as.numeric(lfs_f) + 0.5, 
                     y = value, yend = value,
                     colour = name)) +
    # connect values with a line segment
    geom_segment(data = plot_data, aes(x = as.numeric(lfs_f), xend = as.numeric(lfs_f), y = min, yend = max)) +
    # plot title
    ggtitle("EQ-5D values plotted against the LFS (Level Frequency Score)") +
    # manipulate x-axis
    scale_x_continuous(name = "Level Frequency Score", 
                       breaks = x_breaks,
                       labels = x_labels,
                       expand = expansion(add = c(0.5, 0.5))) +
    # manipulate y-axis
    scale_y_continuous(name = "EQ-5D value",
                       breaks = y_breaks) +
    # tidy up the graph
    theme(
      # rotate labels in the x-axis
      axis.text.x = element_text(angle = 90))
  
  # tidy up summary
  plot_data <- plot_data %>%
    select(-lfs_f) %>%
    rename(LFS = lfs,
           Median = median,
           Minimum = min,
           Maximum = max)
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' Figure 3.1: EQ VAS scores
#' 
#' @param df Data frame with the VAS column
#' @param name_vas Character string for the VAS column
#' @return Summary plot and data used for plotting
#' @examples
#' tmp <- figure_3_1(df = example_data)
#' tmp$p
#' tmp$plot_data
#' @export
#'
figure_3_1 <- function(df, name_vas = NULL){
  
  ### data preparation ###
  # replace NULL names with defaults
  temp <- .get_names(name_vas = name_vas)
  name_vas <- temp$name_vas
  # check existence of columns 
  names_all <- c(name_vas)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_vas(df = df, name = name_vas)
  
  ### analysis ###
  
  # plot
  p <- ggplot(df, aes(x = vas)) +
    geom_bar(width = 0.1, fill = "blue") + 
    scale_x_continuous(name = "EQ VAS score", 
                       breaks = seq(from = 0, to = 100, by = 5),
                       limits = c(-3, 103),
                       expand = c(0, 0)) +
    scale_y_continuous(name = "Frequency",
                       expand = expansion(mult = c(0, 0.05))) + 
    ggtitle("EQ VAS Scores") + 
    # tidy up the graph
    theme(
      # reinstate x-axis ticks
      axis.ticks.x = element_line(colour = "grey50"))
  
  # output plotting data
  plot_data <- df %>%
    # count for each value between 0 & 100
    count(vas = factor(vas, levels = 1:100), .drop = FALSE) %>%
    # order
    arrange(vas) %>%
    rename(count = n)
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' Figure 3.1: Mid-point EQ VAS scores
#' 
#' @param df Data frame with the VAS column
#' @param name_vas Character string for the VAS column
#' @return Summary plot and data used for plotting
#' @examples
#' tmp <- figure_3_2(df = example_data)
#' tmp$p
#' tmp$plot_data
#' @export
#'
figure_3_2 <- function(df, name_vas = NULL){
  
  # produce output to be plotted (Table 3.2)
  # do not add totals
  plot_data <- table_3_2(df = df, name_vas = name_vas, add_na_total = FALSE) %>%
    # remove Range column, not needed
    select(-Range)
  
  # plot
  p <- ggplot(plot_data, aes(x = Midpoint, y = Frequency)) +
    geom_bar(width = 0.7, fill = "blue", stat = "identity") + coord_flip() +
    scale_x_continuous(name = "EQ VAS score (Midpoint)", 
                       breaks = seq(from = 0, to = 100, by = 10),
                       limits = c(-3, 103),
                       expand = c(0, 0)) +
    scale_y_continuous(name = "Frequency",
                       expand = expansion(mult = c(0, 0.05))) + 
    ggtitle("EQ VAS Scores (Midpoint)") +
    # tidy up the graph
    theme(
      # reinstate x-axis ticks
      axis.ticks.x = element_line(colour = "grey50"),
      # remove y-axis ticks
      axis.ticks.y = element_blank())
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' Figure 4.2: EQ-5D values by timepoints: mean values and 95% confidence intervals
#' 
#' @param df Data frame with the VAS columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary plot and data used for plotting
#' @examples
#' tmp <- figure_4_2(df = example_data, name_fu = "month", country = "United States")
#' tmp$p
#' tmp$plot_data
#' @export
#'
figure_4_2 <- function(df,
                       names_eq5d = NULL,
                       name_fu = NULL,
                       levels_fu = NULL,
                       eq5d_version = NULL,
                       country) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, names_eq5d = names_eq5d, 
                     name_fu = name_fu, levels_fu = levels_fu,
                     eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d, name_fu)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_profile = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  df <- df %>%
    select(fu, utility)
  
  ### analysis ###
  
  # prepare data for plotting
  plot_data <- .summary_mean_ci(df = df, group_by = "fu")
  
  # plot
  p <- ggplot(data = plot_data, aes(x = fu, 
                                    y = mean, ymin = ci_lb, ymax = ci_ub)) +
    # connect means with a line
    # create a dummy group variable as geom_line only connects points that belong to the same group
    geom_line(aes(group = 1), colour = "red") +
    # plot means
    geom_point(colour = "green") + 
    # add error bars
    geom_errorbar(width = 0.1) +
    # plot title
    ggtitle(str_c("EQ-5D values by ", name_fu, ": mean and 95% confidence intervals")) +
    # manipulate x-axis
    scale_x_discrete(name = "time") +
    # manipulate y-axis
    scale_y_continuous(name = "EQ-5D value")
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' Figure 4.3: Mean EQ-5D values and 95% confidence intervals: all vs by groupvar
#' 
#' @param df Data frame with the EQ-5D and grouping columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_groupvar Character string for the grouping column
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary plot and data used for plotting
#' @examples
#' tmp <- figure_4_3(df = example_data, name_groupvar = "surgtype", country = "United States")
#' tmp$p
#' tmp$plot_data
#' @export
#'
figure_4_3 <- function(df,
                       names_eq5d = NULL,
                       name_groupvar,
                       eq5d_version = NULL,
                       country) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(name_groupvar, names_eq5d)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- df %>%
    rename(groupvar = !!quo_name(name_groupvar))
  df <- .prep_eq5d(df = df, names = names_eq5d, 
                   add_profile = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country) %>%
    select(groupvar, utility)
  
  ### analysis ###
  
  # prepare data for plotting
  plot_data <- .summary_mean_ci(df = df, group_by = "groupvar")
  
  # add totals
  plot_data_total <- .summary_mean_ci(df = df, group_by = NULL) %>%
    mutate(groupvar = "All groups") %>%
    select(groupvar, everything())
  
  # combine
  groupvar_levels <- c(plot_data$groupvar, "All groups")
  plot_data <- bind_rows(plot_data, plot_data_total) %>%
    mutate(groupvar = factor(groupvar, levels = groupvar_levels)) %>%
    arrange(groupvar)
  
  # plot
  p <- ggplot(data = plot_data, aes(x = groupvar, y = mean, ymin = ci_lb, ymax = ci_ub)) +
    # plot means
    geom_bar(colour = "blue", fill = "blue", stat = "identity", width = 0.5) + 
    geom_point() +
    # add error bars
    geom_errorbar(width = 0.1, size = 1) +
    # plot title
    ggtitle(str_c("Mean EQ-5D values 95% confidence intervals: all vs by ", name_groupvar)) +
    # manipulate x-axis
    scale_x_discrete(name = name_groupvar) +
    # manipulate y-axis
    scale_y_continuous(name = "EQ-5D value",
                       limits = c(0, 1),
                       breaks = seq(from = 0, to = 1, by = 0.1),
                       expand = c(0, 0))
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' Figure 4.4: EQ-5D values: smoothed lines and confidence intervals by groupvar
#' 
#' @param df Data frame with the EQ-5D, follow-up and grouping columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param name_groupvar Character string for the grouping column
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary plot and data used for plotting
#' @examples
#' tmp <- figure_4_4(df = example_data, name_fu = "month", 
#'   name_groupvar = "gender", country = "United States")
#' tmp$p
#' tmp$plot_data
#' @export
#'
figure_4_4 <- function(df, 
                       names_eq5d = NULL,
                       name_fu = NULL,
                       levels_fu = NULL,
                       name_groupvar,
                       eq5d_version = NULL,
                       country) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, names_eq5d = names_eq5d, 
                     name_fu = name_fu, levels_fu = levels_fu,
                     eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d, name_fu, name_groupvar)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- df %>%
    rename(groupvar = !!quo_name(name_groupvar)) %>%
    # factorise groupvar variable
    mutate(groupvar = factor(groupvar))
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_profile = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  df <- df %>%
    select(fu, groupvar, utility)
  
  ### analysis ###
  
  # prepare data for plotting
  plot_data <- .summary_mean_ci(df = df, group_by = c("fu", "groupvar"))
  
  # plot
  p <- ggplot(data = plot_data, aes(x = fu, 
                                    y = mean, ymin = ci_lb, ymax = ci_ub,
                                    group = groupvar, colour = groupvar)) +
    # plot means
    geom_point() +
    # add error bars
    geom_errorbar(width = 0.1, alpha = 0.5) +
    # connect means with a smooth line
    geom_line() +
    # plot title
    ggtitle(str_c("Longitudinal EQ-5D values by ", name_fu, " and ", name_groupvar, ":\nmeans and 95% confidence intervals")) +
    # manipulate x-axis
    scale_x_discrete(name = name_fu) +
    # manipulate y-axis
    scale_y_continuous(name = "EQ-5D value")
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' Figure 4.7: EQ-5D values: smoothed lines and confidence intervals by groupvar
#' 
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary plot and data used for plotting
#' @examples
#' tmp <- figure_4_7(df = example_data, country = "United States")
#' tmp$p
#' tmp$plot_data
#' @export
#'
figure_4_7 <- function(df, 
                       names_eq5d = NULL,
                       eq5d_version = NULL,
                       country) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_profile = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country) %>%
    select(utility)
  
  ### analysis ###
  
  # prepare data for plotting
  plot_data <- df %>%
    group_by(utility) %>%
    summarise(count = n()) %>%
    arrange(utility)
  
  # plot
  p <- ggplot(plot_data, aes(x = utility, y = count)) +
    geom_bar(fill = "blue", stat = "identity") + 
    scale_x_continuous(name = "EQ-5D value") +
    scale_y_continuous(name = "Frequency") + 
    ggtitle("Distribution of EQ-5D values") + 
    theme(
      # reinstate axis.ticks
      axis.ticks = element_line(colour = "grey50"))
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' Figure 4.11: EQ-5D values: smoothed lines and confidence intervals by groupvar
#' 
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_vas Character string for the VAS column
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary plot and data used for plotting
#' @examples
#' tmp <- figure_4_11(df = example_data, country = "United States")
#' tmp$p
#' tmp$plot_data
#' @export
#'
figure_4_11 <- function(df,
                        names_eq5d = NULL,
                        name_vas = NULL,
                        eq5d_version = NULL,
                        country) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(names_eq5d = names_eq5d, name_vas = name_vas, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  name_vas <- temp$name_vas
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d, name_vas)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_vas(df = df, name = name_vas)
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_profile = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country) %>%
    select(vas, utility)
  
  ### analysis ###
  
  # prepare data for plotting
  plot_data <- df %>%
    arrange(vas, utility)
  
  # plot
  p <- ggplot(plot_data, aes(x = vas, y = utility)) +
    geom_point(colour = "blue") + 
    scale_x_continuous(name = "EQ VAS") +
    scale_y_continuous(name = "EQ-5D value") + 
    ggtitle("EQ-5D values plotted against EQ VAS")
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}