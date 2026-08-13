# Supreme court average age over time

# R libraries ----
library(logger)
log_threshold(WARN)
library(tidyverse)
library(rvest)
library(dplyr)


# Define input data URL address
ipath <- list(
  supreme_court_justices = "https://en.wikipedia.org/wiki/List_of_justices_of_the_Supreme_Court_of_the_United_States"
)

# Define local raw data cache
opath <- list(
  justices_raw = "data/supreme_court/justices_raw.rds", 
  justices_tidy = "data/supreme_court/justices_tidy.rds", 
  temporal_court = "data/supreme_court/temporal_court.rds"
)


#' Make a new (recursive) directory for a given file path if one does not exist
#'
#' @param file_path A file path.
#'
#' @returns File path input
#'
#' @examples
#' mkdir("temp/test/myfile.txt")
#' mkdir(file.path("temp", "test", "myfile.txt"))
mkdir <- function(file_path) {
  d <- dirname(file_path)
  if (!dir.exists(d)) {
    log_info(paste("Creating directory", d))
    dir.create(d, recursive = TRUE)
  }
  return(file_path)
}

#' Call up raw html supreme court justice tables
#'
#' @returns raw tables
call_justices_raw <- function() {
  cache <- opath$justices_raw
  url <- ipath$supreme_court_justices
  if (file.exists(cache)) {
    # Read data from cache
    log_debug("raw data found at {cache}")
    return(readRDS(cache))
  } else {
    # Read the HTML and extract all tables
    log_debug("parsing new html")
    df <- read_html(url) |> 
      html_table()
    saveRDS(df, mkdir(cache))
    return(df)
  }
}


#' Call up tidy supreme court justice data frame
#'
#' @returns tidy dataframe with line of succession variable
call_justices_tidy <- function() {
  cache <- opath$justices_tidy
  if (file.exists(cache)) {
    # Read data from cache
    log_debug("tidy data found at {cache}")
    return(readRDS(cache))
  } else {
    # Read and process the raw data 
    log_debug("parsing new tidy court")

    # Select desired table and variables
    df <- call_justices_raw()[[2]] |>
      data.frame() |>
      select(c(1, 3, 6, 8, 9)) |>
      `colnames<-`(c("id", "justice", "succession", "tenure", "tenure_length")) |>
      separate_wider_delim(tenure, delim = "–", names = c("start_date", "end_date")) |>
      mutate(
        across(start_date:end_date, ~ .x |>
          str_extract("[A-Za-z0-9,\\s]*") |>
          as.Date(format = "%B %d, %Y") |>
          replace_na(today()))
      ) |>
      mutate(row_id = row_number()) |>
      mutate(birth_day = justice |>
        str_extract("[0-9]+") |>
        paste0("-07-01") |>
        as.Date()) |>
      mutate(name = justice |>
        str_extract("^[A-Za-z.'\\s]+") |>
        sub(" II", "", x = _) |>
        sub(" Jr.", "", x = _))

    # Trace line of succession for each seat on the bench
    df <- df |>
      mutate(seat = succession |>
        sub(" II", "", x = _) |>
        str_extract("[A-Za-z']*$")) |>
      # Five associate justices were later appointed chief justice: John Rutledge, Edward Douglass White, Charles Evans Hughes, Harlan F. Stone and William Rehnquist.
      # John Hessin Clarke succeeded Charles Evans Hughes when he resigned June 10 1916 while he later came back as chief February 24, 1930
      # Thomas Johnson succeeded John Rutledge when he resigned March 5, 1971 while he later came back as chief August 12, 1795
      mutate(seat = case_match(
        name,
        "Willis Van Devanter" ~ "Blatchford",
        "Robert H. Jackson" ~ "McKenna",
        "Antonin Scalia" ~ "Harlan",
        .default = seat
      ))
    x <- 0
    for (i in 1:nrow(df)) {
      if (df$succession[i] %in% c("Inaugural", "New seat")) {
        df$seat[i] <- x
        x <- x + 1
      } else {
        df$seat[i] <- df$name[1:i] %>%
          str_extract("[A-Za-z']*$") %>%
          grepl(paste0("^", df$seat[i], "$"), .) %>%
          df$row_id[1:i][.] %>%
          as.numeric() %>%
          max() %>%
          df$seat[1:i][.]
      }
    }

    # Generate inaugural age
    df$inaug_age <- interval(start = df$birth_day, end = df$start_date) |>
      as.duration() |>
      as.numeric("years")

    # Generate consummate age
    df$consum_age <- interval(start = df$birth_day, end = df$end_date) |>
      as.duration() |>
      as.numeric("years")

    # Save new data to disk
    saveRDS(df, mkdir(cache))
    return(df)
  }
}


#' Call up longitudinal supreme court data frame
#'
#' @returns long data frame composition of the court seats over time
call_temporal_court <- function() {
  cache <- opath$temporal_court
  if (file.exists(cache)) {
    # Read data from cache
    log_debug("temporal data found at {cache}")
    return(readRDS(cache))
  } else {
    # Read and process the raw data
    log_debug("parsing new temporal court")

    # Call input data frame
    tj <- call_justices_tidy()

    # Generate new long data.frame across time
    df <- data.frame(time = seq(as.Date("1789-10-01"), today(), by = "month")) |>
      mutate(row_id = row_number(), .before = 1)
    df[paste0("seat_", 0:10)] <- NaN

    # Populate birthday value of each seat across time
    for (j in 0:10) {
      for (i in 1:sum(tj$seat == j)) {
        current_seat <- paste0("seat_", j)
        start_time <- tj$start_date[tj$seat == j][i]
        end_time <- tj$end_date[tj$seat == j][i]
        birth_val <- tj$birth_day[tj$seat == j][i]
        target_rows <- df$time %within% interval(start_time, end_time)
        df[target_rows, current_seat] <- birth_val
      }
    }

    # Transform birthday value into age equivalent
    for (j in 0:10) {
      for (i in 1:nrow(df)) {
        df[[paste0("seat_", j)]][i] <- df[[paste0("seat_", j)]][i] |>
          as.Date() |>
          interval(start = _, end = df$time[i]) |>
          as.duration() |>
          as.numeric("years")
      }
    }

    # Generate court summary statistics
    df$court_size <- rowSums(!is.na(df[paste0("seat_", 0:10)]))
    df$mean_age <- rowMeans(df[paste0("seat_", 0:10)], na.rm = TRUE)
    df$min_age <- do.call(pmin, c(df[paste0("seat_", 0:10)], na.rm = TRUE))
    df$max_age <- do.call(pmax, c(df[paste0("seat_", 0:10)], na.rm = TRUE))

    # Save new data to disk
    saveRDS(df, mkdir(cache))
    return(df)
  }
}


# Tests ----
test_court <- function(){
  jr <- call_justices_raw()
  jt <- call_justices_tidy()
  tc <- call_temporal_court()
}



