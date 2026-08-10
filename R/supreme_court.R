# Supreme court average age over time

# R libraries ----
library(logger)
log_threshold(DEBUG)
library(tidyverse)
library(rvest)
library(dplyr)

# Define the raw data URL 
url <- "https://en.wikipedia.org/wiki/List_of_justices_of_the_Supreme_Court_of_the_United_States"

# Read the HTML and extract all tables
tables <- read_html(url) |> 
  html_table()

# Select desired table and variables
df <- tables[[2]] |>
  data.frame() |>
  slice(-1) |>
  select(c(1, 3, 6, 8, 9)) |>
  `colnames<-`(c("id", "justice", "succession", "tenure", "tenure_length"))

# Generate new variables and tidy formatting
df <- df |> 
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
df$seat <- df$succession |> 
  sub(" II", "", x=_) |> 
  str_extract("[A-Za-z']*$") |> 
  replace(x=_, 65, "Blatchford") |> 
  replace(x=_, 86, "McKenna") |> 
  replace(x=_, 108, "Harlan")
x <- 0
for (i in 1:nrow(df)){
  if (df$succession[i] %in% c("Inaugural","new seat")){
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
rm(x, i)

# Generate inaugural age
df$inaug_age <- interval(start=df$birth_day, end=df$start_date) |> 
  as.duration() |> 
  as.numeric("years") 

# Generate new long data.frame across time 
dft <- data.frame(time = seq(as.Date("1789-10-01"), today(), by = "month")) |> 
  mutate(row_id = row_number(), .before = 1) 
dft[paste0("seat_", 0:10)] <- NaN

# Populate birthday value of each seat across time
for (j in 0:10){
  for (i in 1:sum(df$seat == j)){
    current_seat <- paste0("seat_", j)
    start_time   <- df$start_date[df$seat == j][i]
    end_time     <- df$end_date[df$seat == j][i]
    birth_val    <- df$birth_day[df$seat == j][i]
    target_rows <- dft$time %within% interval(start_time, end_time)
    dft[target_rows, current_seat] <- birth_val
    }
}

# Transform birthday value into age equivalent 
for (j in 0:10){
  for(i in 1:nrow(dft)){
    dft[[paste0("seat_", j)]][i] <- dft[[paste0("seat_", j)]][i] |> 
      as.Date() |> 
      interval(start=_, end=dft$time[i]) |> 
      as.duration() |> 
      as.numeric("years") 
  }
} 

# Generate court summary statistics 
dft$court_size <- rowSums(!is.na(dft[paste0("seat_", 0:10)]))
dft$mean_age <- rowMeans(dft[paste0("seat_", 0:10)], na.rm = TRUE) 
dft$min_age <-do.call(pmin, c(dft[paste0("seat_", 0:10)], na.rm = TRUE))
dft$max_age <-do.call(pmax, c(dft[paste0("seat_", 0:10)], na.rm = TRUE))


##TODO: add scatter by inauguration
##TODO: add average tenure length over time
# Plot summary statistics 
dft |> 
  ggplot(data = _) +
  geom_ribbon(aes(x=time, ymin = min_age, ymax = max_age), fill = "grey70") +
  geom_line(aes(x=time, y=mean_age))

dft |> 
  ggplot(data = _) +
  geom_line(aes(x=time, y=court_size))


dft |> slice(-1) |> 
  select(c(time, seat_0:seat_10)) |>
  pivot_longer(!c(time)) |>
  ggplot(data = _) + 
  geom_point(aes(x=time, y=value, color=name))

df |> 
  ggplot(data = _) + 
  geom_point(aes(x=start_date, y=inaug_age, color=seat)) + 
  geom_hline(yintercept = mean(df$inaug_age))



library(ggiraph)
g <- ggplot(df, aes(x=start_date, y=inaug_age, color=seat, tooltip=name)) + 
  geom_point_interactive()

girafe(ggobj = g)


