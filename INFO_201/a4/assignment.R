# A4 Data Wrangling

# We provide this line to delete all variables in your workspace.
# This will make it easier to test your script.
rm(list = ls())

# Loading and Exploring Data -------------------------------- (**29 points**)

# First, search online for a dplyr cheatsheet and put the link to one you
# like in the comments here (it's ok if the link makes the line too long):
# <https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf>


# To begin, you'll need to download the Kickstarter Projects data from the
# Kaggle website: https://www.kaggle.com/kemical/kickstarter-projects
# Download the `ks-projects-201801.csv` file into a new folder called `data/`

# Load the `dplyr` package
library(dplyr)

# Load your data, making sure to not interpret strings as factors.
ks_project <- read.csv("data/ks-projects-201801.csv", stringsAsFactors = FALSE)

# To start, write the code to get some basic information about the dataframe:
# - What are the column names?
# - How many rows is the data frame?
# - How many columns are in the data frame?
colnames(ks_project)
nrow(ks_project)
ncol(ks_project)

# Use the `summary` function to get some summary information
summary(ks_project)

# Unfortunately, this doesn't give us a great set of insights. Let's write a
# few functions to try and do this better.
# First, let's write a function `get_col_info()` that takes as parameters a
# column name and a dataframe. If the values in the column are *numeric*,
# the function should return a list with the keys:
# - `min`: the minimum value of the column
# - `max`: the maximum value of the column
# - `mean`: the mean value of the column
# If the column is *not* numeric and there are fewer than 10 unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `unique_values`: a vector of each unique value in the column
# If the column is *not* numeric and there are 10 or *more* unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `sample_values`: a vector containing a random sample of 10 column values
# Hint: use `typeof()` to determine the column type
get_col_info <- function(df, col) {
  if (!typeof(df$col) == "character") {
    results <- ks_project %>%
      summarise(
        minimum = min(col, na.rm = TRUE),
        maximum = max(col, na.rm = TRUE),
        average = mean(col, na.rm = TRUE)
      )
  } else if (length(unique(df$col)) < 10) {
    n_values <- ks_project %>%
      summarise(num_unique = length(unique(col)))
    unique_values <- ks_project %>%
      summarise(unique(col))
    results <- list(n_values, unique_values)
  } else {
    n_values <- ks_project %>%
      summarise(num_unique = length(unique(col)))
    sample_values <- ks_project %>%
      summarise(sample(col, 10))
    results <- list(n_values, sample_values)
  }
  return(results)
}
# Demonstrate that your function works by passing a column name of your choice
# and the kickstarter data to your function. Store the result in a variable
# with a meaningful name
country_info <- get_col_info(ks_project, "country")

# To take this one step further, write a function `get_summary_info()`,
# that takes in a data frame  and returns a *list* of information for each
# column (where the *keys* of the returned list are the column names, and the
# _values_ are the summary information returned by the `get_col_info()` function
# The suggested approach is to use the appropriate `*apply` method to
# do this, though you can write a loop
get_summary_info <- function(df) {
  results <- as.list(sapply(df, get_col_info, col))

  return(results)
}

# Demonstrate that your function works by passing the kickstarter data
# into it and saving the result in a variable
summary <- get_summary_info(ks_project)

# Take note of 3 observations that you find interesting from this summary
# information (and/or questions that arise that want to investigate further)
# 1) The highest goal is 100000000.00 I want to know which project had that.
# 2) I want to know how many project failed.
# 3) The highest amount pledged is 100000000.00 I want to know which project
# had that.

# Asking questions of the data ----------------------------- (**29 points**)

# Write the appropriate dplyr code to answer each one of the following questions
# Make sure to return (only) the desired value of interest (e.g., use `pull()`)
# Store the result of each question in a variable with a clear + expressive name
# If there are multiple observations that meet each condition, the results
# can be in a vector. Make sure to *handle NA values* throughout!
# You should answer each question using a single statement with multiple pipe
# operations!
# Note: For questions about goals and pledged, use the usd_pledged_real
# and the usd_goal_real columns, since they standardize the currancy.


# What was the name of the project(s) with the highest goal?
highest_goal <- ks_project %>%
  filter(usd_goal_real == max(usd_goal_real, na.rm = TRUE)) %>%
  pull(name)

# What was the category of the project(s) with the lowest goal?
lowest_goal <- ks_project %>%
  filter(usd_goal_real == min(usd_goal_real, na.rm = TRUE)) %>%
  pull(category)

# How many projects had a deadline in 2018?
# Hint: start by googling "r get year from date" and then look up more about
# different functions you find
deadline_2018 <- ks_project %>%
  mutate(
    deadline_year =
      as.numeric(format(as.Date(deadline, "%Y-%m-%d"), "%Y"))
  ) %>%
  filter(deadline_year == "2018") %>%
  count(deadline_year) %>%
  pull(n)

# What proportion of projects weren't marked successful (e.g., failed or live)?
# Your result can be a decimal
succeed_num <- ks_project %>%
  filter(state == "successful") %>%
  count(state) %>%
  pull(n)

failed_percent <- (1 - succeed_num / nrow(ks_project)) * 100

# What was the amount pledged for the project with the most backers?
most_backers <- ks_project %>%
  filter(backers == max(backers, na.rm = TRUE)) %>%
  pull(usd_pledged_real)

# Of all of the projects that *failed*, what was the name of the project with
# the highest amount of money pledged?
most_in_failed <- ks_project %>%
  filter(!state == "successful") %>%
  filter(usd_pledged_real == max(usd_pledged_real)) %>%
  pull(name)

# How much total money was pledged to projects that weren't marked successful?
total_in_failed <- ks_project %>%
  filter(!state == "successful") %>%
  summarise(sum(usd_pledged_real)) %>%
  pull()

# Performing analysis by *grouped* observations ----------------- (31 Points)

# Which category had the most money pledged (total)?
most_category <- ks_project %>%
  group_by(category) %>%
  summarise(total = sum(usd_pledged_real, na.rm = TRUE)) %>%
  filter(total == max(total, na.rm = TRUE)) %>%
  pull(category)

# Which country had the most backers?
most_country <- ks_project %>%
  group_by(country) %>%
  summarise(total = sum(backers, na.rm = TRUE)) %>%
  filter(total == max(total, na.rm = TRUE)) %>%
  pull(country)

# Which year had the most money pledged (hint: you may have to create a new
# column)?
# Note: To answer this question you can choose to get the year from either
# deadline or launched dates.
most_pledged <- ks_project %>%
  mutate(
    deadline_year =
      as.numeric(format(as.Date(deadline, "%Y-%m-%d"), "%Y"))
  ) %>%
  group_by(deadline_year) %>%
  summarise(total = sum(usd_pledged_real, na.rm = TRUE)) %>%
  filter(total == max(total, na.rm = TRUE)) %>%
  pull(deadline_year)

# Write one sentance below on why you chose deadline or launched dates to
# get the year from: I chose deadline as the dates because I think when they
# first started they didn't have any money pledged, so the dates should be the
# deadline.

# What were the top 3 main categories in 2018 (as ranked by number of backers)?
top_3 <- ks_project %>%
  mutate(
    deadline_year =
      as.numeric(format(as.Date(deadline, "%Y-%m-%d"), "%Y"))
  ) %>%
  filter(deadline_year == "2018") %>%
  group_by(category) %>%
  summarise(total = sum(backers, na.rm = TRUE)) %>%
  arrange(-total) %>%
  head(3) %>%
  pull(category)

# What was the most common day of the week on which to launch a project?
# (return the name of the day, e.g. "Sunday", "Monday"....)
most_common_day <- ks_project %>%
  mutate(weekday = weekdays(as.Date(deadline, "%Y-%m-%d"))) %>%
  count(weekday) %>%
  filter(n == max(n, na.rm = TRUE)) %>%
  pull(weekday)

# What was the least successful day on which to launch a project? In other
# words, which day had the lowest success rate (lowest proportion of projects
# that were marked successful )? This might require creative problem solving...
# Hint: Try googling "r summarize with condition in dplyr"
least_day <- ks_project %>%
  mutate(weekday = weekdays(as.Date(deadline, "%Y-%m-%d"))) %>%
  group_by(weekday) %>%
  count(state = state == "successful")

friday <- least_day %>%
  filter(weekday == "Friday")
