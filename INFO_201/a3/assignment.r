# Assignment 3: Using Data
#
# Before you get started:
# - Set your working directory to "source file location" using the Session menu
# - Run the following line of code to delete all variables in your workspace
#     (This will make it easier to test your script)
rm(list = ls())


### Built in R Data ###########################################################

# In this section, you'll work with the variable `Titanic`, a data set which is
# built into the R environment.
# This data set actually loads in a format called a *table*
# See https://cran.r-project.org/web/packages/data.table/data.table.pdf
# Use the `is.data.frame()` function to test if it is a table.
is.data.frame(Titanic)

# Create a variable `titanic_df` by converting `Titanic` into a data frame;
# you can use the `data.frame()` function or `as.data.frame()`
# Hint: Be sure to **not** treat strings as factors!
titanic_df <- data.frame(Titanic)

# It's important to understand the _meaning_ of each column before analyzing it
# Using comments below, describe what information is stored in each column
# For categorical variables, list all possible values
# Class: [which of the three classes were the passengers in
#         or they were crew members]
# Sex: [the gender of the person]
# Age: [the age of the person]
# Survived: [whether or not the person survived]
# Freq: [the number of people that falls into the same category]


# Create a variable `children` that is a data frame containing only the rows
# from `titanic_df` with information about children on the Titanic
# Hints:
# - Filter rows using a vector of boolean values (like vector filtering)
# - See chapter 10.2.3
children <- titanic_df[titanic_df$Age == "Child", ]

# Create a variable `num_children` that is the total number of children.
# Hint: Remember the `sum()` function!
num_children <- sum(children$Freq)

# Create a variable `most_lost` that is the *row* from `titanic_df` with the
# largest absolute number of losses (people who did not survive)
# You can use multiple lines of code if you find that helpful
# to create this variable
# Hint: Filter for those who did not survive, then look for the row
most_lost <- titanic_df[titanic_df$Freq == max(titanic_df$Freq), ]

# Define a function called `survival_rate()` that takes in two arguments which
# must be in *the following order*:
# - a ticket class (e.g., "1st", "2nd"), and
# - the dataframe itself (it's good practice to explicitly pass in data frames)
#
# This function should return a sentence that states the *survival rate*
# (# survived / # in group) of adult men and "women and children" in that
# ticketing class.
# It should read (for example):
# >"Of Crew class, 87% of women and children survived and 22% of men survived."
#
# This is a complicated function! We recommend the following approach:
# - Filter for all rows representing the given ticketing class and save the
#   new data frame to a variable
# - Using this data frame, filter for all rows representing Adult Males
# - Find the total number of men and total number of male survivors to
#   calculate the survival rate
# - Likewise, use the data frame to filter for all Children and Adult Females
# - Perform the above calculation for this group as well
#
# Other approaches are also acceptable, please comment to explain what you do!
survival_rate <- function(class, df) {
  ticket <- titanic_df[titanic_df$Class == class, ]
  adult <- ticket[ticket$Age == "Adult", ]
  adult_males <- adult[adult$Sex == "Male", ]
  males_alive <- adult_males[adult_males$Survived == "Yes", ]
  adult_females <- adult[adult$Sex == "Female", ]
  females_alive <- adult_females[adult_females$Survived == "Yes", ]
  child <- ticket[ticket$Age == "Child", ]
  children_alive <- child[child$Survived == "Yes", ]
  male_percent <- round(sum(males_alive$Freq) /
    sum(adult_males$Freq) * 100, digits = 0)
  child_female_percent <-
    round((sum(children_alive$Freq) + sum(females_alive$Freq)) /
      (sum(adult_females$Freq) + sum(child$Freq)) * 100, digits = 0)

  return(paste0(
    "Of ",
    class,
    " class, ",
    child_female_percent,
    "% of women and children survived and ",
    male_percent,
    "% of men survived."
  ))
}

# Create variables `first_survived`, `second_survived`, `third_survived` and
# `crew_survived` by passing each class and the `titanic_df` data frames
# to your `survival_rate` function
first_survived <- survival_rate("1st", titanic_df)
second_survived <- survival_rate("2nd", titanic_df)
third_survived <- survival_rate("3rd", titanic_df)
crew_survived <- survival_rate("Crew", titanic_df)


# What notable differences do you observe in the survival rates across classes?
# Note at least 2 observations.
# [(1) The survival rate of the first class is much higher than the other two
#       classes and crew members.
# (2) The survival rate of the third class is much lower than the other
#     classes.]


# What notable differences do you observe in the survival rates between the
# women and children versus the men in each group?
# Note at least 2 observations.
# [(1) The biggest difference between the survival rate of adult males and the
#     the survival rate of females and children appeared in the second class.
# (2) The survival rates of males was generally much lower than those of females
#   and children in all three classes and also the crew class]


### Reading in Data ###########################################################

# In this section, you'll work with .csv data of
# First, download the csv file of `Life Expectancy` data from GapMinder:
# https://www.gapminder.org/data/
# You should save the .csv file into your `data` directory


# Before getting started, explore the GapMinder website to better understand
# the *original* source of the data (e.g., who calculated these estimates)
# Place a brief summary of the each data source here (e.g., x1 - 2 sentences
# per data source)
# life expectancy(years): The average number of years a new born child would
#     live if current mortality patterns were to stay the same.
# life expectancy(male & female): The life expectancy at birth for male and
#     female with projections.
# life expectancy at birth:The life expectancy at birth, the data is taken from
#     Institute for health metrics and evaluation.
# Data quality - life expectancy: The data rates the quality for each
#     observation of "Life expectancy at birth"


# Using the `read.csv` function, read the life_expectancy_years.csv file into
# a variable called `life_exp`. Make sure not to read strings as factors
life_exp <- read.csv("data/life_expectancy_years.csv", stringsAsFactors = FALSE)

# Write a function `get_col_mean()` that takes a column name and a data frame
# and returns the mean of that column. Make sure to properly handle NA values
# Hint: `mean()` takes in an argument called `na.rm`
get_col_mean <- function(df, column) {
  column_picked <- df[, column]
  col_mean <- mean(column_picked, na.rm = TRUE)

  return(col_mean)
}

# Create a list `col_means` that has the mean value of each column in the
# data frame (except the `Country` column). You should use your function above.
# Hint: Use an `*apply` function (lapply, sapply, etc.)
clo_means <- sapply("X1800":"X2100", get_col_mean)

# Create a variable `avg_diff` that is the difference in average country life
# expectancy between 1800 and 2018
avg_diff <- get_col_mean(life_exp, "X2018") - get_col_mean(life_exp, "X1800")

# Create a column `life_exp$change` that is the change in life
# expectancy from 2000 to 2018. Increases in life expectancy should
# be *positive*
life_exp$change <- life_exp$X2018 - life_exp$X2000

# Create a variable `most_improved` that is the *name* of the country
# with the largest gain in life expectancy. Make sure to filter NA values
# Hint: `max()` takes in an argument called `na.rm`
most_improved <- life_exp[
  life_exp$change == max(life_exp$change, na.rm = TRUE),
  "country"
]
most_improved <- most_improved[is.na(most_improved) == FALSE]

# Create a variable `num_small_gain` that has the *number* of countries
# whose life expectance has improved less than 1 year between 2000 and 2018
# Make sure to filter NA values
# Hint: Lookup `is.na()`
num_small_gain <- life_exp[life_exp$change < 1, "country"]
num_small_gain <- num_small_gain[is.na(num_small_gain) == FALSE]

# Write a function `country_change()` that takes in a country's name,
# two years as numbers (not strings), and the `life_exp` data frame
# Parameters should be written *in the above order*
# It should return the phrase:
# "Between YEAR1 and YEAR2, the life expectancy in COUNTRY went DIRECTION by
# SOME_YEARS years".
# Make sure to properly indictate the DIRECTION as "up" or "down"
# Hint: Use an if/else statement to help compute DIRECTION
country_change <- function(name, year1, year2, df) {
  country_picked <- df[name, ]
  year1_col <- paste0("X", year1)
  year2_col <- paste0("X", year2)
  improve <- country_picked$year2_col - country_picked$year1_col
  if (improve >= 0) {
    answer1 <- paste0(
      "Between ",
      year1,
      " and ",
      year2,
      ", the life expectancy in ",
      name,
      " went up by ",
      improve,
      " years."
    )
    return(answer1)
  } else {
    answer2 <- paste0(
      "Between ",
      year1,
      " and ",
      year2,
      ", the life expectancy in ",
      name,
      " went down by ",
      abs(improve),
      " years."
    )
    return(answer2)
  }
}

# Using your `country_change()` function, create a variable `sweden_change`
# that is the change in life expectancy from 1960 to 1990 in Sweden
sweden_change <- country_change("Sweden", 1960, 1990, life_exp)

# Write a function `compare_change()` that takes in two country names and your
# `life_exp` data frame as parameters, and returns a sentence that describes
# their change in life expectancy from 2000 to 2018 (the `change` column)
# For example, if you passed the values "China", and "Bolivia" to you function,
# It would return this:
# "The country with the bigger change in life expectancy was China (gain=6.9),
#  whose life expectancy grew by 0.6 years more than Bolivia's (gain=6.3)."
# Make sure to round your numbers to one digit (though only after calculations)
# Hint: Use an if/else statement to paste the countries in the correct order
compare_change <- function(name1, name2, df) {
  change1 <- df[df$country == name1, "change"]
  change2 <- df[df$country == name2, "change"]
  diff <- change1 - change2
  diff <- round(diff, digits = 1)
  if (diff >= 0) {
    answer1 <- paste0(
      "The country with the bigger change in life expectancy was ",
      name1,
      " (gain=",
      round(change1, digits = 1),
      "), whose life expectancy grew by ",
      round(diff, digits = 1),
      " years more than ",
      name2, "(gain=",
      round(change2, digits = 1),
      ")."
    )
    return(answer1)
  } else {
    answer2 <- paste0(
      "The country with the bigger change in life expectancy was ",
      name2,
      " (gain=",
      round(change2, digits = 1),
      "), whose life expectancy grew by ",
      abs(diff),
      " years more than ",
      name1,
      "(gain=",
      round(change1, digits = 1),
      ")."
    )
    return(answer2)
  }
}

# Using your `bigger_change()` function, create a variable `usa_or_france`
# that describes who had a larger gain in life expectancy (the U.S. or France)
usa_or_france <- compare_change("United States", "France", life_exp)

# Write your `life_exp` data.frame to a new .csv file to your
# data/ directory with the filename `life_exp_with_change.csv`.
# Make sure not to write row names.
write.csv(life_exp, "data/life_exp_with_change.csv", row.names = FALSE)
