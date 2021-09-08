###
# Processing Script
# Zane Billings
# This script loads the raw data, processes and cleans it 
# and saves it as Rds file in the processed_data folder.
###

# Load needed packages.
library(RSocrata) # for calling Socrata Open Data API
library(tidyverse) # for data processing and plotting
library(here) # to set paths
library(lubridate) # for date manipulation

# Data is from #https://data.cdc.gov/Flu-Vaccinations/Influenza-Vaccination-Coverage-for-All-Ages-6-Mont/vh55-3he6
# I thought it might be interesting to look at distribution of seasonal flu 
# vaccine coverage by state, over time, and across the different age groups.
# Note that vaccine coverage = % of people vaccinated.

# Call Socrata API to download the dataset of interest. This will likely take a
# few seconds to run.
data_link <- "https://data.cdc.gov/resource/vh55-3he6.json"
rawdata <- RSocrata::read.socrata(data_link)

# Looks like all of our data has been imported as character type, so we have
# a little bit of fixing to do before we can even get started look for issues.
dat1 <- rawdata |>
  # First, let's deal with the two columns that represent numeric data
  # I want to explicitly recode the missing values as R's "NA" value
  dplyr::mutate(
    # Normally I would use dplyr::na_if, but there are multiple values that I
    # want to recode as missing in coverage_estimate, and na_if is not flexible
    # enough to do that. But it is easy enough to do like this.
    coverage_estimate = dplyr::if_else(
      condition = coverage_estimate %in% c("NR †", "NR *", "NR"),
      true = NA_character_,
      false = coverage_estimate
    ),
    # In the CI column, there are "NA" strings that should be actual NA values
    X_95_ci = dplyr::na_if(X_95_ci, "NA")
  ) |>
  # Next, I want to separate the CI column into two columns. The CI should be
  # split into two columns at the " to ", which can be thrown away. The result
  # here should be two character columns that are coercible to numeric type.
  tidyr::separate(
    col = X_95_ci,
    into = c("coverage_ci_lwr", "coverage_ci_upr"),
    sep = " to "
  ) |>
  # Now I want to coerce all three of these columns into numeric values. Using
  # the parse_number function allows for handling of some weird situations and
  # will warn us if something in the column isn't a number. If nothing weird
  # is going on, this behaves exactly like as.numeric().
  dplyr::mutate(
    across(starts_with("coverage"), readr::parse_number)
  )

# Next we can do some miscellaneous type casting before we tackle what I think
# is the most annoying problem in this data set. Here are the type castings
# that need to occur:
# population_sample_size -> integer;
# vaccine -> filter out only seasonal;
# geography_type -> factor;
# geography can stay as character for now. there are 1922 unique levels but
#  they are nested in geography_type, so can be type casted where needed;
# fips can stay as character. Most are sparse and this info should be encoded
#  in the other geography variables, so no need to use RAM on a big factor;
# year/season and month -> combine into date variable.
# dimension_type / dimension: these are the annoying ones that I will do in
#  a separate step.
# Let's do the simple conversions (everything except date and dimension) first.

dat2 <- dat1 |>
  # filter for seasonal vaccine only
  dplyr::filter(vaccine == "Seasonal Influenza") |>
  # type castings that don't need other work
  dplyr::mutate(
    geography_type = as.factor(geography_type),
    population_sample_size = readr::parse_integer(population_sample_size)
  )

# Next step is to combine the dates together. There are multiple ways to deal
#  with this, but for now I'll take the simplistic approach of marking each
#  date as the first day of the month.
# Then afterwards, I can convert the flu season to a factor, as well as month
#  and the constructed year to numeric.
# For this conversion, I will assume that month 8 - 12 are in the first year of
#  the season, and month 1 - 5 are in the second year of the season. This is
#  the norm for flu data, but it is not technically in the documentation, so it
#  is an assumption.

dat3 <- dat2 |>
  dplyr::mutate(
    # First, get the year based on month and season
    year = dplyr::if_else(
      # check if month is between August to December
      as.numeric(month) >= 8,
      # if yes, year is first part of year_season character variable
      as.integer(substr(year_season, 1, 4)),
      # if no, year is second half (needs to be formatted correctly)
      as.integer(paste0("20", substr(year_season, 6, 7)))
    ),
    # Paste the date together in ISO-8601 format with day as 01, so that it
    #  can be parsed into a date-type variable
    date = lubridate::as_date(paste0(year, "-", month, "-01")),
    # Convert month to a nice factor
    month = factor(month.name[as.numeric(month)], levels=month.name),
    # Also, make year_season an ordered factor.
    year_season = factor(year_season, ordered = TRUE)
  )

# Now I will say I am only interested in state-level data. (Mainly because this
# will make the rest of cleaning easier.) So let's get rid of all the
# observations that are not state level. This also means we can drop the
# "geography_type" column.
#
# I'm also going to create a row number, because this will make it much easier
# to pivot the dimension and dimension_type columns. Furthermore, I'm going to
# filter out the dimension_type so that I only get the Age columns, because
# cleaning up the rest is really just a pain and I don't need to do it for this
# exercise.

dat4 <- dat3 |>
  dplyr::filter(
    # I think the easiest way to get only states is to filter by the FIPS codes
    # which are for states (have 2 digits)
    nchar(fips) == 2,
    dimension_type == "Age"
  ) |>
  # now that there should only be 51 levels, let's make geography a factor
  dplyr::mutate(
    geography = factor(geography)
  )

# Now we can actually pivot the dimension column, and it won't be too much of
# a pain to clean up.
dat5 <- dat4 |>
  # Each row is distinct, but we don't have a superkey, so we need to create
  # a simple one. If we don't, there will be issues with places NA for all
  # age groups getting turned into list columns in the next step.
  dplyr::mutate(
    r_id = row_number()
  ) |>
  # Now pivot the dimension data so we get an age column.
  tidyr::pivot_wider(
    names_from = dimension_type,
    values_from = dimension
  ) 

# Finally, we need to clean up the new age column. It is REALLY bad with a
# a bunch of absolutely insane values, so I'm going to filter for these ones:
# 6 Months - 4 years, 5 - 12 Years, 13 - 17 Years, 18 - 65 Years, >= 65 years.
# We will lose all the data about high risk, but oh well.
age_groups <- c("6 Months - 4 Years",
                "5-12 Years",
                "13-17 Years",
                "18-64 Years",
                ">= 65 Years")

dat6 <- dat5 |>
  # filter for only the listed age groups
  filter(Age %in% age_groups) |>
  # make age an ordered factor
  dplyr::mutate(
    Age = factor(Age, ordered = TRUE, levels = age_groups)
  )

clean_dat <- dat6 |>
  # rename Age to age for consistency.
  # also rename geography to state.
  dplyr::rename(
    age = Age,
    state = geography,
    flu_season = year_season
  ) |>
  # get rid of redundant/unnecessary vars
  dplyr::select(
    !c(fips, r_id, geography_type, vaccine)
  )

# Now the data is clean :) (clean enough to use anyways)
# save data as RDS
# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(clean_dat, file = save_data_location)


