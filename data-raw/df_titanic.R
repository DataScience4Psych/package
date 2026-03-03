#  These data are from the Titanic dataset available in the `titanic` R package combined with additional data from the Titanic Kaggle competition. The dataset includes information on passengers' demographics, ticket details, and survival status.

library(tidyverse)
library(here)
library(readr)
library(usethis)
library(haven)
library(readxl)
library(BGmisc)
library(titanic)


## Create dataframe

df_ship <- readxl::read_excel("data-raw/titanic3.xls")

data("titanic_train")
data("titanic_test")

# Rename columns to lowercase
names(df_ship) <- names(df_ship) |> tolower()
names(titanic_train) <- names(titanic_train) |> tolower()
names(titanic_test) <- names(titanic_test) |> tolower()

# Check for overlapping names between datasets
df_ship |>
  filter(tolower(name) %in% tolower(titanic_train$name) &
    tolower(name) %in% tolower(titanic_test$name)) |>
  select(name)

# Create a variable to identify which dataset each row belongs to
trimms <- '["\\\\\\(\\)\\s]'

df <- df_ship |>
  mutate(
    name = str_remove_all(name, '"'),
    test = case_when( # there are two Kelly, Mr. James  and Connolly, Miss. Kate
      str_remove_all(tolower(name), trimms) %in% str_remove_all(tolower(titanic_train$name), trimms) &
        tolower(ticket) %in% tolower(titanic_train$ticket) ~ 0,
      str_remove_all(tolower(name), trimms) %in% str_remove_all(tolower(titanic_test$name), trimms) &
        tolower(ticket) %in% tolower(titanic_test$ticket) ~ 1,
      TRUE ~ NA_real_
    )
  )
summary(df$test)

sum(df$test == 0, na.rm = TRUE) # 891
sum(df$test == 1, na.rm = TRUE) # 418

df |>
  filter(is.na(test)) |>
  select(name)

titanic_passengers <- df


write_csv(titanic_passengers, here("data-raw", "titanic_passengers.csv"), na = "")

usethis::use_data(titanic_passengers, overwrite = TRUE, compress = "xz")
