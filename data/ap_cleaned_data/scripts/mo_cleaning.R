# Load necessary libraries
library(tidyverse)
library(here)
library(rio)
library(dplyr)
library(stringdist)
library(janitor)

# Set the file path using 'here' function
filepath <- here::here('data','mo_teen_ap.csv')  # Change file name to jau_tng.csv

# Import data into a tibble using pipe operator
df <- filepath |> rio::import(setclass = 'tibble')

df |> glimpse()

# Cleaning the data
df <-
  df |>
  janitor::clean_names()

# Convert all columns to character to handle '-' as a string
df[] <- lapply(df, as.character)

# Filter rows that contain '-'
partly_filled <- df[apply(df, 1, function(row) any(row == '-')), ]

# Filter rows that do not contain '-'
fully_filled <- df[apply(df, 1, function(row) !any(row == '-')), ]

#--------------------------------------------------------------------------------

# Define a function to clean text: convert to lower case and remove non-ASCII characters
clean_text <- function(text) {
  sapply(text, function(x) {
    if (is.na(x)) return(NA) # Handle NA values
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT") # Convert to ASCII
    tolower(x) # Convert to lowercase
  })
}

# Clean the names in the specified columns of the dataframe
partly_filled <- partly_filled %>%
  clean_names() %>%
  mutate(
    name_pre = clean_text(name_pre),
    name_post = clean_text(name_post),
    phc_name_pre = clean_text(phc_name_pre),
    phc_name_post = clean_text(phc_name_post)
  )

#--------------------------------------------------------------------------------

# Name wise clean up
# Create a unique identifier by combining name_pre and name_post
# Sort the names so that (name_pre, name_post) and (name_post, name_pre) are treated the same
partly_filled <- partly_filled %>%
  rowwise() %>%
  mutate(
    name_pair = paste(sort(c(name_pre, name_post)), collapse = "_")
  ) %>%
  ungroup()

# Group by the name_pair and merge the rows
merged_data <- partly_filled %>%
  group_by(name_pair) %>%
  summarise(across(everything(), ~ ifelse(all(. == "-"), "-", max(., na.rm = TRUE))))

# Drop the name_pair column as it's no longer needed
merged_data <- merged_data %>%
  select(-name_pair)

# Step 1: Filter merged_data to keep rows without any '-' values
final_name_wise <- merged_data %>%
  filter(!apply(., 1, function(row) any(row == "-")))

# Step 1: Filter merged_data to keep rows with any '-' values
merged_data_with_dash <- merged_data %>%
  filter(apply(., 1, function(row) any(row == "-")))

final_name_wise <- final_name_wise %>%
  filter(entry_pre == entry_post)

#-------------------------------------------------------------------------------

# PHC wise clean up

# Step 1: Create a unique identifier by combining 'phc_name_pre' and 'phc_name_post'
merged_data_with_dash <- merged_data_with_dash %>%
  rowwise() %>%
  mutate(
    phc_name_pair = paste(sort(c(phc_name_pre, phc_name_post)), collapse = "_"),
    age_pair = paste(sort(c(age_pre, age_post)), collapse = "_"),
    entry_pair = paste(sort(c(entry_pre, entry_post)), collapse = "_"),
    district_pair = paste(sort(c(districts_pre, districts_post)), collapse = "_")
  ) %>%
  ungroup()

# Step 2: Group the data by the 'phc_name_pair', and also by 'age', 'entry', and 'district' pairs
merged_data_phc <- merged_data_with_dash %>%
  group_by(
    phc_name_pair = paste(sort(c(age_pre, age_post)), collapse = "_"), 
    age_pair = paste(sort(c(age_pre, age_post)), collapse = "_"),
    entry_pair = paste(sort(c(entry_pre, entry_post)), collapse = "_"),
    district_pair = paste(sort(c(districts_pre, districts_post)), collapse = "_")
  ) %>%
  summarise(across(everything(), ~ ifelse(all(. == "-"), "-", max(., na.rm = TRUE)))) %>%
  ungroup()

# Step 3: Drop the identifier columns that are no longer needed
merged_data_phc <- merged_data_phc %>%
  select(-phc_name_pair, -age_pair, -entry_pair, -district_pair)

# Step 1: Filter merged_data_phc to keep rows without any '-' values
final_phc <- merged_data_phc %>%
  filter(!apply(., 1, function(row) any(row == "-")))

# Step 1: Filter merged_data_phc to keep rows with any '-' values
only_pre_or_post_filled <- merged_data_phc %>%
  filter(apply(., 1, function(row) any(row == "-")))

# Manually remove some rows
final_phc <- final_phc %>%
  filter(!name_pre %in% c(""))

#-------------------------------------------------------------------------------

# Step 2: Combine the filtered merged_data with fully_filled
clean_df <- bind_rows(fully_filled, final_name_wise, final_phc)

#-------------------------------------------------------------------------------

# Save the cleaned dataframe to a CSV file
write.csv(clean_df, "data/ap_cleaned_data/mo_cleaned_teen_ap.csv", row.names = FALSE)  # Save file with updated name

