# Load necessary libraries
library(tidyverse)
library(here)
library(rio)
library(dplyr)
library(ggplot2)
library(gtsummary)
library(gt)
library(tidyr)
library(viridis)

# Define the file paths
filepaths <- c(
  here::here('pre_post_test','data','ap_cleaned_data','mo_cleaned_plscs_ap.csv'),
  here::here('pre_post_test','data','ap_cleaned_data','mo_cleaned_hrt_ap.csv'),
  here::here('pre_post_test','data','ap_cleaned_data','mo_cleaned_tb_ap.csv'),
  here::here('pre_post_test','data','ap_cleaned_data','mo_cleaned_fever_ap.csv'),
  here::here('pre_post_test','data','ap_cleaned_data','mo_cleaned_fgr_ap.csv'),
  here::here('pre_post_test','data', 'ap_cleaned_data','mo_cleaned_ble_ap.csv'),
  here::here('pre_post_test','data','ap_cleaned_data','mo_cleaned_pprom_ap.csv'),
  here::here('pre_post_test','data','ap_cleaned_data','mo_cleaned_hiv_ap.csv'),
  here::here('pre_post_test','data','ap_cleaned_data','mo_cleaned_rfm_ap.csv'),
  here::here('pre_post_test','data','ap_cleaned_data','mo_cleaned_hbsag_ap.csv'),
  here::here('pre_post_test','data','ap_cleaned_data','mo_cleaned_rh_ap.csv'),
  here::here('pre_post_test','data','ap_cleaned_data','mo_cleaned_teen_ap.csv'),
  here::here('pre_post_test','data', 'ap_cleaned_data','mo_cleaned_pt_ap.csv'))


# Import all files as data frames
data_frames <- lapply(filepaths, read.csv)

# Bind all data frames into a single data frame
df_mo <- do.call(rbind, data_frames)

#Cleaning the data

df_mo <-
  df_mo |>
  janitor::clean_names()

#Remove unwanted columns

df_mo <- df_mo |> 
  select(-c("number",
            "name_post", 
            "age_post", 
            "entry_post", 
            "districts_post", 
            "phc_name_post", 
            "experience_post", 
            "mobile_no_post",
            "date_post"))

#Districts with district names

df_mo$age_pre <- df_mo$age_pre |> as.character()

# Ensure `age_pre` is numeric
df_mo$age_pre <- as.numeric(df_mo$age_pre)

# Current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Identify values in `age_pre` that are in the year format and convert them to age
df_mo$age_pre <- ifelse(df_mo$age_pre >= 1900 & df_mo$age_pre <= current_year,  # Assuming any value between 1900 and the current year is a year
                        current_year - df_mo$age_pre,  # Convert year to age
                        df_mo$age_pre)  # Leave other values unchanged

#Change the district names and mutate a column with the concerned district names

df_mo <- df_mo %>%
  mutate(district_name = case_when(
    districts_pre == 1 ~ "Alluri Sitaram Raju",
    districts_pre == 2 ~ "Anakapalli",
    districts_pre == 3 ~ "Ananthapuram",
    districts_pre == 4 ~ "Annamayya",
    districts_pre == 5 ~ "Bapatla",
    districts_pre == 6 ~ "Chittoor",
    districts_pre == 7 ~ "Kadapa",
    districts_pre == 8 ~ "East Godavari",
    districts_pre == 9 ~ "Eluru",
    districts_pre == 10 ~ "Guntur",
    districts_pre == 11 ~ "Kakinada",
    districts_pre == 12 ~ "Konaseema",
    districts_pre == 13 ~ "Krishna",
    districts_pre == 14 ~ "Kurnool",
    districts_pre == 15 ~ "NTR",
    districts_pre == 16 ~ "Nandyal",
    districts_pre == 17 ~ "Nellore",
    districts_pre == 18 ~ "Palnadu",
    districts_pre == 19 ~ "Parvatipuram Manyam",
    districts_pre == 20 ~ "Prakasam",
    districts_pre == 21 ~ "Srikakulam",
    districts_pre == 22 ~ "Sri Sathya Sai",
    districts_pre == 23 ~ "Tirupati",
    districts_pre == 24 ~ "Visakhapatnam",
    districts_pre == 25 ~ "Vijayanagaram",
    districts_pre == 26 ~ "West Godavari",
    TRUE ~ NA_character_
  ))

#PLSCS Correct answers 
#Pre 
df_mo <- df_mo %>%
  mutate(
    q1_pre = ifelse(condition == "plscs", ifelse(q1_pre == 4, 1, 0), q1_pre),
    q2_pre = ifelse(condition == "plscs", ifelse(q2_pre == 4, 1, 0), q2_pre),
    q3_pre = ifelse(condition == "plscs", ifelse(q3_pre == 3, 1, 0), q3_pre),
    q4_pre = ifelse(condition == "plscs", ifelse(q4_pre == 3, 1, 0), q4_pre),
    q5_pre = ifelse(condition == "plscs", ifelse(q5_pre == 1, 1, 0), q5_pre),
    q6_pre = ifelse(condition == "plscs", ifelse(q6_pre == 2, 1, 0), q6_pre),
    q7_pre = ifelse(condition == "plscs", ifelse(q7_pre == 4, 1, 0), q7_pre),
    q8_pre = ifelse(condition == "plscs", ifelse(q8_pre == 3, 1, 0), q8_pre),
    q9_pre = ifelse(condition == "plscs", ifelse(q9_pre == 1, 1, 0), q9_pre),
    q10_pre = ifelse(condition == "plscs", ifelse(q10_pre == 4, 1, 0), q10_pre))

#Post
df_mo <- df_mo %>%
  mutate(
    q1_post = ifelse(condition == "plscs", ifelse(q1_post == 4, 1, 0), q1_post),
    q2_post = ifelse(condition == "plscs", ifelse(q2_post == 4, 1, 0), q2_post),
    q3_post = ifelse(condition == "plscs", ifelse(q3_post == 3, 1, 0), q3_post),
    q4_post = ifelse(condition == "plscs", ifelse(q4_post == 3, 1, 0), q4_post),
    q5_post = ifelse(condition == "plscs", ifelse(q5_post == 1, 1, 0), q5_post),
    q6_post = ifelse(condition == "plscs", ifelse(q6_post == 2, 1, 0), q6_post),
    q7_post = ifelse(condition == "plscs", ifelse(q7_post == 4, 1, 0), q7_post),
    q8_post = ifelse(condition == "plscs", ifelse(q8_post == 3, 1, 0), q8_post),
    q9_post = ifelse(condition == "plscs", ifelse(q9_post == 1,1, 0), q9_post),
    q10_post = ifelse(condition == "plscs", ifelse(q10_post == 4, 1, 0), q10_post))

#HRT Correct answers
#Pre
df_mo <- df_mo %>%
  mutate(
    q1_pre = ifelse(condition == "hrt", ifelse(q1_pre == 3, 1, 0), q1_pre),
    q2_pre = ifelse(condition == "hrt", ifelse(q2_pre == 3, 1, 0), q2_pre),
    q3_pre = ifelse(condition == "hrt", ifelse(q3_pre == 2, 1, 0), q3_pre),
    q4_pre = ifelse(condition == "hrt", ifelse(q4_pre == 3, 1, 0), q4_pre),
    q5_pre = ifelse(condition == "hrt", ifelse(q5_pre == 3, 1, 0), q5_pre),
    q6_pre = ifelse(condition == "hrt", ifelse(q6_pre == 4, 1, 0), q6_pre),
    q7_pre = ifelse(condition == "hrt", ifelse(q7_pre == 2, 1, 0), q7_pre),
    q8_pre = ifelse(condition == "hrt", ifelse(q8_pre == 4, 1, 0), q8_pre),
    q9_pre = ifelse(condition == "hrt", ifelse(q9_pre == 3, 1, 0), q9_pre),
    q10_pre = ifelse(condition == "hrt", ifelse(q10_pre == 2, 1, 0), q10_pre))

#Post
df_mo <- df_mo %>%
  mutate(
    q1_post = ifelse(condition == "hrt", ifelse(q1_post == 3, 1, 0), q1_post),
    q2_post = ifelse(condition == "hrt", ifelse(q2_post == 3, 1, 0), q2_post),
    q3_post = ifelse(condition == "hrt", ifelse(q3_post == 2, 1, 0), q3_post),
    q4_post = ifelse(condition == "hrt", ifelse(q4_post == 3, 1, 0), q4_post),
    q5_post = ifelse(condition == "hrt", ifelse(q5_post == 3, 1, 0), q5_post),
    q6_post = ifelse(condition == "hrt", ifelse(q6_post == 4, 1, 0), q6_post),
    q7_post = ifelse(condition == "hrt", ifelse(q7_post == 2, 1, 0), q7_post),
    q8_post = ifelse(condition == "hrt", ifelse(q8_post == 4, 1, 0), q8_post),
    q9_post = ifelse(condition == "hrt", ifelse(q9_post == 3, 1, 0), q9_post),
    q10_post = ifelse(condition == "hrt", ifelse(q10_post == 2, 1, 0), q10_post))

#TB Correct answers
#Pre
df_mo <- df_mo %>%
  mutate(
    q1_pre = ifelse(condition == "tb", ifelse(q1_pre == 4, 1, 0), q1_pre),
    q2_pre = ifelse(condition == "tb", ifelse(q2_pre == 3, 1, 0), q2_pre),
    q3_pre = ifelse(condition == "tb", ifelse(q3_pre == 2, 1, 0), q3_pre),
    q4_pre = ifelse(condition == "tb", ifelse(q4_pre == 3, 1, 0), q4_pre),
    q5_pre = ifelse(condition == "tb", ifelse(q5_pre == 1, 1, 0), q5_pre),
    q6_pre = ifelse(condition == "tb", ifelse(q6_pre == 4, 1, 0), q6_pre),
    q7_pre = ifelse(condition == "tb", ifelse(q7_pre == 3, 1, 0), q7_pre),
    q8_pre = ifelse(condition == "tb", ifelse(q8_pre == 4, 1, 0), q8_pre),
    q9_pre = ifelse(condition == "tb", ifelse(q9_pre == 1, 1, 0), q9_pre),
    q10_pre = ifelse(condition == "tb", ifelse(q10_pre == 2, 1, 0), q10_pre))

#Post
df_mo <- df_mo %>%
  mutate(
    q1_post = ifelse(condition == "tb", ifelse(q1_post == 4, 1, 0), q1_post),
    q2_post = ifelse(condition == "tb", ifelse(q2_post == 3, 1, 0), q2_post),
    q3_post = ifelse(condition == "tb", ifelse(q3_post == 2, 1, 0), q3_post),
    q4_post = ifelse(condition == "tb", ifelse(q4_post == 3, 1, 0), q4_post),
    q5_post = ifelse(condition == "tb", ifelse(q5_post == 1, 1, 0), q5_post),
    q6_post = ifelse(condition == "tb", ifelse(q6_post == 4, 1, 0), q6_post),
    q7_post = ifelse(condition == "tb", ifelse(q7_post == 3, 1, 0), q7_post),
    q8_post = ifelse(condition == "tb", ifelse(q8_post == 4, 1, 0), q8_post),
    q9_post = ifelse(condition == "tb", ifelse(q9_post == 1, 1, 0), q9_post),
    q10_post = ifelse(condition == "tb", ifelse(q10_post == 2, 1, 0), q10_post))


#Fever Correct answers 
#Pre 
df_mo <- df_mo %>%
  mutate(
    q1_pre = ifelse(condition == "fever", ifelse(q1_pre == 4, 1, 0), q1_pre),
    q2_pre = ifelse(condition == "fever", ifelse(q2_pre == 4, 1, 0), q2_pre),
    q3_pre = ifelse(condition == "fever", ifelse(q3_pre == 4, 1, 0), q3_pre),
    q4_pre = ifelse(condition == "fever", ifelse(q4_pre == 4, 1, 0), q4_pre),
    q5_pre = ifelse(condition == "fever", ifelse(q5_pre == 2, 1, 0), q5_pre),
    q6_pre = ifelse(condition == "fever", ifelse(q6_pre == 1, 1, 0), q6_pre),
    q7_pre = ifelse(condition == "fever", ifelse(q7_pre == 4, 1, 0), q7_pre),
    q8_pre = ifelse(condition == "fever", ifelse(q8_pre == 3, 1, 0), q8_pre),
    q9_pre = ifelse(condition == "fever", ifelse(q9_pre == 4, 1, 0), q9_pre),
    q10_pre = ifelse(condition == "fever", ifelse(q10_pre == 2, 1, 0), q10_pre))

#Post
df_mo <- df_mo %>%
  mutate(
    q1_post = ifelse(condition == "fever", ifelse(q1_post == 4, 1, 0), q1_post),
    q2_post = ifelse(condition == "fever", ifelse(q2_post == 4, 1, 0), q2_post),
    q3_post = ifelse(condition == "fever", ifelse(q3_post == 4, 1, 0), q3_post),
    q4_post = ifelse(condition == "fever", ifelse(q4_post == 4, 1, 0), q4_post),
    q5_post = ifelse(condition == "fever", ifelse(q5_post == 2, 1, 0), q5_post),
    q6_post = ifelse(condition == "fever", ifelse(q6_post == 1, 1, 0), q6_post),
    q7_post = ifelse(condition == "fever", ifelse(q7_post == 4, 1, 0), q7_post),
    q8_post = ifelse(condition == "fever", ifelse(q8_post == 3, 1, 0), q8_post),
    q9_post = ifelse(condition == "fever", ifelse(q9_post == 4, 1, 0), q9_post),
    q10_post = ifelse(condition == "fever", ifelse(q10_post == 2, 1, 0), q10_post))

#FGR Correct answers 
#Pre
df_mo <- df_mo %>%
  mutate(
    q1_pre = ifelse(condition == "fgr", ifelse(q1_pre == 2, 1, 0), q1_pre),
    q2_pre = ifelse(condition == "fgr", ifelse(q2_pre == 4, 1, 0), q2_pre),
    q3_pre = ifelse(condition == "fgr", ifelse(q3_pre == 6, 1, 0), q3_pre),
    q4_pre = ifelse(condition == "fgr", ifelse(q4_pre == 4, 1, 0), q4_pre),
    q5_pre = ifelse(condition == "fgr", ifelse(q5_pre == 4, 1, 0), q5_pre),
    q6_pre = ifelse(condition == "fgr", ifelse(q6_pre == 3, 1, 0), q6_pre),
    q7_pre = ifelse(condition == "fgr", ifelse(q7_pre == 4, 1, 0), q7_pre),
    q8_pre = ifelse(condition == "fgr", ifelse(q8_pre == 1, 1, 0), q8_pre),
    q9_pre = ifelse(condition == "fgr", ifelse(q9_pre == 4, 1, 0), q9_pre),
    q10_pre = ifelse(condition == "fgr", ifelse(q10_pre == 4, 1, 0), q10_pre))

#Post
df_mo <- df_mo %>%
  mutate(
    q1_post = ifelse(condition == "fgr", ifelse(q1_post == 2, 1, 0), q1_post),
    q2_post = ifelse(condition == "fgr", ifelse(q2_post == 4, 1, 0), q2_post),
    q3_post = ifelse(condition == "fgr", ifelse(q3_post == 6, 1, 0), q3_post),
    q4_post = ifelse(condition == "fgr", ifelse(q4_post == 4, 1, 0), q4_post),
    q5_post = ifelse(condition == "fgr", ifelse(q5_post == 4, 1, 0), q5_post),
    q6_post = ifelse(condition == "fgr", ifelse(q6_post == 3, 1, 0), q6_post),
    q7_post = ifelse(condition == "fgr", ifelse(q7_post == 4, 1, 0), q7_post),
    q8_post = ifelse(condition == "fgr", ifelse(q8_post == 1, 1, 0), q8_post),
    q9_post = ifelse(condition == "fgr", ifelse(q9_post == 4, 1, 0), q9_post),
    q10_post = ifelse(condition == "fgr", ifelse(q10_post == 4, 1, 0), q10_post))

#BLE Correct answers 

#Pre
df_mo <- df_mo %>%
  mutate(
    q1_pre = ifelse(condition == "ble", ifelse(q1_pre == 4, 1, 0), q1_pre),
    q2_pre = ifelse(condition == "ble", ifelse(q2_pre == 3, 1, 0), q2_pre),
    q3_pre = ifelse(condition == "ble", ifelse(q3_pre == 1, 1, 0), q3_pre),
    q4_pre = ifelse(condition == "ble", ifelse(q4_pre == 1, 1, 0), q4_pre),
    q5_pre = ifelse(condition == "ble", ifelse(q5_pre == 1, 1, 0), q5_pre),
    q6_pre = ifelse(condition == "ble", ifelse(q6_pre == 3, 1, 0), q6_pre),
    q7_pre = ifelse(condition == "ble", ifelse(q7_pre == 1, 1, 0), q7_pre),
    q8_pre = ifelse(condition == "ble", ifelse(q8_pre == 5, 1, 0), q8_pre),
    q9_pre = ifelse(condition == "ble", ifelse(q9_pre == 1, 1, 0), q9_pre),
    q10_pre = ifelse(condition == "ble", ifelse(q10_pre == 3, 1, 0), q10_pre))

#Post
df_mo <- df_mo %>%
  mutate(
    q1_post = ifelse(condition == "ble", ifelse(q1_post == 4, 1, 0), q1_post),
    q2_post = ifelse(condition == "ble", ifelse(q2_post == 3, 1, 0), q2_post),
    q3_post = ifelse(condition == "ble", ifelse(q3_post == 1, 1, 0), q3_post),
    q4_post = ifelse(condition == "ble", ifelse(q4_post == 1, 1, 0), q4_post),
    q5_post = ifelse(condition == "ble", ifelse(q5_post == 1, 1, 0), q5_post),
    q6_post = ifelse(condition == "ble", ifelse(q6_post == 3, 1, 0), q6_post),
    q7_post = ifelse(condition == "ble", ifelse(q7_post == 1, 1, 0), q7_post),
    q8_post = ifelse(condition == "ble", ifelse(q8_post == 5, 1, 0), q8_post),
    q9_post = ifelse(condition == "ble", ifelse(q9_post == 1, 1, 0), q9_post),
    q10_post = ifelse(condition == "ble", ifelse(q10_post == 3, 1, 0), q10_post))
 
#PPROM Correct answers 

#Pre
df_mo <- df_mo %>%
  mutate(
    q1_pre = ifelse(condition == "pprom", ifelse(q1_pre == 3, 1, 0), q1_pre),
    q2_pre = ifelse(condition == "pprom", ifelse(q2_pre == 1, 1, 0), q2_pre),
    q3_pre = ifelse(condition == "pprom", ifelse(q3_pre == 4, 1, 0), q3_pre),
    q4_pre = ifelse(condition == "pprom", ifelse(q4_pre == 1, 1, 0), q4_pre),
    q5_pre = ifelse(condition == "pprom", ifelse(q5_pre == 5, 1, 0), q5_pre),
    q6_pre = ifelse(condition == "pprom", ifelse(q6_pre == 4, 1, 0), q6_pre),
    q7_pre = ifelse(condition == "pprom", ifelse(q7_pre == 3, 1, 0), q7_pre),
    q8_pre = ifelse(condition == "pprom", ifelse(q8_pre == 4, 1, 0), q8_pre),
    q9_pre = ifelse(condition == "pprom", ifelse(q9_pre == 4, 1, 0), q9_pre),
    q10_pre = ifelse(condition == "pprom", ifelse(q10_pre == 4, 1, 0), q10_pre))

#Post
df_mo <- df_mo %>%
  mutate(
    q1_post = ifelse(condition == "pprom", ifelse(q1_post == 3, 1, 0), q1_post),
    q2_post = ifelse(condition == "pprom", ifelse(q2_post == 1, 1, 0), q2_post),
    q3_post = ifelse(condition == "pprom", ifelse(q3_post == 4, 1, 0), q3_post),
    q4_post = ifelse(condition == "pprom", ifelse(q4_post == 1, 1, 0), q4_post),
    q5_post = ifelse(condition == "pprom", ifelse(q5_post == 5, 1, 0), q5_post),
    q6_post = ifelse(condition == "pprom", ifelse(q6_post == 4, 1, 0), q6_post),
    q7_post = ifelse(condition == "pprom", ifelse(q7_post == 3, 1, 0), q7_post),
    q8_post = ifelse(condition == "pprom", ifelse(q8_post == 4, 1, 0), q8_post),
    q9_post = ifelse(condition == "pprom", ifelse(q9_post == 4, 1, 0), q9_post),
    q10_post = ifelse(condition == "pprom", ifelse(q10_post == 4, 1, 0), q10_post))

#HIV Correct answers 

#Pre
df_mo <- df_mo %>%
  mutate(
    q1_pre = ifelse(condition == "hiv", ifelse(q1_pre == 1, 1, 0), q1_pre),
    q2_pre = ifelse(condition == "hiv", ifelse(q2_pre == 4, 1, 0), q2_pre),
    q3_pre = ifelse(condition == "hiv", ifelse(q3_pre == 2, 1, 0), q3_pre),
    q4_pre = ifelse(condition == "hiv", ifelse(q4_pre == 5, 1, 0), q4_pre),
    q5_pre = ifelse(condition == "hiv", ifelse(q5_pre == 4, 1, 0), q5_pre),
    q6_pre = ifelse(condition == "hiv", ifelse(q6_pre == 4, 1, 0), q6_pre),
    q7_pre = ifelse(condition == "hiv", ifelse(q7_pre == 1, 1, 0), q7_pre),
    q8_pre = ifelse(condition == "hiv", ifelse(q8_pre == 4, 1, 0), q8_pre),
    q9_pre = ifelse(condition == "hiv", ifelse(q9_pre == 3, 1, 0), q9_pre),
    q10_pre = ifelse(condition == "hiv", ifelse(q10_pre == 4, 1, 0), q10_pre))

#Post
df_mo <- df_mo %>%
  mutate(
    q1_post = ifelse(condition == "hiv", ifelse(q1_post == 1, 1, 0), q1_post),
    q2_post = ifelse(condition == "hiv", ifelse(q2_post == 4, 1, 0), q2_post),
    q3_post = ifelse(condition == "hiv", ifelse(q3_post == 2, 1, 0), q3_post),
    q4_post = ifelse(condition == "hiv", ifelse(q4_post == 5, 1, 0), q4_post),
    q5_post = ifelse(condition == "hiv", ifelse(q5_post == 4, 1, 0), q5_post),
    q6_post = ifelse(condition == "hiv", ifelse(q6_post == 4, 1, 0), q6_post),
    q7_post = ifelse(condition == "hiv", ifelse(q7_post == 1, 1, 0), q7_post),
    q8_post = ifelse(condition == "hiv", ifelse(q8_post == 4, 1, 0), q8_post),
    q9_post = ifelse(condition == "hiv", ifelse(q9_post == 3, 1, 0), q9_post),
    q10_post = ifelse(condition == "hiv", ifelse(q10_post == 4, 1, 0), q10_post))

#RFM Correct answers 

#Pre
df_mo <- df_mo %>%
  mutate(
    q1_pre = ifelse(condition == "rfm", ifelse(q1_pre == 4, 1, 0), q1_pre),
    q2_pre = ifelse(condition == "rfm", ifelse(q2_pre == 4, 1, 0), q2_pre),
    q3_pre = ifelse(condition == "rfm", ifelse(q3_pre == 3, 1, 0), q3_pre),
    q4_pre = ifelse(condition == "rfm", ifelse(q4_pre == 4, 1, 0), q4_pre),
    q5_pre = ifelse(condition == "rfm", ifelse(q5_pre == 3, 1, 0), q5_pre),
    q6_pre = ifelse(condition == "rfm", ifelse(q6_pre == 3, 1, 0), q6_pre),
    q7_pre = ifelse(condition == "rfm", ifelse(q7_pre == 4, 1, 0), q7_pre),
    q8_pre = ifelse(condition == "rfm", ifelse(q8_pre == 4, 1, 0), q8_pre),
    q9_pre = ifelse(condition == "rfm", ifelse(q9_pre == 0, 0, 0), q9_pre),
    q10_pre = ifelse(condition == "rfm", ifelse(q10_pre == 0, 0, 0), q10_pre))

#Post
df_mo <- df_mo %>%
  mutate(
    q1_post = ifelse(condition == "rfm", ifelse(q1_post == 4, 1, 0), q1_post),
    q2_post = ifelse(condition == "rfm", ifelse(q2_post == 4, 1, 0), q2_post),
    q3_post = ifelse(condition == "rfm", ifelse(q3_post == 3, 1, 0), q3_post),
    q4_post = ifelse(condition == "rfm", ifelse(q4_post == 4, 1, 0), q4_post),
    q5_post = ifelse(condition == "rfm", ifelse(q5_post == 3, 1, 0), q5_post),
    q6_post = ifelse(condition == "rfm", ifelse(q6_post == 3, 1, 0), q6_post),
    q7_post = ifelse(condition == "rfm", ifelse(q7_post == 4, 1, 0), q7_post),
    q8_post = ifelse(condition == "rfm", ifelse(q8_post == 4, 1, 0), q8_post),
    q9_post = ifelse(condition == "rfm", ifelse(q9_post == 0, 0, 0), q9_post),
    q10_post = ifelse(condition == "rfm", ifelse(q10_post == 0, 0, 0), q10_post))

#HBSAG Correct answers 

#Pre
df_mo <- df_mo %>%
  mutate(
    q1_pre = ifelse(condition == "hbsag", ifelse(q1_pre == 2, 1, 0), q1_pre),
    q2_pre = ifelse(condition == "hbsag", ifelse(q2_pre == 2, 1, 0), q2_pre),
    q3_pre = ifelse(condition == "hbsag", ifelse(q3_pre == 2, 1, 0), q3_pre),
    q4_pre = ifelse(condition == "hbsag", ifelse(q4_pre == 3, 1, 0), q4_pre),
    q5_pre = ifelse(condition == "hbsag", ifelse(q5_pre == 2, 1, 0), q5_pre),
    q6_pre = ifelse(condition == "hbsag", ifelse(q6_pre == 3, 1, 0), q6_pre),
    q7_pre = ifelse(condition == "hbsag", ifelse(q7_pre == 4, 1, 0), q7_pre),
    q8_pre = ifelse(condition == "hbsag", ifelse(q8_pre == 4, 1, 0), q8_pre),
    q9_pre = ifelse(condition == "hbsag", ifelse(q9_pre == 3, 1, 0), q9_pre),
    q10_pre = ifelse(condition == "hbsag", ifelse(q10_pre == 3, 1, 0), q10_pre))

#Post
df_mo <- df_mo %>%
  mutate(
    q1_post = ifelse(condition == "hbsag", ifelse(q1_post == 2, 1, 0), q1_post),
    q2_post = ifelse(condition == "hbsag", ifelse(q2_post == 2, 1, 0), q2_post),
    q3_post = ifelse(condition == "hbsag", ifelse(q3_post == 2, 1, 0), q3_post),
    q4_post = ifelse(condition == "hbsag", ifelse(q4_post == 3, 1, 0), q4_post),
    q5_post = ifelse(condition == "hbsag", ifelse(q5_post == 2, 1, 0), q5_post),
    q6_post = ifelse(condition == "hbsag", ifelse(q6_post == 3, 1, 0), q6_post),
    q7_post = ifelse(condition == "hbsag", ifelse(q7_post == 4, 1, 0), q7_post),
    q8_post = ifelse(condition == "hbsag", ifelse(q8_post == 4, 1, 0), q8_post),
    q9_post = ifelse(condition == "hbsag", ifelse(q9_post == 3, 1, 0), q9_post),
    q10_post = ifelse(condition == "hbsag", ifelse(q10_post == 3, 1, 0), q10_post))

#RH Correct answers 

#Pre
df_mo <- df_mo %>%
  mutate(
    q1_pre = ifelse(condition == "rh", ifelse(q1_pre == 2, 1, 0), q1_pre),
    q2_pre = ifelse(condition == "rh", ifelse(q2_pre == 3, 1, 0), q2_pre),
    q3_pre = ifelse(condition == "rh", ifelse(q3_pre == 1, 1, 0), q3_pre),
    q4_pre = ifelse(condition == "rh", ifelse(q4_pre == 3, 1, 0), q4_pre),
    q5_pre = ifelse(condition == "rh", ifelse(q5_pre == 3, 1, 0), q5_pre),
    q6_pre = ifelse(condition == "rh", ifelse(q6_pre == 2, 1, 0), q6_pre),
    q7_pre = ifelse(condition == "rh", ifelse(q7_pre == 3, 1, 0), q7_pre),
    q8_pre = ifelse(condition == "rh", ifelse(q8_pre == 2, 1, 0), q8_pre),
    q9_pre = ifelse(condition == "rh", ifelse(q9_pre == 1, 1, 0), q9_pre),
    q10_pre = ifelse(condition == "rh", ifelse(q10_pre == 2, 1, 0), q10_pre))

#Post
df_mo <- df_mo %>%
  mutate(
    q1_post = ifelse(condition == "rh", ifelse(q1_post == 2, 1, 0), q1_post),
    q2_post = ifelse(condition == "rh", ifelse(q2_post == 3, 1, 0), q2_post),
    q3_post = ifelse(condition == "rh", ifelse(q3_post == 1, 1, 0), q3_post),
    q4_post = ifelse(condition == "rh", ifelse(q4_post == 3, 1, 0), q4_post),
    q5_post = ifelse(condition == "rh", ifelse(q5_post == 3, 1, 0), q5_post),
    q6_post = ifelse(condition == "rh", ifelse(q6_post == 2, 1, 0), q6_post),
    q7_post = ifelse(condition == "rh", ifelse(q7_post == 3, 1, 0), q7_post),
    q8_post = ifelse(condition == "rh", ifelse(q8_post == 2, 1, 0), q8_post),
    q9_post = ifelse(condition == "rh", ifelse(q9_post == 1, 1, 0), q9_post),
    q10_post = ifelse(condition == "rh", ifelse(q10_post == 2, 1, 0), q10_post))

#TEEN Correct answers 

#Pre
df_mo <- df_mo %>%
  mutate(
    q1_pre = ifelse(condition == "teen", ifelse(q1_pre == 2, 1, 0), q1_pre),
    q2_pre = ifelse(condition == "teen", ifelse(q2_pre == 2, 1, 0), q2_pre),
    q3_pre = ifelse(condition == "teen", ifelse(q3_pre == 1, 1, 0), q3_pre),
    q4_pre = ifelse(condition == "teen", ifelse(q4_pre == 1, 1, 0), q4_pre),
    q5_pre = ifelse(condition == "teen", ifelse(q5_pre == 4, 1, 0), q5_pre),
    q6_pre = ifelse(condition == "teen", ifelse(q6_pre == 2, 1, 0), q6_pre),
    q7_pre = ifelse(condition == "teen", ifelse(q7_pre == 1, 1, 0), q7_pre),
    q8_pre = ifelse(condition == "teen", ifelse(q8_pre == 3, 1, 0), q8_pre),
    q9_pre = ifelse(condition == "teen", ifelse(q9_pre == 4, 1, 0), q9_pre),
    q10_pre = ifelse(condition == "teen", ifelse(q10_pre == 4, 1, 0), q10_pre))

#Post
df_mo <- df_mo %>%
  mutate(
    q1_post = ifelse(condition == "teen", ifelse(q1_post == 2, 1, 0), q1_post),
    q2_post = ifelse(condition == "teen", ifelse(q2_post == 2, 1, 0), q2_post),
    q3_post = ifelse(condition == "teen", ifelse(q3_post == 1, 1, 0), q3_post),
    q4_post = ifelse(condition == "teen", ifelse(q4_post == 1, 1, 0), q4_post),
    q5_post = ifelse(condition == "teen", ifelse(q5_post == 4, 1, 0), q5_post),
    q6_post = ifelse(condition == "teen", ifelse(q6_post == 2, 1, 0), q6_post),
    q7_post = ifelse(condition == "teen", ifelse(q7_post == 1, 1, 0), q7_post),
    q8_post = ifelse(condition == "teen", ifelse(q8_post == 3, 1, 0), q8_post),
    q9_post = ifelse(condition == "teen", ifelse(q9_post == 4, 1, 0), q9_post),
    q10_post = ifelse(condition == "teen", ifelse(q10_post == 4, 1, 0), q10_post))

#PT Correct answers 

#Pre
df_mo <- df_mo %>%
  mutate(
    q1_pre = ifelse(condition == "pt", ifelse(q1_pre == 3, 1, 0), q1_pre),
    q2_pre = ifelse(condition == "pt", ifelse(q2_pre == 1, 1, 0), q2_pre),
    q3_pre = ifelse(condition == "pt", ifelse(q3_pre == 1, 1, 0), q3_pre),
    q4_pre = ifelse(condition == "pt", ifelse(q4_pre == 1, 1, 0), q4_pre),
    q5_pre = ifelse(condition == "pt", ifelse(q5_pre == 1, 1, 0), q5_pre),
    q6_pre = ifelse(condition == "pt", ifelse(q6_pre == 1, 1, 0), q6_pre),
    q7_pre = ifelse(condition == "pt", ifelse(q7_pre == 3, 1, 0), q7_pre),
    q8_pre = ifelse(condition == "pt", ifelse(q8_pre == 4, 1, 0), q8_pre),
    q9_pre = ifelse(condition == "pt", ifelse(q9_pre == 2, 1, 0), q9_pre),
    q10_pre = ifelse(condition == "pt", ifelse(q10_pre == 1, 1, 0), q10_pre))

#Post
df_mo <- df_mo %>%
  mutate(
    q1_post = ifelse(condition == "pt", ifelse(q1_post == 3, 1, 0), q1_post),
    q2_post = ifelse(condition == "pt", ifelse(q2_post == 1, 1, 0), q2_post),
    q3_post = ifelse(condition == "pt", ifelse(q3_post == 1, 1, 0), q3_post),
    q4_post = ifelse(condition == "pt", ifelse(q4_post == 1, 1, 0), q4_post),
    q5_post = ifelse(condition == "pt", ifelse(q5_post == 1, 1, 0), q5_post),
    q6_post = ifelse(condition == "pt", ifelse(q6_post == 1, 1, 0), q6_post),
    q7_post = ifelse(condition == "pt", ifelse(q7_post == 3, 1, 0), q7_post),
    q8_post = ifelse(condition == "pt", ifelse(q8_post == 4, 1, 0), q8_post),
    q9_post = ifelse(condition == "pt", ifelse(q9_post == 2, 1, 0), q9_post),
    q10_post = ifelse(condition == "pt", ifelse(q10_post == 1, 1, 0), q10_post))


#Sum of Pre and post total correct scores

df_mo <- df_mo %>%
  
  mutate(
    
    pre_total_correct = rowSums(select(., starts_with("q") & ends_with("pre"))),
    
    post_total_correct = rowSums(select(., starts_with("q") & ends_with("post")))
    
  )

df_mo <- df_mo |> 
  
  mutate(relative_change = post_total_correct - pre_total_correct)

df_mo <- df_mo %>%
  
  mutate(rc_code = case_when(
    
    relative_change == 0 ~ "No Change",
    
    relative_change %in% c(-1, -2) ~ "-1 to -2",
    
    relative_change %in% c(1, 2) ~ "1 to 2",
    
    relative_change >= 3 ~ "+3 and above",
    
    relative_change <= -3 ~ "-3 and below",
    
    TRUE ~ NA_character_  # Ensure that the NA type is character to match the other outcomes
    
  ))

df_mo <- df_mo %>%
  
  mutate(rc_positive_negative_code = case_when(
    
    relative_change == 0 ~ "No Change",
    
    relative_change %in% c(-1, -2) ~ "Negative change",
    
    relative_change %in% c(1, 2) ~ "Positive change",
    
    relative_change >= 3 ~ "Positive change",
    
    relative_change <= -3 ~ "Negative change",
    
    TRUE ~ NA_character_  # Ensure that the NA type is character to match the other outcomes
    
  ))



# Create summary statistics grouped by district_name and condition

sta1_mo <- df_mo %>%
  group_by(district_name, condition) %>%
  summarise(
    pre_mean = round(mean(pre_total_correct, na.rm = TRUE), 1),
    pre_std = round(sd(pre_total_correct, na.rm = TRUE), 1),
    pre_mean_pct = round((mean(pre_total_correct, na.rm = TRUE) / 10) * 100, 1),
    post_mean = round(mean(post_total_correct, na.rm = TRUE), 1),
    post_std = round(sd(post_total_correct, na.rm = TRUE), 1),
    post_mean_pct = round((mean(post_total_correct, na.rm = TRUE) / 10) * 100, 1),
    p_value = tryCatch({
      if (length(unique(na.omit(pre_total_correct))) > 1 &&
          length(unique(na.omit(post_total_correct))) > 1) {
        round(t.test(pre_total_correct, post_total_correct)$p.value, 6)
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)
  )


# Save sta1 as a CSV file

write.csv(sta1_mo, "pre_post_test/tables/sta1_summary_mo_ap(2,3).csv", row.names = FALSE)

# Overall mean, std and percentage grouped by condition

sta2_mo <- df_mo %>%
  
  group_by(condition) %>%
  
  summarise(
    
    n = n(),  # Count of rows for each condition
    
    pre_mean = round(mean(pre_total_correct, na.rm = TRUE), 1),
    
    pre_std = round(sd(pre_total_correct, na.rm = TRUE), 1),
    
    pre_mean_pct = round((mean(pre_total_correct, na.rm = TRUE) / 10) * 100, 1),  # Pre-test percentage
    
    post_mean = round(mean(post_total_correct, na.rm = TRUE), 1),
    
    post_std = round(sd(post_total_correct, na.rm = TRUE), 1),
    
    post_mean_pct = round((mean(post_total_correct, na.rm = TRUE) / 10) * 100, 1)  # Post-test percentage
    
  )

# Save sta2 as a CSV file

write.csv(sta2_mo, "pre_post_test/tables/sta2_summary_mo_ap(2,3).csv", row.names = FALSE)

# Reorder condition factor based on post-test mean percentage in descending order

sta2_mo <- sta2_mo %>%
  
  arrange(desc(post_mean_pct)) %>%
  
  mutate(condition = factor(condition, levels = condition))

# Reshape the data from wide to long format using tidyr's pivot_longer

sta2_long <- tidyr::pivot_longer(sta2_mo, 
                                 
                                 cols = c("pre_mean_pct", "post_mean_pct"), 
                                 
                                 names_to = "Time", 
                                 
                                 values_to = "Mean_Pct")

# Calculate the gain in knowledge for each condition

knowledge_gain <- sta2_mo %>%
  
  mutate(Gain = post_mean_pct - pre_mean_pct) %>%
  
  select(condition, Gain)

# Reorder 'Time' factor levels to ensure 'pre' comes before 'post'

sta2_long$Time <- factor(sta2_long$Time, levels = c("pre_mean_pct", "post_mean_pct"))

# Create the ggplot bar chart

pre_post_mean_pct <- ggplot(sta2_long, aes(x = condition, y = Mean_Pct, fill = Time)) +
  
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  
  scale_fill_manual(values = c("pre_mean_pct" = "blue", "post_mean_pct" = "orange")) +
  
  geom_text(aes(label = round(Mean_Pct, 1)), 
            
            position = position_dodge(width = 0.9), 
            
            vjust = -0.5) +  # Adjust label position
  
  labs(x = "Condition", y = "Mean %") +
  
  theme_minimal() +
  
  theme(
    
    legend.title = element_blank(),
    
    axis.text.x = element_text(size = 12, face = "bold"),
    
    axis.title.x = element_text(size = 14, face = "bold")
    
  ) +
  
  # Add gain in knowledge as annotations below the x-axis labels
  
  annotate("text", 
           
           x = knowledge_gain$condition, 
           
           y = -5,  # Position slightly below the x-axis
           
           label = paste0(round(knowledge_gain$Gain, 1), "%"),
           
           size = 3.5, 
           
           color = "black")

# Print the plot

print(pre_post_mean_pct)

# Save the plot in the "plots" directory

ggsave("plots/pre_post_mean_percentage_mo_ap(2,3).png", 
       
       plot = pre_post_mean_pct, 
       
       width = 10, 
       
       height = 6, 
       
       dpi = 300)

####Question wise analysis

# Previous LSCS
# Assuming df has a 'condition' column and we're filtering for rows where condition is 'plscs'
df_plscs <- subset(df_mo, condition == "plscs")

# Calculate total_n for the filtered data
total_n <- nrow(df_plscs)

# Create the new data frame with percentages based on total_n when condition == 'plscs'
st3_plscs <- data.frame(
  q1_pre = (sum(df_plscs$q1_pre) / total_n) * 100,
  q1_post = (sum(df_plscs$q1_post) / total_n) * 100,
  
  q2_pre = (sum(df_plscs$q2_pre) / total_n) * 100,
  q2_post = (sum(df_plscs$q2_post) / total_n) * 100,
  
  q3_pre = (sum(df_plscs$q3_pre) / total_n) * 100,
  q3_post = (sum(df_plscs$q3_post) / total_n) * 100,
  
  q4_pre = (sum(df_plscs$q4_pre) / total_n) * 100,
  q4_post = (sum(df_plscs$q4_post) / total_n) * 100,
  
  q5_pre = (sum(df_plscs$q5_pre) / total_n) * 100,
  q5_post = (sum(df_plscs$q5_post) / total_n) * 100,
  
  q6_pre = (sum(df_plscs$q6_pre) / total_n) * 100,
  q6_post = (sum(df_plscs$q6_post) / total_n) * 100,
  
  q7_pre = (sum(df_plscs$q7_pre) / total_n) * 100,
  q7_post = (sum(df_plscs$q7_post) / total_n) * 100,
  
  q8_pre = (sum(df_plscs$q8_pre) / total_n) * 100,
  q8_post = (sum(df_plscs$q8_post) / total_n) * 100,
  
  q9_pre = (sum(df_plscs$q9_pre) / total_n) * 100,
  q9_post = (sum(df_plscs$q9_post) / total_n) * 100,
  
  q10_pre = (sum(df_plscs$q10_pre) / total_n) * 100,
  q10_post = (sum(df_plscs$q10_post) / total_n) * 100
)

# Assuming st3_plscs is your data frame with percentages
st3_plscs_long <- st3_plscs %>%
  pivot_longer(cols = everything(), 
               names_to = c("question", "time"), 
               names_sep = "_", 
               values_to = "percentage")

# Create a factor to maintain the order of questions
st3_plscs_long$question <- factor(st3_plscs_long$question, 
                                  levels = paste0("q", 1:10), 
                                  labels = paste("Question", 1:10))

# Create the bar graph
plot_plscs <- ggplot(st3_plscs_long, aes(x = question, y = percentage, fill = time)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) + # Adjust vjust to position labels
  labs(title = "Pre and Post Comparison for Each Question - PLSCS",
       x = "Questions",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("pre" = "lightblue", "post" = "lightgreen"), 
                    name = "Time",
                    labels = c("pre" = "Pre-Test", "post" = "Post-Test"))

# Save the plot in the "plots" directory
ggsave("plots/pre_post_comparison_plscs_mo_ap.png", 
       plot = plot_plscs, 
       width = 10, 
       height = 6, 
       dpi = 300)


# Pregnancy with heart disease (HRT)
# Assuming df has a 'condition' column and we're filtering for rows where condition is 'hrt'
df_hrt <- subset(df_mo, condition == "hrt")

# Calculate total_n for the filtered data
total_n <- nrow(df_hrt)

# Create the new data frame with percentages based on total_n when condition == 'hrt'
st3_hrt <- data.frame(
  q1_pre = (sum(df_hrt$q1_pre) / total_n) * 100,
  q1_post = (sum(df_hrt$q1_post) / total_n) * 100,
  
  q2_pre = (sum(df_hrt$q2_pre) / total_n) * 100,
  q2_post = (sum(df_hrt$q2_post) / total_n) * 100,
  
  q3_pre = (sum(df_hrt$q3_pre) / total_n) * 100,
  q3_post = (sum(df_hrt$q3_post) / total_n) * 100,
  
  q4_pre = (sum(df_hrt$q4_pre) / total_n) * 100,
  q4_post = (sum(df_hrt$q4_post) / total_n) * 100,
  
  q5_pre = (sum(df_hrt$q5_pre) / total_n) * 100,
  q5_post = (sum(df_hrt$q5_post) / total_n) * 100,
  
  q6_pre = (sum(df_hrt$q6_pre) / total_n) * 100,
  q6_post = (sum(df_hrt$q6_post) / total_n) * 100,
  
  q7_pre = (sum(df_hrt$q7_pre) / total_n) * 100,
  q7_post = (sum(df_hrt$q7_post) / total_n) * 100,
  
  q8_pre = (sum(df_hrt$q8_pre) / total_n) * 100,
  q8_post = (sum(df_hrt$q8_post) / total_n) * 100,
  
  q9_pre = (sum(df_hrt$q9_pre) / total_n) * 100,
  q9_post = (sum(df_hrt$q9_post) / total_n) * 100,
  
  q10_pre = (sum(df_hrt$q10_pre) / total_n) * 100,
  q10_post = (sum(df_hrt$q10_post) / total_n) * 100
)

# Assuming st3_hrt is your data frame with percentages
st3_hrt_long <- st3_hrt %>%
  pivot_longer(cols = everything(), 
               names_to = c("question", "time"), 
               names_sep = "_", 
               values_to = "percentage")

# Create a factor to maintain the order of questions
st3_hrt_long$question <- factor(st3_hrt_long$question, 
                                levels = paste0("q", 1:10), 
                                labels = paste("Question", 1:10))

# Create the bar graph
plot_hrt <- ggplot(st3_hrt_long, aes(x = question, y = percentage, fill = time)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) + # Adjust vjust to position labels
  labs(title = "Pre and Post Comparison for Each Question - HRT",
       x = "Questions",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("pre" = "lightblue", "post" = "lightgreen"), 
                    name = "Time",
                    labels = c("pre" = "Pre-Test", "post" = "Post-Test"))

# Save the plot in the "plots" directory
ggsave("plots/pre_post_comparison_hrt_mo_ap.png", 
       plot = plot_hrt, 
       width = 10, 
       height = 6, 
       dpi = 300)


# Tuberculosis
# Assuming df has a 'condition' column and we're filtering for rows where condition is 'tb'
df_tb <- subset(df_mo, condition == "tb")

# Calculate total_n for the filtered data
total_n <- nrow(df_tb)

# Create the new data frame with percentages based on total_n when condition == 'tb'
st3_tb <- data.frame(
  q1_pre = (sum(df_tb$q1_pre) / total_n) * 100,
  q1_post = (sum(df_tb$q1_post) / total_n) * 100,
  
  q2_pre = (sum(df_tb$q2_pre) / total_n) * 100,
  q2_post = (sum(df_tb$q2_post) / total_n) * 100,
  
  q3_pre = (sum(df_tb$q3_pre) / total_n) * 100,
  q3_post = (sum(df_tb$q3_post) / total_n) * 100,
  
  q4_pre = (sum(df_tb$q4_pre) / total_n) * 100,
  q4_post = (sum(df_tb$q4_post) / total_n) * 100,
  
  q5_pre = (sum(df_tb$q5_pre) / total_n) * 100,
  q5_post = (sum(df_tb$q5_post) / total_n) * 100,
  
  q6_pre = (sum(df_tb$q6_pre) / total_n) * 100,
  q6_post = (sum(df_tb$q6_post) / total_n) * 100,
  
  q7_pre = (sum(df_tb$q7_pre) / total_n) * 100,
  q7_post = (sum(df_tb$q7_post) / total_n) * 100,
  
  q8_pre = (sum(df_tb$q8_pre) / total_n) * 100,
  q8_post = (sum(df_tb$q8_post) / total_n) * 100,
  
  q9_pre = (sum(df_tb$q9_pre) / total_n) * 100,
  q9_post = (sum(df_tb$q9_post) / total_n) * 100,
  
  q10_pre = (sum(df_tb$q10_pre) / total_n) * 100,
  q10_post = (sum(df_tb$q10_post) / total_n) * 100
)

# Assuming st3_tb is your data frame with percentages
st3_tb_long <- st3_tb %>%
  pivot_longer(cols = everything(), 
               names_to = c("question", "time"), 
               names_sep = "_", 
               values_to = "percentage")

# Create a factor to maintain the order of questions
st3_tb_long$question <- factor(st3_tb_long$question, 
                               levels = paste0("q", 1:10), 
                               labels = paste("Question", 1:10))

# Create the bar graph
plot_tb <- ggplot(st3_tb_long, aes(x = question, y = percentage, fill = time)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) + # Adjust vjust to position labels
  labs(title = "Pre and Post Comparison for Each Question - Tuberculosis",
       x = "Questions",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("pre" = "lightblue", "post" = "lightgreen"), 
                    name = "Time",
                    labels = c("pre" = "Pre-Test", "post" = "Post-Test"))

# Save the plot in the "plots" directory
ggsave("plots/pre_post_comparison_tb_mo_ap.png", 
       plot = plot_tb, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Fever
# Assuming df has a 'condition' column and we're filtering for rows where condition is 'fever'
df_fever <- subset(df_mo, condition == "fever")

# Calculate total_n for the filtered data
total_n <- nrow(df_fever)

# Create the new data frame with percentages based on total_n when condition == 'fever'
st3_fever <- data.frame(
  q1_pre = (sum(df_fever$q1_pre) / total_n) * 100,
  q1_post = (sum(df_fever$q1_post) / total_n) * 100,
  
  q2_pre = (sum(df_fever$q2_pre) / total_n) * 100,
  q2_post = (sum(df_fever$q2_post) / total_n) * 100,
  
  q3_pre = (sum(df_fever$q3_pre) / total_n) * 100,
  q3_post = (sum(df_fever$q3_post) / total_n) * 100,
  
  q4_pre = (sum(df_fever$q4_pre) / total_n) * 100,
  q4_post = (sum(df_fever$q4_post) / total_n) * 100,
  
  q5_pre = (sum(df_fever$q5_pre) / total_n) * 100,
  q5_post = (sum(df_fever$q5_post) / total_n) * 100,
  
  q6_pre = (sum(df_fever$q6_pre) / total_n) * 100,
  q6_post = (sum(df_fever$q6_post) / total_n) * 100,
  
  q7_pre = (sum(df_fever$q7_pre) / total_n) * 100,
  q7_post = (sum(df_fever$q7_post) / total_n) * 100,
  
  q8_pre = (sum(df_fever$q8_pre) / total_n) * 100,
  q8_post = (sum(df_fever$q8_post) / total_n) * 100,
  
  q9_pre = (sum(df_fever$q9_pre) / total_n) * 100,
  q9_post = (sum(df_fever$q9_post) / total_n) * 100,
  
  q10_pre = (sum(df_fever$q10_pre) / total_n) * 100,
  q10_post = (sum(df_fever$q10_post) / total_n) * 100
)

# Assuming st3_fever is your data frame with percentages
st3_fever_long <- st3_fever %>%
  pivot_longer(cols = everything(), 
               names_to = c("question", "time"), 
               names_sep = "_", 
               values_to = "percentage")

# Create a factor to maintain the order of questions
st3_fever_long$question <- factor(st3_fever_long$question, 
                                  levels = paste0("q", 1:10), 
                                  labels = paste("Question", 1:10))

# Create the bar graph
plot_fever <- ggplot(st3_fever_long, aes(x = question, y = percentage, fill = time)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) + # Adjust vjust to position labels
  labs(title = "Pre and Post Comparison for Each Question - Fever",
       x = "Questions",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("pre" = "lightblue", "post" = "lightgreen"), 
                    name = "Time",
                    labels = c("pre" = "Pre-Test", "post" = "Post-Test"))

# Save the plot in the "plots" directory
ggsave("plots/pre_post_comparison_fever_mo_ap.png", 
       plot = plot_fever, 
       width = 10, 
       height = 6, 
       dpi = 300)


# Fetal Growth Restriction (FGR)
# Assuming df has a 'condition' column and we're filtering for rows where condition is 'fgr'
df_fgr <- subset(df_mo, condition == "fgr")

# Calculate total_n for the filtered data
total_n <- nrow(df_fgr)

# Create the new data frame with percentages based on total_n when condition == 'fgr'
st3_fgr <- data.frame(
  q1_pre = (sum(df_fgr$q1_pre) / total_n) * 100,
  q1_post = (sum(df_fgr$q1_post) / total_n) * 100,
  
  q2_pre = (sum(df_fgr$q2_pre) / total_n) * 100,
  q2_post = (sum(df_fgr$q2_post) / total_n) * 100,
  
  q3_pre = (sum(df_fgr$q3_pre) / total_n) * 100,
  q3_post = (sum(df_fgr$q3_post) / total_n) * 100,
  
  q4_pre = (sum(df_fgr$q4_pre) / total_n) * 100,
  q4_post = (sum(df_fgr$q4_post) / total_n) * 100,
  
  q5_pre = (sum(df_fgr$q5_pre) / total_n) * 100,
  q5_post = (sum(df_fgr$q5_post) / total_n) * 100,
  
  q6_pre = (sum(df_fgr$q6_pre) / total_n) * 100,
  q6_post = (sum(df_fgr$q6_post) / total_n) * 100,
  
  q7_pre = (sum(df_fgr$q7_pre) / total_n) * 100,
  q7_post = (sum(df_fgr$q7_post) / total_n) * 100,
  
  q8_pre = (sum(df_fgr$q8_pre) / total_n) * 100,
  q8_post = (sum(df_fgr$q8_post) / total_n) * 100,
  
  q9_pre = (sum(df_fgr$q9_pre) / total_n) * 100,
  q9_post = (sum(df_fgr$q9_post) / total_n) * 100,
  
  q10_pre = (sum(df_fgr$q10_pre) / total_n) * 100,
  q10_post = (sum(df_fgr$q10_post) / total_n) * 100)

# Assuming st3_fgr is your data frame with percentages
st3_fgr_long <- st3_fgr %>%
  pivot_longer(cols = everything(), 
               names_to = c("question", "time"), 
               names_sep = "_", 
               values_to = "percentage")

# Create a factor to maintain the order of questions
st3_fgr_long$question <- factor(st3_fgr_long$question, 
                                levels = paste0("q", 1:10), 
                                labels = paste("Question", 1:10))

# Create the bar graph
plot_fgr <- ggplot(st3_fgr_long, aes(x = question, y = percentage, fill = time)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) + # Adjust vjust to position labels
  labs(title = "Pre and Post Comparison for Each Question - Fetal Growth Restriction",
       x = "Questions",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("pre" = "lightblue", "post" = "lightgreen"), 
                    name = "Time",
                    labels = c("pre" = "Pre-Test", "post" = "Post-Test"))

# Save the plot in the "plots" directory
ggsave("plots/pre_post_comparison_fgr_mo_ap.png", 
       plot = plot_fgr, 
       width = 10, 
       height = 6, 
       dpi = 300)

#### Question wise analysis

# Bleeding before 20 weeks (BLE)
# Assuming df has a 'condition' column and we're filtering for rows where condition is 'ble'
df_ble <- subset(df_mo, condition == "ble")

# Calculate total_n for the filtered data
total_n <- nrow(df_ble)

# Create the new data frame with percentages based on total_n when condition == 'ble'
st3_ble <- data.frame(
  q1_pre = (sum(df_ble$q1_pre) / total_n) * 100,
  q1_post = (sum(df_ble$q1_post) / total_n) * 100,
  
  q2_pre = (sum(df_ble$q2_pre) / total_n) * 100,
  q2_post = (sum(df_ble$q2_post) / total_n) * 100,
  
  q3_pre = (sum(df_ble$q3_pre) / total_n) * 100,
  q3_post = (sum(df_ble$q3_post) / total_n) * 100,
  
  q4_pre = (sum(df_ble$q4_pre) / total_n) * 100,
  q4_post = (sum(df_ble$q4_post) / total_n) * 100,
  
  q5_pre = (sum(df_ble$q5_pre) / total_n) * 100,
  q5_post = (sum(df_ble$q5_post) / total_n) * 100,
  
  q6_pre = (sum(df_ble$q6_pre) / total_n) * 100,
  q6_post = (sum(df_ble$q6_post) / total_n) * 100,
  
  q7_pre = (sum(df_ble$q7_pre) / total_n) * 100,
  q7_post = (sum(df_ble$q7_post) / total_n) * 100,
  
  q8_pre = (sum(df_ble$q8_pre) / total_n) * 100,
  q8_post = (sum(df_ble$q8_post) / total_n) * 100,
  
  q9_pre = (sum(df_ble$q9_pre) / total_n) * 100,
  q9_post = (sum(df_ble$q9_post) / total_n) * 100,
  
  q10_pre = (sum(df_ble$q10_pre) / total_n) * 100,
  q10_post = (sum(df_ble$q10_post) / total_n) * 100)

# Assuming st3_ble is your data frame with percentages
st3_ble_long <- st3_ble %>%
  pivot_longer(cols = everything(), 
               names_to = c("question", "time"), 
               names_sep = "_", 
               values_to = "percentage")

# Create a factor to maintain the order of questions
st3_ble_long$question <- factor(st3_ble_long$question, 
                                levels = paste0("q", 1:10), 
                                labels = paste("Question", 1:10))

# Create the bar graph
plot_ble <- ggplot(st3_ble_long, aes(x = question, y = percentage, fill = time)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) + # Adjust vjust to position labels
  labs(title = "Pre and Post Comparison for Each Question - Bleeding",
       x = "Questions",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("pre" = "lightblue", "post" = "lightgreen"), 
                    name = "Time",
                    labels = c("pre" = "Pre-Test", "post" = "Post-Test"))

# Save the plot in the "plots" directory
ggsave("plots/pre_post_comparison_ble_mo_ap.png", 
       plot = plot_ble, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Preterm labor and PPROM
# Assuming df has a 'condition' column and we're filtering for rows where condition is 'pprom'
df_pprom <- subset(df_mo, condition == "pprom")

# Calculate total_n for the filtered data
total_n <- nrow(df_pprom)

# Create the new data frame with percentages based on total_n when condition == 'pprom'
st3_pprom <- data.frame(
  q1_pre = (sum(df_pprom$q1_pre) / total_n) * 100,
  q1_post = (sum(df_pprom$q1_post) / total_n) * 100,
  
  q2_pre = (sum(df_pprom$q2_pre) / total_n) * 100,
  q2_post = (sum(df_pprom$q2_post) / total_n) * 100,
  
  q3_pre = (sum(df_pprom$q3_pre) / total_n) * 100,
  q3_post = (sum(df_pprom$q3_post) / total_n) * 100,
  
  q4_pre = (sum(df_pprom$q4_pre) / total_n) * 100,
  q4_post = (sum(df_pprom$q4_post) / total_n) * 100,
  
  q5_pre = (sum(df_pprom$q5_pre) / total_n) * 100,
  q5_post = (sum(df_pprom$q5_post) / total_n) * 100,
  
  q6_pre = (sum(df_pprom$q6_pre) / total_n) * 100,
  q6_post = (sum(df_pprom$q6_post) / total_n) * 100,
  
  q7_pre = (sum(df_pprom$q7_pre) / total_n) * 100,
  q7_post = (sum(df_pprom$q7_post) / total_n) * 100,
  
  q8_pre = (sum(df_pprom$q8_pre) / total_n) * 100,
  q8_post = (sum(df_pprom$q8_post) / total_n) * 100,
  
  q9_pre = (sum(df_pprom$q9_pre) / total_n) * 100,
  q9_post = (sum(df_pprom$q9_post) / total_n) * 100,
  
  q10_pre = (sum(df_pprom$q10_pre) / total_n) * 100,
  q10_post = (sum(df_pprom$q10_post) / total_n) * 100)

# Assuming st3_pprom is your data frame with percentages
st3_pprom_long <- st3_pprom %>%
  pivot_longer(cols = everything(), 
               names_to = c("question", "time"), 
               names_sep = "_", 
               values_to = "percentage")

# Create a factor to maintain the order of questions
st3_pprom_long$question <- factor(st3_pprom_long$question, 
                                  levels = paste0("q", 1:10), 
                                  labels = paste("Question", 1:10))

# Create the bar graph
plot_pprom <- ggplot(st3_pprom_long, aes(x = question, y = percentage, fill = time)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) + # Adjust vjust to position labels
  labs(title = "Pre and Post Comparison for Each Question - Preterm Premature Rupture of Membranes",
       x = "Questions",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("pre" = "lightblue", "post" = "lightgreen"), 
                    name = "Time",
                    labels = c("pre" = "Pre-Test", "post" = "Post-Test"))

# Save the plot in the "plots" directory
ggsave("plots/pre_post_comparison_pprom_mo_ap.png", 
       plot = plot_pprom, 
       width = 10, 
       height = 6, 
       dpi = 300)


# HIV
# Assuming df has a 'condition' column and we're filtering for rows where condition is 'hiv'
df_hiv <- subset(df_mo, condition == "hiv")

# Calculate total_n for the filtered data
total_n <- nrow(df_hiv)

# Create the new data frame with percentages based on total_n when condition == 'hiv'
st3_hiv <- data.frame(
  q1_pre = (sum(df_hiv$q1_pre) / total_n) * 100,
  q1_post = (sum(df_hiv$q1_post) / total_n) * 100,
  
  q2_pre = (sum(df_hiv$q2_pre) / total_n) * 100,
  q2_post = (sum(df_hiv$q2_post) / total_n) * 100,
  
  q3_pre = (sum(df_hiv$q3_pre) / total_n) * 100,
  q3_post = (sum(df_hiv$q3_post) / total_n) * 100,
  
  q4_pre = (sum(df_hiv$q4_pre) / total_n) * 100,
  q4_post = (sum(df_hiv$q4_post) / total_n) * 100,
  
  q5_pre = (sum(df_hiv$q5_pre) / total_n) * 100,
  q5_post = (sum(df_hiv$q5_post) / total_n) * 100,
  
  q6_pre = (sum(df_hiv$q6_pre) / total_n) * 100,
  q6_post = (sum(df_hiv$q6_post) / total_n) * 100,
  
  q7_pre = (sum(df_hiv$q7_pre) / total_n) * 100,
  q7_post = (sum(df_hiv$q7_post) / total_n) * 100,
  
  q8_pre = (sum(df_hiv$q8_pre) / total_n) * 100,
  q8_post = (sum(df_hiv$q8_post) / total_n) * 100,
  
  q9_pre = (sum(df_hiv$q9_pre) / total_n) * 100,
  q9_post = (sum(df_hiv$q9_post) / total_n) * 100,
  
  q10_pre = (sum(df_hiv$q10_pre) / total_n) * 100,
  q10_post = (sum(df_hiv$q10_post) / total_n) * 100)

# Assuming st3_hiv is your data frame with percentages
st3_hiv_long <- st3_hiv %>%
  pivot_longer(cols = everything(), 
               names_to = c("question", "time"), 
               names_sep = "_", 
               values_to = "percentage")

# Create a factor to maintain the order of questions
st3_hiv_long$question <- factor(st3_hiv_long$question, 
                                levels = paste0("q", 1:10), 
                                labels = paste("Question", 1:10))

# Create the bar graph
plot_hiv <- ggplot(st3_hiv_long, aes(x = question, y = percentage, fill = time)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) + # Adjust vjust to position labels
  labs(title = "Pre and Post Comparison for Each Question - HIV",
       x = "Questions",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("pre" = "lightblue", "post" = "lightgreen"), 
                    name = "Time",
                    labels = c("pre" = "Pre-Test", "post" = "Post-Test"))

# Save the plot in the "plots" directory
ggsave("plots/pre_post_comparison_hiv_mo_ap.png", 
       plot = plot_hiv, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Reduced fetal movements (RFM)
# Assuming df has a 'condition' column and we're filtering for rows where condition is 'rfm'
df_rfm <- subset(df_mo, condition == "rfm")

# Calculate total_n for the filtered data
total_n <- nrow(df_rfm)

# Create the new data frame with percentages based on total_n when condition == 'rfm'
st3_rfm <- data.frame(
  q1_pre = (sum(df_rfm$q1_pre) / total_n) * 100,
  q1_post = (sum(df_rfm$q1_post) / total_n) * 100,
  
  q2_pre = (sum(df_rfm$q2_pre) / total_n) * 100,
  q2_post = (sum(df_rfm$q2_post) / total_n) * 100,
  
  q3_pre = (sum(df_rfm$q3_pre) / total_n) * 100,
  q3_post = (sum(df_rfm$q3_post) / total_n) * 100,
  
  q4_pre = (sum(df_rfm$q4_pre) / total_n) * 100,
  q4_post = (sum(df_rfm$q4_post) / total_n) * 100,
  
  q5_pre = (sum(df_rfm$q5_pre) / total_n) * 100,
  q5_post = (sum(df_rfm$q5_post) / total_n) * 100,
  
  q6_pre = (sum(df_rfm$q6_pre) / total_n) * 100,
  q6_post = (sum(df_rfm$q6_post) / total_n) * 100,
  
  q7_pre = (sum(df_rfm$q7_pre) / total_n) * 100,
  q7_post = (sum(df_rfm$q7_post) / total_n) * 100,
  
  q8_pre = (sum(df_rfm$q8_pre) / total_n) * 100,
  q8_post = (sum(df_rfm$q8_post) / total_n) * 100,
  
  q9_pre = (sum(df_rfm$q9_pre) / total_n) * 100,
  q9_post = (sum(df_rfm$q9_post) / total_n) * 100,
  
  q10_pre = (sum(df_rfm$q10_pre) / total_n) * 100,
  q10_post = (sum(df_rfm$q10_post) / total_n) * 100)

# Assuming st3_rfm is your data frame with percentages
st3_rfm_long <- st3_rfm %>%
  pivot_longer(cols = everything(), 
               names_to = c("question", "time"), 
               names_sep = "_", 
               values_to = "percentage")

# Create a factor to maintain the order of questions
st3_rfm_long$question <- factor(st3_rfm_long$question, 
                                levels = paste0("q", 1:10), 
                                labels = paste("Question", 1:10))

# Create the bar graph
plot_rfm <- ggplot(st3_rfm_long, aes(x = question, y = percentage, fill = time)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) + # Adjust vjust to position labels
  labs(title = "Pre and Post Comparison for Each Question - Rupture of Fetal Membranes",
       x = "Questions",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("pre" = "lightblue", "post" = "lightgreen"), 
                    name = "Time",
                    labels = c("pre" = "Pre-Test", "post" = "Post-Test"))

# Save the plot in the "plots" directory
ggsave("plots/pre_post_comparison_rfm_mo_ap.png", 
       plot = plot_rfm, 
       width = 10, 
       height = 6, 
       dpi = 300)

# HBsAg
# Assuming df has a 'condition' column and we're filtering for rows where condition is 'hbsag'
df_hbsag <- subset(df_mo, condition == "hbsag")

# Calculate total_n for the filtered data
total_n <- nrow(df_hbsag)

# Create the new data frame with percentages based on total_n when condition == 'hbsag'
st3_hbsag <- data.frame(
  q1_pre = (sum(df_hbsag$q1_pre) / total_n) * 100,
  q1_post = (sum(df_hbsag$q1_post) / total_n) * 100,
  
  q2_pre = (sum(df_hbsag$q2_pre) / total_n) * 100,
  q2_post = (sum(df_hbsag$q2_post) / total_n) * 100,
  
  q3_pre = (sum(df_hbsag$q3_pre) / total_n) * 100,
  q3_post = (sum(df_hbsag$q3_post) / total_n) * 100,
  
  q4_pre = (sum(df_hbsag$q4_pre) / total_n) * 100,
  q4_post = (sum(df_hbsag$q4_post) / total_n) * 100,
  
  q5_pre = (sum(df_hbsag$q5_pre) / total_n) * 100,
  q5_post = (sum(df_hbsag$q5_post) / total_n) * 100,
  
  q6_pre = (sum(df_hbsag$q6_pre) / total_n) * 100,
  q6_post = (sum(df_hbsag$q6_post) / total_n) * 100,
  
  q7_pre = (sum(df_hbsag$q7_pre) / total_n) * 100,
  q7_post = (sum(df_hbsag$q7_post) / total_n) * 100,
  
  q8_pre = (sum(df_hbsag$q8_pre) / total_n) * 100,
  q8_post = (sum(df_hbsag$q8_post) / total_n) * 100,
  
  q9_pre = (sum(df_hbsag$q9_pre) / total_n) * 100,
  q9_post = (sum(df_hbsag$q9_post) / total_n) * 100,
  
  q10_pre = (sum(df_hbsag$q10_pre) / total_n) * 100,
  q10_post = (sum(df_hbsag$q10_post) / total_n) * 100)

# Assuming st3_hbsag is your data frame with percentages
st3_hbsag_long <- st3_hbsag %>%
  pivot_longer(cols = everything(), 
               names_to = c("question", "time"), 
               names_sep = "_", 
               values_to = "percentage")

# Create a factor to maintain the order of questions
st3_hbsag_long$question <- factor(st3_hbsag_long$question, 
                                  levels = paste0("q", 1:10), 
                                  labels = paste("Question", 1:10))

# Create the bar graph
plot_hbsag <- ggplot(st3_hbsag_long, aes(x = question, y = percentage, fill = time)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) + # Adjust vjust to position labels
  labs(title = "Pre and Post Comparison for Each Question - HBsAg",
       x = "Questions",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("pre" = "lightblue", "post" = "lightgreen"), 
                    name = "Time",
                    labels = c("pre" = "Pre-Test", "post" = "Post-Test"))

# Save the plot in the "plots" directory
ggsave("plots/pre_post_comparison_hbsag_mo_ap.png", 
       plot = plot_hbsag, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Rh negative
# Assuming df has a 'condition' column and we're filtering for rows where condition is 'rh'
df_rh <- subset(df_mo, condition == "rh")

# Calculate total_n for the filtered data
total_n <- nrow(df_rh)

# Create the new data frame with percentages based on total_n when condition == 'rh'
st3_rh <- data.frame(
  q1_pre = (sum(df_rh$q1_pre) / total_n) * 100,
  q1_post = (sum(df_rh$q1_post) / total_n) * 100,
  
  q2_pre = (sum(df_rh$q2_pre) / total_n) * 100,
  q2_post = (sum(df_rh$q2_post) / total_n) * 100,
  
  q3_pre = (sum(df_rh$q3_pre) / total_n) * 100,
  q3_post = (sum(df_rh$q3_post) / total_n) * 100,
  
  q4_pre = (sum(df_rh$q4_pre) / total_n) * 100,
  q4_post = (sum(df_rh$q4_post) / total_n) * 100,
  
  q5_pre = (sum(df_rh$q5_pre) / total_n) * 100,
  q5_post = (sum(df_rh$q5_post) / total_n) * 100,
  
  q6_pre = (sum(df_rh$q6_pre) / total_n) * 100,
  q6_post = (sum(df_rh$q6_post) / total_n) * 100,
  
  q7_pre = (sum(df_rh$q7_pre) / total_n) * 100,
  q7_post = (sum(df_rh$q7_post) / total_n) * 100,
  
  q8_pre = (sum(df_rh$q8_pre) / total_n) * 100,
  q8_post = (sum(df_rh$q8_post) / total_n) * 100,
  
  q9_pre = (sum(df_rh$q9_pre) / total_n) * 100,
  q9_post = (sum(df_rh$q9_post) / total_n) * 100,
  
  q10_pre = (sum(df_rh$q10_pre) / total_n) * 100,
  q10_post = (sum(df_rh$q10_post) / total_n) * 100)

# Assuming st3_rh is your data frame with percentages
st3_rh_long <- st3_rh %>%
  pivot_longer(cols = everything(), 
               names_to = c("question", "time"), 
               names_sep = "_", 
               values_to = "percentage")

# Create a factor to maintain the order of questions
st3_rh_long$question <- factor(st3_rh_long$question, 
                               levels = paste0("q", 1:10), 
                               labels = paste("Question", 1:10))

# Create the bar graph
plot_rh <- ggplot(st3_rh_long, aes(x = question, y = percentage, fill = time)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) + # Adjust vjust to position labels
  labs(title = "Pre and Post Comparison for Each Question - Rh Incompatibility",
       x = "Questions",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("pre" = "lightblue", "post" = "lightgreen"), 
                    name = "Time",
                    labels = c("pre" = "Pre-Test", "post" = "Post-Test"))

# Save the plot in the "plots" directory
ggsave("plots/pre_post_comparison_rh_mo_ap.png", 
       plot = plot_rh, 
       width = 10, 
       height = 6, 
       dpi = 300)


# Teenage pregnancy
# Assuming df has a 'condition' column and we're filtering for rows where condition is 'teen'
df_teen <- subset(df_mo, condition == "teen")

# Calculate total_n for the filtered data
total_n <- nrow(df_teen)

# Create the new data frame with percentages based on total_n when condition == 'teen'
st3_teen <- data.frame(
  q1_pre = (sum(df_teen$q1_pre) / total_n) * 100,
  q1_post = (sum(df_teen$q1_post) / total_n) * 100,
  
  q2_pre = (sum(df_teen$q2_pre) / total_n) * 100,
  q2_post = (sum(df_teen$q2_post) / total_n) * 100,
  
  q3_pre = (sum(df_teen$q3_pre) / total_n) * 100,
  q3_post = (sum(df_teen$q3_post) / total_n) * 100,
  
  q4_pre = (sum(df_teen$q4_pre) / total_n) * 100,
  q4_post = (sum(df_teen$q4_post) / total_n) * 100,
  
  q5_pre = (sum(df_teen$q5_pre) / total_n) * 100,
  q5_post = (sum(df_teen$q5_post) / total_n) * 100,
  
  q6_pre = (sum(df_teen$q6_pre) / total_n) * 100,
  q6_post = (sum(df_teen$q6_post) / total_n) * 100,
  
  q7_pre = (sum(df_teen$q7_pre) / total_n) * 100,
  q7_post = (sum(df_teen$q7_post) / total_n) * 100,
  
  q8_pre = (sum(df_teen$q8_pre) / total_n) * 100,
  q8_post = (sum(df_teen$q8_post) / total_n) * 100,
  
  q9_pre = (sum(df_teen$q9_pre) / total_n) * 100,
  q9_post = (sum(df_teen$q9_post) / total_n) * 100,
  
  q10_pre = (sum(df_teen$q10_pre) / total_n) * 100,
  q10_post = (sum(df_teen$q10_post) / total_n) * 100)

# Assuming st3_teen is your data frame with percentages
st3_teen_long <- st3_teen %>%
  pivot_longer(cols = everything(), 
               names_to = c("question", "time"), 
               names_sep = "_", 
               values_to = "percentage")

# Create a factor to maintain the order of questions
st3_teen_long$question <- factor(st3_teen_long$question, 
                                 levels = paste0("q", 1:10), 
                                 labels = paste("Question", 1:10))

# Create the bar graph
plot_teen <- ggplot(st3_teen_long, aes(x = question, y = percentage, fill = time)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) + # Adjust vjust to position labels
  labs(title = "Pre and Post Comparison for Each Question - Teen",
       x = "Questions",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("pre" = "lightblue", "post" = "lightgreen"), 
                    name = "Time",
                    labels = c("pre" = "Pre-Test", "post" = "Post-Test"))

# Save the plot in the "plots" directory
ggsave("plots/pre_post_comparison_teen_mo_ap.png", 
       plot = plot_teen, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Prevention of perineal trauma
# Assuming df has a 'condition' column and we're filtering for rows where condition is 'pt'
df_pt <- subset(df_mo, condition == "pt")

# Calculate total_n for the filtered data
total_n <- nrow(df_pt)

# Create the new data frame with percentages based on total_n when condition == 'pt'
st3_pt <- data.frame(
  q1_pre = (sum(df_pt$q1_pre) / total_n) * 100,
  q1_post = (sum(df_pt$q1_post) / total_n) * 100,
  
  q2_pre = (sum(df_pt$q2_pre) / total_n) * 100,
  q2_post = (sum(df_pt$q2_post) / total_n) * 100,
  
  q3_pre = (sum(df_pt$q3_pre) / total_n) * 100,
  q3_post = (sum(df_pt$q3_post) / total_n) * 100,
  
  q4_pre = (sum(df_pt$q4_pre) / total_n) * 100,
  q4_post = (sum(df_pt$q4_post) / total_n) * 100,
  
  q5_pre = (sum(df_pt$q5_pre) / total_n) * 100,
  q5_post = (sum(df_pt$q5_post) / total_n) * 100,
  
  q6_pre = (sum(df_pt$q6_pre) / total_n) * 100,
  q6_post = (sum(df_pt$q6_post) / total_n) * 100,
  
  q7_pre = (sum(df_pt$q7_pre) / total_n) * 100,
  q7_post = (sum(df_pt$q7_post) / total_n) * 100,
  
  q8_pre = (sum(df_pt$q8_pre) / total_n) * 100,
  q8_post = (sum(df_pt$q8_post) / total_n) * 100,
  
  q9_pre = (sum(df_pt$q9_pre) / total_n) * 100,
  q9_post = (sum(df_pt$q9_post) / total_n) * 100,
  
  q10_pre = (sum(df_pt$q10_pre) / total_n) * 100,
  q10_post = (sum(df_pt$q10_post) / total_n) * 100)

# Assuming st3_pt is your data frame with percentages
st3_pt_long <- st3_pt %>%
  pivot_longer(cols = everything(), 
               names_to = c("question", "time"), 
               names_sep = "_", 
               values_to = "percentage")

# Create a factor to maintain the order of questions
st3_pt_long$question <- factor(st3_pt_long$question, 
                               levels = paste0("q", 1:10), 
                               labels = paste("Question", 1:10))

# Create the bar graph
plot_pt <- ggplot(st3_pt_long, aes(x = question, y = percentage, fill = time)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) + # Adjust vjust to position labels
  labs(title = "Pre and Post Comparison for Each Question - PT Incompatibility",
       x = "Questions",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("pre" = "lightblue", "post" = "lightgreen"), 
                    name = "Time",
                    labels = c("pre" = "Pre-Test", "post" = "Post-Test"))

# Save the plot in the "plots" directory
ggsave("plots/pre_post_comparison_pt_mo_ap.png", 
       plot = plot_pt, 
       width = 10, 
       height = 6, 
       dpi = 300)


####Plot by relative change 
# PLSCS

relative_change1_plscs <- df_mo %>%
  filter(condition == "plscs") %>%  # Filter for the specified condition
  count(rc_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_code)) +  # x = percentage, y = rc_code
  geom_bar(stat = "identity", aes(fill = factor(rc_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - PLSCS") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen", "lightblue", "brown")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot in the "plots" directory
ggsave("plots/relative_change1_plscs_mo_ap.png", 
       plot = relative_change1_plscs, 
       width = 10, 
       height = 6, 
       dpi = 300)


# Relative Change for HRT

relative_change1_hrt <- df_mo %>%
  filter(condition == "hrt") %>%  # Filter for the specified condition
  count(rc_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_code)) +  # x = percentage, y = rc_code
  geom_bar(stat = "identity", aes(fill = factor(rc_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - HRT") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen", "lightblue", "brown")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot in the "plots" directory
ggsave("plots/relative_change1_hrt_mo_ap.png", 
       plot = relative_change1_hrt, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Relative Change for TB

relative_change1_tb <- df_mo %>%
  filter(condition == "tb") %>%  # Filter for the specified condition
  count(rc_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_code)) +  # x = percentage, y = rc_code
  geom_bar(stat = "identity", aes(fill = factor(rc_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - TB") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen", "lightblue", "brown")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot in the "plots" directory
ggsave("plots/relative_change1_tb_mo_ap.png", 
       plot = relative_change1_tb, 
       width = 10, 
       height = 6, 
       dpi = 300)


# Relative Change for Fever

relative_change1_fever <- df_mo %>%
  filter(condition == "fever") %>%  # Filter for the specified condition
  count(rc_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_code)) +  # x = percentage, y = rc_code
  geom_bar(stat = "identity", aes(fill = factor(rc_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - Fever") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen", "lightblue", "brown")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot in the "plots" directory
ggsave("plots/relative_change1_fever_mo_ap.png", 
       plot = relative_change1_fever, 
       width = 10, 
       height = 6, 
       dpi = 300)


# Relative Change for FGR

relative_change1_fgr <- df_mo %>%
  filter(condition == "fgr") %>%  # Filter for the specified condition
  count(rc_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_code)) +  # x = percentage, y = rc_code
  geom_bar(stat = "identity", aes(fill = factor(rc_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - FGR") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen", "lightblue", "brown")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot in the "plots" directory
ggsave("plots/relative_change1_fgr_mo_ap.png", 
       plot = relative_change1_fgr, 
       width = 10, 
       height = 6, 
       dpi = 300)


# Relative Change for BLE

relative_change1_ble <- df_mo %>%
  filter(condition == "ble") %>%  # Filter for the specified condition
  count(rc_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_code)) +  # x = percentage, y = rc_code
  geom_bar(stat = "identity", aes(fill = factor(rc_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - BLE") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen", "lightblue", "brown")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot in the "plots" directory
ggsave("plots/relative_change1_ble_mo_ap.png", 
       plot = relative_change1_ble, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Relative Change for PPROM

relative_change1_pprom <- df_mo %>%
  filter(condition == "pprom") %>%  # Filter for the specified condition
  count(rc_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_code)) +  # x = percentage, y = rc_code
  geom_bar(stat = "identity", aes(fill = factor(rc_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - PPROM") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen", "lightblue", "brown")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot in the "plots" directory
ggsave("plots/relative_change1_pprom_mo_ap.png", 
       plot = relative_change1_pprom, 
       width = 10, 
       height = 6, 
       dpi = 300)
# Relative Change for HIV

relative_change1_hiv <- df_mo %>%
  filter(condition == "hiv") %>%  # Filter for the specified condition
  count(rc_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_code)) +  # x = percentage, y = rc_code
  geom_bar(stat = "identity", aes(fill = factor(rc_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - HIV") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen", "lightblue", "brown")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot in the "plots" directory
ggsave("plots/relative_change1_hiv_mo_ap.png", 
       plot = relative_change1_hiv, 
       width = 10, 
       height = 6, 
       dpi = 300)
# Relative Change for RFM

relative_change1_rfm <- df_mo %>%
  filter(condition == "rfm") %>%  # Filter for the specified condition
  count(rc_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_code)) +  # x = percentage, y = rc_code
  geom_bar(stat = "identity", aes(fill = factor(rc_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - RFM") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen", "lightblue", "brown")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot in the "plots" directory
ggsave("plots/relative_change1_rfm_mo_ap.png", 
       plot = relative_change1_rfm, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Relative Change for HBSAG

relative_change1_hbsag <- df_mo %>%
  filter(condition == "hbsag") %>%  # Filter for the specified condition
  count(rc_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_code)) +  # x = percentage, y = rc_code
  geom_bar(stat = "identity", aes(fill = factor(rc_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - HBSAG") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen", "lightblue", "brown")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot in the "plots" directory
ggsave("plots/relative_change1_hbsag_mo_ap.png", 
       plot = relative_change1_hbsag, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Relative Change for RH negative

relative_change1_rh <- df_mo %>%
  filter(condition == "rh") %>%  # Filter for the specified condition
  count(rc_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_code)) +  # x = percentage, y = rc_code
  geom_bar(stat = "identity", aes(fill = factor(rc_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - RH") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen", "lightblue", "brown")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot in the "plots" directory
ggsave("plots/relative_change1_rh_mo_ap.png", 
       plot = relative_change1_rh, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Relative Change for Teenage pregnancy

relative_change1_teen <- df_mo %>%
  filter(condition == "teen") %>%  # Filter for the specified condition
  count(rc_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_code)) +  # x = percentage, y = rc_code
  geom_bar(stat = "identity", aes(fill = factor(rc_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - TEEN") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen", "lightblue", "brown")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot in the "plots" directory
ggsave("plots/relative_change1_teen_mo_ap.png", 
       plot = relative_change1_teen, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Relative Change for Perineal trauma

relative_change1_pt <- df_mo %>%
  filter(condition == "pt") %>%  # Filter for the specified condition
  count(rc_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_code)) +  # x = percentage, y = rc_code
  geom_bar(stat = "identity", aes(fill = factor(rc_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - PT") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen", "lightblue", "brown")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot in the "plots" directory
ggsave("plots/relative_change1_pt_mo_ap.png", 
       plot = relative_change1_pt, 
       width = 10, 
       height = 6, 
       dpi = 300)


####Positive, Negative and No change 
# Relative Change for PLSCS

relative_change2_plscs <- df_mo %>%
  filter(condition == "plscs") %>%  # Filter for the specified condition
  count(rc_positive_negative_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_positive_negative_code)) +  # x = percentage, y = rc_positive_negative_code
  geom_bar(stat = "identity", aes(fill = factor(rc_positive_negative_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - PLSCS") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 1))

# Save the plot in the "plots" directory
ggsave("plots/relative_change2_plscs_mo_ap.png", 
       plot = relative_change2_plscs, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Relative Change for HRT

relative_change2_hrt <- df_mo %>%
  filter(condition == "hrt") %>%  # Filter for the specified condition
  count(rc_positive_negative_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_positive_negative_code)) +  # x = percentage, y = rc_positive_negative_code
  geom_bar(stat = "identity", aes(fill = factor(rc_positive_negative_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - HRT") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 1))

# Save the plot in the "plots" directory
ggsave("plots/relative_change2_hrt_mo_ap.png", 
       plot = relative_change2_hrt, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Relative Change for TB

relative_change2_tb <- df_mo %>%
  filter(condition == "tb") %>%  # Filter for the specified condition
  count(rc_positive_negative_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_positive_negative_code)) +  # x = percentage, y = rc_positive_negative_code
  geom_bar(stat = "identity", aes(fill = factor(rc_positive_negative_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - TB") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 1))

# Save the plot in the "plots" directory
ggsave("plots/relative_change2_tb_mo_ap.png", 
       plot = relative_change2_tb, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Relative Change for Fever

relative_change2_fever <- df_mo %>%
  filter(condition == "fever") %>%  # Filter for the specified condition
  count(rc_positive_negative_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_positive_negative_code)) +  # x = percentage, y = rc_positive_negative_code
  geom_bar(stat = "identity", aes(fill = factor(rc_positive_negative_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - Fever") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 1))

# Save the plot in the "plots" directory
ggsave("plots/relative_change2_fever_mo_ap.png", 
       plot = relative_change2_fever, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Relative Change for FGR

relative_change2_fgr <- df_mo %>%
  filter(condition == "fgr") %>%  # Filter for the specified condition
  count(rc_positive_negative_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_positive_negative_code)) +  # x = percentage, y = rc_positive_negative_code
  geom_bar(stat = "identity", aes(fill = factor(rc_positive_negative_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - FGR") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 1))

# Save the plot in the "plots" directory
ggsave("plots/relative_change2_fgr_mo_ap.png", 
       plot = relative_change2_fgr, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Relative Change for BLE

relative_change2_ble <- df_mo %>%
  filter(condition == "ble") %>%  # Filter for the specified condition
  count(rc_positive_negative_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_positive_negative_code)) +  # x = percentage, y = rc_positive_negative_code
  geom_bar(stat = "identity", aes(fill = factor(rc_positive_negative_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - BLE") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 1))

# Save the plot in the "plots" directory
ggsave("plots/relative_change2_ble_mo_ap.png", 
       plot = relative_change2_ble, 
       width = 10, 
       height = 6, 
       dpi = 300)


# Create a summary table with percentages, now named sta4
sta4 <- df_mo %>%
  group_by(condition) %>%
  summarise(
    count_positive_change = sum(rc_positive_negative_code == "Positive change"),
    total_cases = n(),
    percentage_positive_change = (count_positive_change / total_cases) * 100
  )

# View the summary table
print(sta4)

# Create a bar plot for the percentage of "Positive change"
positive_change <- ggplot(sta4, aes(x = condition, y = percentage_positive_change)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_text(aes(label = round(percentage_positive_change, 1)), 
            vjust = -0.5, size = 5) +  # Add labels above the bars
  labs(title = "Percentage of Positive Change by Condition",
       x = "Condition",
       y = "Percentage of Positive Change (%)") +
  theme_minimal()

# Save the plot in the "plots" directory
ggsave("plots/positive_change_mo_ap(2,3).png", 
       plot = positive_change, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Relative Change for PPROM

relative_change2_pprom <- df_mo %>%
  filter(condition == "pprom") %>%  # Filter for the specified condition
  count(rc_positive_negative_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_positive_negative_code)) +  # x = percentage, y = rc_positive_negative_code
  geom_bar(stat = "identity", aes(fill = factor(rc_positive_negative_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - PPROM") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 1))

# Save the plot in the "plots" directory
ggsave("plots/relative_change2_pprom_mo_ap.png", 
       plot = relative_change2_pprom, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Relative Change for HIV

relative_change2_hiv <- df_mo %>%
  filter(condition == "hiv") %>%  # Filter for the specified condition
  count(rc_positive_negative_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_positive_negative_code)) +  # x = percentage, y = rc_positive_negative_code
  geom_bar(stat = "identity", aes(fill = factor(rc_positive_negative_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - HIV") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 1))

# Save the plot in the "plots" directory
ggsave("plots/relative_change2_hiv_mo_ap.png", 
       plot = relative_change2_hiv, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Relative Change for RFM

relative_change2_rfm <- df_mo %>%
  filter(condition == "rfm") %>%  # Filter for the specified condition
  count(rc_positive_negative_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_positive_negative_code)) +  # x = percentage, y = rc_positive_negative_code
  geom_bar(stat = "identity", aes(fill = factor(rc_positive_negative_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - RFM") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 1))

# Save the plot in the "plots" directory
ggsave("plots/relative_change2_rfm_mo_ap.png", 
       plot = relative_change2_rfm, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Relative Change for HBSAG

relative_change2_hbsag <- df_mo %>%
  filter(condition == "hbsag") %>%  # Filter for the specified condition
  count(rc_positive_negative_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_positive_negative_code)) +  # x = percentage, y = rc_positive_negative_code
  geom_bar(stat = "identity", aes(fill = factor(rc_positive_negative_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - HBSAG") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 1))

# Save the plot in the "plots" directory
ggsave("plots/relative_change2_hbsag_mo_ap.png", 
       plot = relative_change2_hbsag, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Relative Change for RH

relative_change2_rh <- df_mo %>%
  filter(condition == "rh") %>%  # Filter for the specified condition
  count(rc_positive_negative_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_positive_negative_code)) +  # x = percentage, y = rc_positive_negative_code
  geom_bar(stat = "identity", aes(fill = factor(rc_positive_negative_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - RH") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 1))

# Save the plot in the "plots" directory
ggsave("plots/relative_change2_rh_mo_ap.png", 
       plot = relative_change2_rh, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Relative Change for TEEN

relative_change2_teen <- df_mo %>%
  filter(condition == "teen") %>%  # Filter for the specified condition
  count(rc_positive_negative_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_positive_negative_code)) +  # x = percentage, y = rc_positive_negative_code
  geom_bar(stat = "identity", aes(fill = factor(rc_positive_negative_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - TEEN") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 1))

# Save the plot in the "plots" directory
ggsave("plots/relative_change2_teen_mo_ap.png", 
       plot = relative_change2_teen, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Relative Change for PT

relative_change2_pt <- df_mo %>%
  filter(condition == "pt") %>%  # Filter for the specified condition
  count(rc_positive_negative_code) %>%  # Count occurrences
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot(aes(x = percentage, y = rc_positive_negative_code)) +  # x = percentage, y = rc_positive_negative_code
  geom_bar(stat = "identity", aes(fill = factor(rc_positive_negative_code))) +  # Create horizontal bars with percentage heights
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +  # Add percentage labels
  labs(x = "Percentage", y = "Relative Change", title = "Relative Change in Post-Test Scores - PT") +  
  scale_fill_manual(values = c("pink", "yellow", "darkgreen")) +  # Set colors manually
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +  # Adjust for horizontal bars
  theme(plot.title = element_text(hjust = 1))

# Save the plot in the "plots" directory
ggsave("plots/relative_change2_pt_mo_ap.png", 
       plot = relative_change2_pt, 
       width = 10, 
       height = 6, 
       dpi = 300)


####District-wise 
# PLSCS
district_wise_scores <- df_mo |>
  filter(condition == "plscs") |>
  group_by(district_name) |>
  summarise(mean_pre_test = mean(pre_total_correct, na.rm = TRUE), 
            mean_post_test = mean(post_total_correct, na.rm = TRUE))

district_wise_scores_long <- district_wise_scores |>
  pivot_longer(cols = starts_with("mean_"), names_to = "test_type", values_to = "mean_score")  

districtwise_plscs <- ggplot(district_wise_scores_long, aes(x = district_name, y = mean_score, fill = test_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean_score, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  labs(title = "Comparison of Pre-Test and Post-Test Mean Scores by District - PLSCS",
       x = "District Name",
       y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/districtwise_plscs_mo_ap.png", 
       plot = districtwise_plscs, 
       width = 10, 
       height = 6, 
       dpi = 300)

# HRT
district_wise_scores <- df_mo |>
  filter(condition == "hrt") |>
  group_by(district_name) |>
  summarise(mean_pre_test = mean(pre_total_correct, na.rm = TRUE), 
            mean_post_test = mean(post_total_correct, na.rm = TRUE))

district_wise_scores_long <- district_wise_scores |>
  pivot_longer(cols = starts_with("mean_"), names_to = "test_type", values_to = "mean_score")  

districtwise_hrt <- ggplot(district_wise_scores_long, aes(x = district_name, y = mean_score, fill = test_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean_score, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  labs(title = "Comparison of Pre-Test and Post-Test Mean Scores by District - HRT",
       x = "District Name",
       y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/districtwise_hrt_mo_ap.png", 
       plot = districtwise_hrt, 
       width = 10, 
       height = 6, 
       dpi = 300)

# TB
district_wise_scores <- df_mo |>
  filter(condition == "tb") |>
  group_by(district_name) |>
  summarise(mean_pre_test = mean(pre_total_correct, na.rm = TRUE), 
            mean_post_test = mean(post_total_correct, na.rm = TRUE))

district_wise_scores_long <- district_wise_scores |>
  pivot_longer(cols = starts_with("mean_"), names_to = "test_type", values_to = "mean_score")  

districtwise_tb <- ggplot(district_wise_scores_long, aes(x = district_name, y = mean_score, fill = test_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean_score, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  labs(title = "Comparison of Pre-Test and Post-Test Mean Scores by District - Tuberculosis",
       x = "District Name",
       y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/districtwise_tb_mo_ap.png", 
       plot = districtwise_tb, 
       width = 10, 
       height = 6, 
       dpi = 300)

# FEVER
district_wise_scores <- df_mo |>
  filter(condition == "fever") |>
  group_by(district_name) |>
  summarise(mean_pre_test = mean(pre_total_correct, na.rm = TRUE), 
            mean_post_test = mean(post_total_correct, na.rm = TRUE))

district_wise_scores_long <- district_wise_scores |>
  pivot_longer(cols = starts_with("mean_"), names_to = "test_type", values_to = "mean_score")  

districtwise_fever <- ggplot(district_wise_scores_long, aes(x = district_name, y = mean_score, fill = test_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean_score, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  labs(title = "Comparison of Pre-Test and Post-Test Mean Scores by District - FEVER",
       x = "District Name",
       y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/districtwise_fever_mo_ap.png", 
       plot = districtwise_fever, 
       width = 10, 
       height = 6, 
       dpi = 300)

# FGR
district_wise_scores <- df_mo |>
  filter(condition == "fgr") |>
  group_by(district_name) |>
  summarise(mean_pre_test = mean(pre_total_correct, na.rm = TRUE), 
            mean_post_test = mean(post_total_correct, na.rm = TRUE))

district_wise_scores_long <- district_wise_scores |>
  pivot_longer(cols = starts_with("mean_"), names_to = "test_type", values_to = "mean_score")  

districtwise_fgr <- ggplot(district_wise_scores_long, aes(x = district_name, y = mean_score, fill = test_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean_score, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  labs(title = "Comparison of Pre-Test and Post-Test Mean Scores by District - FGR - Fetal Growth Restriction",
       x = "District Name",
       y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/districtwise_fgr_mo_ap.png", 
       plot = districtwise_fgr, 
       width = 10, 
       height = 6, 
       dpi = 300)

# BLE
district_wise_scores <- df_mo |>
  filter(condition == "ble") |>
  group_by(district_name) |>
  summarise(mean_pre_test = mean(pre_total_correct, na.rm = TRUE), 
            mean_post_test = mean(post_total_correct, na.rm = TRUE))

district_wise_scores_long <- district_wise_scores |>
  pivot_longer(cols = starts_with("mean_"), names_to = "test_type", values_to = "mean_score")  

districtwise_ble <- ggplot(district_wise_scores_long, aes(x = district_name, y = mean_score, fill = test_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean_score, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  labs(title = "Comparison of Pre-Test and Post-Test Mean Scores by District - BLEEDING",
       x = "District Name",
       y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/districtwise_ble_mo_ap.png", 
       plot = districtwise_ble, 
       width = 10, 
       height = 6, 
       dpi = 300)
# PPROM
district_wise_scores <- df_mo |>
  filter(condition == "pprom") |>
  group_by(district_name) |>
  summarise(mean_pre_test = mean(pre_total_correct, na.rm = TRUE), 
            mean_post_test = mean(post_total_correct, na.rm = TRUE))

district_wise_scores_long <- district_wise_scores |>
  pivot_longer(cols = starts_with("mean_"), names_to = "test_type", values_to = "mean_score")  

districtwise_pprom <- ggplot(district_wise_scores_long, aes(x = district_name, y = mean_score, fill = test_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean_score, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  labs(title = "Comparison of Pre-Test and Post-Test Mean Scores by District - PPROM",
       x = "District Name",
       y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/districtwise_pprom_mo_ap.png", 
       plot = districtwise_pprom, 
       width = 10, 
       height = 6, 
       dpi = 300)

# HIV
district_wise_scores <- df_mo |>
  filter(condition == "hiv") |>
  group_by(district_name) |>
  summarise(mean_pre_test = mean(pre_total_correct, na.rm = TRUE), 
            mean_post_test = mean(post_total_correct, na.rm = TRUE))

district_wise_scores_long <- district_wise_scores |>
  pivot_longer(cols = starts_with("mean_"), names_to = "test_type", values_to = "mean_score")  

districtwise_hiv <- ggplot(district_wise_scores_long, aes(x = district_name, y = mean_score, fill = test_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean_score, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  labs(title = "Comparison of Pre-Test and Post-Test Mean Scores by District - HIV",
       x = "District Name",
       y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/districtwise_hiv_mo_ap.png", 
       plot = districtwise_hiv, 
       width = 10, 
       height = 6, 
       dpi = 300)

# RFM
district_wise_scores <- df_mo |>
  filter(condition == "rfm") |>
  group_by(district_name) |>
  summarise(mean_pre_test = mean(pre_total_correct, na.rm = TRUE), 
            mean_post_test = mean(post_total_correct, na.rm = TRUE))

district_wise_scores_long <- district_wise_scores |>
  pivot_longer(cols = starts_with("mean_"), names_to = "test_type", values_to = "mean_score")  

districtwise_rfm <- ggplot(district_wise_scores_long, aes(x = district_name, y = mean_score, fill = test_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean_score, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  labs(title = "Comparison of Pre-Test and Post-Test Mean Scores by District - RFM",
       x = "District Name",
       y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/districtwise_rfm_mo_ap.png", 
       plot = districtwise_rfm, 
       width = 10, 
       height = 6, 
       dpi = 300)

# HBSAG
district_wise_scores <- df_mo |>
  filter(condition == "hbsag") |>
  group_by(district_name) |>
  summarise(mean_pre_test = mean(pre_total_correct, na.rm = TRUE), 
            mean_post_test = mean(post_total_correct, na.rm = TRUE))

district_wise_scores_long <- district_wise_scores |>
  pivot_longer(cols = starts_with("mean_"), names_to = "test_type", values_to = "mean_score")  

districtwise_hbsag <- ggplot(district_wise_scores_long, aes(x = district_name, y = mean_score, fill = test_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean_score, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  labs(title = "Comparison of Pre-Test and Post-Test Mean Scores by District - HBSAG",
       x = "District Name",
       y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/districtwise_hbsag_mo_ap.png", 
       plot = districtwise_hbsag, 
       width = 10, 
       height = 6, 
       dpi = 300)

# RH
district_wise_scores <- df_mo |>
  filter(condition == "rh") |>
  group_by(district_name) |>
  summarise(mean_pre_test = mean(pre_total_correct, na.rm = TRUE), 
            mean_post_test = mean(post_total_correct, na.rm = TRUE))

district_wise_scores_long <- district_wise_scores |>
  pivot_longer(cols = starts_with("mean_"), names_to = "test_type", values_to = "mean_score")  

districtwise_rh <- ggplot(district_wise_scores_long, aes(x = district_name, y = mean_score, fill = test_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean_score, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  labs(title = "Comparison of Pre-Test and Post-Test Mean Scores by District - RH",
       x = "District Name",
       y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/districtwise_rh_mo_ap.png", 
       plot = districtwise_rh, 
       width = 10, 
       height = 6, 
       dpi = 300)

# TEEN
district_wise_scores <- df_mo |>
  filter(condition == "teen") |>
  group_by(district_name) |>
  summarise(mean_pre_test = mean(pre_total_correct, na.rm = TRUE), 
            mean_post_test = mean(post_total_correct, na.rm = TRUE))

district_wise_scores_long <- district_wise_scores |>
  pivot_longer(cols = starts_with("mean_"), names_to = "test_type", values_to = "mean_score")  

districtwise_teen <- ggplot(district_wise_scores_long, aes(x = district_name, y = mean_score, fill = test_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean_score, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  labs(title = "Comparison of Pre-Test and Post-Test Mean Scores by District - TEEN",
       x = "District Name",
       y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/districtwise_teen_mo_ap.png", 
       plot = districtwise_teen, 
       width = 10, 
       height = 6, 
       dpi = 300)
# PT
district_wise_scores <- df_mo |>
  filter(condition == "pt") |>
  group_by(district_name) |>
  summarise(mean_pre_test = mean(pre_total_correct, na.rm = TRUE), 
            mean_post_test = mean(post_total_correct, na.rm = TRUE))

district_wise_scores_long <- district_wise_scores |>
  pivot_longer(cols = starts_with("mean_"), names_to = "test_type", values_to = "mean_score")  

districtwise_pt <- ggplot(district_wise_scores_long, aes(x = district_name, y = mean_score, fill = test_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean_score, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  labs(title = "Comparison of Pre-Test and Post-Test Mean Scores by District - PT",
       x = "District Name",
       y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/districtwise_pt_mo_ap(2,3).png", 
       plot = districtwise_pt, 
       width = 10, 
       height = 6, 
       dpi = 300)


# Combine data for all conditions
relative_change_combined <- df_mo %>%
  count(condition, rc_positive_negative_code) %>%  # Count occurrences for each condition and change type
  group_by(condition) %>%  # Group by condition
  mutate(percentage = n / sum(n) * 100)  # Calculate percentage

# Save the data frame as a CSV file
write.csv(relative_change_combined, "relative_change_combined.csv", row.names = FALSE)

# Add condition names
relative_change_combined <- relative_change_combined %>%
  mutate(condition_name = case_when(
    condition == "plscs" ~ "Previous LSCS",
    condition == "hrt" ~ "Pregnancy with heart disease",
    condition == "tb" ~ "Tuberculosis",
    condition == "fever" ~ "FEVER",
    condition == "fgr" ~ "Fetal growth restriction",
    condition == "ble" ~ "Bleeding before 20 weeks",
    condition == "pprom" ~ "Preterm labor and PPROM",
    condition == "hiv" ~ "HIV",
    condition == "rfm" ~ "Reduced fetal movements",
    condition == "hbsag" ~ "HBsAg",
    condition == "rh" ~ "RH Negative",
    condition == "teen" ~ "Teenage pregnancy",
    condition == "pt" ~ "Prevention of Perineal trauma",
    TRUE ~ condition
  ))

# Filter for 'Positive change'
positive_changes <- relative_change_combined %>% 
  filter(rc_positive_negative_code == "Positive change")

# Find the index of the maximum percentage
max_index <- which.max(positive_changes$percentage)

# Extract the row with the highest percentage
highest_positive_change <- positive_changes[max_index, ]

# Extract the corresponding condition name
condition_name <- highest_positive_change$condition

# Print the result
print(highest_positive_change)
print(paste("Condition with the highest positive change:", condition_name))

#Map - Trainings happening in districts
#Pull number of responses
district_responses <- df_mo %>%
  group_by(district_name) %>%
  summarise(no_of_responses = n())

#2. Import the Data
# define file path
ap_map_path<- here::here("pre_post_test","data","ap_mch_data_sf.rds")

# read data
ap_map <- rio::import(here(ap_map_path))

library(sf)

#Plotting a map

ap_mo_sf <- st_as_sf(ap_map)

ap_mo_sf <- ap_mo_sf |> 
  select(-c("percentage_of_women_who_had_4_an_cs",
            "percentage_of_women_identified_as_high_risk"))

# Modify the district name
ap_mo_sf <- ap_mo_sf %>%
  mutate(district = ifelse(district == "Alluri Sitharama Raju", "Alluri Sitaram Raju", district))

ap_mo_sf <- ap_mo_sf %>%
  mutate(district = ifelse(district == "Parvathipuram Manyam", "Parvatipuram Manyam", district))

ap_mo_sf <- ap_mo_sf %>%
  mutate(district = ifelse(district == "Prakasam\xa0", "Prakasam", district))

# Left join using different column names
merged_sf_map <- left_join(ap_mo_sf, district_responses, by = c("district" = "district_name"))

#Identify districts in training
merged_sf_map <- merged_sf_map %>%
  mutate(map = ifelse(!is.na(no_of_responses), 1, 0))

# Check the geometry column name
geom_col <- attr(merged_sf_map, "sf_column")

map <- ggplot(merged_sf_map) +
  geom_sf(aes(fill = factor(map)), color = "black") +
  scale_fill_manual(values = c("0" = "grey90", "1" = "lightgreen")) +
  geom_sf_text(data = merged_sf_map %>% filter(map == 1), aes(label = district), size = 4) +  # Add labels for map == 1
  theme_void() +
  theme(legend.position = "none")

# Save the plot in the "plots" directory
ggsave("plots/map.png", 
       plot = map, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Display the plot
print(map)

# Combine data for all conditions
relative_change_combined <- df_mo %>%
  count(condition, rc_positive_negative_code) %>%  # Count occurrences for each condition and change type
  group_by(condition) %>%  # Group by condition
  mutate(percentage = n / sum(n) * 100)  # Calculate percentage

# Reorder 'condition' based on descending percentage of "Positive change"
positive_order <- relative_change_combined %>%
  filter(rc_positive_negative_code == "Positive change") %>%
  arrange(percentage) %>%  # Sort in ascending order
  pull(condition)  # Extract the ordered conditions

relative_change_combined <- relative_change_combined %>%
  mutate(condition = factor(condition, levels = positive_order))  # Reorder factor levels

# Create the plot object
plot_relative_change <- ggplot(relative_change_combined, aes(x = percentage, y = condition, fill = rc_positive_negative_code)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.9), 
            hjust = -0.2, 
            size = 3) +
  labs(x = "Percentage (%)", y = "Condition") +
  scale_fill_manual(values = c("Positive change" = "darkgreen", 
                               "Negative change" = "red", 
                               "No Change" = "orange")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )

# Save the plot in the "plots" directory
ggsave("plots/relative_change_combined(2,3).png", 
       plot = plot_relative_change, 
       width = 10, 
       height = 6, 
       dpi = 300)


# Calculate the district-wise mean percentage for pre and post test scores
district_wise_combined <- df_mo |>
  group_by(district_name) |>
  summarise(mean_pre_test = mean(pre_total_correct, na.rm = TRUE) * 100 / max(pre_total_correct, na.rm = TRUE), 
            mean_post_test = mean(post_total_correct, na.rm = TRUE) * 100 / max(post_total_correct, na.rm = TRUE))

# Reorder district_name factor by post-test mean percentage in descending order
district_wise_combined <- district_wise_combined |>
  arrange(desc(mean_post_test)) |>
  mutate(district_name = factor(district_name, levels = district_name))

# Reshape the data to long format
district_wise_combined_long <- district_wise_combined |>
  pivot_longer(cols = starts_with("mean_"), names_to = "test_type", values_to = "mean_percentage")

# Reorder 'test_type' factor levels to ensure 'pre' comes before 'post'
district_wise_combined_long$test_type <- factor(district_wise_combined_long$test_type, 
                                                levels = c("mean_pre_test", "mean_post_test"),
                                                labels = c("pre", "post"))

# Create the updated ggplot bar chart
districtwise_combined_percentage_plot <- ggplot(district_wise_combined_long, aes(x = district_name, y = mean_percentage, fill = test_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = round(mean_percentage, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +  # Reduce the data label size
  labs(x = "District Name",
       y = "Mean Percentage Score",
       fill = "Test Type") +  # Add legend title
  scale_fill_manual(values = c("pre" = "coral", "post" = "cyan3")) +  # Set colors for pre and post
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )

# Save the plot in the "plots" directory
ggsave("plots/districtwise_combined_percentage_mo_ap(2,3).png", 
       plot = districtwise_combined_percentage_plot, 
       width = 10, 
       height = 6, 
       dpi = 300)

