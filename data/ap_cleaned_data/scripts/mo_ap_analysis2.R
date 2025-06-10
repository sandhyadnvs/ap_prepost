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
  here::here('pre_post_test','data', 'ap_cleaned_data','mo_cleaned_ble_ap.csv')
)


# Import all files as data frames
data_frames <- lapply(filepaths, read.csv)

# Bind all data frames into a single data frame
df_mo2 <- do.call(rbind, data_frames)

#Cleaning the data

df_mo2 <-
  df_mo2 |>
  janitor::clean_names()

#Remove unwanted columns

df_mo2 <- df_mo2 |> 
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

df_mo2$age_pre <- df_mo2$age_pre |> as.character()

# Ensure `age_pre` is numeric
df_mo2$age_pre <- as.numeric(df_mo2$age_pre)

# Current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Identify values in `age_pre` that are in the year format and convert them to age
df_mo2$age_pre <- ifelse(df_mo2$age_pre >= 1900 & df_mo2$age_pre <= current_year,  # Assuming any value between 1900 and the current year is a year
                         current_year - df_mo2$age_pre,  # Convert year to age
                         df_mo2$age_pre)  # Leave other values unchanged

#Change the district names and mutate a column with the concerned district names

df_mo2 <- df_mo2 %>%
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
df_mo2 <- df_mo2 %>%
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
df_mo2 <- df_mo2 %>%
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
df_mo2 <- df_mo2 %>%
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
df_mo2 <- df_mo2 %>%
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
df_mo2 <- df_mo2 %>%
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
df_mo2 <- df_mo2 %>%
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
df_mo2 <- df_mo2 %>%
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
df_mo2 <- df_mo2 %>%
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
df_mo2 <- df_mo2 %>%
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
df_mo2 <- df_mo2 %>%
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
df_mo2 <- df_mo2 %>%
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
df_mo2 <- df_mo2 %>%
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

# Sum of Pre and post total correct scores
df_mo2 <- df_mo2 %>%
  mutate(
    pre_total_correct = rowSums(select(., starts_with("q") & ends_with("pre"))),
    post_total_correct = rowSums(select(., starts_with("q") & ends_with("post")))
  )

df_mo2 <- df_mo2 |> 
  mutate(relative_change = post_total_correct - pre_total_correct)

# Adding relative change column and creating code for the same
df_mo2 <- df_mo2 %>%
  mutate(rc_code = case_when(
    relative_change == 0 ~ "No Change",
    relative_change %in% c(-1, -2) ~ "-1 to -2",
    relative_change %in% c(1, 2) ~ "1 to 2",
    relative_change >= 3 ~ "+3 and above",
    relative_change <= -3 ~ "-3 and below",
    TRUE ~ NA_character_  # Ensure that the NA type is character to match the other outcomes
  ))

df_mo2 <- df_mo2 %>%
  mutate(rc_positive_negative_code = case_when(
    relative_change == 0 ~ "No Change",
    relative_change %in% c(-1, -2) ~ "Negative change",
    relative_change %in% c(1, 2) ~ "Positive change",
    relative_change >= 3 ~ "Positive change",
    relative_change <= -3 ~ "Negative change",
    TRUE ~ NA_character_  # Ensure that the NA type is character to match the other outcomes
  ))

# Create summary statistics grouped by district_name and condition
sta1_mo2 <- df_mo2 %>%
  group_by(district_name, condition) %>%
  summarise(
    pre_mean = round(mean(pre_total_correct, na.rm = TRUE), 1),
    pre_std = round(sd(pre_total_correct, na.rm = TRUE), 1),
    pre_mean_pct = round((mean(pre_total_correct, na.rm = TRUE) / 10) * 100, 1),  # Pre-test percentage
    post_mean = round(mean(post_total_correct, na.rm = TRUE), 1),
    post_std = round(sd(post_total_correct, na.rm = TRUE), 1),
    post_mean_pct = round((mean(post_total_correct, na.rm = TRUE) / 10) * 100, 1),  # Post-test percentage
    p_value = ifelse(
      sum(!is.na(pre_total_correct)) > 1 & sum(!is.na(post_total_correct)) > 1, 
      round(t.test(pre_total_correct, post_total_correct)$p.value, 6), 
      NA )
  )

# Calculate state average (overall mean, std, and percentage grouped by condition)
state_avg_mo2 <- df_mo2 %>%
  group_by(condition) %>%
  summarise(
    pre_mean = round(mean(pre_total_correct, na.rm = TRUE), 1),
    pre_std = round(sd(pre_total_correct, na.rm = TRUE), 1),
    pre_mean_pct = round((mean(pre_total_correct, na.rm = TRUE) / 10) * 100, 1),  # Pre-test percentage
    post_mean = round(mean(post_total_correct, na.rm = TRUE), 1),
    post_std = round(sd(post_total_correct, na.rm = TRUE), 1),
    post_mean_pct = round((mean(post_total_correct, na.rm = TRUE) / 10) * 100, 1)  # Post-test percentage
  ) %>%
  mutate(district_name = "Overall Average")  # Add state average label

# Combine district-level and state-level data
sta1_mo2 <- bind_rows(sta1_mo2, state_avg_mo2)

# Save sta1 as a CSV file
write.csv(sta1_mo2, "tables/sta1_summary_mo2_ap.csv", row.names = FALSE)

# Overall mean, std, and percentage grouped by condition
sta2_mo2 <- df_mo2 %>%
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

# Calculate state average (same as above)
state_avg_mo2 <- df_mo2 %>%
  summarise(
    pre_mean = round(mean(pre_total_correct, na.rm = TRUE), 1),
    pre_std = round(sd(pre_total_correct, na.rm = TRUE), 1),
    pre_mean_pct = round((mean(pre_total_correct, na.rm = TRUE) / 10) * 100, 1),  # Pre-test percentage
    post_mean = round(mean(post_total_correct, na.rm = TRUE), 1),
    post_std = round(sd(post_total_correct, na.rm = TRUE), 1),
    post_mean_pct = round((mean(post_total_correct, na.rm = TRUE) / 10) * 100, 1)  # Post-test percentage
  ) %>%
  mutate(condition = "Overall Average")  # Add state average label

# Combine condition-level and state-level data
sta2_mo2 <- bind_rows(sta2_mo2, state_avg_mo2)

# Save sta2 as a CSV file
write.csv(sta2_mo2, "tables/sta2_summary_mo2_ap.csv", row.names = FALSE)

# Reorder condition factor based on post-test mean percentage in descending order
sta2_mo2 <- sta2_mo2 %>%
  arrange(desc(post_mean_pct)) %>%
  mutate(condition = factor(condition, levels = condition))

# Reshape the data from wide to long format using tidyr's pivot_longer
sta2_long_mo2 <- tidyr::pivot_longer(sta2_mo2, 
                                     cols = c("pre_mean_pct", "post_mean_pct"), 
                                     names_to = "Time", 
                                     values_to = "Mean_Pct")

# Calculate the gain in knowledge for each condition
knowledge_gain_mo2 <- sta2_mo2 %>%
  mutate(Gain = post_mean_pct - pre_mean_pct) %>%
  select(condition, Gain)

# Reorder 'Time' factor levels to ensure 'pre' comes before 'post'
sta2_long_mo2$Time <- factor(sta2_long_mo2$Time, levels = c("pre_mean_pct", "post_mean_pct"))

# Combine data for all conditions
relative_change_combined_mo2 <- df_mo2 %>%
  count(condition, rc_positive_negative_code) %>%  # Count occurrences for each condition and change type
  group_by(condition) %>%  # Group by condition
  mutate(percentage = n / sum(n) * 100)  # Calculate percentage

# Reorder 'condition' based on descending percentage of "Positive change"
positive_order_mo2 <- relative_change_combined_mo2 %>%
  filter(rc_positive_negative_code == "Positive change") %>%
  arrange(percentage) %>%  # Sort in ascending order
  pull(condition)  # Extract the ordered conditions

relative_change_combined_mo2 <- relative_change_combined_mo2 %>%
  mutate(condition = factor(condition, levels = positive_order_mo2))  
# Reorder factor levels

# Replace condition abbreviations with full names
sta2_mo2 <- sta2_mo2 %>%
  mutate(condition_name = case_when(
    condition == "plscs" ~ "Previous LSCS",
    condition == "hrt" ~ "Heart Disease",
    condition == "tb" ~ "Tuberculosis",
    condition == "fever" ~ "Fever",
    condition == "fgr" ~ "Fetal Growth Restriction",
    condition == "ble" ~ "Bleeding before 20 weeks",
    TRUE ~ condition  # Keep other values unchanged (if any)
  ))

# Map - Trainings happening in districts
# Pull number of responses
district_responses <- df_mo2 %>%
  group_by(district_name) %>%
  summarise(no_of_responses = n())

# 2. Import the Data
# Define file path for the shapefile
ap_map_path <- here::here("data", "ap_cleaned_data", "ap_mch_data_sf.rds")

# Read the shapefile data
ap_map <- rio::import(here(ap_map_path))

library(sf)

# Convert the shapefile data to spatial object
ap_mo2_sf <- st_as_sf(ap_map)

# Remove unnecessary columns from the shapefile
ap_mo2_sf <- ap_mo2_sf |> 
  select(-c("percentage_of_women_who_had_4_an_cs",
            "percentage_of_women_identified_as_high_risk"))

# Modify district names to ensure consistency
ap_mo2_sf <- ap_mo2_sf %>%
  mutate(district = ifelse(district == "Alluri Sitharama Raju", "Alluri Sitaram Raju", district))

ap_mo2_sf <- ap_mo2_sf %>%
  mutate(district = ifelse(district == "Parvathipuram Manyam", "Parvatipuram Manyam", district))

ap_mo2_sf <- ap_mo2_sf %>%
  mutate(district = ifelse(district == "Prakasam", "Prakasam", district))

# Left join using different column names (district_name vs district)
merged_sf_map <- left_join(ap_mo2_sf, district_responses, by = c("district" = "district_name"))

# Identify districts with training (those with responses)
merged_sf_map <- merged_sf_map %>%
  mutate(map = ifelse(!is.na(no_of_responses), 1, 0))

# Check the geometry column name
geom_col <- attr(merged_sf_map, "sf_column")

# Calculate the knowledge gain for each condition
knowledge_gain <- sta2_mo2 %>%
  mutate(Gain = post_mean_pct - pre_mean_pct) %>%
  select(condition, Gain)

# Reorder 'Time' factor levels to ensure 'pre' comes before 'post'
sta2_long_mo2$Time <- factor(sta2_long_mo2$Time, levels = c("pre_mean_pct", "post_mean_pct"))

# Combine data for all conditions to calculate relative change percentages
relative_change_combined <- df_mo2 %>%
  count(condition, rc_positive_negative_code) %>%
  group_by(condition) %>%
  mutate(percentage = n / sum(n) * 100)

# Reorder conditions based on descending percentage of "Positive change"
positive_order <- relative_change_combined %>%
  filter(rc_positive_negative_code == "Positive change") %>%
  arrange(percentage) %>%
  pull(condition)

relative_change_combined <- relative_change_combined %>%
  mutate(condition = factor(condition, levels = positive_order))

# Plotting the map
ap_mo2_map <- ggplot(merged_sf_map) +
  geom_sf(aes(fill = factor(map)), color = "black") +
  scale_fill_manual(values = c("0" = "grey90", "1" = "lightgreen")) +  # Highlight districts with training
  geom_sf_text(data = merged_sf_map %>% filter(map == 1), aes(label = district), size = 3) +  # Add labels
  theme_void() +
  theme(legend.position = "none")  # Remove legend

# Save the map as a JPG file
ggsave("plots/ap_mo2_map.jpg", plot = ap_mo2_map, device = "jpg", width = 10, height = 8, dpi = 300)

# Mean pre-post percentage
# Create a new column to differentiate the state average from other conditions
sta2_long_mo2 <- sta2_long_mo2 %>%
  mutate(state_avg = ifelse(condition == "Overall Average", "Overall Average", "Conditions"))

# Create the ggplot bar chart with adjustments for spacing and legibility
pre_post_mean_pct_mo2 <- ggplot(sta2_long_mo2, aes(x = condition, y = Mean_Pct, fill = interaction(Time, state_avg))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Increase spacing
  scale_fill_manual(values = c("pre_mean_pct.Conditions" = "blue", 
                               "post_mean_pct.Conditions" = "orange",
                               "pre_mean_pct.Overall Average" = "lightskyblue", 
                               "post_mean_pct.Overall Average" = "lightgreen")) +
  geom_text(aes(label = round(Mean_Pct, 1)), 
            position = position_dodge(width = 0.8),  # Adjust text position to align with the new dodge width
            vjust = -0.5) +  # Adjust label position
  labs(x = "Condition", y = "Mean %") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold", angle = 45, hjust = 1),  # Rotate axis labels for better legibility
    axis.title.x = element_text(size = 14, face = "bold"),
    plot.margin = margin(10, 20, 10, 20),  # Add space around the plot
    panel.grid.major.x = element_blank(),  # Optional: remove vertical grid lines for clarity
    panel.grid.minor.x = element_blank()
  ) +
  # Add gain in knowledge as annotations below the x-axis labels
  annotate("text", 
           x = knowledge_gain_mo2$condition, 
           y = -5,  # Position slightly below the x-axis
           label = paste0(round(knowledge_gain_mo2$Gain, 1), "%"),
           size = 3.5, 
           color = "red")

# Save the plot in the "plots" directory
ggsave("plots/pre_post_mean_pct_mo2_1.png", 
       plot = pre_post_mean_pct_mo2, 
       width = 10, 
       height = 6, 
       dpi = 300)

## Relative Change-
# Create the plot object
plot_relative_change_mo2 <- ggplot(relative_change_combined_mo2, aes(x = percentage, y = condition, fill = rc_positive_negative_code)) +
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
ggsave("plots/plot_relative_change_mo2_ap.png", 
       plot = plot_relative_change_mo2, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Add condition names
relative_change_combined_mo2 <- relative_change_combined_mo2 %>%
  mutate(condition_name = case_when(
    # Add your specific conditions here for each case
    condition == "plscs" ~ "Previous LSCS",
    condition == "hrt" ~ "Heart Disease",
    condition == "tb" ~ "Tuberculosis",
    condition == "fever" ~ "Fever",
    condition == "fgr" ~ "Fetal Growth Restriction",
    condition == "ble" ~ "Bleeding before 20 weeks",
    TRUE ~ condition  # If no condition matches, it stays the same
  ))

# Filter for 'Positive change'
positive_changes_mo2 <- relative_change_combined_mo2 %>% 
  filter(rc_positive_negative_code == "Positive change")

# Find the index of the maximum percentage
max_index_mo2 <- which.max(positive_changes_mo2$percentage)

# Extract the row with the highest percentage
highest_positive_change_mo2 <- positive_changes_mo2[max_index_mo2, ]

# Extract the corresponding condition name
condition_name1_mo2 <- highest_positive_change_mo2$condition_name

# Filter for 'Negative change'
negative_changes_mo2 <- relative_change_combined_mo2 %>% 
  filter(rc_positive_negative_code == "Negative change")

# Find the index of the maximum percentage
max_index_mo2 <- which.max(negative_changes_mo2$percentage)

# Extract the row with the highest percentage
highest_negative_change_mo2 <- negative_changes_mo2[max_index_mo2, ]

# Extract the corresponding condition name
condition_name_neg_mo2 <- highest_negative_change_mo2$condition_name

# Calculate the district-wise mean percentage for pre and post test scores
district_wise_combined_mo2 <- df_mo2 |>
  group_by(district_name) |>
  summarise(mean_pre_test = mean(pre_total_correct, na.rm = TRUE) * 100 / max(pre_total_correct, na.rm = TRUE), 
            mean_post_test = mean(post_total_correct, na.rm = TRUE) * 100 / max(post_total_correct, na.rm = TRUE))

# Calculate Overall Average and add it to the data
overall_avg_mo2 <- df_mo2 |>
  summarise(mean_pre_test = mean(pre_total_correct, na.rm = TRUE) * 100 / max(pre_total_correct, na.rm = TRUE), 
            mean_post_test = mean(post_total_correct, na.rm = TRUE) * 100 / max(post_total_correct, na.rm = TRUE)) |>
  mutate(district_name = "Overall Average")

# Combine the overall average with the district-wise data
district_wise_combined_mo2 <- bind_rows(district_wise_combined_mo2, overall_avg_mo2)

# Reorder district_name factor by post-test mean percentage in descending order
district_wise_combined_mo2 <- district_wise_combined_mo2 |>
  arrange(desc(mean_post_test)) |>
  mutate(district_name = factor(district_name, levels = district_name))

# Reshape the data to long format
district_wise_combined_long_mo2 <- district_wise_combined_mo2 |>
  pivot_longer(cols = starts_with("mean_"), names_to = "test_type", values_to = "mean_percentage")

# Reorder 'test_type' factor levels to ensure 'pre' comes before 'post'
district_wise_combined_long_mo2$test_type <- factor(district_wise_combined_long_mo2$test_type, 
                                                    levels = c("mean_pre_test", "mean_post_test"),
                                                    labels = c("pre", "post"))

# Create a new column for color mapping based on test type and overall average
district_wise_combined_long_mo2 <- district_wise_combined_long_mo2 |>
  mutate(color_group = case_when(
    district_name == "Overall Average" & test_type == "pre" ~ "Overall Average Pre",
    district_name == "Overall Average" & test_type == "post" ~ "Overall Average Post",
    test_type == "pre" ~ "Pre",
    test_type == "post" ~ "Post",
    TRUE ~ "Other"
  ))

# Reorder district names based on "Post" mean percentage in descending order
district_wise_combined_long_mo2 <- district_wise_combined_long_mo2 %>%
  # Create a temporary column to store "Post" mean percentage
  dplyr::mutate(post_percentage = ifelse(color_group == "Post", mean_percentage, NA)) %>%
  # Group by district_name and get the max of post_percentage (for sorting)
  dplyr::group_by(district_name) %>%
  dplyr::mutate(post_percentage_max = max(post_percentage, na.rm = TRUE)) %>%
  # Arrange districts based on the max post_percentage in descending order
  dplyr::arrange(dplyr::desc(post_percentage_max)) %>%
  # Set district_name as a factor in the order of the arranged rows
  dplyr::mutate(district_name = factor(district_name, levels = unique(district_name))) %>%
  # Clean up the temporary columns
  dplyr::select(-post_percentage, -post_percentage_max)

# Set factor levels to ensure 'Pre' comes before 'Post' for each district
district_wise_combined_long_mo2$color_group <- factor(district_wise_combined_long_mo2$color_group, 
                                                      levels = c("Pre", "Post", "Overall Average Pre", "Overall Average Post"))

# Create the ggplot bar chart
districtwise_combined_percentage_plot_mo2 <- ggplot(district_wise_combined_long_mo2, aes(x = district_name, y = mean_percentage, fill = color_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = round(mean_percentage, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +  # Reduce the data label size
  labs(x = "District Name",
       y = "Mean Percentage Score",
       fill = "Test Type") +  # Add legend title
  scale_fill_manual(values = c("Pre" = "coral", "Post" = "cyan3", 
                               "Overall Average Pre" = "purple", "Overall Average Post" = "blue")) +  # Different colors for Overall Average
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )

# Save the plot in the "plots" directory
ggsave("plots/districtwise_combined_percentage_plot_mo2_ap.png", 
       plot = districtwise_combined_percentage_plot_mo2, 
       width = 10, 
       height = 6, 
       dpi = 300)

####Question wise analysis
##PLSCS
# Filter for 'plscs' condition
df_plscs_mo2 <- subset(df_mo2, condition == "plscs")

# Calculate total_n for the filtered data
total_n_mo2 <- nrow(df_plscs_mo2)

# Create the data frame with pre and post percentages for each question
st3_plscs_mo2 <- data.frame(
  question = paste("Q", 1:10),
  plscs_pre = c(
    (sum(df_plscs_mo2$q1_pre) / total_n_mo2) * 100,
    (sum(df_plscs_mo2$q2_pre) / total_n_mo2) * 100,
    (sum(df_plscs_mo2$q3_pre) / total_n_mo2) * 100,
    (sum(df_plscs_mo2$q4_pre) / total_n_mo2) * 100,
    (sum(df_plscs_mo2$q5_pre) / total_n_mo2) * 100,
    (sum(df_plscs_mo2$q6_pre) / total_n_mo2) * 100,
    (sum(df_plscs_mo2$q7_pre) / total_n_mo2) * 100,
    (sum(df_plscs_mo2$q8_pre) / total_n_mo2) * 100,
    (sum(df_plscs_mo2$q9_pre) / total_n_mo2) * 100,
    (sum(df_plscs_mo2$q10_pre) / total_n_mo2) * 100
  ),
  plscs_post = c(
    (sum(df_plscs_mo2$q1_post) / total_n_mo2) * 100,
    (sum(df_plscs_mo2$q2_post) / total_n_mo2) * 100,
    (sum(df_plscs_mo2$q3_post) / total_n_mo2) * 100,
    (sum(df_plscs_mo2$q4_post) / total_n_mo2) * 100,
    (sum(df_plscs_mo2$q5_post) / total_n_mo2) * 100,
    (sum(df_plscs_mo2$q6_post) / total_n_mo2) * 100,
    (sum(df_plscs_mo2$q7_post) / total_n_mo2) * 100,
    (sum(df_plscs_mo2$q8_post) / total_n_mo2) * 100,
    (sum(df_plscs_mo2$q9_post) / total_n_mo2) * 100,
    (sum(df_plscs_mo2$q10_post) / total_n_mo2) * 100
  )
)

##HRT
# Filter for 'hrt' condition
df_hrt_mo2 <- subset(df_mo2, condition == "hrt")

# Calculate total_n for the filtered data
total_n_mo2 <- nrow(df_hrt_mo2)

# Create the data frame with pre and post percentages for each question
st3_hrt_mo2 <- data.frame(
  question = paste("Q", 1:10),
  hrt_pre = c(
    (sum(df_hrt_mo2$q1_pre) / total_n_mo2) * 100,
    (sum(df_hrt_mo2$q2_pre) / total_n_mo2) * 100,
    (sum(df_hrt_mo2$q3_pre) / total_n_mo2) * 100,
    (sum(df_hrt_mo2$q4_pre) / total_n_mo2) * 100,
    (sum(df_hrt_mo2$q5_pre) / total_n_mo2) * 100,
    (sum(df_hrt_mo2$q6_pre) / total_n_mo2) * 100,
    (sum(df_hrt_mo2$q7_pre) / total_n_mo2) * 100,
    (sum(df_hrt_mo2$q8_pre) / total_n_mo2) * 100,
    (sum(df_hrt_mo2$q9_pre) / total_n_mo2) * 100,
    (sum(df_hrt_mo2$q10_pre) / total_n_mo2) * 100
  ),
  hrt_post = c(
    (sum(df_hrt_mo2$q1_post) / total_n_mo2) * 100,
    (sum(df_hrt_mo2$q2_post) / total_n_mo2) * 100,
    (sum(df_hrt_mo2$q3_post) / total_n_mo2) * 100,
    (sum(df_hrt_mo2$q4_post) / total_n_mo2) * 100,
    (sum(df_hrt_mo2$q5_post) / total_n_mo2) * 100,
    (sum(df_hrt_mo2$q6_post) / total_n_mo2) * 100,
    (sum(df_hrt_mo2$q7_post) / total_n_mo2) * 100,
    (sum(df_hrt_mo2$q8_post) / total_n_mo2) * 100,
    (sum(df_hrt_mo2$q9_post) / total_n_mo2) * 100,
    (sum(df_hrt_mo2$q10_post) / total_n_mo2) * 100
  )
)

##TB
# Filter for 'tb' condition
df_tb_mo2 <- subset(df_mo2, condition == "tb")

# Calculate total_n for the filtered data
total_n_mo2 <- nrow(df_tb_mo2)

# Create the data frame with pre and post percentages for each question
st3_tb_mo2 <- data.frame(
  question = paste("Q", 1:10),
  tb_pre = c(
    (sum(df_tb_mo2$q1_pre) / total_n_mo2) * 100,
    (sum(df_tb_mo2$q2_pre) / total_n_mo2) * 100,
    (sum(df_tb_mo2$q3_pre) / total_n_mo2) * 100,
    (sum(df_tb_mo2$q4_pre) / total_n_mo2) * 100,
    (sum(df_tb_mo2$q5_pre) / total_n_mo2) * 100,
    (sum(df_tb_mo2$q6_pre) / total_n_mo2) * 100,
    (sum(df_tb_mo2$q7_pre) / total_n_mo2) * 100,
    (sum(df_tb_mo2$q8_pre) / total_n_mo2) * 100,
    (sum(df_tb_mo2$q9_pre) / total_n_mo2) * 100,
    (sum(df_tb_mo2$q10_pre) / total_n_mo2) * 100
  ),
  tb_post = c(
    (sum(df_tb_mo2$q1_post) / total_n_mo2) * 100,
    (sum(df_tb_mo2$q2_post) / total_n_mo2) * 100,
    (sum(df_tb_mo2$q3_post) / total_n_mo2) * 100,
    (sum(df_tb_mo2$q4_post) / total_n_mo2) * 100,
    (sum(df_tb_mo2$q5_post) / total_n_mo2) * 100,
    (sum(df_tb_mo2$q6_post) / total_n_mo2) * 100,
    (sum(df_tb_mo2$q7_post) / total_n_mo2) * 100,
    (sum(df_tb_mo2$q8_post) / total_n_mo2) * 100,
    (sum(df_tb_mo2$q9_post) / total_n_mo2) * 100,
    (sum(df_tb_mo2$q10_post) / total_n_mo2) * 100
  )
)

##Fever-
# Filter for 'fever' condition
df_fever_mo2 <- subset(df_mo2, condition == "fever")

# Calculate total_n for the filtered data
total_n_mo2 <- nrow(df_fever_mo2)

# Create the data frame with pre and post percentages for each question
st3_fever_mo2 <- data.frame(
  question = paste("Q", 1:10),
  fever_pre = c(
    (sum(df_fever_mo2$q1_pre) / total_n_mo2) * 100,
    (sum(df_fever_mo2$q2_pre) / total_n_mo2) * 100,
    (sum(df_fever_mo2$q3_pre) / total_n_mo2) * 100,
    (sum(df_fever_mo2$q4_pre) / total_n_mo2) * 100,
    (sum(df_fever_mo2$q5_pre) / total_n_mo2) * 100,
    (sum(df_fever_mo2$q6_pre) / total_n_mo2) * 100,
    (sum(df_fever_mo2$q7_pre) / total_n_mo2) * 100,
    (sum(df_fever_mo2$q8_pre) / total_n_mo2) * 100,
    (sum(df_fever_mo2$q9_pre) / total_n_mo2) * 100,
    (sum(df_fever_mo2$q10_pre) / total_n_mo2) * 100
  ),
  fever_post = c(
    (sum(df_fever_mo2$q1_post) / total_n_mo2) * 100,
    (sum(df_fever_mo2$q2_post) / total_n_mo2) * 100,
    (sum(df_fever_mo2$q3_post) / total_n_mo2) * 100,
    (sum(df_fever_mo2$q4_post) / total_n_mo2) * 100,
    (sum(df_fever_mo2$q5_post) / total_n_mo2) * 100,
    (sum(df_fever_mo2$q6_post) / total_n_mo2) * 100,
    (sum(df_fever_mo2$q7_post) / total_n_mo2) * 100,
    (sum(df_fever_mo2$q8_post) / total_n_mo2) * 100,
    (sum(df_fever_mo2$q9_post) / total_n_mo2) * 100,
    (sum(df_fever_mo2$q10_post) / total_n_mo2) * 100
  )
)

##FGR-
# Filter for 'fgr' condition
df_fgr_mo2 <- subset(df_mo2, condition == "fgr")

# Calculate total_n for the filtered data
total_n_mo2 <- nrow(df_fgr_mo2)

# Create the data frame with pre and post percentages for each question
st3_fgr_mo2 <- data.frame(
  question = paste("Q", 1:10),
  fgr_pre = c(
    (sum(df_fgr_mo2$q1_pre) / total_n_mo2) * 100,
    (sum(df_fgr_mo2$q2_pre) / total_n_mo2) * 100,
    (sum(df_fgr_mo2$q3_pre) / total_n_mo2) * 100,
    (sum(df_fgr_mo2$q4_pre) / total_n_mo2) * 100,
    (sum(df_fgr_mo2$q5_pre) / total_n_mo2) * 100,
    (sum(df_fgr_mo2$q6_pre) / total_n_mo2) * 100,
    (sum(df_fgr_mo2$q7_pre) / total_n_mo2) * 100,
    (sum(df_fgr_mo2$q8_pre) / total_n_mo2) * 100,
    (sum(df_fgr_mo2$q9_pre) / total_n_mo2) * 100,
    (sum(df_fgr_mo2$q10_pre) / total_n_mo2) * 100
  ),
  fgr_post = c(
    (sum(df_fgr_mo2$q1_post) / total_n_mo2) * 100,
    (sum(df_fgr_mo2$q2_post) / total_n_mo2) * 100,
    (sum(df_fgr_mo2$q3_post) / total_n_mo2) * 100,
    (sum(df_fgr_mo2$q4_post) / total_n_mo2) * 100,
    (sum(df_fgr_mo2$q5_post) / total_n_mo2) * 100,
    (sum(df_fgr_mo2$q6_post) / total_n_mo2) * 100,
    (sum(df_fgr_mo2$q7_post) / total_n_mo2) * 100,
    (sum(df_fgr_mo2$q8_post) / total_n_mo2) * 100,
    (sum(df_fgr_mo2$q9_post) / total_n_mo2) * 100,
    (sum(df_fgr_mo2$q10_post) / total_n_mo2) * 100
  )
)

##Bleeding before 20 weeks-
# Filter for 'ble' condition
df_ble_mo2 <- subset(df_mo2, condition == "ble")

# Calculate total_n for the filtered data
total_n_mo2 <- nrow(df_ble_mo2)

# Create the data frame with pre and post percentages for each question
st3_ble_mo2 <- data.frame(
  question = paste("Q", 1:10),
  ble_pre = c(
    (sum(df_ble_mo2$q1_pre) / total_n_mo2) * 100,
    (sum(df_ble_mo2$q2_pre) / total_n_mo2) * 100,
    (sum(df_ble_mo2$q3_pre) / total_n_mo2) * 100,
    (sum(df_ble_mo2$q4_pre) / total_n_mo2) * 100,
    (sum(df_ble_mo2$q5_pre) / total_n_mo2) * 100,
    (sum(df_ble_mo2$q6_pre) / total_n_mo2) * 100,
    (sum(df_ble_mo2$q7_pre) / total_n_mo2) * 100,
    (sum(df_ble_mo2$q8_pre) / total_n_mo2) * 100,
    (sum(df_ble_mo2$q9_pre) / total_n_mo2) * 100,
    (sum(df_ble_mo2$q10_pre) / total_n_mo2) * 100
  ),
  ble_post = c(
    (sum(df_ble_mo2$q1_post) / total_n_mo2) * 100,
    (sum(df_ble_mo2$q2_post) / total_n_mo2) * 100,
    (sum(df_ble_mo2$q3_post) / total_n_mo2) * 100,
    (sum(df_ble_mo2$q4_post) / total_n_mo2) * 100,
    (sum(df_ble_mo2$q5_post) / total_n_mo2) * 100,
    (sum(df_ble_mo2$q6_post) / total_n_mo2) * 100,
    (sum(df_ble_mo2$q7_post) / total_n_mo2) * 100,
    (sum(df_ble_mo2$q8_post) / total_n_mo2) * 100,
    (sum(df_ble_mo2$q9_post) / total_n_mo2) * 100,
    (sum(df_ble_mo2$q10_post) / total_n_mo2) * 100
  )
)

# Combine the data frames using cbind
df_questions_combined_mo2 <- cbind(
  st3_plscs_mo2[, c("question", "plscs_pre", "plscs_post")],
  st3_hrt_mo2[, c("hrt_pre", "hrt_post")],
  st3_tb_mo2[, c("tb_pre", "tb_post")],
  st3_fever_mo2[, c("fever_pre", "fever_post")],
  st3_fgr_mo2[, c("fgr_pre", "fgr_post")],
  st3_ble_mo2[, c("ble_pre", "ble_post")]
)

# Round all percentage columns to 1 decimal place
df_questions_combined_mo2[, 2:ncol(df_questions_combined_mo2)] <- round(df_questions_combined_mo2[, 2:ncol(df_questions_combined_mo2)], 1)

# Create a gt table and apply some custom formatting
df_questions_combined_mo2_gt <- df_questions_combined_mo2 %>%
  gt() %>%
  cols_label(
    question = "Question",
    plscs_pre = "PLSCS Pre",
    plscs_post = "PLSCS Post",
    hrt_pre = "HRT Pre",
    hrt_post = "HRT Post",
    tb_pre = "TB Pre",
    tb_post = "TB Post",
    fever_pre = "Fever Pre",
    fever_post = "Fever Post",
    fgr_pre = "FGR Pre",
    fgr_post = "FGR Post",
    ble_pre = "BLE Pre",
    ble_post = "BLE Post"
  ) %>%
  tab_spanner(
    label = "Pre-Post Score Percentage ",
    columns = c("plscs_pre", "plscs_post", "hrt_pre", "hrt_post", 
                "tb_pre", "tb_post", "fever_pre", "fever_post", 
                "fgr_pre", "fgr_post", "ble_pre", "ble_post")
  ) 

# Apply tab_style() to highlight cells with values < 50
for (col in c("plscs_pre", "plscs_post", "hrt_pre", "hrt_post", 
              "tb_pre", "tb_post", "fever_pre", "fever_post", 
              "fgr_pre", "fgr_post", "ble_pre", "ble_post")) {
  df_questions_combined_mo2_gt <- df_questions_combined_mo2_gt %>%
    tab_style(
      style = list(
        cell_fill(color = "yellow"),  # Highlight only the specific cells
        cell_text(weight = "bold")    # Make text bold
      ),
      locations = cells_body(
        columns = col, 
        rows = df_questions_combined_mo2[[col]] < 50  # Apply condition per column
      )
    )
}

# Save the data frame to a CSV file
write.csv(df_questions_combined_mo2_gt, "tables/df_questions_combined_mo2_ap_gt.csv", row.names = FALSE)