library(dplyr)
library(tidyr)
library(mlogit)

# Create unique choice occasions for every visit
data_rum <- final_data_2018 %>%
  filter(VisitCount > 0) %>%
  uncount(VisitCount) %>%
  group_by(id) %>%
  mutate(choice_id = paste0(id, "_", row_number())) %>%
  ungroup() %>%
  select(choice_id, id, chosen_park = parkname) %>%
  expand_grid(parkname = unique(final_data_2018$parkname)) %>%
  mutate(choice = (chosen_park == parkname)) %>%
  left_join(final_data_2018 %>% select(id, parkname, price, travel_distance_km,income), 
            by = c("id", "parkname"))

# Create Stay at Home

# Create the Stay-at-Home option for every choice occasion
outside_home <- data_rum %>%
  distinct(choice_id, id) %>%
  mutate(
    parkname = "Stay_Home",
    choice = FALSE,      
    price = 0,
    travel_distance_km = 0
  )

# Combine with your existing data
data_rum<- bind_rows(data_rum, outside_home)

# Re-format for mlogit
df_rum <- dfidx(data_rum, 
                        idx = list("choice_id", "parkname"), 
                        shape = "long")

# Simple Multinomial Logit
rum_mle <- mlogit(choice ~ price  | 1, data = df_rum)
summary(rum_mle)



# Calculate the 'Logsum' (Total Utility) for the status quo
logsum_base <- logsum(rum_mle)

# 1. Convert df_rum to a standard data frame for our math
df_math <- as.data.frame(df_rum)

# 2. Extract the IDs and Park names from the 'idx' column
# idx(df_rum, 1) gets the 'choice_id'
# idx(df_rum, 2) gets the 'parkname'
df_math$trip_id   <- as.character(idx(df_rum, 1))
df_math$park_name <- as.character(idx(df_rum, 2))

# 3. Calculate the Systematic Utility (V)
# This uses the model coefficients and the data matrix
df_math$V <- as.numeric(model.matrix(rum_mle) %*% coef(rum_mle))

# 4. Double check the columns are there
head(df_math[, c("trip_id", "park_name", "V")])

calc_rum_welfare_manual <- function(target_park) {
  
  beta_price <- coef(rum_mle)["price"]
  
  # 1. Baseline Logsum
  ls_base <- df_math %>%
    group_by(trip_id) %>%
    summarise(ls_base = log(sum(exp(V))), .groups = "drop")
  
  # 2. Policy Logsum (Removing the park)
  ls_policy <- df_math %>%
    filter(park_name != target_park) %>%
    group_by(trip_id) %>%
    summarise(ls_policy = log(sum(exp(V))), .groups = "drop")
  
  # 3. Calculate CV for EVERY individual trip
  comparison <- left_join(ls_base, ls_policy, by = "trip_id") %>%
    mutate(cv = (ls_policy - ls_base) / -beta_price)
  
  # 4. Calculate Statistics across all trips
  # We use the standard error of the mean for the CIs
  res <- comparison %>%
    summarise(
      mean_cv_rum = mean(cv, na.rm = TRUE),
      std_dev_rum = sd(cv, na.rm = TRUE),
      n = n(),
      # Standard Error = SD / sqrt(n)
      se_rum = std_dev_rum / sqrt(n),
      # 95% CI calculation (Mean +/- 1.96 * SE)
      ci_lo_2.5_rum = mean_cv_rum - (1.96 * se_rum),
      ci_hi_97.5_rum = mean_cv_rum + (1.96 * se_rum)
    )
  
  return(res)
}

# 6. Test it
welfare_rum_peacock <- calc_rum_welfare_manual("Peacock Flats Campsite")
print(welfare_rum_peacock)


# Get the unique list of parks from your data
# We exclude "Stay_Home" because we aren't "closing" the option to stay home
all_parks <- unique(df_math$park_name)
all_parks <- all_parks[all_parks != "Stay_Home"]

library(purrr)

# 1. Define the list of parks
all_parks <- unique(df_math$park_name)
all_parks <- all_parks[all_parks != "Stay_Home"]

# 2. Run the function for all parks and combine results
# This replaces your 'for' loop entirely
rum_results_2018 <- map_df(all_parks, function(p) {
  
  # Run your function (the one that returns mean, sd, ci)
  stats <- calc_rum_welfare_manual(p)
  
  # Add the park name so we know which row is which
  stats$parkname <- p
  
  return(stats)
})

# 3. Reorder columns to put parkname first
rum_results_2018 <- rum_results_2018 %>% 
  select(parkname, mean_cv_rum, std_dev_rum, ci_lo_2.5_rum, ci_hi_97.5_rum, n)

print(rum_results_2018)

rum_results_2018 <- rum_results_2018 %>% 
  select(parkname, mean_cv_rum, std_dev_rum, ci_lo_2.5_rum, ci_hi_97.5_rum, n) %>%
  mutate(
    # Use mean_cv_rum because that is what your function produced
    rum_total_annual_loss = mean_cv_rum * n
  )

library(dplyr)
library(stringr)
park_number <- final_data_2018 %>%
  # Select only the two columns you need
  select(parkname, parkname_numeric) %>%
  # Keep only the unique combinations
  distinct() %>%
  # Sort them by the numeric ID for easier reading
  arrange(parkname_numeric)

welfare_summary=summary(welfare_2018)

welfare_summary=welfare_summary$CoefTable
welfare_summary=welfare_summary %>%
  mutate(
    # Extract only the digits from the "policy" string and make it an integer
    parkname_numeric = as.integer(str_extract(policy, "\\d+"))
  ) %>%
  # 2. Merge with your park_number key (created in the previous step)
  left_join(park_number, by = "parkname_numeric") %>%
  # 3. Clean up: Move parkname to the front and remove the "policy" column
  select(parkname, parkname_numeric, mean, std.dev, everything(), -policy)


# Comparsion

final_comparison_2018 <- rum_results_2018 %>%
  select(parkname, rum_mean_per_trip=mean_cv_rum, std_dev_rum, ci_lo_2.5_rum, ci_hi_97.5_rum, rum_total_annual_loss) %>%
  left_join(welfare_summary, by = "parkname") %>%
  rename(mdcev_mean_per_person = mean) %>%
  mutate(
    # 1. RUM Annual = (Mean per Trip) * (Total Number of Trips)
    rum_annual_total = rum_mean_per_trip * sum(final_data_2019$VisitCount),
    
    # 2. MDCEV Annual = (Mean per Person) * (Number of Unique People)
    # We use n_distinct(id) to get exactly 1,380
    mdcev_annual_total = mdcev_mean_per_person * n_distinct(final_data_2019$id),
    
    # 3. The Gap
    difference = rum_annual_total - mdcev_annual_total
  )




### 2019


# Create unique choice occasions for every visit
data_rum <- final_data_2019 %>%
  filter(VisitCount > 0) %>%
  uncount(VisitCount) %>%
  group_by(id) %>%
  mutate(choice_id = paste0(id, "_", row_number())) %>%
  ungroup() %>%
  select(choice_id, id, chosen_park = parkname) %>%
  expand_grid(parkname = unique(final_data_2019$parkname)) %>%
  mutate(choice = (chosen_park == parkname)) %>%
  left_join(final_data_2019 %>% select(id, parkname, price, travel_distance_km), 
            by = c("id", "parkname"))

# Create Stay at Home

# Create the Stay-at-Home option for every choice occasion
outside_home <- data_rum %>%
  distinct(choice_id, id) %>%
  mutate(
    parkname = "Stay_Home",
    choice = FALSE,      
    price = 0,
    travel_distance_km = 0
  )

# Combine with existing data
data_rum<- bind_rows(data_rum, outside_home)

# Re-format for mlogit
df_rum <- dfidx(data_rum, 
                idx = list("choice_id", "parkname"), 
                shape = "long")

# Simple Multinomial Logit
rum_mle <- mlogit(choice ~ price  | 1, data = df_rum)
summary(rum_mle)



# Calculate the 'Logsum' (Total Utility) for the status quo
logsum_base <- logsum(rum_mle)

# 1. Convert df_rum to a standard data frame for our math
df_math <- as.data.frame(df_rum)

# 2. Extract the IDs and Park names from the 'idx' column
# idx(df_rum, 1) gets the 'choice_id'
# idx(df_rum, 2) gets the 'parkname'
df_math$trip_id   <- as.character(idx(df_rum, 1))
df_math$park_name <- as.character(idx(df_rum, 2))

# 3. Calculate the Systematic Utility (V)
# This uses the model coefficients and the data matrix
df_math$V <- as.numeric(model.matrix(rum_mle) %*% coef(rum_mle))

# 4. Double check the columns are there
head(df_math[, c("trip_id", "park_name", "V")])


# Get the unique list of parks from your data
# We exclude "Stay_Home" because we aren't "closing" the option to stay home
all_parks <- unique(df_math$park_name)
all_parks <- all_parks[all_parks != "Stay_Home"]

# 2. Run the function for all parks and combine results
# This replaces your 'for' loop entirely
rum_results_2019 <- map_df(all_parks, function(p) {
  
  # Run your function (the one that returns mean, sd, ci)
  stats <- calc_rum_welfare_manual(p)
  
  # Add the park name so we know which row is which
  stats$parkname <- p
  
  return(stats)
})

# 3. Reorder columns to put parkname first
rum_results_2019 <- rum_results_2019 %>% 
  select(parkname, mean_cv_rum, std_dev_rum, ci_lo_2.5_rum, ci_hi_97.5_rum, n)

print(rum_results_2019)

rum_results_2019 <- rum_results_2019 %>% 
  select(parkname, mean_cv_rum, std_dev_rum, ci_lo_2.5_rum, ci_hi_97.5_rum, n) %>%
  mutate(
    # Use mean_cv_rum because that is what your function produced
    rum_total_annual_loss = mean_cv_rum * n
  )

library(dplyr)
library(stringr)
park_number <- final_data_2019 %>%
  # Select only the two columns you need
  select(parkname, parkname_numeric) %>%
  # Keep only the unique combinations
  distinct() %>%
  # Sort them by the numeric ID for easier reading
  arrange(parkname_numeric)

welfare_summary=summary(welfare_2019)

welfare_summary=welfare_summary$CoefTable
welfare_summary=welfare_summary %>%
  mutate(
    # Extract only the digits from the "policy" string and make it an integer
    parkname_numeric = as.integer(str_extract(policy, "\\d+"))
  ) %>%
  # 2. Merge with your park_number key (created in the previous step)
  left_join(park_number, by = "parkname_numeric") %>%
  # 3. Clean up: Move parkname to the front and remove the "policy" column
  select(parkname, parkname_numeric, mean, std.dev, everything(), -policy)




# Comparsion

final_comparison_2019 <- rum_results_2019 %>%
  select(parkname, rum_mean_per_trip=mean_cv_rum, std_dev_rum, ci_lo_2.5_rum, ci_hi_97.5_rum, rum_total_annual_loss) %>%
  left_join(welfare_summary, by = "parkname") %>%
  rename(mdcev_mean_per_person = mean) %>%
  mutate(
    # 1. RUM Annual = (Mean per Trip) * (Total Number of Trips)
    rum_annual_total = rum_mean_per_trip * sum(final_data_2019$VisitCount),
    
    # 2. MDCEV Annual = (Mean per Person) * (Number of Unique People)
    # We use n_distinct(id) to get exactly 1,380
    mdcev_annual_total = mdcev_mean_per_person * n_distinct(final_data_2019$id),
    
    # 3. The Gap
    difference = rum_annual_total - mdcev_annual_total
  )


### 2020


# Create unique choice occasions for every visit
data_rum <- final_data_2020 %>%
  filter(VisitCount > 0) %>%
  uncount(VisitCount) %>%
  group_by(id) %>%
  mutate(choice_id = paste0(id, "_", row_number())) %>%
  ungroup() %>%
  select(choice_id, id, chosen_park = parkname) %>%
  expand_grid(parkname = unique(final_data_2020$parkname)) %>%
  mutate(choice = (chosen_park == parkname)) %>%
  left_join(final_data_2020 %>% select(id, parkname, price, travel_distance_km), 
            by = c("id", "parkname"))

# Create Stay at Home

# Create the Stay-at-Home option for every choice occasion
outside_home <- data_rum %>%
  distinct(choice_id, id) %>%
  mutate(
    parkname = "Stay_Home",
    choice = FALSE,      
    price = 0,
    travel_distance_km = 0
  )

# Combine with existing data
data_rum<- bind_rows(data_rum, outside_home)

# Re-format for mlogit
df_rum <- dfidx(data_rum, 
                idx = list("choice_id", "parkname"), 
                shape = "long")

# Simple Multinomial Logit
rum_mle <- mlogit(choice ~ price  | 1, data = df_rum)
summary(rum_mle)



# Calculate the 'Logsum' (Total Utility) for the status quo
logsum_base <- logsum(rum_mle)

# 1. Convert df_rum to a standard data frame for our math
df_math <- as.data.frame(df_rum)

# 2. Extract the IDs and Park names from the 'idx' column
# idx(df_rum, 1) gets the 'choice_id'
# idx(df_rum, 2) gets the 'parkname'
df_math$trip_id   <- as.character(idx(df_rum, 1))
df_math$park_name <- as.character(idx(df_rum, 2))

# 3. Calculate the Systematic Utility (V)
# This uses the model coefficients and the data matrix
df_math$V <- as.numeric(model.matrix(rum_mle) %*% coef(rum_mle))

# 4. Double check the columns are there
head(df_math[, c("trip_id", "park_name", "V")])


# Get the unique list of parks from your data
# We exclude "Stay_Home" because we aren't "closing" the option to stay home
all_parks <- unique(df_math$park_name)
all_parks <- all_parks[all_parks != "Stay_Home"]

# Create a data frame to store the results

# Get the unique list of parks from your data
# We exclude "Stay_Home" because we aren't "closing" the option to stay home
all_parks <- unique(df_math$park_name)
all_parks <- all_parks[all_parks != "Stay_Home"]

# Run the function for all parks and combine results
# This replaces your 'for' loop entirely
rum_results_2020 <- map_df(all_parks, function(p) {
  
  # Run your function (the one that returns mean, sd, ci)
  stats <- calc_rum_welfare_manual(p)
  
  # Add the park name so we know which row is which
  stats$parkname <- p
  
  return(stats)
})

# 3. Reorder columns to put parkname first
rum_results_2020 <- rum_results_2020 %>% 
  select(parkname, mean_cv_rum, std_dev_rum, ci_lo_2.5_rum, ci_hi_97.5_rum, n)

print(rum_results_2020)

rum_results_2020<- rum_results_2020 %>% 
  select(parkname, mean_cv_rum, std_dev_rum, ci_lo_2.5_rum, ci_hi_97.5_rum, n) %>%
  mutate(
    # Use mean_cv_rum because that is what your function produced
    rum_total_annual_loss = mean_cv_rum * n
  )

park_number <- final_data_2020 %>%
  # Select only the two columns you need
  select(parkname, parkname_numeric) %>%
  # Keep only the unique combinations
  distinct() %>%
  # Sort them by the numeric ID for easier reading
  arrange(parkname_numeric)

welfare_summary=summary(welfare_2020)

welfare_summary=welfare_summary$CoefTable
welfare_summary=welfare_summary %>%
  mutate(
    # Extract only the digits from the "policy" string and make it an integer
    parkname_numeric = as.integer(str_extract(policy, "\\d+"))
  ) %>%
  # 2. Merge with your park_number key (created in the previous step)
  left_join(park_number, by = "parkname_numeric") %>%
  # 3. Clean up: Move parkname to the front and remove the "policy" column
  select(parkname, parkname_numeric, mean, std.dev, everything(), -policy)


#  Join with RUM
final_comparison_2020 <- rum_results_2020 %>%
  select(parkname, rum_mean_per_trip=mean_cv_rum, std_dev_rum, ci_lo_2.5_rum, ci_hi_97.5_rum, rum_total_annual_loss) %>%
  left_join(welfare_summary, by = "parkname") %>%
  rename(mdcev_mean_per_person = mean) %>%
  mutate(
    # 1. RUM Annual = (Mean per Trip) * (Total Number of Trips)
    rum_annual_total = rum_mean_per_trip * sum(final_data_2019$VisitCount),
    
    # 2. MDCEV Annual = (Mean per Person) * (Number of Unique People)
    # We use n_distinct(id) to get exactly 1,380
    mdcev_annual_total = mdcev_mean_per_person * n_distinct(final_data_2019$id),
    
    # 3. The Gap
    difference = rum_annual_total - mdcev_annual_total
  )

### 2021


# Create unique choice occasions for every visit
data_rum <- final_data_2021 %>%
  filter(VisitCount > 0) %>%
  uncount(VisitCount) %>%
  group_by(id) %>%
  mutate(choice_id = paste0(id, "_", row_number())) %>%
  ungroup() %>%
  select(choice_id, id, chosen_park = parkname) %>%
  expand_grid(parkname = unique(final_data_2021$parkname)) %>%
  mutate(choice = (chosen_park == parkname)) %>%
  left_join(final_data_2021 %>% select(id, parkname, price, travel_distance_km), 
            by = c("id", "parkname"))

# Create Stay at Home

# Create the Stay-at-Home option for every choice occasion
outside_home <- data_rum %>%
  distinct(choice_id, id) %>%
  mutate(
    parkname = "Stay_Home",
    choice = FALSE,      
    price = 0,
    travel_distance_km = 0
  )

# Combine with existing data
data_rum<- bind_rows(data_rum, outside_home)

# Re-format for mlogit
df_rum <- dfidx(data_rum, 
                idx = list("choice_id", "parkname"), 
                shape = "long")

# Simple Multinomial Logit
rum_mle <- mlogit(choice ~ price  | 1, data = df_rum)
summary(rum_mle)



# Calculate the 'Logsum' (Total Utility) for the status quo
logsum_base <- logsum(rum_mle)

# 1. Convert df_rum to a standard data frame for our math
df_math <- as.data.frame(df_rum)

# 2. Extract the IDs and Park names from the 'idx' column
# idx(df_rum, 1) gets the 'choice_id'
# idx(df_rum, 2) gets the 'parkname'
df_math$trip_id   <- as.character(idx(df_rum, 1))
df_math$park_name <- as.character(idx(df_rum, 2))

# Calculate the Systematic Utility (V)
# This uses the model coefficients and the data matrix
df_math$V <- as.numeric(model.matrix(rum_mle) %*% coef(rum_mle))

# Double check the columns are there
head(df_math[, c("trip_id", "park_name", "V")])



# Get the unique list of parks from your data
# We exclude "Stay_Home" because we aren't "closing" the option to stay home
all_parks <- unique(df_math$park_name)
all_parks <- all_parks[all_parks != "Stay_Home"]


# Run the function for all parks and combine results
# This replaces your 'for' loop entirely
rum_results_2021 <- map_df(all_parks, function(p) {
  
  # Run your function (the one that returns mean, sd, ci)
  stats <- calc_rum_welfare_manual(p)
  
  # Add the park name so we know which row is which
  stats$parkname <- p
  
  return(stats)
})

# 3. Reorder columns to put parkname first
rum_results_2021 <- rum_results_2021 %>% 
  select(parkname, mean_cv_rum, std_dev_rum, ci_lo_2.5_rum, ci_hi_97.5_rum, n)

print(rum_results_2021)

rum_results_2021<- rum_results_2021 %>% 
  select(parkname, mean_cv_rum, std_dev_rum, ci_lo_2.5_rum, ci_hi_97.5_rum, n) %>%
  mutate(
    # Use mean_cv_rum because that is what your function produced
    rum_total_annual_loss = mean_cv_rum * n
  )

park_number <- final_data_2021 %>%
  # Select only the two columns you need
  select(parkname, parkname_numeric) %>%
  # Keep only the unique combinations
  distinct() %>%
  # Sort them by the numeric ID for easier reading
  arrange(parkname_numeric)

welfare_summary=summary(welfare_2021)

welfare_summary=welfare_summary$CoefTable
welfare_summary=welfare_summary %>%
  mutate(
    # Extract only the digits from the "policy" string and make it an integer
    parkname_numeric = as.integer(str_extract(policy, "\\d+"))
  ) %>%
  # 2. Merge with your park_number key (created in the previous step)
  left_join(park_number, by = "parkname_numeric") %>%
  # 3. Clean up: Move parkname to the front and remove the "policy" column
  select(parkname, parkname_numeric, mean, std.dev, everything(), -policy)

#  Join with RUM

final_comparison_2021 <- rum_results_2021 %>%
  select(parkname, rum_mean_per_trip=mean_cv_rum, std_dev_rum, ci_lo_2.5_rum, ci_hi_97.5_rum, rum_total_annual_loss) %>%
  left_join(welfare_summary, by = "parkname") %>%
  rename(mdcev_mean_per_person = mean) %>%
  mutate(
    # 1. RUM Annual = (Mean per Trip) * (Total Number of Trips)
    rum_annual_total = rum_mean_per_trip * sum(final_data_2019$VisitCount),
    
    # 2. MDCEV Annual = (Mean per Person) * (Number of Unique People)
    # We use n_distinct(id) to get exactly 1,380
    mdcev_annual_total = mdcev_mean_per_person * n_distinct(final_data_2019$id),
    
    # 3. The Gap
    difference = rum_annual_total - mdcev_annual_total
  )


### 2022


# Create unique choice occasions for every visit
data_rum <- final_data_2022 %>%
  filter(VisitCount > 0) %>%
  uncount(VisitCount) %>%
  group_by(id) %>%
  mutate(choice_id = paste0(id, "_", row_number())) %>%
  ungroup() %>%
  select(choice_id, id, chosen_park = parkname) %>%
  expand_grid(parkname = unique(final_data_2022$parkname)) %>%
  mutate(choice = (chosen_park == parkname)) %>%
  left_join(final_data_2022 %>% select(id, parkname, price, travel_distance_km), 
            by = c("id", "parkname"))

# Create Stay at Home

# Create the Stay-at-Home option for every choice occasion
outside_home <- data_rum %>%
  distinct(choice_id, id) %>%
  mutate(
    parkname = "Stay_Home",
    choice = FALSE,      
    price = 0,
    travel_distance_km = 0
  )

# Combine with existing data
data_rum<- bind_rows(data_rum, outside_home)

# Re-format for mlogit
df_rum <- dfidx(data_rum, 
                idx = list("choice_id", "parkname"), 
                shape = "long")

# Simple Multinomial Logit
rum_mle <- mlogit(choice ~ price  | 1, data = df_rum)
summary(rum_mle)



# Calculate the 'Logsum' (Total Utility) for the status quo
logsum_base <- logsum(rum_mle)

# Convert df_rum to a standard data frame for our math
df_math <- as.data.frame(df_rum)

# Extract the IDs and Park names from the 'idx' column
# idx(df_rum, 1) gets the 'choice_id'
# idx(df_rum, 2) gets the 'parkname'
df_math$trip_id   <- as.character(idx(df_rum, 1))
df_math$park_name <- as.character(idx(df_rum, 2))

#  Calculate the Systematic Utility (V)
# This uses the model coefficients and the data matrix
df_math$V <- as.numeric(model.matrix(rum_mle) %*% coef(rum_mle))

# Double check the columns are there
head(df_math[, c("trip_id", "park_name", "V")])


# Get the unique list of parks from your data
# We exclude "Stay_Home" because we aren't "closing" the option to stay home
all_parks <- unique(df_math$park_name)
all_parks <- all_parks[all_parks != "Stay_Home"]

# 2. Run the function for all parks and combine results
# This replaces your 'for' loop entirely
rum_results_2022 <- map_df(all_parks, function(p) {
  
  # Run your function (the one that returns mean, sd, ci)
  stats <- calc_rum_welfare_manual(p)
  
  # Add the park name so we know which row is which
  stats$parkname <- p
  
  return(stats)
})

# 3. Reorder columns to put parkname first
rum_results_2022 <- rum_results_2022 %>% 
  select(parkname, mean_cv_rum, std_dev_rum, ci_lo_2.5_rum, ci_hi_97.5_rum, n)

print(rum_results_2022)

rum_results_2022<- rum_results_2022 %>% 
  select(parkname, mean_cv_rum, std_dev_rum, ci_lo_2.5_rum, ci_hi_97.5_rum, n) %>%
  mutate(
    # Use mean_cv_rum because that is what your function produced
    rum_total_annual_loss = mean_cv_rum * n
  )

park_number <- final_data_2022 %>%
  # Select only the two columns you need
  select(parkname, parkname_numeric) %>%
  # Keep only the unique combinations
  distinct() %>%
  # Sort them by the numeric ID for easier reading
  arrange(parkname_numeric)

welfare_summary=summary(welfare_2022)

welfare_summary=welfare_summary$CoefTable
welfare_summary=welfare_summary %>%
  mutate(
    # Extract only the digits from the "policy" string and make it an integer
    parkname_numeric = as.integer(str_extract(policy, "\\d+"))
  ) %>%
  # 2. Merge with your park_number key (created in the previous step)
  left_join(park_number, by = "parkname_numeric") %>%
  # 3. Clean up: Move parkname to the front and remove the "policy" column
  select(parkname, parkname_numeric, mean, std.dev, everything(), -policy)


#  Join with RUM
final_comparison_2022 <- rum_results_2022 %>%
  select(parkname, rum_mean_per_trip=mean_cv_rum, std_dev_rum, ci_lo_2.5_rum, ci_hi_97.5_rum, rum_total_annual_loss) %>%
  left_join(welfare_summary, by = "parkname") %>%
  rename(mdcev_mean_per_person = mean) %>%
  mutate(
    # 1. RUM Annual = (Mean per Trip) * (Total Number of Trips)
    rum_annual_total = rum_mean_per_trip * sum(final_data_2019$VisitCount),
    
    # 2. MDCEV Annual = (Mean per Person) * (Number of Unique People)
    # We use n_distinct(id) to get exactly 1,380
    mdcev_annual_total = mdcev_mean_per_person * n_distinct(final_data_2019$id),
    
    # 3. The Gap
    difference = rum_annual_total - mdcev_annual_total
  )

### 2023 

# Create unique choice occasions for every visit
data_rum <- final_data_2023 %>%
  filter(VisitCount > 0) %>%
  uncount(VisitCount) %>%
  group_by(id) %>%
  mutate(choice_id = paste0(id, "_", row_number())) %>%
  ungroup() %>%
  select(choice_id, id, chosen_park = parkname) %>%
  expand_grid(parkname = unique(final_data_2023$parkname)) %>%
  mutate(choice = (chosen_park == parkname)) %>%
  left_join(final_data_2023 %>% select(id, parkname, price, travel_distance_km), 
            by = c("id", "parkname"))

# Create Stay at Home

# Create the Stay-at-Home option for every choice occasion
outside_home <- data_rum %>%
  distinct(choice_id, id) %>%
  mutate(
    parkname = "Stay_Home",
    choice = FALSE,      
    price = 0,
    travel_distance_km = 0
  )

# Combine with existing data
data_rum<- bind_rows(data_rum, outside_home)

# Re-format for mlogit
df_rum <- dfidx(data_rum, 
                idx = list("choice_id", "parkname"), 
                shape = "long")

# Simple Multinomial Logit
rum_mle <- mlogit(choice ~ price  | 1, data = df_rum)
summary(rum_mle)



# Calculate the 'Logsum' (Total Utility) for the status quo
logsum_base <- logsum(rum_mle)

# 1. Convert df_rum to a standard data frame for our math
df_math <- as.data.frame(df_rum)

# 2. Extract the IDs and Park names from the 'idx' column
# idx(df_rum, 1) gets the 'choice_id'
# idx(df_rum, 2) gets the 'parkname'
df_math$trip_id   <- as.character(idx(df_rum, 1))
df_math$park_name <- as.character(idx(df_rum, 2))

# 3. Calculate the Systematic Utility (V)
# This uses the model coefficients and the data matrix
df_math$V <- as.numeric(model.matrix(rum_mle) %*% coef(rum_mle))

# 4. Double check the columns are there
head(df_math[, c("trip_id", "park_name", "V")])


# Get the unique list of parks from your data
# We exclude "Stay_Home" because we aren't "closing" the option to stay home
all_parks <- unique(df_math$park_name)
all_parks <- all_parks[all_parks != "Stay_Home"]

# 2. Run the function for all parks and combine results
# This replaces your 'for' loop entirely
rum_results_2023 <- map_df(all_parks, function(p) {
  
  # Run your function (the one that returns mean, sd, ci)
  stats <- calc_rum_welfare_manual(p)
  
  # Add the park name so we know which row is which
  stats$parkname <- p
  
  return(stats)
})

# 3. Reorder columns to put parkname first
rum_results_2023 <- rum_results_2023 %>% 
  select(parkname, mean_cv_rum, std_dev_rum, ci_lo_2.5_rum, ci_hi_97.5_rum, n)

print(rum_results_2023)

rum_results_2023<- rum_results_2023 %>% 
  select(parkname, mean_cv_rum, std_dev_rum, ci_lo_2.5_rum, ci_hi_97.5_rum, n) %>%
  mutate(
    # Use mean_cv_rum because that is what your function produced
    rum_total_annual_loss = mean_cv_rum * n
  )

park_number <- final_data_2023 %>%
  # Select only the two columns you need
  select(parkname, parkname_numeric) %>%
  # Keep only the unique combinations
  distinct() %>%
  # Sort them by the numeric ID for easier reading
  arrange(parkname_numeric)

welfare_summary=summary(welfare_2023)

welfare_summary=welfare_summary$CoefTable
welfare_summary=welfare_summary %>%
  mutate(
    # Extract only the digits from the "policy" string and make it an integer
    parkname_numeric = as.integer(str_extract(policy, "\\d+"))
  ) %>%
  # 2. Merge with your park_number key (created in the previous step)
  left_join(park_number, by = "parkname_numeric") %>%
  # 3. Clean up: Move parkname to the front and remove the "policy" column
  select(parkname, parkname_numeric, mean, std.dev, everything(), -policy)



#  Join with RUM

  final_comparison_2023 <- rum_results_2023 %>%
  select(parkname, rum_mean_per_trip=mean_cv_rum, std_dev_rum, ci_lo_2.5_rum, ci_hi_97.5_rum, rum_total_annual_loss) %>%
  left_join(welfare_summary, by = "parkname") %>%
  rename(mdcev_mean_per_person = mean) %>%
  mutate(
    # 1. RUM Annual = (Mean per Trip) * (Total Number of Trips)
    rum_annual_total = rum_mean_per_trip * sum(final_data_2019$VisitCount),
    
    # 2. MDCEV Annual = (Mean per Person) * (Number of Unique People)
    # We use n_distinct(id) to get exactly 1,380
    mdcev_annual_total = mdcev_mean_per_person * n_distinct(final_data_2019$id),
    
    # 3. The Gap
    difference = rum_annual_total - mdcev_annual_total
  )

final_comparison_2018$year=2018
final_comparison_2019$year=2019
final_comparison_2020$year=2020
final_comparison_2021$year=2021
final_comparison_2022$year=2022
final_comparison_2023$year=2023

combined_welfare=rbind(final_comparison_2018,final_comparison_2019,final_comparison_2020,final_comparison_2021,final_comparison_2022,final_comparison_2023)

write_csv(combined_welfare,file="rum_combined_totalWF.csv")