### Clear memory
rm(list = ls())
gc()

sink("output_MDCEV_yearly.txt", split = TRUE)

### Set seed for replication
set.seed(898989)

####################
# MAKE long FORMAT #
####################
setwd("C:/Users/andu/OneDrive - Norwegian University of Life Sciences/Desktop/KT data")

#if (!require(devtools)) {
#  install.packages("devtools")
#  library(devtools)
#}
#install_github("plloydsmith/rmdcev", dependencies = TRUE, INSTALL_opts="--no-multiarch")

library(dplyr)
library(tidyr)
library(rmdcev)
library(haven)
library(readr)

# Load datasets
reservation_data <- read_dta("reservation_data_merged_id_complete.dta")
intra_island_distance <- read.csv("intra_island_distance2.csv")

# Add year
reservation_data$year <- as.numeric(format(as.Date(reservation_data$CheckinDate, "%Y-%m-%d"), "%Y"))

reservation_data <- reservation_data %>%
  mutate(zip = ifelse(zip == 96812, 96813, zip))

# Group and compute yearly averages
avg_days_per_year <- reservation_data %>%
  group_by(year, id) %>%
  summarize(avg_days_visited = mean(days_visited, na.rm = TRUE)) %>%
  ungroup()

avg_days_2018 <- avg_days_per_year %>% filter(year == 2018)
avg_days_2019 <- avg_days_per_year %>% filter(year == 2019)
avg_days_2020 <- avg_days_per_year %>% filter(year == 2020)
avg_days_2021 <- avg_days_per_year %>% filter(year == 2021)
avg_days_2022 <- avg_days_per_year %>% filter(year == 2022)
avg_days_2023 <- avg_days_per_year %>% filter(year == 2023)


# Function to process data for a single year
process_year <- function(year_input) {
  data_year <- reservation_data %>% filter(year == year_input)
  
  unique_visits <- data_year %>%
    filter(!is.na(CheckinDate)) %>%
    distinct(id, parkname, CheckinDate) %>%
    group_by(id, parkname) %>%
    summarize(VisitCount = n(), .groups = "drop")
  
  all_combinations <- expand.grid(
    id = unique(data_year$id),
    parkname = unique(data_year$parkname),
    stringsAsFactors = FALSE
  )
  
  visit_data <- all_combinations %>%
    left_join(unique_visits, by = c("id", "parkname")) %>%
    mutate(VisitCount = ifelse(is.na(VisitCount), 0, VisitCount))
  
  person_info <- data_year %>%
    select(id, Permittee, zip, Zip, NoOfGuests, days_visited, year) %>%
    distinct()
  
  long_resdata_count <- left_join(visit_data, person_info, by = "id")
  
  long_resdata_count <- long_resdata_count %>%
    mutate(island_park = case_when(
      parkname == "Ainapo Cabin" ~ "Hawaii",
      parkname == "Hipalau" ~ "Kauai",
      parkname == "Kaluahaulu" ~ "Kauai",
      parkname == "Kamananui Trail" ~ "Oahu",
      parkname == "Kaunala Trail" ~ "Oahu",
      parkname == "Kawaikoi" ~ "Kauai",
      parkname == "Keanakolu Ranger & Bunkhouse Cabins" ~ "Hawaii",
      parkname == "Kuaokala Trail" ~ "Oahu",
      parkname == "Kulanaahane Trail" ~ "Oahu",
      parkname == "Kuliouou Trail" ~ "Oahu",
      parkname == "Lonomea" ~ "Kauai",
      parkname == "Maakua Ridge Trail" ~ "Oahu",
      parkname == "Manana Trail" ~ "Oahu",
      parkname == "Peacock Flats Campsite" ~ "Oahu",
      parkname == "Sugi Grove" ~ "Kauai",
      parkname == "Waialae Cabin Campsite" ~ "Kauai",
      parkname == "Waikoali" ~ "Kauai",
      parkname == "Waikolu" ~ "Molokai",
      parkname == "Waimano Trail" ~ "Oahu",
      parkname == "Waimanu Campsite" ~ "Hawaii",
      parkname == "Wiliwili" ~ "Kauai",
      parkname == "Wiliwilinui Ridge Trail" ~ "Oahu",
      TRUE ~ NA_character_
    ))
  
  long_resdata_count <- long_resdata_count[order(long_resdata_count$id, long_resdata_count$parkname), ]
  long_resdata_count$parkname_numeric <- as.integer(factor(long_resdata_count$parkname, levels = unique(long_resdata_count$parkname)))
  
  long_resdata_count <- long_resdata_count %>%
    distinct(id, parkname, .keep_all = TRUE)
  
  final_data <- long_resdata_count %>%
    left_join(
      intra_island_distance %>%
        select(zip, parkname, income, residents_distance_meters, residents_duration, island_live),
      by = c("zip", "parkname")
    )
  
  final_data <- final_data %>%
    group_by(id) %>%
    mutate(visited_another_island = ifelse(any(island_park != island_live & VisitCount > 0), 1, 0)) %>%
    ungroup()
  
  final_data$travel_distance_km <- (final_data$residents_distance_meters / 1000) * 2
  
  
  return(final_data)
}

# Run the function for each year
final_data_2018 <- process_year(2018)
final_data_2019 <- process_year(2019)
final_data_2020 <- process_year(2020)
final_data_2021 <- process_year(2021)
final_data_2022 <- process_year(2022)
final_data_2023 <- process_year(2023)

# Add correct avg_days_visited for each year by joining on id

final_data_2018 <- final_data_2018 %>%
  left_join(avg_days_2018 %>% select(id, avg_days_visited), by = "id") %>%
  mutate(days_visited = avg_days_visited)

final_data_2019 <- final_data_2019 %>%
  left_join(avg_days_2019 %>% select(id, avg_days_visited), by = "id") %>%
  mutate(days_visited = avg_days_visited)

final_data_2020 <- final_data_2020 %>%
  left_join(avg_days_2020 %>% select(id, avg_days_visited), by = "id") %>%
  mutate(days_visited = avg_days_visited)

final_data_2021 <- final_data_2021 %>%
  left_join(avg_days_2021 %>% select(id, avg_days_visited), by = "id") %>%
  mutate(days_visited = avg_days_visited)

final_data_2022 <- final_data_2022 %>%
  left_join(avg_days_2022 %>% select(id, avg_days_visited), by = "id") %>%
  mutate(days_visited = avg_days_visited)

final_data_2023 <- final_data_2023 %>%
  left_join(avg_days_2023 %>% select(id, avg_days_visited), by = "id") %>%
  mutate(days_visited = avg_days_visited)

## Travel cost per year calculations ##


##Adjusted for inflation
gas_prices <- data.frame(
  year = c(2018, 2019, 2020, 2021, 2022, 2023),
  gas_price = c(4.68, 4.49, 4.09, 4.75, 5.81, 5.09)
)

mean_gas_price <- (4.68+4.49+4.09+4.75+5.81+5.09)/6


# Inputs based on AAA Brochure 2018
fuel_efficiency_mpg <- 25
gas_price_hawaii_2018 <- 4.68  # in USD/gallon
maintenance_per_mile <- 0.0858*1.2816  # in USD/mile 2025 price
annual_miles <- 15000
annual_ownership_cost <- 6202*1.2816  # USD 2025 price

# Calculate total cost per mile
cost_per_mile <- (
  (1 / fuel_efficiency_mpg * gas_price_hawaii_2018 * annual_miles) +  # fuel cost
    (maintenance_per_mile * annual_miles) +                             # maintenance
    annual_ownership_cost                                               # ownership
) / annual_miles  # divide by miles to get per-mile cost

#https://exchange.aaa.com/wp-content/uploads/2018/09/18-0090_2018-Your-Driving-Costs-Brochure_FNL-Lo-5-2.pdf

# Show result
cost_per_mile

miles_to_km <- 1.60934

cost_per_km <- cost_per_mile / miles_to_km
cost_per_km

final_data_2018$price <- (final_data_2018$travel_distance_km * cost_per_km) / 3 +
  ((final_data_2018$income * 0.75) / 2080 * final_data_2018$travel_distance_km / 50)


# Inputs based on AAA Brochure 2019
fuel_efficiency_mpg <- 25
gas_price_hawaii_2019 <- 4.49  # in USD/gallon
maintenance_per_mile <- 0.0960*1.262  # in USD/mile 2025 price
annual_miles <- 15000
annual_ownership_cost <- 6845*1.262  # USD 2025 price

# Calculate total cost per mile
cost_per_mile <- (
  (1 / fuel_efficiency_mpg * gas_price_hawaii_2019 * annual_miles) +  # fuel cost
    (maintenance_per_mile * annual_miles) +                             # maintenance
    annual_ownership_cost                                               # ownership
) / annual_miles  # divide by miles to get per-mile cost

#https://exchange.aaa.com/wp-content/uploads/2019/09/AAA-Your-Driving-Costs-2019.pdf

# Show result
cost_per_mile

miles_to_km <- 1.60934

cost_per_km <- cost_per_mile / miles_to_km
cost_per_km

final_data_2019$price <- (final_data_2019$travel_distance_km * cost_per_km) / 3 +
  ((final_data_2019$income * 0.75) / 2080 * final_data_2019$travel_distance_km / 50)

# Inputs based on AAA Brochure 2020
fuel_efficiency_mpg <- 25
gas_price_hawaii_2020 <- 4.09  # in USD/gallon
maintenance_per_mile <- 0.1011*1.2314  # in USD/mile 2025 price
annual_miles <- 15000
annual_ownership_cost <- 6831*1.2314  # USD 2025 price

# Calculate total cost per mile
cost_per_mile <- (
  (1 / fuel_efficiency_mpg * gas_price_hawaii_2020 * annual_miles) +  # fuel cost
    (maintenance_per_mile * annual_miles) +                             # maintenance
    annual_ownership_cost                                               # ownership
) / annual_miles  # divide by miles to get per-mile cost

#https://newsroom.aaa.com/wp-content/uploads/2020/12/2020-Your-Driving-Costs-Brochure-Interactive-FINAL-12-9-20.pdf

# Show result
cost_per_mile

miles_to_km <- 1.60934

cost_per_km <- cost_per_mile / miles_to_km
cost_per_km

final_data_2020$price <- (final_data_2020$travel_distance_km * cost_per_km) / 3 +
  ((final_data_2020$income * 0.75) / 2080 * final_data_2020$travel_distance_km / 50)

# Inputs based on AAA Brochure 2021
fuel_efficiency_mpg <- 25
gas_price_hawaii_2021 <- 4.75  # in USD/gallon
maintenance_per_mile <- 0.1043*1.12144  # in USD/mile 2025 price
annual_miles <- 15000
annual_ownership_cost <- 6633*1.12144  # USD 2025 price

# Calculate total cost per mile
cost_per_mile <- (
  (1 / fuel_efficiency_mpg * gas_price_hawaii_2021 * annual_miles) +  # fuel cost
    (maintenance_per_mile * annual_miles) +                             # maintenance
    annual_ownership_cost                                               # ownership
) / annual_miles  # divide by miles to get per-mile cost

#https://newsroom.aaa.com/wp-content/uploads/2021/08/2021-YDC-Brochure-Live.pdf

# Show result
cost_per_mile

miles_to_km <- 1.60934

cost_per_km <- cost_per_mile / miles_to_km
cost_per_km

final_data_2021$price <- (final_data_2021$travel_distance_km * cost_per_km) / 3 +
  ((final_data_2021$income * 0.75) / 2080 * final_data_2021$travel_distance_km / 50)

# Inputs based on AAA Brochure 2022
fuel_efficiency_mpg <- 25
gas_price_hawaii_2022 <- 5.81  # in USD/gallon
maintenance_per_mile <- 0.1064*1.113  # in USD/mile 2025 price
annual_miles <- 15000
annual_ownership_cost <- 6599*1.113  # USD 2025 price

# Calculate total cost per mile
cost_per_mile <- (
  (1 / fuel_efficiency_mpg * gas_price_hawaii_2022 * annual_miles) +  # fuel cost
    (maintenance_per_mile * annual_miles) +                             # maintenance
    annual_ownership_cost                                               # ownership
) / annual_miles  # divide by miles to get per-mile cost

# Show result
cost_per_mile

miles_to_km <- 1.60934

cost_per_km <- cost_per_mile / miles_to_km
cost_per_km

final_data_2022$price <- (final_data_2022$travel_distance_km * cost_per_km) / 3 +
  ((final_data_2022$income * 0.75) / 2080 * final_data_2022$travel_distance_km / 50)

#https://newsroom.aaa.com/wp-content/uploads/2022/08/2022-YDC-Costs-Break-Out-by-Category.pdf

# Inputs based on AAA Brochure 2023
fuel_efficiency_mpg <- 25
gas_price_hawaii_2023 <- 5.09  # in USD/gallon
maintenance_per_mile <- 0.1085*1.0618  # in USD/mile 2025 price
annual_miles <- 15000
annual_ownership_cost <- 7542*1.0618  # USD 2025 price

# Calculate total cost per mile
cost_per_mile <- (
  (1 / fuel_efficiency_mpg * gas_price_hawaii_2023 * annual_miles) +  # fuel cost
    (maintenance_per_mile * annual_miles) +                             # maintenance
    annual_ownership_cost                                               # ownership
) / annual_miles  # divide by miles to get per-mile cost

# Show result
cost_per_mile

miles_to_km <- 1.60934

cost_per_km <- cost_per_mile / miles_to_km
cost_per_km

final_data_2023$price <- (final_data_2023$travel_distance_km * cost_per_km) / 3 +
  ((final_data_2023$income * 0.75) / 2080 * final_data_2023$travel_distance_km / 50)

#https://newsroom.aaa.com/wp-content/uploads/2023/08/YDC-Brochure_2023-FINAL-8.30.23-.pdf


final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(parkname == "Ainapo Cabin", (30*days_visited)/3, 0),
         price = price + ifelse(parkname == "Hipalau", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kaluahaulu", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kamananui Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kaunala Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kawaikoi", (20*days_visited)/3, 0),
         price = price + ifelse(parkname == "Keanakolu Ranger & Bunkhouse Cabins", (60*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kuaokala Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kulanaahane Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kuliouou Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Lonomea", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Maakua Ridge Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Manana Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Peacock Flats Campsite", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Sugi Grove", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waialae Cabin Campsite", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waikoali", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waikolu", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waimano Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waimanu Campsite", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Wiliwili", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Wiliwilinui Ridge Trail", (12*days_visited)/3, 0))


final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(parkname == "Ainapo Cabin", (30*days_visited)/3, 0),
         price = price + ifelse(parkname == "Hipalau", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kaluahaulu", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kamananui Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kaunala Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kawaikoi", (20*days_visited)/3, 0),
         price = price + ifelse(parkname == "Keanakolu Ranger & Bunkhouse Cabins", (60*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kuaokala Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kulanaahane Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kuliouou Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Lonomea", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Maakua Ridge Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Manana Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Peacock Flats Campsite", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Sugi Grove", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waialae Cabin Campsite", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waikoali", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waikolu", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waimano Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waimanu Campsite", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Wiliwili", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Wiliwilinui Ridge Trail", (12*days_visited)/3, 0))

final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(parkname == "Ainapo Cabin", (30*days_visited)/3, 0),
         price = price + ifelse(parkname == "Hipalau", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kaluahaulu", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kamananui Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kaunala Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kawaikoi", (20*days_visited)/3, 0),
         price = price + ifelse(parkname == "Keanakolu Ranger & Bunkhouse Cabins", (60*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kuaokala Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kulanaahane Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kuliouou Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Lonomea", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Maakua Ridge Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Manana Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Peacock Flats Campsite", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Sugi Grove", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waialae Cabin Campsite", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waikoali", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waikolu", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waimano Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waimanu Campsite", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Wiliwili", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Wiliwilinui Ridge Trail", (12*days_visited)/3, 0))

final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(parkname == "Ainapo Cabin", (30*days_visited)/3, 0),
         price = price + ifelse(parkname == "Hipalau", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kaluahaulu", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kamananui Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kaunala Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kawaikoi", (20*days_visited)/3, 0),
         price = price + ifelse(parkname == "Keanakolu Ranger & Bunkhouse Cabins", (60*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kuaokala Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kulanaahane Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kuliouou Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Lonomea", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Maakua Ridge Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Manana Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Peacock Flats Campsite", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Sugi Grove", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waialae Cabin Campsite", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waikoali", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waikolu", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waimano Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waimanu Campsite", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Wiliwili", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Wiliwilinui Ridge Trail", (12*days_visited)/3, 0))

final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(parkname == "Ainapo Cabin", (30*days_visited)/3, 0),
         price = price + ifelse(parkname == "Hipalau", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kaluahaulu", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kamananui Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kaunala Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kawaikoi", (20*days_visited)/3, 0),
         price = price + ifelse(parkname == "Keanakolu Ranger & Bunkhouse Cabins", (60*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kuaokala Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kulanaahane Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kuliouou Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Lonomea", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Maakua Ridge Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Manana Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Peacock Flats Campsite", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Sugi Grove", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waialae Cabin Campsite", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waikoali", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waikolu", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waimano Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waimanu Campsite", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Wiliwili", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Wiliwilinui Ridge Trail", (12*days_visited)/3, 0))

final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(parkname == "Ainapo Cabin", (30*days_visited)/3, 0),
         price = price + ifelse(parkname == "Hipalau", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kaluahaulu", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kamananui Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kaunala Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kawaikoi", (20*days_visited)/3, 0),
         price = price + ifelse(parkname == "Keanakolu Ranger & Bunkhouse Cabins", (60*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kuaokala Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kulanaahane Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Kuliouou Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Lonomea", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Maakua Ridge Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Manana Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Peacock Flats Campsite", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Sugi Grove", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waialae Cabin Campsite", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waikoali", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waikolu", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waimano Trail", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Waimanu Campsite", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Wiliwili", (12*days_visited)/3, 0),
         price = price + ifelse(parkname == "Wiliwilinui Ridge Trail", (12*days_visited)/3, 0))

#########Value of travel time in air#########

### BIG ISLAND ###
##Insert value of spare time of travelling with flight to BI if live in Oahu (90 minutes before flight + 50 minutes flight time)
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu",
                                (((income * 0.75) / 2080) * 2.33) * 2, 0))

##Insert value of spare time of travelling with flight to BI if live in Kauai (90 minutes before flight + 55 minutes flight time)
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai",
                                (((income * 0.75) / 2080) * 2.42) * 2, 0))

##Insert value of spare time of travelling with flight to BI if live in Molokai (90 minutes before flight + 1.5 hour flight time one way)
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Molokai",
                                (((income * 0.75) / 2080) * 3.5) * 2, 0))

### OAHU ###
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii",
                                (((income * 0.75) / 2080) * 2.33) * 2, 0))

final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Kauai",
                                (((income * 0.75) / 2080) * 2.17) * 2, 0))

final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Molokai",
                                (((income * 0.75) / 2080) * 2.17) * 2, 0))

### KAUAI ###
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii",
                                (((income * 0.75) / 2080) * 2.42) * 2, 0))

final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Oahu",
                                (((income * 0.75) / 2080) * 2.17) * 2, 0))

final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Molokai",
                                (((income * 0.75) / 2080) * 3.5) * 2, 0))

### MOLOKAI ###
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii",
                                (((income * 0.75) / 2080) * 3.5) * 2, 0))

final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Oahu",
                                (((income * 0.75) / 2080) * 2.17) * 2, 0))

final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Kauai",
                                (((income * 0.75) / 2080) * 3.5) * 2, 0))

### MAUI ###
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Maui",
                                (((income * 0.75) / 2080) * 2) * 2, 0))

final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Maui",
                                (((income * 0.75) / 2080) * 2.17) * 2, 0))

final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Maui",
                                (((income * 0.75) / 2080) * 2.33) * 2, 0))

final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Maui",
                                (((income * 0.75) / 2080) * 2.17) * 2, 0))

### LANAI ###
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Lanai",
                                (((income * 0.75) / 2080) * 2) * 2, 0))

final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Lanai",
                                (((income * 0.75) / 2080) * 2.17) * 2, 0))

final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Lanai",
                                (((income * 0.75) / 2080) * 4.34) * 2, 0))

final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Lanai",
                                (((income * 0.75) / 2080) * 4.34) * 2, 0))


#########Value of travel time in air#########

### BIG ISLAND ###
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", (((income * 0.75) / 2080) * 2.33) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", (((income * 0.75) / 2080) * 2.42) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Molokai", (((income * 0.75) / 2080) * 3.5) * 2, 0))

### OAHU ###
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", (((income * 0.75) / 2080) * 2.33) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Molokai", (((income * 0.75) / 2080) * 2.17) * 2, 0))

### KAUAI ###
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", (((income * 0.75) / 2080) * 2.42) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Molokai", (((income * 0.75) / 2080) * 3.5) * 2, 0))

### MOLOKAI ###
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", (((income * 0.75) / 2080) * 3.5) * 2, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", (((income * 0.75) / 2080) * 3.5) * 2, 0))

### MAUI ###
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Maui", (((income * 0.75) / 2080) * 2) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Maui", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Maui", (((income * 0.75) / 2080) * 2.33) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Maui", (((income * 0.75) / 2080) * 2.17) * 2, 0))

### LANAI ###
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Lanai", (((income * 0.75) / 2080) * 2) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Lanai", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Lanai", (((income * 0.75) / 2080) * 4.34) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Lanai", (((income * 0.75) / 2080) * 4.34) * 2, 0))

#########Value of travel time in air#########

### BIG ISLAND ###
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", (((income * 0.75) / 2080) * 2.33) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", (((income * 0.75) / 2080) * 2.42) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Molokai", (((income * 0.75) / 2080) * 3.5) * 2, 0))

### OAHU ###
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", (((income * 0.75) / 2080) * 2.33) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Molokai", (((income * 0.75) / 2080) * 2.17) * 2, 0))

### KAUAI ###
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", (((income * 0.75) / 2080) * 2.42) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Molokai", (((income * 0.75) / 2080) * 3.5) * 2, 0))

### MOLOKAI ###
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", (((income * 0.75) / 2080) * 3.5) * 2, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", (((income * 0.75) / 2080) * 3.5) * 2, 0))

### MAUI ###
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Maui", (((income * 0.75) / 2080) * 2) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Maui", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Maui", (((income * 0.75) / 2080) * 2.33) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Maui", (((income * 0.75) / 2080) * 2.17) * 2, 0))

### LANAI ###
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Lanai", (((income * 0.75) / 2080) * 2) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Lanai", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Lanai", (((income * 0.75) / 2080) * 4.34) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Lanai", (((income * 0.75) / 2080) * 4.34) * 2, 0))

##

### BIG ISLAND ###
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", (((income * 0.75) / 2080) * 2.33) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", (((income * 0.75) / 2080) * 2.42) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Molokai", (((income * 0.75) / 2080) * 3.5) * 2, 0))

### OAHU ###
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", (((income * 0.75) / 2080) * 2.33) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Molokai", (((income * 0.75) / 2080) * 2.17) * 2, 0))

### KAUAI ###
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", (((income * 0.75) / 2080) * 2.42) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Molokai", (((income * 0.75) / 2080) * 3.5) * 2, 0))

### MOLOKAI ###
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", (((income * 0.75) / 2080) * 3.5) * 2, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", (((income * 0.75) / 2080) * 3.5) * 2, 0))

### MAUI ###
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Maui", (((income * 0.75) / 2080) * 2) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Maui", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Maui", (((income * 0.75) / 2080) * 2.33) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Maui", (((income * 0.75) / 2080) * 2.17) * 2, 0))

### LANAI ###
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Lanai", (((income * 0.75) / 2080) * 2) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Lanai", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Lanai", (((income * 0.75) / 2080) * 4.34) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Lanai", (((income * 0.75) / 2080) * 4.34) * 2, 0))

##

### BIG ISLAND ###
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", (((income * 0.75) / 2080) * 2.33) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", (((income * 0.75) / 2080) * 2.42) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Molokai", (((income * 0.75) / 2080) * 3.5) * 2, 0))

### OAHU ###
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", (((income * 0.75) / 2080) * 2.33) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Molokai", (((income * 0.75) / 2080) * 2.17) * 2, 0))

### KAUAI ###
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", (((income * 0.75) / 2080) * 2.42) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Molokai", (((income * 0.75) / 2080) * 3.5) * 2, 0))

### MOLOKAI ###
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", (((income * 0.75) / 2080) * 3.5) * 2, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", (((income * 0.75) / 2080) * 3.5) * 2, 0))

### MAUI ###
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Maui", (((income * 0.75) / 2080) * 2) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Maui", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Maui", (((income * 0.75) / 2080) * 2.33) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Maui", (((income * 0.75) / 2080) * 2.17) * 2, 0))

### LANAI ###
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Lanai", (((income * 0.75) / 2080) * 2) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Lanai", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Lanai", (((income * 0.75) / 2080) * 4.34) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Lanai", (((income * 0.75) / 2080) * 4.34) * 2, 0))

##

### BIG ISLAND ###
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", (((income * 0.75) / 2080) * 2.33) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", (((income * 0.75) / 2080) * 2.42) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Molokai", (((income * 0.75) / 2080) * 3.5) * 2, 0))

### OAHU ###
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", (((income * 0.75) / 2080) * 2.33) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Molokai", (((income * 0.75) / 2080) * 2.17) * 2, 0))

### KAUAI ###
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", (((income * 0.75) / 2080) * 2.42) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Molokai", (((income * 0.75) / 2080) * 3.5) * 2, 0))

### MOLOKAI ###
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", (((income * 0.75) / 2080) * 3.5) * 2, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", (((income * 0.75) / 2080) * 3.5) * 2, 0))

### MAUI ###
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Maui", (((income * 0.75) / 2080) * 2) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Maui", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Maui", (((income * 0.75) / 2080) * 2.33) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Maui", (((income * 0.75) / 2080) * 2.17) * 2, 0))

### LANAI ###
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Lanai", (((income * 0.75) / 2080) * 2) * 2, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Lanai", (((income * 0.75) / 2080) * 2.17) * 2, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Lanai", (((income * 0.75) / 2080) * 4.34) * 2, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Lanai", (((income * 0.75) / 2080) * 4.34) * 2, 0))



## Flight costs 2018
#########Flight cost - 2018 #########

## BIG ISLAND ##
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", 354.0447586, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", 368.0753954, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Molokai", 308.7662656, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Maui", 273.2598837, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Lanai", 354.0447586 + 137.8000852, 0))  # via Oahu

## OAHU ##
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", 354.0447586, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", 269.4129138, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Molokai", 112.5000815, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Maui", 291.0965957, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Lanai", 137.8000852, 0))

## KAUAI ##
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", 368.0753954, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", 269.4129138, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Molokai", 317.7951903, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Maui", 359.4733617, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Lanai", 336.1283623, 0))

## MOLOKAI ##
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", 308.7662656, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", 112.5000815, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", 317.7951903, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Maui", 118.8094464, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Lanai", 137.8000852 + 112.5000815, 0))  # via Oahu

## Flight costs 2019
## BIG ISLAND ##
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", 348.630216, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", 362.446277, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Molokai", 304.044185, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Maui", 269.080816, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Lanai", 348.630216 + 135.692656, 0))  # via Oahu

## OAHU ##
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", 348.630216, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", 265.292679, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Molokai", 110.779575, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Maui", 286.644744, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Lanai", 135.692656, 0))

## KAUAI ##
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", 362.446277, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", 265.292679, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Molokai", 312.935027, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Maui", 353.975798, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Lanai", 330.987822, 0))

## MOLOKAI ##
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", 304.044185, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", 110.779575, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", 312.935027, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Maui", 118.809446, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Lanai", 135.692656 + 110.779575, 0))  # via Oahu


## Flight costs 2020
## BIG ISLAND ##
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", 340.176901, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", 353.657960, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Molokai", 296.671957, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Maui", 262.556352, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Lanai", 340.176901 + 132.402485, 0))  # via Oahu

## OAHU ##
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", 340.176901, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", 258.860067, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Molokai", 108.093477, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Maui", 279.694404, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Lanai", 132.402485, 0))

## KAUAI ##
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", 353.657960, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", 258.860067, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Molokai", 305.347220, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Maui", 345.392866, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Lanai", 322.962286, 0))

## MOLOKAI ##
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", 296.671957, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", 108.093477, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", 305.347220, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Maui", 108.093477 + 279.6944039, 0), # via Oahu
         price = price + ifelse(island_park == "Molokai" & island_live == "Lanai", 132.402485 + 108.093477, 0))  # via Oahu

##Flight costs 2021
## Flight costs 2021
## BIG ISLAND ##
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", 309.800214, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", 322.077459, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Molokai", 296.6719565, 0), #Use value for 2020
         price = price + ifelse(island_park == "Hawaii" & island_live == "Maui", 239.110927, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Lanai", 309.800214 + 132.4024851, 0))  # via Oahu #Use value for 2020 for Oahu to Lanai

## OAHU ##
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", 309.800214, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", 235.744708, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Molokai", 126.263151, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Maui", 254.718607, 0), 
         price = price + ifelse(island_park == "Oahu" & island_live == "Lanai", 132.4024851, 0)) #Use value for 2020

## KAUAI ##
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", 322.077459, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", 235.744708, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Molokai", 389.441668, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Maui", 314.550411, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Lanai", 235.744708 + 132.4024851, 0))  # via Oahu #Use value for 2020 for Oahu to Lanai

## MOLOKAI ##
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", 296.6719565, 0), #Use value for 2020
         price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", 120.699975, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", 389.441668, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Maui", 120.699975 + 254.718607, 0), # via Oahu
         price = price + ifelse(island_park == "Molokai" & island_live == "Lanai", 126.263151 + 120.699975, 0))  # via Oahu

##Flight cost 2022
## Flight costs 2022

## BIG ISLAND ##
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", 307.468646, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", 319.6534918, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Molokai", 296.6719565, 0), #Use value for 2020
         price = price + ifelse(island_park == "Hawaii" & island_live == "Maui", 237.311369, 0),
         price = price + ifelse(island_park == "Hawaii" & island_live == "Lanai", 307.468646 + 132.4024851, 0))  # via Oahu

## OAHU ##
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", 307.468646, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", 233.970485, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Molokai", 169.716317, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Maui", 252.801585, 0),
         price = price + ifelse(island_park == "Oahu" & island_live == "Lanai", 132.4024851, 0))  #Use value for 2020

## KAUAI ##
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", 319.6534918, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", 233.970485, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Molokai", 589.133955, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Maui", 312.1830927, 0),
         price = price + ifelse(island_park == "Kauai" & island_live == "Lanai", 233.970485 + 132.4024851, 0))  # via Oahu #Use value for 2020

## MOLOKAI ##
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", 296.6719565, 0), #Use value for 2020
         price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", 169.716317, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", 275.9878642, 0),
         price = price + ifelse(island_park == "Molokai" & island_live == "Maui", 169.716317 + 252.8015848, 0),  # via Oahu
         price = price + ifelse(island_park == "Molokai" & island_live == "Lanai", 169.716317 + 132.4024851, 0))  # via Oahu

##Flight cost 2023
## HAWAII ##
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", 293.324536, 0)) %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", 304.948857, 0)) %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Molokai", 296.6719565, 0)) %>% #Use value for 2020
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Maui", 226.39462, 0)) %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Lanai", 132.4024851 + 293.324536, 0))

## OAHU ##
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", 293.324536, 0)) %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", 223.207422, 0)) %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Molokai", 169.464607, 0)) %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Maui", 241.172258, 0)) %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Lanai", 132.4024851, 0)) #Use value for 2020

## KAUAI ##
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", 304.948857, 0)) %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", 223.207422, 0)) %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Molokai", 500.736032, 0)) %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Maui", 297.822109, 0)) %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Lanai", 223.207422 + 132.4024851, 0))

## MOLOKAI ##
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", 296.6719565, 0)) %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", 169.464607, 0)) %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", 263.291927, 0)) %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Maui", 241.172258 + 169.464607, 0)) %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Lanai", 132.4024851 + 169.464607, 0))



#########Car rental cost#########

### BIG ISLAND ###

#On average a rental car in The Big Island costs $430 per week ($61 per day).

##Insert car rental cost in BI
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", (61 * days_visited)/3.78, 0))

##Insert car rental cost in BI
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", (61 * days_visited)/3.78, 0))

##Insert car rental cost in BI
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Molokai", (61 * days_visited)/3.78, 0))

### OAHU ###

#On average a rental car in Oahu is $48/day

##Insert car rental cost in Oahu
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", (48 * days_visited)/3.78, 0))

##Insert car rental cost in Oahu
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", (48 * days_visited)/3.78, 0))

##Insert car rental cost in Oahu
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Molokai", (48 * days_visited)/3.78, 0))

### Kauai ###

#On average a rental car in Kauai is $62/day

##Insert car rental cost in Kauai
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", (63 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", (63 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Molokai", (63 * days_visited)/3.78, 0))

### Molokai ###

#On average a rental car in Molokai is $107/day

##Insert car rental cost in Kauai
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", (107 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", (107 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", (107 * days_visited)/3.78, 0))

#########Parking fees#########

### Oahu ###

final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", (22 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", (22 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", (22 * days_visited)/3.78, 0))

### Kauai ###

final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", (24 * days_visited)/3.78, 0))

### Big Island ###

final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2018 <- final_data_2018 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", (24 * days_visited)/3.78, 0))

### BIG ISLAND ###

#On average a rental car in The Big Island costs $430 per week ($61 per day).

##Insert car rental cost in BI
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", (61 * days_visited)/3.78, 0))

##Insert car rental cost in BI
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", (61 * days_visited)/3.78, 0))

##Insert car rental cost in BI
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Molokai", (61 * days_visited)/3.78, 0))

### OAHU ###

#On average a rental car in Oahu is $48/day

##Insert car rental cost in Oahu
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", (48 * days_visited)/3.78, 0))

##Insert car rental cost in Oahu
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", (48 * days_visited)/3.78, 0))

##Insert car rental cost in Oahu
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Molokai", (48 * days_visited)/3.78, 0))

### Kauai ###

#On average a rental car in Kauai is $62/day

##Insert car rental cost in Kauai
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", (63 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", (63 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Molokai", (63 * days_visited)/3.78, 0))

### Molokai ###

#On average a rental car in Molokai is $107/day

##Insert car rental cost in Kauai
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", (107 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", (107 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", (107 * days_visited)/3.78, 0))

#########Parking fees#########

### Oahu ###

final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", (22 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", (22 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", (22 * days_visited)/3.78, 0))

### Kauai ###

final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", (24 * days_visited)/3.78, 0))

### Big Island ###

final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2019 <- final_data_2019 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", (24 * days_visited)/3.78, 0))

### BIG ISLAND ###

#On average a rental car in The Big Island costs $430 per week ($61 per day).

##Insert car rental cost in BI
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", (61 * days_visited)/3.78, 0))

##Insert car rental cost in BI
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", (61 * days_visited)/3.78, 0))

##Insert car rental cost in BI
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Molokai", (61 * days_visited)/3.78, 0))

### OAHU ###

#On average a rental car in Oahu is $48/day

##Insert car rental cost in Oahu
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", (48 * days_visited)/3.78, 0))

##Insert car rental cost in Oahu
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", (48 * days_visited)/3.78, 0))

##Insert car rental cost in Oahu
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Molokai", (48 * days_visited)/3.78, 0))

### Kauai ###

#On average a rental car in Kauai is $62/day

##Insert car rental cost in Kauai
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", (63 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", (63 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Molokai", (63 * days_visited)/3.78, 0))

### Molokai ###

#On average a rental car in Molokai is $107/day

##Insert car rental cost in Kauai
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", (107 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", (107 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", (107 * days_visited)/3.78, 0))

#########Parking fees#########

### Oahu ###

final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", (22 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", (22 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", (22 * days_visited)/3.78, 0))

### Kauai ###

final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", (24 * days_visited)/3.78, 0))

### Big Island ###

final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2020 <- final_data_2020 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", (24 * days_visited)/3.78, 0))


### BIG ISLAND ###

#On average a rental car in The Big Island costs $430 per week ($61 per day).

##Insert car rental cost in BI
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", (61 * days_visited)/3.78, 0))

##Insert car rental cost in BI
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", (61 * days_visited)/3.78, 0))

##Insert car rental cost in BI
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Molokai", (61 * days_visited)/3.78, 0))

### OAHU ###

#On average a rental car in Oahu is $48/day

##Insert car rental cost in Oahu
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", (48 * days_visited)/3.78, 0))

##Insert car rental cost in Oahu
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", (48 * days_visited)/3.78, 0))

##Insert car rental cost in Oahu
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Molokai", (48 * days_visited)/3.78, 0))

### Kauai ###

#On average a rental car in Kauai is $62/day

##Insert car rental cost in Kauai
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", (63 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", (63 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Molokai", (63 * days_visited)/3.78, 0))

### Molokai ###

#On average a rental car in Molokai is $107/day

##Insert car rental cost in Kauai
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", (107 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", (107 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", (107 * days_visited)/3.78, 0))

#########Parking fees#########

### Oahu ###

final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", (22 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", (22 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", (22 * days_visited)/3.78, 0))

### Kauai ###

final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", (24 * days_visited)/3.78, 0))

### Big Island ###

final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2021 <- final_data_2021 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", (24 * days_visited)/3.78, 0))


### BIG ISLAND ###

#On average a rental car in The Big Island costs $430 per week ($61 per day).

##Insert car rental cost in BI
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", (61 * days_visited)/3.78, 0))

##Insert car rental cost in BI
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", (61 * days_visited)/3.78, 0))

##Insert car rental cost in BI
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Molokai", (61 * days_visited)/3.78, 0))

### OAHU ###

#On average a rental car in Oahu is $48/day

##Insert car rental cost in Oahu
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", (48 * days_visited)/3.78, 0))

##Insert car rental cost in Oahu
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", (48 * days_visited)/3.78, 0))

##Insert car rental cost in Oahu
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Molokai", (48 * days_visited)/3.78, 0))

### Kauai ###

#On average a rental car in Kauai is $62/day

##Insert car rental cost in Kauai
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", (63 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", (63 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Molokai", (63 * days_visited)/3.78, 0))

### Molokai ###

#On average a rental car in Molokai is $107/day

##Insert car rental cost in Kauai
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", (107 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", (107 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", (107 * days_visited)/3.78, 0))

#########Parking fees#########

### Oahu ###

final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", (22 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", (22 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", (22 * days_visited)/3.78, 0))

### Kauai ###

final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", (24 * days_visited)/3.78, 0))

### Big Island ###

final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2022 <- final_data_2022 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", (24 * days_visited)/3.78, 0))


### BIG ISLAND ###

#On average a rental car in The Big Island costs $430 per week ($61 per day).

##Insert car rental cost in BI
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", (61 * days_visited)/3.78, 0))

##Insert car rental cost in BI
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", (61 * days_visited)/3.78, 0))

##Insert car rental cost in BI
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Molokai", (61 * days_visited)/3.78, 0))

### OAHU ###

#On average a rental car in Oahu is $48/day

##Insert car rental cost in Oahu
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", (48 * days_visited)/3.78, 0))

##Insert car rental cost in Oahu
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", (48 * days_visited)/3.78, 0))

##Insert car rental cost in Oahu
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Molokai", (48 * days_visited)/3.78, 0))

### Kauai ###

#On average a rental car in Kauai is $62/day

##Insert car rental cost in Kauai
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", (63 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", (63 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Molokai", (63 * days_visited)/3.78, 0))

### Molokai ###

#On average a rental car in Molokai is $107/day

##Insert car rental cost in Kauai
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", (107 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", (107 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", (107 * days_visited)/3.78, 0))

#########Parking fees#########

### Oahu ###

final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Oahu", (22 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Oahu", (22 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Oahu", (22 * days_visited)/3.78, 0))

### Kauai ###

final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Kauai", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Hawaii" & island_live == "Kauai", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Kauai", (24 * days_visited)/3.78, 0))

### Big Island ###

final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Oahu" & island_live == "Hawaii", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Kauai" & island_live == "Hawaii", (24 * days_visited)/3.78, 0))

##Insert car rental cost in Kauai
final_data_2023 <- final_data_2023 %>%
  mutate(price = price + ifelse(island_park == "Molokai" & island_live == "Hawaii", (24 * days_visited)/3.78, 0))


##Mean travel cost
summary(final_data_2018$price)

final_data_2018 <- final_data_2018 %>%
  arrange(Permittee, parkname)

final_data_2018$parkname_numeric <- as.integer(factor(final_data_2018$parkname, levels = unique(final_data_2018$parkname)))

##Mean travel cost
summary(final_data_2019$price)

final_data_2019 <- final_data_2019 %>%
  arrange(Permittee, parkname)

final_data_2019$parkname_numeric <- as.integer(factor(final_data_2019$parkname, levels = unique(final_data_2019$parkname)))

##Mean travel cost
summary(final_data_2020$price)

final_data_2020 <- final_data_2020 %>%
  arrange(Permittee, parkname)

final_data_2020$parkname_numeric <- as.integer(factor(final_data_2020$parkname, levels = unique(final_data_2020$parkname)))

##Mean travel cost
summary(final_data_2021$price)

final_data_2021 <- final_data_2021 %>%
  arrange(Permittee, parkname)

final_data_2021$parkname_numeric <- as.integer(factor(final_data_2021$parkname, levels = unique(final_data_2021$parkname)))

##Mean travel cost
summary(final_data_2022$price)

final_data_2022 <- final_data_2022 %>%
  arrange(Permittee, parkname)

final_data_2022$parkname_numeric <- as.integer(factor(final_data_2022$parkname, levels = unique(final_data_2022$parkname)))

##Mean travel cost
summary(final_data_2023$price)

final_data_2023 <- final_data_2023 %>%
  arrange(Permittee, parkname)

final_data_2023$parkname_numeric <- as.integer(factor(final_data_2023$parkname, levels = unique(final_data_2023$parkname)))


#############
# 2018 model #
#############


aggregate(cbind(VisitCount, price) ~ parkname, data = final_data_2018, FUN = mean )

##Total number of visits to the most popular site
final_data_2018 %>%
  filter(parkname == "Peacock Flats Campsite") %>%
  summarise(total_visits = sum(VisitCount))

data_mdcev <- mdcev.data(final_data_2018,
                         id.var = "id",
                         alt.var = "parkname",
                         choice = "VisitCount")

args(mdcev)

f1 = ~ 0

data_model <- mdcev.data(final_data_2018,
                         id.var = "id",
                         alt.var = "parkname",
                         choice = "VisitCount")
##Estimate model
mdcev_mle <- mdcev(~ 1,
                   data = data_model,
                   model = "gamma", #test with hybrid instead of gamma
                   algorithm = "MLE",
                   std_errors = "mvn",
                   n_draws = 100,
                   trunc_data = 1,
                   print_iterations = TRUE)

summary(mdcev_mle)


## Policy simulation
nalts <- mdcev_mle$stan_data[["J"]]  # Should be 21

npols <- 22  # 21 individual site closures + 1 for closing all

# Create blank policies
policies <- CreateBlankPolicies(npols = npols,
                                model = mdcev_mle,
                                price_change_only = TRUE)

# Set price extremely high to simulate closure for each individual site (sites 1 to 21)
for (i in 1:21) {
  # +1 to skip the numeraire in position 1
  policies$price_p[[i]][i + 1] <- 999999
}

# Policy 22: Close all 21 sites
# The price vector has length = nalts + 1 (includes numeraire)
policies$price_p[[22]] <- c(0, rep(999999, nalts))

# Check lengths for safety (optional but recommended)
stopifnot(all(sapply(policies$price_p, length) == nalts + 1))

# Prepare data for simulation
df_sim <- PrepareSimulationData(mdcev_mle, policies)

# Run simulation
welfare_2018 <- mdcev.sim(df_sim$df_indiv,
                     df_common = df_sim$df_common,
                     sim_options = df_sim$sim_options,
                     cond_err = 1,
                     nerrs = 100,
                     draw_mlhs = 1,
                     sim_type = "welfare")

# Optional: Custom summary printer
print.summary.mdcev.sim <- function(x, digits = max(3, getOption("digits") - 2),
                                    width = getOption("width"),
                                    ...) {
  print(x$CoefTable, digits = digits, n = Inf)
  invisible(x)
}

# View results
summary(welfare_2018)

#############
# 2019 model #
#############


aggregate(cbind(VisitCount, price) ~ parkname, data = final_data_2019, FUN = mean )

##Total number of visits to the most popular site
final_data_2019 %>%
  filter(parkname == "Peacock Flats Campsite") %>%
  summarise(total_visits = sum(VisitCount))

data_mdcev <- mdcev.data(final_data_2019,
                         id.var = "id",
                         alt.var = "parkname",
                         choice = "VisitCount")

args(mdcev)

f1 = ~ 0

data_model <- mdcev.data(final_data_2019,
                         id.var = "id",
                         alt.var = "parkname",
                         choice = "VisitCount")
##Estimate model
mdcev_mle <- mdcev(~ 1,
                   data = data_model,
                   model = "gamma", #test with hybrid instead of gamma
                   algorithm = "MLE",
                   std_errors = "mvn",
                   n_draws = 100,
                   trunc_data = 1,
                   print_iterations = TRUE)

summary(mdcev_mle)


## Policy simulation
nalts <- mdcev_mle$stan_data[["J"]]  # Should be 21

npols <- 22  # 21 individual site closures + 1 for closing all

# Create blank policies
policies <- CreateBlankPolicies(npols = npols,
                                model = mdcev_mle,
                                price_change_only = TRUE)

# Set price extremely high to simulate closure for each individual site (sites 1 to 21)
for (i in 1:21) {
  # +1 to skip the numeraire in position 1
  policies$price_p[[i]][i + 1] <- 999999
}

# Policy 22: Close all 21 sites
# The price vector has length = nalts + 1 (includes numeraire)
policies$price_p[[22]] <- c(0, rep(999999, nalts))

# Check lengths for safety (optional but recommended)
stopifnot(all(sapply(policies$price_p, length) == nalts + 1))

# Prepare data for simulation
df_sim <- PrepareSimulationData(mdcev_mle, policies)

# Run simulation
welfare_2019 <- mdcev.sim(df_sim$df_indiv,
                     df_common = df_sim$df_common,
                     sim_options = df_sim$sim_options,
                     cond_err = 1,
                     nerrs = 100,
                     draw_mlhs = 1,
                     sim_type = "welfare")

# Optional: Custom summary printer
print.summary.mdcev.sim <- function(x, digits = max(3, getOption("digits") - 2),
                                    width = getOption("width"),
                                    ...) {
  print(x$CoefTable, digits = digits, n = Inf)
  invisible(x)
}

# View results
summary(welfare_2019)

##############
# 2020 model #
##############


aggregate(cbind(VisitCount, price) ~ parkname, data = final_data_2020, FUN = mean )

##Total number of visits to the most popular site
final_data_2020 %>%
  filter(parkname == "Peacock Flats Campsite") %>%
  summarise(total_visits = sum(VisitCount))

data_mdcev <- mdcev.data(final_data_2020,
                         id.var = "id",
                         alt.var = "parkname",
                         choice = "VisitCount")

args(mdcev)

f1 = ~ 0

data_model <- mdcev.data(final_data_2020,
                         id.var = "id",
                         alt.var = "parkname",
                         choice = "VisitCount")
##Estimate model
mdcev_mle <- mdcev(~ 1,
                   data = data_model,
                   model = "gamma", #test with hybrid instead of gamma
                   algorithm = "MLE",
                   std_errors = "mvn",
                   n_draws = 100,
                   trunc_data = 1,
                   print_iterations = TRUE)

summary(mdcev_mle)

## Policy simulation
nalts <- mdcev_mle$stan_data[["J"]]

npols <- 23

policies<- CreateBlankPolicies(npols = npols,
                               model = mdcev_mle,
                               price_change_only = TRUE)

## SET price extremely high "To simulate
#CSH of site closures, p1 is set to a very
#large number so that the new price is much
#higher than the choke price and therefore essentially
#has the same effect as site closures!!"

policies$price_p[[1]][2]   <- 999999 #
policies$price_p[[2]][3]   <- 999999 #
policies$price_p[[3]][4]   <- 999999 #
policies$price_p[[4]][5]   <- 999999 #
policies$price_p[[5]][6]   <- 999999 #
policies$price_p[[6]][7]   <- 999999 #
policies$price_p[[7]][8]   <- 999999 #
policies$price_p[[8]][9]   <- 999999 #
policies$price_p[[9]][10]  <- 999999 #
policies$price_p[[10]][11] <- 999999 #
policies$price_p[[11]][12] <- 999999 #
policies$price_p[[12]][13] <- 999999 #
policies$price_p[[13]][14] <- 999999 #
policies$price_p[[14]][15] <- 999999 #
policies$price_p[[15]][16] <- 999999 #
policies$price_p[[16]][17] <- 999999 #
policies$price_p[[17]][18] <- 999999 #
policies$price_p[[18]][19] <- 999999 #
policies$price_p[[19]][20] <- 999999 #
policies$price_p[[20]][21] <- 999999 #
policies$price_p[[21]][22] <- 999999 #
policies$price_p[[22]][23] <- 999999 #

#23rd policy: close all sites
policies$price_p[[23]] <- c(0, rep(999999, 22))


df_sim <- PrepareSimulationData(mdcev_mle, policies)

welfare_2020 <- mdcev.sim(df_sim$df_indiv,
                     df_common = df_sim$df_common,
                     sim_options = df_sim$sim_options,
                     cond_err = 1,
                     nerrs = 100,
                     draw_mlhs = 1,
                     sim_type = "welfare")

print.summary.mdcev.sim <- function(x, digits = max(3, getOption("digits") - 2),
                                    width = getOption("width"),
                                    ...){
  # Print all rows of the tibble
  print(x$CoefTable, digits = digits, n = Inf)
  
  invisible(x)
}

summary(welfare_2020)

##############
# 2021 model #
##############

aggregate(cbind(VisitCount, price) ~ parkname, data = final_data_2021, FUN = mean )

##Total number of visits to the most popular site
final_data_2021 %>%
  filter(parkname == "Peacock Flats Campsite") %>%
  summarise(total_visits = sum(VisitCount))

data_mdcev <- mdcev.data(final_data_2021,
                         id.var = "id",
                         alt.var = "parkname",
                         choice = "VisitCount")

args(mdcev)

f1 = ~ 0

data_model <- mdcev.data(final_data_2021,
                         id.var = "id",
                         alt.var = "parkname",
                         choice = "VisitCount")
##Estimate model
mdcev_mle <- mdcev(~ 1,
                   data = data_model,
                   model = "gamma", #test with hybrid instead of gamma
                   algorithm = "MLE",
                   std_errors = "mvn",
                   n_draws = 100,
                   trunc_data = 1,
                   print_iterations = TRUE)

summary(mdcev_mle)

## Policy simulation
nalts <- mdcev_mle$stan_data[["J"]]

npols <- 23

policies<- CreateBlankPolicies(npols = npols,
                               model = mdcev_mle,
                               price_change_only = TRUE)

## SET price extremely high "To simulate
#CSH of site closures, p1 is set to a very
#large number so that the new price is much
#higher than the choke price and therefore essentially
#has the same effect as site closures!!"

policies$price_p[[1]][2]   <- 999999 #
policies$price_p[[2]][3]   <- 999999 #
policies$price_p[[3]][4]   <- 999999 #
policies$price_p[[4]][5]   <- 999999 #
policies$price_p[[5]][6]   <- 999999 #
policies$price_p[[6]][7]   <- 999999 #
policies$price_p[[7]][8]   <- 999999 #
policies$price_p[[8]][9]   <- 999999 #
policies$price_p[[9]][10]  <- 999999 #
policies$price_p[[10]][11] <- 999999 #
policies$price_p[[11]][12] <- 999999 #
policies$price_p[[12]][13] <- 999999 #
policies$price_p[[13]][14] <- 999999 #
policies$price_p[[14]][15] <- 999999 #
policies$price_p[[15]][16] <- 999999 #
policies$price_p[[16]][17] <- 999999 #
policies$price_p[[17]][18] <- 999999 #
policies$price_p[[18]][19] <- 999999 #
policies$price_p[[19]][20] <- 999999 #
policies$price_p[[20]][21] <- 999999 #
policies$price_p[[21]][22] <- 999999 #
policies$price_p[[22]][23] <- 999999 #

#23rd policy: close all sites
policies$price_p[[23]] <- c(0, rep(999999, 22))


df_sim <- PrepareSimulationData(mdcev_mle, policies)

welfare_2021 <- mdcev.sim(df_sim$df_indiv,
                     df_common = df_sim$df_common,
                     sim_options = df_sim$sim_options,
                     cond_err = 1,
                     nerrs = 100,
                     draw_mlhs = 1,
                     sim_type = "welfare")

print.summary.mdcev.sim <- function(x, digits = max(3, getOption("digits") - 2),
                                    width = getOption("width"),
                                    ...){
  # Print all rows of the tibble
  print(x$CoefTable, digits = digits, n = Inf)
  
  invisible(x)
}

summary(welfare_2021)

##############
# 2022 model #
##############

aggregate(cbind(VisitCount, price) ~ parkname, data = final_data_2022, FUN = mean )

##Total number of visits to the most popular site
final_data_2022 %>%
  filter(parkname == "Peacock Flats Campsite") %>%
  summarise(total_visits = sum(VisitCount))

data_mdcev <- mdcev.data(final_data_2022,
                         id.var = "id",
                         alt.var = "parkname",
                         choice = "VisitCount")

args(mdcev)

f1 = ~ 0

data_model <- mdcev.data(final_data_2022,
                         id.var = "id",
                         alt.var = "parkname",
                         choice = "VisitCount")
##Estimate model
mdcev_mle <- mdcev(~ 1,
                   data = data_model,
                   model = "gamma", #test with hybrid instead of gamma
                   algorithm = "MLE",
                   std_errors = "mvn",
                   n_draws = 100,
                   trunc_data = 1,
                   print_iterations = TRUE)

summary(mdcev_mle)

## Policy simulation
nalts <- mdcev_mle$stan_data[["J"]]

npols <- 23

policies<- CreateBlankPolicies(npols = npols,
                               model = mdcev_mle,
                               price_change_only = TRUE)

## SET price extremely high "To simulate
#CSH of site closures, p1 is set to a very
#large number so that the new price is much
#higher than the choke price and therefore essentially
#has the same effect as site closures!!"

policies$price_p[[1]][2]   <- 999999 #
policies$price_p[[2]][3]   <- 999999 #
policies$price_p[[3]][4]   <- 999999 #
policies$price_p[[4]][5]   <- 999999 #
policies$price_p[[5]][6]   <- 999999 #
policies$price_p[[6]][7]   <- 999999 #
policies$price_p[[7]][8]   <- 999999 #
policies$price_p[[8]][9]   <- 999999 #
policies$price_p[[9]][10]  <- 999999 #
policies$price_p[[10]][11] <- 999999 #
policies$price_p[[11]][12] <- 999999 #
policies$price_p[[12]][13] <- 999999 #
policies$price_p[[13]][14] <- 999999 #
policies$price_p[[14]][15] <- 999999 #
policies$price_p[[15]][16] <- 999999 #
policies$price_p[[16]][17] <- 999999 #
policies$price_p[[17]][18] <- 999999 #
policies$price_p[[18]][19] <- 999999 #
policies$price_p[[19]][20] <- 999999 #
policies$price_p[[20]][21] <- 999999 #
policies$price_p[[21]][22] <- 999999 #
policies$price_p[[22]][23] <- 999999 #

#23rd policy: close all sites
policies$price_p[[23]] <- c(0, rep(999999, 22))


df_sim <- PrepareSimulationData(mdcev_mle, policies)

welfare_2022 <- mdcev.sim(df_sim$df_indiv,
                     df_common = df_sim$df_common,
                     sim_options = df_sim$sim_options,
                     cond_err = 1,
                     nerrs = 100,
                     draw_mlhs = 1,
                     sim_type = "welfare")

print.summary.mdcev.sim <- function(x, digits = max(3, getOption("digits") - 2),
                                    width = getOption("width"),
                                    ...){
  # Print all rows of the tibble
  print(x$CoefTable, digits = digits, n = Inf)
  
  invisible(x)
}

summary(welfare_2022)

##############
# 2023 model #
##############

aggregate(cbind(VisitCount, price) ~ parkname, data = final_data_2023, FUN = mean )

##Total number of visits to the most popular site
final_data_2023 %>%
  filter(parkname == "Peacock Flats Campsite") %>%
  summarise(total_visits = sum(VisitCount))

data_mdcev <- mdcev.data(final_data_2023,
                         id.var = "id",
                         alt.var = "parkname",
                         choice = "VisitCount")

args(mdcev)

f1 = ~ 0

data_model <- mdcev.data(final_data_2023,
                         id.var = "id",
                         alt.var = "parkname",
                         choice = "VisitCount")
##Estimate model
mdcev_mle <- mdcev(~ 1,
                   data = data_model,
                   model = "gamma", #test with hybrid instead of gamma
                   algorithm = "MLE",
                   std_errors = "mvn",
                   n_draws = 100,
                   trunc_data = 1,
                   print_iterations = TRUE)

summary(mdcev_mle)

## Policy simulation
nalts <- mdcev_mle$stan_data[["J"]]

npols <- 22

policies<- CreateBlankPolicies(npols = npols,
                               model = mdcev_mle,
                               price_change_only = TRUE)

## SET price extremely high "To simulate
#CSH of site closures, p1 is set to a very
#large number so that the new price is much
#higher than the choke price and therefore essentially
#has the same effect as site closures!!"

policies$price_p[[1]][2]   <- 999999 #
policies$price_p[[2]][3]   <- 999999 #
policies$price_p[[3]][4]   <- 999999 #
policies$price_p[[4]][5]   <- 999999 #
policies$price_p[[5]][6]   <- 999999 #
policies$price_p[[6]][7]   <- 999999 #
policies$price_p[[7]][8]   <- 999999 #
policies$price_p[[8]][9]   <- 999999 #
policies$price_p[[9]][10]  <- 999999 #
policies$price_p[[10]][11] <- 999999 #
policies$price_p[[11]][12] <- 999999 #
policies$price_p[[12]][13] <- 999999 #
policies$price_p[[13]][14] <- 999999 #
policies$price_p[[14]][15] <- 999999 #
policies$price_p[[15]][16] <- 999999 #
policies$price_p[[16]][17] <- 999999 #
policies$price_p[[17]][18] <- 999999 #
policies$price_p[[18]][19] <- 999999 #
policies$price_p[[19]][20] <- 999999 #
policies$price_p[[20]][21] <- 999999 #
policies$price_p[[21]][22] <- 999999 #

#23rd policy: close all sites
policies$price_p[[22]] <- c(0, rep(999999, 21))


df_sim <- PrepareSimulationData(mdcev_mle, policies)

welfare_2023 <- mdcev.sim(df_sim$df_indiv,
                     df_common = df_sim$df_common,
                     sim_options = df_sim$sim_options,
                     cond_err = 1,
                     nerrs = 100,
                     draw_mlhs = 1,
                     sim_type = "welfare")

print.summary.mdcev.sim <- function(x, digits = max(3, getOption("digits") - 2),
                                    width = getOption("width"),
                                    ...){
  # Print all rows of the tibble
  print(x$CoefTable, digits = digits, n = Inf)
  
  invisible(x)
}

summary(welfare_2023)

sink()

# List of welfare objects and corresponding years
welfare_list <- list(
  "2018" = welfare_2018,
  "2019" = welfare_2019,
  "2020" = welfare_2020,
  "2021" = welfare_2021,
  "2022" = welfare_2022,
  "2023" = welfare_2023
)

# Loop through each item and save the summary as CSV
for (year in names(welfare_list)) {
  welfare_obj <- welfare_list[[year]]
  
  # Get summary and extract the coefficient table
  welfare_summary <- summary(welfare_obj)$CoefTable
  
  # Save to CSV
  write.csv(welfare_summary,
            file = paste0("welfare_summary_", year, ".csv"),
            row.names = FALSE)
}



############## Create individual-specific mean value per site 2018 ############## 

# Step 1: Get unique IDs from final_data_2018 in the same order
unique_ids <- unique(final_data_2018$id)
unique_zips <- final_data_2018 %>%
  distinct(id, zip) %>%
  arrange(match(id, unique_ids))  # Ensure same order as IDs

# Step 2: Create updated welfare_2018_ind with real IDs and ZIPs
welfare_2018_ind <- data.frame(individual = integer(), 
                               site = integer(), 
                               mean_value = numeric(),
                               id = integer(),
                               zip = character())

for (i in seq_along(welfare_2018)) {
  individual_matrix <- welfare_2018[[i]]
  mean_values <- apply(individual_matrix, 2, mean, na.rm = TRUE)
  
  individual_df <- data.frame(
    individual = rep(i, length(mean_values)),              # Sequential index (not used anymore)
    site = seq_along(mean_values),
    mean_value = mean_values,
    id = rep(unique_ids[i], length(mean_values)),          # Actual id
    zip = rep(unique_zips$zip[i], length(mean_values))     # Corresponding zip
  )
  
  welfare_2018_ind <- rbind(welfare_2018_ind, individual_df)
}

# Optional: Drop the 'individual' column if only real id is needed
welfare_2018_ind$individual <- NULL




############## Create individual-specific mean value per site 2019 ############## 

# Step 1: Get unique IDs from final_data_2019 in the same order
unique_ids <- unique(final_data_2019$id)
unique_zips <- final_data_2019 %>%
  distinct(id, zip) %>%
  arrange(match(id, unique_ids))  # Ensure same order as IDs

# Step 2: Create updated welfare_2019_ind with real IDs and ZIPs
welfare_2019_ind <- data.frame(individual = integer(), 
                               site = integer(), 
                               mean_value = numeric(),
                               id = integer(),
                               zip = character())

for (i in seq_along(welfare_2019)) {
  individual_matrix <- welfare_2019[[i]]
  mean_values <- apply(individual_matrix, 2, mean, na.rm = TRUE)
  
  individual_df <- data.frame(
    individual = rep(i, length(mean_values)),              # Sequential index (not used anymore)
    site = seq_along(mean_values),
    mean_value = mean_values,
    id = rep(unique_ids[i], length(mean_values)),          # Actual id
    zip = rep(unique_zips$zip[i], length(mean_values))     # Corresponding zip
  )
  
  welfare_2019_ind <- rbind(welfare_2019_ind, individual_df)
}

# Optional: Drop the 'individual' column if only real id is needed
welfare_2019_ind$individual <- NULL

############## Create individual-specific mean value per site 2020 ############## 

# Step 1: Get unique IDs from final_data_2020 in the same order
unique_ids <- unique(final_data_2020$id)
unique_zips <- final_data_2020 %>%
  distinct(id, zip) %>%
  arrange(match(id, unique_ids))  # Ensure same order as IDs

# Step 2: Create updated welfare_2020_ind with real IDs and ZIPs
welfare_2020_ind <- data.frame(individual = integer(), 
                               site = integer(), 
                               mean_value = numeric(),
                               id = integer(),
                               zip = character())

for (i in seq_along(welfare_2020)) {
  individual_matrix <- welfare_2020[[i]]
  mean_values <- apply(individual_matrix, 2, mean, na.rm = TRUE)
  
  individual_df <- data.frame(
    individual = rep(i, length(mean_values)),              # Sequential index (not used anymore)
    site = seq_along(mean_values),
    mean_value = mean_values,
    id = rep(unique_ids[i], length(mean_values)),          # Actual id
    zip = rep(unique_zips$zip[i], length(mean_values))     # Corresponding zip
  )
  
  welfare_2020_ind <- rbind(welfare_2020_ind, individual_df)
}

# Optional: Drop the 'individual' column if only real id is needed
welfare_2020_ind$individual <- NULL


############## Create individual-specific mean value per site 2021 ############## 

# Step 1: Get unique IDs from final_data_2021 in the same order
unique_ids <- unique(final_data_2021$id)
unique_zips <- final_data_2021 %>%
  distinct(id, zip) %>%
  arrange(match(id, unique_ids))  # Ensure same order as IDs

# Step 2: Create updated welfare_2021_ind with real IDs and ZIPs
welfare_2021_ind <- data.frame(individual = integer(), 
                               site = integer(), 
                               mean_value = numeric(),
                               id = integer(),
                               zip = character())

for (i in seq_along(welfare_2021)) {
  individual_matrix <- welfare_2021[[i]]
  mean_values <- apply(individual_matrix, 2, mean, na.rm = TRUE)
  
  individual_df <- data.frame(
    individual = rep(i, length(mean_values)),              # Sequential index (not used anymore)
    site = seq_along(mean_values),
    mean_value = mean_values,
    id = rep(unique_ids[i], length(mean_values)),          # Actual id
    zip = rep(unique_zips$zip[i], length(mean_values))     # Corresponding zip
  )
  
  welfare_2021_ind <- rbind(welfare_2021_ind, individual_df)
}

# Optional: Drop the 'individual' column if only real id is needed
welfare_2021_ind$individual <- NULL

############## Create individual-specific mean value per site 2022 ############## 

# Step 1: Get unique IDs from final_data_2022 in the same order
unique_ids <- unique(final_data_2022$id)
unique_zips <- final_data_2022 %>%
  distinct(id, zip) %>%
  arrange(match(id, unique_ids))  # Ensure same order as IDs

# Step 2: Create updated welfare_2022_ind with real IDs and ZIPs
welfare_2022_ind <- data.frame(individual = integer(), 
                               site = integer(), 
                               mean_value = numeric(),
                               id = integer(),
                               zip = character())

for (i in seq_along(welfare_2022)) {
  individual_matrix <- welfare_2022[[i]]
  mean_values <- apply(individual_matrix, 2, mean, na.rm = TRUE)
  
  individual_df <- data.frame(
    individual = rep(i, length(mean_values)),              # Sequential index (not used anymore)
    site = seq_along(mean_values),
    mean_value = mean_values,
    id = rep(unique_ids[i], length(mean_values)),          # Actual id
    zip = rep(unique_zips$zip[i], length(mean_values))     # Corresponding zip
  )
  
  welfare_2022_ind <- rbind(welfare_2022_ind, individual_df)
}

# Optional: Drop the 'individual' column if only real id is needed
welfare_2022_ind$individual <- NULL

############## Create individual-specific mean value per site 2023 ############## 

# Step 1: Get unique IDs from final_data_2023 in the same order
unique_ids <- unique(final_data_2023$id)
unique_zips <- final_data_2023 %>%
  distinct(id, zip) %>%
  arrange(match(id, unique_ids))  # Ensure same order as IDs

# Step 2: Create updated welfare_2023_ind with real IDs and ZIPs
welfare_2023_ind <- data.frame(individual = integer(), 
                               site = integer(), 
                               mean_value = numeric(),
                               id = integer(),
                               zip = character())

for (i in seq_along(welfare_2023)) {
  individual_matrix <- welfare_2023[[i]]
  mean_values <- apply(individual_matrix, 2, mean, na.rm = TRUE)
  
  individual_df <- data.frame(
    individual = rep(i, length(mean_values)),              # Sequential index (not used anymore)
    site = seq_along(mean_values),
    mean_value = mean_values,
    id = rep(unique_ids[i], length(mean_values)),          # Actual id
    zip = rep(unique_zips$zip[i], length(mean_values))     # Corresponding zip
  )
  
  welfare_2023_ind <- rbind(welfare_2023_ind, individual_df)
}

# Optional: Drop the 'individual' column if only real id is needed
welfare_2023_ind$individual <- NULL

# Save each year's welfare data frame as a CSV file
write.csv(welfare_2018_ind, "welfare_2018_ind.csv", row.names = FALSE)
write.csv(welfare_2019_ind, "welfare_2019_ind.csv", row.names = FALSE)
write.csv(welfare_2020_ind, "welfare_2020_ind.csv", row.names = FALSE)
write.csv(welfare_2021_ind, "welfare_2021_ind.csv", row.names = FALSE)
write.csv(welfare_2022_ind, "welfare_2022_ind.csv", row.names = FALSE)
write.csv(welfare_2023_ind, "welfare_2023_ind.csv", row.names = FALSE)


