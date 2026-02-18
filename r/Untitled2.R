
#Load
library(readr)
library(dplyr)
year2023 <- read_csv("~/MobileRaw/Years_trails/year2023.csv")
View(year2023)

year2023$hours=(year2023$end_time-year2023$start_time)/(3600)
year2023$minutes=(year2023$end_time-year2023$start_time)/(60)
year2023$Month <- format(as.Date(year2023$Date), "%b")

n <- year2023 %>%
  group_by(trailname, Month) %>%
  summarise(
    unique_visitors = n_distinct(Hashed.Device.ID),
    total_observations = n(),
    zero_hour_visits = sum(hours == 0, na.rm = TRUE),
    avg_hours_nonzero = mean(hours[hours > 0], na.rm = TRUE),
    .groups = "drop"
  )

n <- year2023 %>%
  # Step 1: Filter out zero-hour visits
  filter(minutes > 5) %>%
  
  # Step 2: Count number of visits per device, trail, and month
  group_by(trailname, Month, Hashed.Device.ID) %>%
  mutate(device_obs = n()) %>%
  
  # Step 3: Keep only devices with ≤ 10 observations at that trail/month
  filter(device_obs <= 10) %>%
  ungroup() %>%
  
  # Step 4: Summarise the filtered data
  group_by(trailname, Month) %>%
  summarise(
    unique_visitors = n_distinct(Hashed.Device.ID),
    total_observations = n(),
    avg_hours = mean(hours, na.rm = TRUE),
    avg_minutes = mean(minutes, na.rm = TRUE),
    .groups = "drop"
  )

n$Months <- match(n$Month, month.abb)
n=merge(n,alltrails)
n <- st_as_sf(n)

ggplot(n) +
  geom_sf(aes(fill = unique_visitors), color = "white") +
  scale_fill_viridis_c(option = "C", direction = -1) +
  theme_minimal() +
  labs(title = "Trail Visitors per Month", 
       fill = "Unique Visitors",
       subtitle = "{closest_state}") +
  transition_states(Months, state_length = 1, wrap = TRUE) +
  ease_aes('cubic-in-out')

