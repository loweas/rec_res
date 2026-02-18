# Install the package if you haven't already:
# install.packages("bea.R")

library(bea.R)

# Replace 'YOUR_API_KEY' with your actual 36-digit BEA API key
beaKey <- "21F782AD-56A6-439D-B3D5-9A592F020E26"

industries <- c("11", "111CA", "113FF")

# Build query parameters
query_params <- list(
  UserID = beaKey,
  Method = "GetData",
  datasetname = "GDPbyIndustry",
  TableID = "1",
  Industry = "11",       # Agriculture, forestry, fishing, and hunting
  Frequency = "A",
  Year = "ALL",
  ResultFormat = "json"
)

# Send request
res <- GET("https://apps.bea.gov/api/data/", query = query_params)

# Parse JSON response
res_text <- content(res, "text", encoding = "UTF-8")
res_json <- fromJSON(res_text, flatten = TRUE)

# Extract data
if ("Data" %in% names(res_json$BEAAPI$Results)) {
  data <- res_json$BEAAPI$Results$Data
  print(head(data))
} else {
  cat("No data found in response\n")
  print(res_json)
}

# Convert list of records to dataframe
df <- as.data.frame(data, stringsAsFactors = FALSE)

# Convert DataValue to numeric
df$DataValue <- as.numeric(df$DataValue)


# Optional: select relevant columns
farms_clean <- df %>%
  select(Year, IndustrYDescription, Value = DataValue)

# View head
head(farms_clean)




# Build query parameters
query_params <- list(
  UserID = beaKey,
  Method = "GetData",
  datasetname = "GDPbyIndustry",
  TableID = "1",
  Industry = "113FF",       # Agriculture, forestry, fishing, and hunting
  Frequency = "A",
  Year = "ALL",
  ResultFormat = "json"
)

# Send request
res <- GET("https://apps.bea.gov/api/data/", query = query_params)

# Parse JSON response
res_text <- content(res, "text", encoding = "UTF-8")
res_json <- fromJSON(res_text, flatten = TRUE)

# Extract data
if ("Data" %in% names(res_json$BEAAPI$Results)) {
  data <- res_json$BEAAPI$Results$Data
  print(head(data))
} else {
  cat("No data found in response\n")
  print(res_json)
}

# Convert list of records to dataframe
df <- as.data.frame(data, stringsAsFactors = FALSE)

# Convert DataValue to numeric
df$DataValue <- as.numeric(df$DataValue)


# Optional: select relevant columns
fishingh_clean <- df %>%
  select(Year, IndustrYDescription, Value = DataValue)

# View head
head(fishingh_clean)




ggsave('/Users/ashleylowe/ForestReserves /Pictures/BlackWhite/social_indicators.png', dpi = 300, height = 8, width = 8, unit = 'in')



valuegdp=ggplot(agi_fish_valueaddedIndustry, aes(x = Year, y = Value, color = IndustrYDescription)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2, linetype = "dashed") +
  scale_color_manual(
    values = c(
      "Farms" = "forestgreen",
      "Forestry, fishing, and related activities" = "steelblue"
    ),
    labels = c(
      "Farms" = "Farms",
      "Forestry, fishing, and related activities" = "Forestry & Fishing"
    )
  ) +
  scale_x_continuous(breaks = seq(1997, 2024, by = 3)) +
  labs(
    title = "Agriculture Value Added as in Billions of Dollars to GDP with Trends",
    subtitle = "Farms vs Forestry, Fishing, and Related Activities (1997-2024)",
    x = "Year",
    y = "Value added to GDP in Billions of Dollars",
    color = "Sector",
    caption = "Source: BEA. Dashed lines show smoothed trends."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Print summary statistics
cat("\n=== Summary Statistics ===\n")
agi_fish %>%
  group_by(IndustrYDescription) %>%
  summarise(
    Min = min(Value),
    Mean = mean(Value),
    Max = max(Value),
    SD = sd(Value),
    Latest_2024 = Value[Year == 2024]
  ) %>%
  print()




line_numbers <- c("8", "10")

query_params <- list(
  UserID = beaKey,
  Method = "GetData",
  datasetname = "NIPA",
  TableName = "T60400D",
  LineNumber = "8",            # ✅ "Farms"
  Frequency = "A",
  Year = "ALL",
  ResultFormat = "json"
)

# Make the API request
res <- GET("https://apps.bea.gov/api/data/", query = query_params)

# Parse the response
res_text <- content(res, "text", encoding = "UTF-8")
res_json <- fromJSON(res_text, flatten = TRUE)
farms_data <- as.data.frame(res_json$BEAAPI$Results$Data)

library(dplyr)

library(dplyr)
library(ggplot2)

# Assuming your original data is called farms_data and has LineDescription, TimePeriod, DataValue

# Clean and subset the data:
subset_data <- farms_data %>%
  filter(LineDescription %in% c("Farms", "Forestry, fishing, and related activities")) %>%
  mutate(
    Year = as.numeric(TimePeriod),
    DataValue = as.numeric(gsub(",", "", DataValue))  # remove commas if present, convert to numeric
  ) %>%
  select(LineDescription, Year, DataValue)

# Now plot
employment=ggplot(subset_data, aes(x = Year, y = DataValue, color = LineDescription)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2, linetype = "dashed") +
  scale_color_manual(
    values = c(
      "Farms" = "forestgreen",
      "Forestry, fishing, and related activities" = "steelblue"
    ),
    labels = c(
      "Farms" = "Farms",
      "Forestry, fishing, and related activities" = "Forestry & Fishing"
    )
  ) +
  scale_x_continuous(breaks = seq(1997, 2024, by = 3)) +
  labs(
    title = "Agriculture Full-Time and Part-Time Employees by Industry",
    subtitle = "Farms vs Forestry, Fishing, and Related Activities (1997-2024)",
    x = "Year",
    y = "Employment Estimates",
    color = "Sector",
    caption = "Source: BEA. Dashed lines show smoothed trends."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


#########################
valuegdp=ggplot(agi_fish_valueaddedIndustry, aes(x = Year, y = Value, color = IndustrYDescription)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2, linetype = "dashed") +
  scale_color_manual(
    values = c(
      "Farms" = "forestgreen",
      "Forestry, fishing, and related activities" = "steelblue",
      "Outdoor Recreation"= "purple"
    ),
    labels = c(
      "Farms" = "Farms",
      "Forestry, fishing, and related activities" = "Forestry & Fishing",
      "Outdoor Recreation"= "Outdoor Recreation Economy"
    )
  ) +
  scale_x_continuous(breaks = seq(1997, 2024, by = 3)) +
  labs(
    x = "Year",
    y = "Value added to GDP in Billions of Dollars",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "none",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

percentagegdp=ggplot(agi_fish, aes(x = Year, y = Value, color = IndustrYDescription)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2, linetype = "dashed") +
  scale_color_manual(
    values = c(
      "Farms" = "forestgreen",
      "Forestry, fishing, and related activities" = "steelblue",
      "Outdoor Recreation"= "purple"
    ),
    labels = c(
      "Farms" = "Farms",
      "Forestry, fishing, and related activities" = "Forestry & Fishing",
      "Outdoor Recreation"= "Outdoor Recreation Economy"
    )
  ) +
  scale_x_continuous(breaks = seq(1997, 2024, by = 3)) +
  labs(
    x = "Year",
    y = "Percent of GDP (%)",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "none",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )



employment=ggplot(subset_data, aes(x = Year, y = DataValue, color = LineDescription)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2, linetype = "dashed") +
  scale_color_manual(
    values = c(
      "Farms" = "forestgreen",
      "Forestry, fishing, and related activities" = "steelblue",
      "Outdoor Recreation"= "purple"
    ),
    labels = c(
      "Farms" = "Farms",
      "Forestry, fishing, and related activities" = "Forestry & Fishing",
      "Outdoor Recreation"= "Outdoor Recreation Economy"
    )
  ) +
  scale_x_continuous(breaks = seq(1997, 2024, by = 3)) +
  labs(
    x = "Year",
    y = "Employment Estimates in Thousands",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "none",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

library(patchwork)

# First: add shared legend settings to one plot only (the one you want to keep the legend from)
# Others will have their legends removed

# Keep legend only on employment plot
valuegdp <- valuegdp + 
  theme(legend.position = "bottom")+
  labs(color = "Sector")  


# Add subtitles to each individual plot (these are your panels)
percentagegdp <- percentagegdp + labs(title = "Percent of GDP")
valuegdp <- valuegdp + labs(title = "Value Added")
employment <- employment + labs(title = "Employment")

# Now combine with shared title and caption
combined_plot <- (percentagegdp | valuegdp | employment) +
  plot_annotation(
    title = "Outdoor Recreation & Agriculture: Farms vs. Forestry & Fishing",
    subtitle = "Farms vs Forestry, Fishing, and Related Activities (1997-2024) \
    Outdoor Recreation Activities (2012-2023)",
    caption = "Source: BEA. Dashed lines show smoothed trends. \
    Note: Outdoor Recreation estimates remove agriculture industry estimates to reduce overcounting ",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),  # 👈 Center subtitle
      plot.caption = element_text(size = 10, hjust = 0.5)
    )
  )

combined_plot


# Show it
combined_plot

labs(
  title = "Agriculture Value Added as in Billions of Dollars to GDP with Trends",
  subtitle = "Farms vs Forestry, Fishing, and Related Activities (1997-2024)",
  ...
)
ggsave('/Users/ashleylowe/all_bea.png', dpi = 300, height = 8, width = 12, unit = 'in')

