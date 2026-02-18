library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)



# 📁 Set your directory with the Excel files
folder_path <- "/Users/ashleylowe/Trails/HTA_Statisfaction"

# 📄 Get all matching Excel files
excel_files <- list.files(
  path = folder_path,
  pattern = "^\\d{4}-vsat-companion-tables\\.xls[x]?$",
  full.names = TRUE
)

# 📦 Create empty list to store cleaned activity tables by year
activity_tables <- list()

# 🧠 Function to read and clean a single file
extract_activity_participation <- function(file_path) {
  # Identify sheet that matches the title
  sheet_names <- excel_sheets(file_path)
  sheet_match <- sheet_names[str_detect(sheet_names, regex("Activity Partic.-Statewide", ignore_case = TRUE))]
  
  if (length(sheet_match) == 0) return(NULL)
  
  # Read the sheet
  df_raw <- read_excel(file_path, sheet = sheet_match[1], col_names = FALSE)
  
  # Detect start of actual data using the keyword "Q23"
  start_row <- which(str_detect(df_raw[[1]], regex("Q23", ignore_case = TRUE)))[1]
  if (is.na(start_row)) return(NULL)
  
  # Subset from start of table down
  df_table <- df_raw[start_row:nrow(df_raw), ]
  
  # Extract column names and clean the data
  col_names_row <- df_table[1, ] %>% unlist() %>% as.character()
  df_table <- df_table[-1, ]
  colnames(df_table) <- col_names_row
  
  df_clean <- df_table %>%
    rename_with(~ str_replace_all(., "[^A-Za-z0-9_]", "_")) %>%
    filter(!is.na(.[[1]])) %>%
    rename(Activity = 2) %>%
    select(-1, -3) %>%  # drop NA or redundant columns
    filter(!is.na(Activity)) %>%
    mutate(across(where(is.character), str_trim))
  
  return(df_clean)
}

# 🔁 Loop over each file, extract year from name, and store result
for (file in excel_files) {
  # Extract 4-digit year from filename, even if it starts with "X"
  year <- str_extract(basename(file), "\\d{4}")
  if (is.na(year)) next
  
  table <- extract_activity_participation(file)
  if (!is.null(table)) {
    activity_tables[[year]] <- table
  }
}

# ✅ Example usage
names(activity_tables)             # Show available years
head(activity_tables[["2024"]])    # Preview the 2024 table

# 4. View result
print(all_data)



# 1. Filter out non-commercial, non-operational, and non-CONUS airports
df_filtered <- airports %>%
  
  # 2. Filter by Operational Status (must be operational)
  filter(OPERSTATUS == "OPERATIONAL") %>%
  
  # 3. Filter by Use (must be public)
  filter(PRIVATEUSE == 0) %>%
  
  # 5. Select and rename key columns
  select(
    Airport_Name = NAME,
    IATA_Code = IDENT,
    Service_City = SERVCITY,
    State = STATE,
    Latitude = lat,
    Longitude = lon
  ) %>%
  
  # 6. Remove duplicates based on location (keeps only unique airport locations)
  distinct(Latitude, Longitude, .keep_all = TRUE)


cat(paste0("Subset complete. Found ", nrow(df_filtered), " major gateway airports.\n"))

return(df_filtered)
}






