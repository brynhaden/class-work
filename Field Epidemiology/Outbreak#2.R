library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)

# Load CALREADIE data
calreadie <- calreadie %>%
  mutate(DOB = parse_date_time(DOB, orders = c("mdy", "dmy", "ymd")),
         LastName = clean_name(LastName),
         FirstName = clean_name(FirstName),
         DtReceived = as.Date(DtReceived, format = "%Y-%m-%d"),
         DtLabResult = as.Date(DtLabResult, format = "%Y-%m-%d"))

# Function to match and print IncidentID
match_and_print <- function(df, calreadie, sheet_name) {
  df <- df %>%
    mutate(DOB = parse_date_time(DOB, orders = c("mdy", "dmy", "ymd")),
           `Last Name` = tolower(`Last Name`),
           `First Name` = tolower(`First Name`))
  
  # Identify rows with non-date DOB values
  non_date_dob <- df %>%
    filter(is.na(DOB))
  
  if (nrow(non_date_dob) > 0) {
    cat("Non-date DOB values in", sheet_name, "sheet:\n")
    print(non_date_dob)
  }
  
  # Match 
  matched <- df %>%
    filter(!is.na(DOB)) %>%
    inner_join(calreadie, by = c("Last Name" = "LastName", "First Name" = "FirstName", "DOB" = "DOB")) %>%
    select(`IncidentID`)
  
  if (nrow(matched) > 0) {
    cat("Matches from", sheet_name, "sheet:\n")
    print(matched)
  } else {
    cat("No matches found in", sheet_name, "sheet.\n")
  }
}

# Load Serenity data 
serenity_resident <- read_excel("serenity.xlsx", sheet = "Resident Information")
serenity_staff <- read_excel("serenity.xlsx", sheet = "Staff Information")

# Match and print for Serenity
match_and_print(serenity_resident, calreadie, "Resident Information")
match_and_print(serenity_staff, calreadie, "Staff Information")

# Load Wells Fargo data
wells_fargo <- read_excel("wells_fargo.xlsx")

# Split Name column into First and Last
wells_fargo <- wells_fargo %>%
  separate(Name, into = c("First Name", "Last Name"), sep = " ") %>%
  mutate(DOB = parse_date_time(DOB, orders = c("mdy", "dmy", "ymd")),
         `Last Name` = tolower(`Last Name`),
         `First Name` = tolower(`First Name`))

# Print column names to verify
cat("Wells Fargo sheet column names:\n")
print(colnames(wells_fargo))

# Identify rows with non-date DOB values
non_date_dob_wells_fargo <- wells_fargo %>%
  filter(is.na(DOB))

if (nrow(non_date_dob_wells_fargo) > 0) {
  cat("Non-date DOB values in Wells Fargo sheet:\n")
  print(non_date_dob_wells_fargo)
}

# Match and print for Wells Fargo
match_and_print(wells_fargo, calreadie, "Wells Fargo")

# Load Rescue Mission data
rescue_mission_staff <- read_excel("rescue_mission.xlsx", sheet = "Staff Information")
rescue_mission_women <- read_excel("rescue_mission.xlsx", sheet = "Women's Resident Information")
rescue_mission_men <- read_excel("rescue_mission.xlsx", sheet = "Men's Resident Information")

# Standardize DOB format and clean names for Rescue Mission data
rescue_mission_staff <- rescue_mission_staff %>%
  mutate(DOB = parse_date_time(DOB, orders = c("mdy", "dmy", "ymd")),
         `Last Name` = clean_name(`Last Name`),
         `First Name` = clean_name(`First Name`))

rescue_mission_women <- rescue_mission_women %>%
  mutate(DOB = parse_date_time(DOB, orders = c("mdy", "dmy", "ymd")),
         `Last Name` = clean_name(`Last Name`),
         `First Name` = clean_name(`First Name`))

rescue_mission_men <- rescue_mission_men %>%
  mutate(DOB = parse_date_time(DOB, orders = c("mdy", "dmy", "ymd")),
         `Last Name` = clean_name(`Last Name`),
         `First Name` = clean_name(`First Name`))

# Match and print for Rescue Mission
match_and_print(rescue_mission_staff, calreadie, "Staff Information")
match_and_print(rescue_mission_women, calreadie, "Women's Resident Information")
match_and_print(rescue_mission_men, calreadie, "Men's Resident Information")

# Filter for Address "123 Mission St"
mission_address_cases <- calreadie %>%
  filter(Address == "123 Mission St")

# Print the IncidentID for these cases
if (nrow(mission_address_cases) > 0) {
  cat("IncidentIDs for cases at 123 Mission St:\n")
  print(mission_address_cases$IncidentID)
} else {
  cat("No cases found at 123 Mission St.\n")
}

# Search for positive cases at the County Jail address with DtLabResult since December 1
county_jail_address <- "5462 Main St"
date_threshold <- as.Date("2020-12-01")

positive_cases <- calreadie %>%
  filter(Address == county_jail_address & DtLabResult >= date_threshold)

# Print the IncidentID for the positive cases
if (nrow(positive_cases) > 0) {
  cat("IncidentIDs for positive cases at the County Jail since December 1:\n")
  print(positive_cases$IncidentID)
} else {
  cat("No positive cases found at the County Jail since December 1.\n")
}

# Filter for OccLocation "Hulk Construction" or "Hulk Construction-Guadalupe"
hulk_construction_cases <- calreadie %>%
  filter(OccLocation %in% c("Hulk Construction", "Hulk Construction - Guadalupe"))

# Print the IncidentID for these cases
if (nrow(hulk_construction_cases) > 0) {
  cat("IncidentIDs for cases at Hulk Construction or Hulk Construction - Guadalupe:\n")
  print(hulk_construction_cases$IncidentID)
} else {
  cat("No cases found at Hulk Construction or Hulk Construction - Guadalupe.\n")
}

#Load Elementary Data
carpinteria <- read_excel ("carpinteria.xlsx")

#Get the data standardized
carpinteria <- carpinteria %>%
  mutate(DOB = parse_date_time(DOB, orders = c("mdy", "dmy", "ymd")),
         `Last Name` = clean_name(`Last Name`),
         `First Name` = clean_name(`First Name`))

# Match and print for Carpinteria
match_and_print(carpinteria, calreadie, "Carpinteria")

# Filter for specific IncidentIDs
incident_ids <- c("10681344", "10813472", "10884004", "10755403", "10691301", "10784248", "10259630")

carpinteria_cases <- calreadie %>%
  filter(IncidentID %in% incident_ids)

# Print the Address, DtOnset, DtLabResult, Occupation, First Name, Last Name and OccLocation for these cases
if (nrow(carpinteria_cases) > 0) {
  cat("Details for specified IncidentIDs:\n")
  print(carpinteria_cases %>% select(Address, DtOnset, DtLabResult, Occupation, FirstName, LastName, OccLocation))
} else {
  cat("No cases found for the specified IncidentIDs.\n")
}

# Filter for OccLocation "Carpinteria Elementary"
carpinteria_cases <- calreadie %>%
  filter(OccLocation == "Carpinteria Elementary")

# Print the Address, DtOnset, DtLabResult, Occupation, and OccLocation for these cases
if (nrow(carpinteria_cases) > 0) {
  cat("Details for cases at Carpinteria Elementary:\n")
  print(carpinteria_cases %>% select(Address, DtOnset, DtLabResult, LastName, FirstName, IncidentID, Occupation, OccLocation))
} else {
  cat("No cases found at Carpinteria Elementary.\n")
}