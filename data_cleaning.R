install.packages("tidyverse")
install.packages("data.table")
install.packages("dplyr")
install.packages("readr")
install.packages("readxl")

library(data.table)
library(tidyverse)
library(dplyr)
library(readr)
library(readxl)

################################################################################
#### LOAD VEHICLE DATAFRAMES
################################################################################

###########################
# 
# Column Produktionsdatum is given in unix timestamp in the col Produktionsdatum_Origin_01011970
# Data/Fahrzeug/Fahrzeuge_OEM2_Typ21.csv
# Data/Fahrzeug/Fahrzeuge_OEM2_Typ22.csv 
#
###########################
#
# Seperated by ';':
# Fahrzeuge_OEM1_Typ12.csv
# Fahrzeuge_OEM2_Typ22.csv 
#


# Load the vehicles from OEM 1
df_fahrzeug_oem_1_type_11 <- read.csv("Data/Fahrzeug/Fahrzeuge_OEM1_Typ11.csv", header = T, stringsAsFactors = F, row.names = "X")
df_fahrzeug_oem_1_type_12 <- read.csv("Data/Fahrzeug/Fahrzeuge_OEM1_Typ12.csv", sep=";", header = T, stringsAsFactors = F, row.names = "X")

# Load the vehicles from OEM 2
df_fahrzeug_oem_2_type_21 <- read.csv("Data/Fahrzeug/Fahrzeuge_OEM2_Typ21.csv", header = T, stringsAsFactors = F, row.names = "X")
df_fahrzeug_oem_2_type_22 <- read.csv("Data/Fahrzeug/Fahrzeuge_OEM2_Typ22.csv", sep=";", header = T, stringsAsFactors = F, row.names = "X")

# Parse dates with Produktionsdatum_Origin_01011970 as number of days since timestamp supplied in column origin
df_fahrzeug_oem_2_type_21 <- df_fahrzeug_oem_2_type_21 %>% mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970, origin = as.Date(origin, "%d-%m-%Y")))
df_fahrzeug_oem_2_type_22 <- df_fahrzeug_oem_2_type_22 %>% mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970, origin = as.Date(origin, "%d-%m-%Y")))


# Drop old Produktionsdatum_Origin_01011970 and origin columns since they are no longer required
df_fahrzeug_oem_2_type_21 <- df_fahrzeug_oem_2_type_21 %>% select(-Produktionsdatum_Origin_01011970, -origin)
df_fahrzeug_oem_2_type_22 <- df_fahrzeug_oem_2_type_22 %>% select(-Produktionsdatum_Origin_01011970, -origin)

# Check if all dataframes have the same col names
if(!setequal(colnames(df_fahrzeug_oem_1_type_11), colnames(df_fahrzeug_oem_1_type_12)) ||
   !setequal(colnames(df_fahrzeug_oem_1_type_11), colnames(df_fahrzeug_oem_2_type_21)) ||
   !setequal(colnames(df_fahrzeug_oem_1_type_11), colnames(df_fahrzeug_oem_2_type_22))
){
  error_message <- "Fahrzeug DataFrames do not have the same column names"
  stop(error_message)
} else{
  cat("[Check passed]: All dataframes have the same column names.")
}

# Concatenate the dataframes
df_fahrzeug <- rbind(df_fahrzeug_oem_1_type_11, df_fahrzeug_oem_1_type_12,df_fahrzeug_oem_2_type_21, df_fahrzeug_oem_2_type_22)
df_fahrzeug <- df_fahrzeug %>% select(ID_Fahrzeug, Produktionsdatum) %>% mutate(Produktionsdatum = as.Date(Produktionsdatum))

# Remove dataframes no longer needed
rm(df_fahrzeug_oem_1_type_11, df_fahrzeug_oem_1_type_12, df_fahrzeug_oem_2_type_21, df_fahrzeug_oem_2_type_22)

# Check if all ID's are unique
if (any(duplicated(df_fahrzeug$ID_Fahrzeug ))) {
  error_message <- "Not all IDs are unique."
  stop(error_message)
} else{
  cat("[Check passed]: all IDs are unique.")
}

# Check if all vehicles are in time bound
max_date <- max(df_fahrzeug$Produktionsdatum, na.rm = TRUE)
min_date <- min(df_fahrzeug$Produktionsdatum, na.rm = TRUE)
if(
  max_date >= as.Date("2016-12-31") || 
  min_date <= as.Date("2008-01-01")
){
  error_message <- "Not all vehicles are produced in the requested timeframe."
  stop(error_message)
} else{
  cat("[Check passed]: all vehicles are produced in the requested timeframe.")
}

################################################################################
#### LOAD VEHICLEPART DATAFRAMES
################################################################################

# Load the vehicleparts from OEM 1
df_fahrzeugteile_oem_1_type_11 <- read.csv("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv", sep=";", header = T, stringsAsFactors = F, row.names = "X")
df_fahrzeugteile_oem_1_type_12 <- read.csv("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ12.csv", sep=";", header = T, stringsAsFactors = F, row.names = "X")

# Load the vehicleparts from OEM 2
df_fahrzeugteile_oem_2_type_21 <- read.csv("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ21.csv", sep=";", header = T, stringsAsFactors = F, row.names = "X")
df_fahrzeugteile_oem_2_type_22 <- read.csv("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ22.csv", sep=";", header = T, stringsAsFactors = F, row.names = "X1")


# Check if all dataframes have the same col names
if(!setequal(colnames(df_fahrzeugteile_oem_1_type_11), colnames(df_fahrzeugteile_oem_1_type_12)) ||
   !setequal(colnames(df_fahrzeugteile_oem_1_type_11), colnames(df_fahrzeugteile_oem_2_type_21)) ||
   !setequal(colnames(df_fahrzeugteile_oem_1_type_11), colnames(df_fahrzeugteile_oem_2_type_22))
){
  error_message <- "Fahrzeugteile DataFrames do not have the same column names"
  stop(error_message)
} else{
  cat("[Check passed]: All dataframes have the same column names.")
}

# Concatenate the parts dataframes
df_fahrzeug_teile <- rbind(df_fahrzeugteile_oem_1_type_11, df_fahrzeugteile_oem_1_type_12,df_fahrzeugteile_oem_2_type_21, df_fahrzeugteile_oem_2_type_22)

# Mutate the row and keep only the substring until the first "-", to get the engine Type
# Then filter for the gasoline engine “K1BE1” and “K1BE2”
df_fahrzeug_teile <- df_fahrzeug_teile %>%
  mutate(Motor = sapply(strsplit(as.character(ID_Motor), "-"), `[`, 1)) %>%
  filter(Motor %in% c("K1BE1", "K1BE2"))
rm(df_fahrzeugteile_oem_1_type_11, df_fahrzeugteile_oem_1_type_12, df_fahrzeugteile_oem_2_type_21,df_fahrzeugteile_oem_2_type_22)

################################################################################
#### LOAD COMPONENT DATAFRAMES
################################################################################

# Load the component dataframes
df_komponenten_K1BE1 <- read.csv("Data/Komponente/Bestandteile_Komponente_K1BE1.csv", sep = ";", header = T, stringsAsFactors = F) 
df_komponenten_K1BE2 <- read.csv("Data/Komponente/Bestandteile_Komponente_K1BE2.csv", sep = ";", header = T, stringsAsFactors = F) 

# Select only the relevant columns
df_komponenten_K1BE1 <- df_komponenten_K1BE1 %>% select(ID_Motor = ID_K1BE1, ID_T2 = ID_T2)
df_komponenten_K1BE2 <- df_komponenten_K1BE2 %>% select(ID_Motor = ID_K1BE2, ID_T2 = ID_T2)

# Concat the component dataframes
df_komponenten <- rbind(df_komponenten_K1BE1, df_komponenten_K1BE2)
rm(df_komponenten_K1BE1, df_komponenten_K1BE2)

################################################################################
#### LOAD CONTROL UNIT T02 DATAFRAMES
################################################################################

file_str <- read_file("Data/Einzelteil/Einzelteil_T02.txt")
# remove " from string
cleaned_string <- gsub('"', '', file_str)
# split on tabs and on spaces
lines <- strsplit(cleaned_string, "\\s+|\t+")[[1]]

n_cols <- 15

# Create an empty data frame with appropriate column names
data_matrix = matrix(
  lines[n_cols+1:length(lines)],
  ncol=n_cols+1,
  byrow = TRUE,
)

# Drop the index row - it will be recreated anyways
data_matrix <- data_matrix[, -1]
# Give the data matrix the correct column names
colnames(data_matrix) = lines[1:n_cols]
# Generate the dataframe
df_controll_unit_t02 <- data.frame(data_matrix)

# The dataset is screwed it has this form
# ... Herstellernummer.x | Herstellernummer.y ...
#     201                | NA         
#     201                | NA
#     201                | NA
#     NA                 | 202
#     NA                 | 202
#     NA                 | 202
# So we need to keep only the .y part of the framework, since we are only interested
# in the parts that originate from the plant 202 
# Then we filter for the correct Plant Id and make sure that the dates align with our information
df_controll_unit_t02 <- df_controll_unit_t02 %>%  #rename
  select(X1=X1,
         ID_T02=ID_T02.y,
         Herstellernummer=Herstellernummer.y,
         Produktionsdatum=Produktionsdatum.y,
         Werksnummer=Werksnummer.y,
         Fehlerhaft=Fehlerhaft.y,
         Fehlerhaft_Datum=Fehlerhaft_Datum.y,
         Fehlerhaft_Fahrleistung=Fehlerhaft_Fahrleistung.y) %>% #filter for plant and manufacturer
  filter(Herstellernummer == "202",
         Werksnummer == "2022") %>% # cast to date
  mutate(Produktionsdatum = as.Date(Produktionsdatum),
         Fehlerhaft_Datum = as.Date(Fehlerhaft_Datum)) %>% # filter for date 
  filter(Produktionsdatum <= as.Date("2010-12-31"),  
         Produktionsdatum >= as.Date("2008-04-01"))

rm(data_matrix, cleaned_string, file_str, lines, n_cols)

################################################################################
#### LOAD REGISTRATIONS
################################################################################

df_registrations <- read.csv("Data/Zulassungen/Zulassungen_alle_Fahrzeuge.csv", sep=";", header = T, stringsAsFactors = F, row.names = "X") %>%
  select(Gemeinde=Gemeinden,ID_Fahrzeug=IDNummer)

# Clean municipality names of numbers
df_registrations$Gemeinde <- gsub("\\d", "", df_registrations$Gemeinde)

# Some municipalities no longer exist since they were integrated into others, 
# accounting for those is possible with the destatis lists since the first registration
# 2009...2023
# However this does not work because unlike real data from KBA 
# the registrations do not contain addresses 
# which would be the basis to determine which municipality they belong to after a change

#Example implementation if that data was available:
#Procedure: Clean lists, read them, Capitalise names, match old names with muns from df_registrations, changes muns from df_regs to new ones
#umgemeindung <- read_xlsx("www/umgemeindungen/2010.xlsx", sheet = 2) %>%  #read list and clean
#  select(5, 11,12) %>% 
#  rename_at(1, ~"Alt") %>% 
#  rename_at(2, ~"Neu") %>%
#  rename_at(3, ~"Datum") %>%
#  mutate(Alt = sub(",.*", "", Alt)) %>% #only keep names, remove suffixes like "Stadt" or "Kreisstadt" since they are not included in the KBA-registrations
#  mutate(Neu = sub(",.*", "", Neu)) %>%
#  filter(Alt != Neu) # ignore operations without name changes (eg municipality key or are change)

#df_registrations <- df_registrations %>% #change municipality names
#  left_join(umgemeindung, by = c("Gemeinde" = "Alt")) %>%
#  mutate(city = ifelse(is.na(Neu), Gemeinde, Neu)) %>%
#  select(-Alt, -Neu)

# Instead of an automatic approach, the affected municipalitys were manually detected 
# and are updated below with destatis information:

# Define the replacements as a named vector
replacements <- c(
  "LICHTE" = "NEUHAUS AM RENNWEG",
  "GEHREN" = "ILMENAU",
  "HERRMANSACKER" = "HARZTOR",
  "REICHMANSDORF" = "SAALFELD",
  "SACHSENBRUNN" = "EISFELD",
  "LANGEWIESEN" = "ILMENAU",
  "NARSDORF" = "GEITHAIN",
  "KOHREN SALIS" = "FROHBURG",
  "SENSBACHTAL" = "OBERZENT",
  "SCHMIEDEFELD" = "SAALFELD",
  "KAMSDORF" = "UNTERWELLENBORN"
)

# Replace rows in the "Gemeinde" column based on the named vector
df_registrations <- df_registrations %>% 
  mutate(Gemeinde = ifelse(Gemeinde %in% names(replacements), replacements[Gemeinde], Gemeinde))


################################################################################
#### COMBINE ALL THE DATAFRAMES TO GET THE DAMAGED VEHICLES
################################################################################

# Join the vehicles and df_fahrzeug_teile via ID_Fahrzeug to get the MotorID
# Join the result and df_komponenten via ID_Motor to get the T2 ID
# Join the results and df_controll_unit_t02 to only keep vehicles with affected T2 units
# Join the result and df_registrations to get the municipalities of affected vehicles
df_affected_vehicles <- df_fahrzeug %>% 
  inner_join(df_fahrzeug_teile, by = "ID_Fahrzeug") %>%
  select(ID_Fahrzeug, ID_Motor) %>%
  inner_join(df_komponenten, by = "ID_Motor") %>%
  select(ID_Fahrzeug, ID_T02 = ID_T2, ID_Motor) %>%
  inner_join(df_controll_unit_t02, by = "ID_T02") %>%
  select(ID_Fahrzeug, ID_T02, ID_Motor) %>%
  inner_join(df_registrations, by="ID_Fahrzeug") %>%
  mutate(Beschaedigt="ja")

############################################################################
#### CLEAN THE GEO DATA
###########################################################################

# Read the input CSV file
df_geo_data <- fread("Data/Geodaten/Geodaten_Gemeinden_v1.2_2017-08-22_TrR.csv",
              header = FALSE, sep = ";", skip = 1)

# Replace commas with periods in the 5th and 6th columns
df_geo_data[, V5 := gsub(",", ".", V5)]
df_geo_data[, V6 := gsub(",", ".", V6)]

# Convert the 5th and 6th columns to numeric
df_geo_data[, c("V5", "V6") := lapply(.SD, as.numeric), .SDcols = c("V5", "V6")]

# Function to format the 5th and 6th columns as floating-point numbers.
# Sometimes the floatingpoint is missing, eg. 56778 instead of 56.778
format_as_float <- function(x) {
  if (is.numeric(x)) {
    return(format(x, nsmall = 6))
  } else {
    return(x)
  }
}

# Apply the format_as_float function to the 5th and 6th columns
df_geo_data$V5 <- sapply(df_geo_data$V5, format_as_float)
df_geo_data$V6 <- sapply(df_geo_data$V6, format_as_float)

# Drop the first two columns from 'df_geo_data', we don't need them
df_geo_data <- df_geo_data[, 3:6, with = FALSE]

# name the columns
colnames(df_geo_data) <- c("PLZ", "Gemeinde", "Longitude", "Latitude")

# We found out that one Gemeinde is missing in Geo_data that is
# needed for the damaged vehicles: Gemeinde SEEG. Add SEEG here
new_data <- data.frame(PLZ= 87637, Gemeinde = "SEEG",
                       Longitude = 10.6085, Latitude = 47.6543)
df_geo_data <- rbind(df_geo_data, new_data)


##########################################################################
#### ADD DISTANCES TO GEO DATA
##########################################################################

# Read the postal code list CSV file, only keep required columns
df_postcodes <- read.csv("www/georef-germany-postleitzahl_data.csv", sep = ";", header = T, stringsAsFactors = F) %>% 
  select(c("Name", "Land.code"))%>% 
  distinct(Name, .keep_all = TRUE)

# Create a dataframe with state capitals and their coordinates; numbering of states based on the given dataset
df_state_capitals <- data.frame(
  State = c(8, 9, 11, 12, 4, 2, 6, 3, 13, 5, 7, 10, 14, 15, 1, 16), 
  StateCapital = c("Stuttgart", "Muenchen", "Berlin", "Potsdam", "Bremen", "Hamburg", "Wiesbaden", "Hannover", "Schwerin", "Duesseldorf", "Mainz", "Saarbruecken", "Dresden", "Magdeburg", "Kiel", "Erfurt"), 
  SC_long = c(9.1829, 11.5755, 13.4050, 13.0650, 8.8017, 9.9937, 8.2430, 9.7393, 11.4018, 6.7728, 8.2473, 6.9965, 13.7384, 11.6292, 11.6276, 11.0283), 
  SC_lat = c(48.7758, 48.1372, 52.5200, 52.3989, 53.0793, 53.5511, 50.0826, 52.3744, 53.6355, 51.2277, 49.9929, 49.2332, 51.0504, 52.1277, 52.1205, 50.9787)
)
# Assign state capitals for each postal code, assuming each code only refers to one state, which holds up for the vast majority of codes
df_postcodes <- inner_join(df_postcodes, df_state_capitals, by=c("Land.code"="State")) %>%  select(c("Name", "StateCapital", "SC_lat", "SC_long")) 

# Assign state capitals for each municipality
df_geo_data <- inner_join(df_geo_data, df_postcodes, by=c("PLZ"="Name"))

# Calculate distance to state capital for each municipality, required for ranking the repair services
df_geo_data$SCdistance<- sqrt((as.numeric(df_geo_data$Longitude)-df_geo_data$SC_long)^2 + (as.numeric(df_geo_data$Latitude)-df_geo_data$SC_lat)^2)


##########################################################################
#### MERGE VEHICLE DAMAGE DATA AND GEO DATA
##########################################################################

# Merge the datasets using the 4th column as the key
df_merged_data <- merge(df_affected_vehicles, df_geo_data, by.x = "Gemeinde",
                     by.y = "Gemeinde", all.x = TRUE)

##########################################################################
#### DETERMINE RANK FOR SERVICE IN STATE CAPITAL
##########################################################################

# Create a list to store waiting lists for each state capital
waiting_lists <- list()

# Iterate through each state capital and create their waiting lists
for (capital in df_state_capitals$StateCapital) {
  waiting_lists[[capital]] <- df_merged_data %>%
    select(ID_Fahrzeug, Gemeinde, SCdistance, StateCapital) %>%
    filter(StateCapital == capital) %>%
    arrange(SCdistance) %>%
    mutate(rank = row_number())
}
rm(capital)

# Bind list of lists to a dataset
df_waiting_lists <- bind_rows(waiting_lists)

a <- nrow(df_waiting_lists)
print("CCCCCCCC")
print(a)

# Calculate mean waiting time for each municipality based on mean rank position and repair rate of 100/day, rounding conservatively
waitingTimes <- df_waiting_lists %>%
  group_by(Gemeinde) %>%
  summarise(MeanRank = mean(rank))

waitingTimes$waitingTime <- ceiling(waitingTimes$MeanRank / 100)

df_merged_data <- inner_join(df_merged_data, waitingTimes, by= "Gemeinde") %>% select(-MeanRank, -SCdistance, -SC_lat, -SC_long)

##########################################################################
#### ADD ALL FUNCTIONING VEHICLES BACK (but with less info)
##########################################################################
registered_vehicles <- fread("Data/Zulassungen/Zulassungen_alle_Fahrzeuge.csv", sep = ";")
registered_vehicles <- registered_vehicles %>%
  rename(Gemeinde = Gemeinden, ID_Fahrzeug=IDNummer)

#drop unused columns
registered_vehicles <- registered_vehicles %>%
  select(-V1, -Zulassung)

# Read the input geo CSV file
df_geo_data <- fread("Data/Geodaten/Geodaten_Gemeinden_v1.2_2017-08-22_TrR.csv",
                     header = FALSE, sep = ";", skip = 1)

# Replace commas with periods in the 5th and 6th columns
df_geo_data[, V5 := gsub(",", ".", V5)]
df_geo_data[, V6 := gsub(",", ".", V6)]

# Convert the 5th and 6th columns to numeric
df_geo_data[, c("V5", "V6") := lapply(.SD, as.numeric), .SDcols = c("V5", "V6")]

# Function to format the 5th and 6th columns as floating-point numbers.
# Sometimes the floatingpoint is missing, eg. 56778 instead of 56.778
format_as_float <- function(x) {
  if (is.numeric(x)) {
    return(format(x, nsmall = 6))
  } else {
    return(x)
  }
}

# Apply the format_as_float function to the 5th and 6th columns
df_geo_data$V5 <- sapply(df_geo_data$V5, format_as_float)
df_geo_data$V6 <- sapply(df_geo_data$V6, format_as_float)

# Drop the first two columns from 'df_geo_data', we don't need them
df_geo_data <- df_geo_data[, 3:6, with = FALSE]

# name the columns
colnames(df_geo_data) <- c("PLZ", "Gemeinde", "Longitude", "Latitude")

# We found out that one Gemeinden is missing in Geo_data that is
# needed for the damaged vehicles: Gemeinden SEEG. Add SEEG here
new_data <- data.frame(PLZ=87637, Gemeinde = "SEEG",
                       Longitude = 10.6085, Latitude = 47.6543)
df_geo_data <- rbind(df_geo_data, new_data)

# Merge the datasets using the 4th column as the key
registered_vehicles <- merge(registered_vehicles, df_geo_data, by.x = "Gemeinde",
                             by.y = "Gemeinde", all.x = TRUE)

# Filter out rows from additionDF where ID_Fahrzeug already exists in df_merged_data
registered_vehicles_filtered <- registered_vehicles %>%
  filter(!ID_Fahrzeug %in% df_merged_data$ID_Fahrzeug)

# Fill all other columns with "-"
registered_vehicles_filtered$ID_T02 <- "-"
registered_vehicles_filtered$ID_Motor <- "-"
registered_vehicles_filtered$Beschaedigt <- "nein"
registered_vehicles_filtered$StateCapital <- "-"
registered_vehicles_filtered$waitingTime <- -1

# make Long and Lat nummeric
registered_vehicles_filtered$Longitude <- as.numeric(registered_vehicles_filtered$Longitude)
registered_vehicles_filtered$Latitude <- as.numeric(registered_vehicles_filtered$Latitude)
df_merged_data$Longitude <- as.numeric(df_merged_data$Longitude)
df_merged_data$Latitude <- as.numeric(df_merged_data$Latitude)

# Bind the rows of df_merged_data and registered_vehicles_filtered together
combinedDF <- bind_rows(df_merged_data, registered_vehicles_filtered)

##########################################################################
#### WRITE FINAL CSV FILE
##########################################################################

write.table(combinedDF, file = "Final_dataset_group_17.csv", sep = ";", row.names = FALSE,
             quote = FALSE)

# Provide key numbers on registration: Overall number of municipalities with registered vehicles and number of those with affected vehicles
muns_registered <- length(unique(df_registrations$Gemeinde))
muns_affected <- length(unique(df_affected_vehicles$Gemeinde))


# Remove unused dataframes and variables
rm(df_registrations, df_controll_unit_t02, df_fahrzeug, df_komponenten, df_fahrzeug_teile, new_data, df_postcodes, df_waiting_lists, waitingTimes, waiting_lists)
