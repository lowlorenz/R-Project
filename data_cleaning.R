#setwd("C:/Users/Lorenz/Documents/R-Project")
#install.packages("tidyverse")
#install.packages("data.table")
library(data.table)
library(tidyverse)
library(dplyr)
library(readr)


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

file_str <- read_file("data/Einzelteil/Einzelteil_T02.txt")
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


################################################################################
#### COMBINE ALL THE DATAFRAMES 
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
  inner_join(df_registrations, by="ID_Fahrzeug")

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

# Drop the first three columns from 'df_geo_data', we don't need them
df_geo_data <- df_geo_data[, 4:6, with = FALSE]

# name the columns
colnames(df_geo_data) <- c("Gemeinde", "Longitude", "Latitude")

# We found out that one Gemeinde is missing in Geo_data that is
# needed for the damaged vehicles: Gemeinde SEEG. Add SEEG here
new_data <- data.frame(Gemeinde = "SEEG",
                       Longitude = 10.6085, Latitude = 47.6543)
df_geo_data <- rbind(df_geo_data, new_data)

##########################################################################
#### MERGE VEHICLE DAMAGE DATA AND GEO DATA
##########################################################################

# Merge the datasets using the 4th column as the key
df_merged_data <- merge(df_affected_vehicles, df_geo_data, by.x = "Gemeinde",
                     by.y = "Gemeinde", all.x = TRUE)

##########################################################################
#### WRITE FINAL CSV FILE
##########################################################################

write.table(df_merged_data, file = "Final_dataset_group_17.csv", sep = ";", row.names = FALSE,
             quote = FALSE)

# Provide key numbers on registration: Overall number of municipalities with registered vehicles and number of those with affected vehicles
muns_registered <- length(unique(df_registrations$Gemeinde))
muns_affected <- length(unique(df_affected_vehicles$Gemeinde))

print(muns_registered)
print(muns_affected)

# Remove unused dataframes
rm(df_registrations, df_controll_unit_t02, df_fahrzeug, df_komponenten, df_fahrzeug_teile)
