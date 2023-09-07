#setwd("C:/Users/Lorenz/Documents/R-Project")
install.packages("tidyverse")
install.packages("data.table")
library(data.table)
library(tidyverse)

###########################
# 
# Column Produktionsdatum is given in unix timestamp in the col Produktionsdatum_Origin_01011970
# data/Fahrzeug/Fahrzeuge_OEM2_Typ21.csv
# data/Fahrzeug/Fahrzeuge_OEM2_Typ22.csv 
#
###########################
#
# Seperated by ';':
# Fahrzeuge_OEM1_Typ12.csv
# Fahrzeuge_OEM2_Typ22.csv 
#


# Load the vehicles from OEM 1
df_fahrzeug_oem_1_type_11 <- read.csv("data/Fahrzeug/Fahrzeuge_OEM1_Typ11.csv", header = T, stringsAsFactors = F, row.names = "X")
df_fahrzeug_oem_1_type_12 <- read.csv("data/Fahrzeug/Fahrzeuge_OEM1_Typ12.csv", sep=";", header = T, stringsAsFactors = F, row.names = "X")

# Load the vehicles from OEM 2
df_fahrzeug_oem_2_type_21 <- read.csv("data/Fahrzeug/Fahrzeuge_OEM2_Typ21.csv", header = T, stringsAsFactors = F, row.names = "X")
df_fahrzeug_oem_2_type_22 <- read.csv("data/Fahrzeug/Fahrzeuge_OEM2_Typ22.csv", sep=";", header = T, stringsAsFactors = F, row.names = "X")

# Parse dates
df_fahrzeug_oem_2_type_21 <- df_fahrzeug_oem_2_type_21 %>% mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970))
df_fahrzeug_oem_2_type_22 <- df_fahrzeug_oem_2_type_22 %>% mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970))

# Drop unnessescary columns
columns_to_drop = c("Produktionsdatum_Origin_01011970", "origin")
df_fahrzeug_oem_2_type_21 <- df_fahrzeug_oem_2_type_21 %>% select(-columns_to_drop)
df_fahrzeug_oem_2_type_22 <- df_fahrzeug_oem_2_type_22 %>% select(-columns_to_drop)

# Add and type
df_fahrzeug_oem_1_type_11 <- df_fahrzeug_oem_1_type_11 %>% mutate(Typ = "11")
df_fahrzeug_oem_1_type_12 <- df_fahrzeug_oem_1_type_12 %>% mutate(Typ = "12")
df_fahrzeug_oem_2_type_21 <- df_fahrzeug_oem_2_type_21 %>% mutate(Typ = "21")
df_fahrzeug_oem_2_type_22 <- df_fahrzeug_oem_2_type_22 %>% mutate(Typ = "22")

# Cast Produktionsdatum to date
df_fahrzeug_oem_1_type_11$Produktionsdatum <- as.Date(df_fahrzeug_oem_1_type_11$Produktionsdatum)
df_fahrzeug_oem_1_type_12$Produktionsdatum <- as.Date(df_fahrzeug_oem_1_type_12$Produktionsdatum)
df_fahrzeug_oem_2_type_21$Produktionsdatum <- as.Date(df_fahrzeug_oem_2_type_21$Produktionsdatum)
df_fahrzeug_oem_2_type_22$Produktionsdatum <- as.Date(df_fahrzeug_oem_2_type_22$Produktionsdatum)

# Print the col names
colnames(df_fahrzeug_oem_1_type_11)
colnames(df_fahrzeug_oem_1_type_12)
colnames(df_fahrzeug_oem_2_type_21)
colnames(df_fahrzeug_oem_2_type_22)

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

# Drop redundant X1 column
df_fahrzeug <- df_fahrzeug %>% select(-c("X1"))
colnames(df_fahrzeug)


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

###########################
#
# Seperated by ';':
# Bestandteile_Fahrzeuge_OEM1_Typ11.csv
# Bestandteile_Fahrzeuge_OEM1_Typ12.csv
# Bestandteile_Fahrzeuge_OEM2_Typ21.csv
# Bestandteile_Fahrzeuge_OEM2_Typ22.csv
#


# Load the vehicleparts from OEM 1
df_fahrzeugteile_oem_1_type_11 <- read.csv("data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv", sep=";", header = T, stringsAsFactors = F, row.names = "X")
df_fahrzeugteile_oem_1_type_12 <- read.csv("data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ12.csv", sep=";", header = T, stringsAsFactors = F, row.names = "X")

# Load the vehicleparts from OEM 2
df_fahrzeugteile_oem_2_type_21 <- read.csv("data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ21.csv", sep=";", header = T, stringsAsFactors = F, row.names = "X")
df_fahrzeugteile_oem_2_type_22 <- read.csv("data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ22.csv", sep=";", header = T, stringsAsFactors = F, row.names = "X1")

# Print the col names
colnames(df_fahrzeugteile_oem_1_type_11)
colnames(df_fahrzeugteile_oem_1_type_12)
colnames(df_fahrzeugteile_oem_2_type_21)
colnames(df_fahrzeugteile_oem_2_type_22)


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



# Read the data with whitespace as the separator
# data <- fread("data/Einzelteil/Einzelteil_T02.txt", sep = " ")
# data <- read.table("data/Einzelteil/Einzelteil_T02.txt", header=FALSE, sep="  ", quote="", comment.char="", stringsAsFactors=FALSE, fill=TRUE)


#library(readr)
#file_str <- read_file("data/Einzelteil/Einzelteil_T02.txt")
# remove " from string
#cleaned_string <- gsub('"', '', file_str)
# split on tabs and on spaces
#lines <- strsplit(cleaned_string, "\\s+|\t+")[[1]]
# extract the colnames
# col_names_t02 = lines[[1]][1:15]
# body <- lines[[1]][16:length(lines[[1]])]

#n_col <- 15

# Create an empty data frame with appropriate column names
# df <- data.frame(matrix(lines[[1]][(elems_per_row + 1):length(lines[[1]])], ncol = elems_per_row))
#data.frame(matrix(lines, ncol=15))

#t02_df <- data.frame(matrix(NA, ncol = elements_per_row, nrow = num_rows))
#colnames(t02_df) <- paste0("Col", 1:elements_per_row)



# Print the data frame
#summary(data)
