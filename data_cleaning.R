setwd("C:/Users/Lorenz/Documents/R-Project")
install.packages("tidyverse")
library(tidyverse)

###########################
# 
# Column Produktionsdatum is given in unix timestamp in the col Produktionsdatum_Origin_01011970
# data/Fahrzeug/Fahrzeuge_OEM2_Typ21.csv
# data/Fahrzeug/Fahrzeuge_OEM2_Typ22.csv 
#
###########################
#
# seperated by ';':
# Fahrzeuge_OEM1_Typ12.csv
# Fahrzeuge_OEM2_Typ22.csv 
#


# load the vehicles from OEM 1
df_fahrzeug_oem_1_type_11 <- read.csv("data/Fahrzeug/Fahrzeuge_OEM1_Typ11.csv", header = T, stringsAsFactors = F)
df_fahrzeug_oem_1_type_12 <- read.csv("data/Fahrzeug/Fahrzeuge_OEM1_Typ12.csv", sep=";", header = T, stringsAsFactors = F)

# load the vehicles from OEM 2
df_fahrzeug_oem_2_type_21 <- read.csv("data/Fahrzeug/Fahrzeuge_OEM2_Typ21.csv", header = T, stringsAsFactors = F)
df_fahrzeug_oem_2_type_22 <- read.csv("data/Fahrzeug/Fahrzeuge_OEM2_Typ22.csv", sep=";", header = T, stringsAsFactors = F)

# parse dates
df_fahrzeug_oem_2_type_21 <- df_fahrzeug_oem_2_type_21 %>% mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970))
df_fahrzeug_oem_2_type_22 <- df_fahrzeug_oem_2_type_22 %>% mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970))

# drop 
columns_to_drop = c("Produktionsdatum_Origin_01011970", "origin")
df_fahrzeug_oem_2_type_21 <- df_fahrzeug_oem_2_type_21 %>% select(-columns_to_drop)
df_fahrzeug_oem_2_type_22 <- df_fahrzeug_oem_2_type_22 %>% select(-columns_to_drop)

# add firm and type
df_fahrzeug_oem_1_type_12 <- df_fahrzeug_oem_1_type_12 %>% mutate(Firma = "OEM2", Typ = "12")
df_fahrzeug_oem_1_type_11 <- df_fahrzeug_oem_1_type_11 %>% mutate(Firma = "OEM1", Typ = "11")
df_fahrzeug_oem_2_type_21 <- df_fahrzeug_oem_2_type_21 %>% mutate(Firma = "OEM2", Typ = "21")
df_fahrzeug_oem_2_type_22 <- df_fahrzeug_oem_2_type_22 %>% mutate(Firma = "OEM2", Typ = "22")


# print the col names
colnames(df_fahrzeug_oem_1_type_11)
colnames(df_fahrzeug_oem_1_type_12)
colnames(df_fahrzeug_oem_2_type_21)
colnames(df_fahrzeug_oem_2_type_22)

# Check if all Dataframes have the same col names
if(!setequal(colnames(df_fahrzeug_oem_1_type_11), colnames(df_fahrzeug_oem_1_type_12)) ||
   !setequal(colnames(df_fahrzeug_oem_1_type_11), colnames(df_fahrzeug_oem_2_type_21)) ||
   !setequal(colnames(df_fahrzeug_oem_1_type_11), colnames(df_fahrzeug_oem_2_type_22))
){
  error_message <- "Fahrezeug DataFrames do not have the same Column names"
  stop(error_message)
}


