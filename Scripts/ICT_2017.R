# load the necessary packages
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(here)
library(git2r)
library(visdat)
library(naniar)
library(chatgpt)
library(gptstudio)
require(devtools)

# Load environment 

load(here("Scripts", "Environments",  "missing_ict_17.RData"))


################################################################################
################################### Load Data ##################################
################################################################################

# Loads ATECO classification 
ateco <- read_xlsx(here("Data", "Tags","ateco_survey.xlsx"))

ateco_b17 <- ateco %>% group_by(ateco_B17) %>% 
  summarise(details = paste(unique(details_sec1), collapse = "; "))

ateco_a17 <- ateco %>% group_by(ateco_A17) %>% 
  summarise(details = paste(unique(details_sec2), collapse = "; "))

# Loads sizes by No. of employees

size <- read_xlsx(here("Data", "Tags","size.xlsx"))

# Loads regions NUTS 1

regions <- read_xlsx(here("Data", "Tags","regions_nuts1.xlsx"))


# This line reads an Excel file "ICT_Microdati_2017.xlsx", located in the "Data" folder, inside the project directory. 
# The "here" function is used to specify the relative path to the file. 
# The data from the Excel file is stored in a new R data frame called ICT_2017.
ICT_2017 <- read_xlsx(here("Data", "Raw", "ICT_Microdati_2017.xlsx"))
ict_17 <- read_xlsx(here("Data", "Raw", "ICT_Microdati_2017.xlsx"))
unique(ict_17$Ateco_1)
vis_miss(ict_17, warn_large_data = FALSE)

ict_17 <- ict_17 %>% replace_with_na_all(condition = ~. == ".") # converting "" into NA within the D
ict_17 <- ict_17 %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the D
ict_17 <- ict_17 %>% replace_with_na_all(condition = ~. == "NA") # converting "" into NA within the D

# creating a copy of ict_14c c meaning cleaned
ict_17c <- ict_17






