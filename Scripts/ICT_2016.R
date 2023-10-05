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

################################################################################
################################### Load Data ##################################
################################################################################


ICT_2016 <- read_xlsx(here("Data", "Raw", "ICT_Microdati_2016.xlsx"))
ict_16 <- read_xlsx(here("Data", "Raw", "ICT_Microdati_2016.xlsx"))

vis_miss(ict_16, warn_large_data = FALSE)

ict_16 <- ict_16 %>% replace_with_na_all(condition = ~. == ".") # converting "" into NA within the D
ict_16 <- ict_16 %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the D
ict_16 <- ict_16 %>% replace_with_na_all(condition = ~. == "NA") # converting "" into NA within the D



