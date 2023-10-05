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

ICT_2019 <- read_xlsx(here("Data", "Raw", "ICT_Microdati_2019.xlsx"))
ict_19 <- read_xlsx(here("Data", "Raw", "ICT_Microdati_2019.xlsx"))


vis_miss(ict_19[,140:157], warn_large_data = FALSE)

ict_19 <- ict_19 %>% replace_with_na_all(condition = ~. == ".") # converting "" into NA within the D
ict_19 <- ict_19 %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the D
ict_19 <- ict_19 %>% replace_with_na_all(condition = ~. == "NA") # converting "" into NA within the D


