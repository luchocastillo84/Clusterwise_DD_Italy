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



ICT_2017 <- read_xlsx(here("Data", "Raw", "ICT_Microdati_2017.xlsx"))
ict_17 <- read_xlsx(here("Data", "Raw", "ICT_Microdati_2017.xlsx"))
unique(ict_17$Ateco_1)

vis_miss(ict_17, warn_large_data = FALSE)

ict_17 <- ict_17 %>% replace_with_na_all(condition = ~. == ".") # converting "" into NA within the D
ict_17 <- ict_17 %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the D
ict_17 <- ict_17 %>% replace_with_na_all(condition = ~. == "NA") # converting "" into NA within the D

count_per_sector <- ict_17 %>%
  count(Ateco_1, sort = TRUE)










