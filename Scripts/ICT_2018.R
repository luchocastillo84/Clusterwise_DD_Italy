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


ICT_2018 <- read_xlsx(here("Data", "Raw", "ICT_Microdati_2018.xlsx"))
ict_18 <- read_xlsx(here("Data", "Raw", "ICT_Microdati_2018.xlsx"))
unique(ict_18$Ateco_1)
vis_miss(ict_18, warn_large_data = FALSE)

ict_18 <- ict_18 %>% replace_with_na_all(condition = ~. == ".") # converting "" into NA within the D
ict_18 <- ict_18 %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the D
ict_18 <- ict_18 %>% replace_with_na_all(condition = ~. == "NA") # converting "" into NA within the D

count_per_sector <- ict_18 %>%
  count(Ateco_1, sort = TRUE)
