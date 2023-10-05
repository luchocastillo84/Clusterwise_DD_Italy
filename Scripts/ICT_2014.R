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
install_github("MichelNivard/gptstudio", force = TRUE)

# Sys.setenv(OPENAI_API_KEY = "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")


################################################################################
################################### Load Data ##################################
################################################################################

# Loads ATECO classification 
ateco <- read_xlsx(here("Data", "Tags","Struttura-ATECO-2007.xlsx"))
# Renames the code column
ateco <- ateco %>% rename(code_ateco = "Codice\r\nAteco 2007")

# Filters ATECO classification in letters
ateco_21 <- ateco[ateco$code_ateco %in% grep("^[A-Za-z]+$", ateco$code_ateco, value = TRUE), ]
# Filters ATECO classification in one digit
ateco_27 <- ateco[grepl("^(0[1-9]|1\\d|2[0-7])$", ateco$code_ateco), ]
ateco_2_digits <- ateco[grepl("^\\d{2}$", ateco$code_ateco), ]

# This line reads an Excel file "ICT_Microdati_2014.xlsx", located in the "Data" folder, inside the project directory. 
# The "here" function is used to specify the relative path to the file. 
# The data from the Excel file is stored in a new R data frame called ICT_2014.
ICT_2014 <- read_xlsx(here("Data", "Raw", "ICT_Microdati_2014.xlsx"))
ict_14 <- read_xlsx(here("Data", "Raw", "ICT_Microdati_2014.xlsx"))

vis_miss(ict_14, warn_large_data = FALSE)

ict_14 <- ict_14 %>% replace_with_na_all(condition = ~. == ".") # converting "" into NA within the D
ict_14 <- ict_14 %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the D
ict_14 <- ict_14 %>% replace_with_na_all(condition = ~. == "NA") # converting "" into NA within the D

# ict_14 <- ict_14 %>%
#   mutate_all(~ case_when(
#     . == "."   ~ NA_character_,
#     . == ""    ~ NA_character_,
#     . == "NA"  ~ NA_character_,
#     TRUE       ~ .
#   ))


ict_14c <- ict_14

# delete N from the column dom1 which contains the ateco codes
ict_14c$dom1 <- gsub("N", "", ict_14c$dom1)
unique(ict_14c$dom1) # visualize unique values

# Create a new column called 'ateco_1' in the 'ict_14c' data frame and assign it the result of the modification done to the initial 'ict_14c' data frame.
# This modification uses the dplyr package to apply 'mutate' to the data frame and add cases to the column 'ateco_1' based on the values of 'dom1' column.
# Depending on the value categories in 'dom1', 'ateco_1' adds a new letter category to that row.
# If 'dom1' is not in any of the defined categories, 'ateco_1' value will be NA.
ict_14c <- ict_14c %>%
  mutate(ateco_1 = case_when(
    dom1 %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09") ~ "C",
    dom1 %in% c("10") ~ "D",
    dom1 %in% c("11") ~ "F",
    dom1 %in% c("12", "13", "14") ~ "G",
    dom1 %in% c("15", "16") ~ "H",
    dom1 %in% c("17", "18") ~ "I",
    dom1 %in% c("19", "20", "21", "22") ~ "J",
    dom1 %in% c("23") ~ "L",
    dom1 %in% c("24") ~ "M",
    dom1 %in% c("25", "26") ~ "N",
    dom1 %in% c("27") ~ "O",
    TRUE ~ NA_character_
  ))

# Deleting the rows that has more than 70 obs missing
row_indices_70 <- which(rowSums(is.na(ict_14c)) > 70)
ict_14c <- ict_14c[-row_indices_70, ]

# Deleting the rows that has more than 40 obs missing
row_indices_40 <- which(rowSums(is.na(ict_14c)) > 40) 
ict_14c <- ict_14c[-row_indices_40, ]
vis_miss(ict_14c, warn_large_data = FALSE)

vis_miss(ict_14c, warn_large_data = FALSE)
vis_miss(ict_14c[, 1:30], warn_large_data = FALSE)
vis_miss(ict_14c[, 31:58], warn_large_data = FALSE)
vis_miss(ict_14c[, 59:87], warn_large_data = FALSE)


# Load the variables from ICT 2014
vars_14 <- read_xlsx(here("Data", "Tags","vars_14.xlsx"),sheet = "Sheet1")

# Create a column called missing with missing percentage of each column 
vars_14$missing <- round(colMeans(is.na(ict_14c)) * 100, 2)

missing_threshold <- 80  # Set the missing threshold percentage

# Create a vector with the name of the column containing missing values
most_missing_14 <- vars_14$acrom[vars_14$missing > missing_threshold]

# Remove the vars that contain more than 80% missing values
ict_14c <- ict_14c[, !names(ict_14c) %in% most_missing_14]

vis_miss(ict_14c, warn_large_data = FALSE)
vis_miss(ict_14c[, 1:30], warn_large_data = FALSE)
vis_miss(ict_14c[, 31:58], warn_large_data = FALSE)
vis_miss(ict_14c[, 59:77], warn_large_data = FALSE)







ict_1 <- ict_14c %>% dplyr::filter(dom4 == "SI_ICT")
vis_miss(ict_1, warn_large_data = FALSE)
ict_0 <- ict_14c %>% dplyr::filter(dom4 == "NO_ICT")
vis_miss(ict_0, warn_large_data = FALSE)


na_percentage <- colMeans(is.na(ict_14c)) * 100

vis_miss(ict_14c, warn_large_data = FALSE)
vis_miss(ict_14c[, 1:30], warn_large_data = FALSE)
vis_miss(ict_14c[, 31:58], warn_large_data = FALSE)
vis_miss(ict_14c[, 59:77], warn_large_data = FALSE)












