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
library(finalfit)
install_github("MichelNivard/gptstudio", force = TRUE)

# Sys.setenv(OPENAI_API_KEY = "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")


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


# This line reads an Excel file "ICT_Microdati_2014.xlsx", located in the "Data" folder, inside the project directory. 
# The "here" function is used to specify the relative path to the file. 
# The data from the Excel file is stored in a new R data frame called ICT_2014.
ICT_2014 <- read_xlsx(here("Data", "Raw", "ICT_Microdati_2014.xlsx"))
ict_14 <- read_xlsx(here("Data", "Raw", "ICT_Microdati_2014.xlsx"))

vis_miss(ict_14, warn_large_data = FALSE)

ict_14 <- ict_14 %>% replace_with_na_all(condition = ~. == ".") # converting "" into NA within the D
ict_14 <- ict_14 %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the D
ict_14 <- ict_14 %>% replace_with_na_all(condition = ~. == "NA") # converting "" into NA within the D


# creating a copy of ict_14c c meaning cleaned
ict_14c <- ict_14


# delete N from the column dom1 which contains the ateco codes
# ict_14c$dom1 <- gsub("N", "", ict_14c$dom1)
unique(ict_14c$dom1) # visualize unique values

# Create a new column called 'ateco_1' in the 'ict_14c' data frame and assign it the result of the modification done to the initial 'ict_14c' data frame.
# This modification uses the dplyr package to apply 'mutate' to the data frame and add cases to the column 'ateco_1' based on the values of 'dom1' column.
# Depending on the value categories in 'dom1', 'ateco_1' adds a new letter category to that row.
# If 'dom1' is not in any of the defined categories, 'ateco_1' value will be NA.
ict_14c <- ict_14c %>%
  mutate(ateco_1 = case_when(
    dom1 %in% c("N01", "N02", "N03", "N04", "N05", "N06", "N07", "N08", "N09") ~ "C",
    dom1 %in% c("N10") ~ "D-E",
    dom1 %in% c("N11") ~ "F",
    dom1 %in% c("N12", "N13", "N14") ~ "G",
    dom1 %in% c("N15", "N16") ~ "H",
    dom1 %in% c("N17", "N18") ~ "I",
    dom1 %in% c("N19", "N20", "N21", "N22") ~ "J",
    dom1 %in% c("N23") ~ "L",
    dom1 %in% c("N24") ~ "M",
    dom1 %in% c("N25", "N26") ~ "N",
    dom1 %in% c("N27") ~ "S",
    TRUE ~ NA_character_
  ))


# Convert variables in the ict_14c dataframe
ict_14c <- ict_14c %>%
  # Convert variable 1 to character
  mutate_at(vars(1), as.character) %>%
  
  # Convert variables 2 to 78 and 84 to 85 to numeric
  mutate_at(vars(2:78, 84:85), as.numeric) %>%
  
  # Convert variables 79 to 83 and 86 to 87 to factor
  mutate_at(vars(79:83, 86:87), as.factor)


# Deleting the rows that has more than 70 obs missing
row_indices_70 <- which(rowSums(is.na(ict_14c)) > 70)
ict_14c <- ict_14c[-row_indices_70, ]

# Deleting the rows that has more than 40 obs missing
row_indices_40 <- which(rowSums(is.na(ict_14c)) > 40) 
ict_14c <- ict_14c[-row_indices_40, ]


vis_miss(ict_14c, warn_large_data = FALSE)
vis_miss(ict_14c[, 1:30], warn_large_data = FALSE)
vis_miss(ict_14c[, 31:58], warn_large_data = FALSE)
vis_miss(ict_14c[, 59:87], warn_large_data = FALSE)


# Load the variables from ICT 2014
vars_14 <- read_xlsx(here("Data", "Tags","vars_14.xlsx"),sheet = "Sheet1")

# Create a column called missing with missing percentage of each column 
vars_14$missing <- round(colMeans(is.na(ict_14c)) * 100, 2)

# vars to show in report 

vars_87 <- vars_14 %>% select(acrom, var_EN, missing)


missing_threshold <- 80  # Set the missing threshold percentage

# Create a vector with the name of the column containing missing values
most_missing_14 <- vars_14$acrom[vars_14$missing > missing_threshold]

# Remove the vars that contain more than 80% missing values
ict_14c <- ict_14c[, !names(ict_14c) %in% most_missing_14]

vis_miss(ict_14c, warn_large_data = FALSE)
vis_miss(ict_14c[, 1:30], warn_large_data = FALSE)
vis_miss(ict_14c[, 31:58], warn_large_data = FALSE)
vis_miss(ict_14c[, 59:87], warn_large_data = FALSE)

n_miss(ict_14c)
prop_miss(ict_14c) *100

gg_miss_fct(ict_14c, fct = clad4)



ict_1 <- ict_14c %>% dplyr::filter(dom4 == "SI_ICT")
vis_miss(ict_1, warn_large_data = FALSE)
ict_0 <- ict_14c %>% dplyr::filter(dom4 == "NO_ICT")
vis_miss(ict_0, warn_large_data = FALSE)


na_percentage <- colMeans(is.na(ict_14c)) * 100

vis_miss(ict_14c, warn_large_data = FALSE)
vis_miss(ict_14c[, 1:30], warn_large_data = FALSE)
vis_miss(ict_14c[, 31:58], warn_large_data = FALSE)
vis_miss(ict_14c[, 59:77], warn_large_data = FALSE)

###############################################################################
################################## WEBSITE USE ################################
###############################################################################

# Subset data for rows where any of the seven variables have missing values
missing_subset <- ict_14c %>% 
  filter(is.na(C8a) | is.na(C8b) | is.na(C8c) | is.na(C8d) | is.na(C8e) | is.na(C8f) | is.na(C8g) | is.na(C8h) | is.na(C8i))

# Descriptive statistics for economic sector and region in the subset
table(missing_subset$dom1)
table(missing_subset$rip)

# Visualization for economic sector of missing cases
ggplot(missing_subset, aes(x = dom1)) + 
  geom_bar() + 
  ggtitle("Economic Sectors of Missing Cases") + 
  xlab("Economic Sector") + 
  ylab("Count")

# Visualization for regions of missing cases
ggplot(missing_subset, aes(x = rip)) + 
  geom_bar() + 
  ggtitle("Regions of Missing Cases") + 
  xlab("Region") + 
  ylab("Count")
# This code will help you identify any patterns or concentrations in the dom1 and rip variables among the observations with missing values in the seven ICT variables. The bar plots will visually present the distribution of missing cases across different sectors and regions.
# 
# If there are specific sectors or regions that have a noticeably higher count of missing cases, it could suggest a relationship between those characteristics and the missingness in the ICT variables.


# Melt the data
long_data <- ict_14c %>%
  select(dom1, clad4, rip, C8a, C8b, C8c, C8d, C8e, C8f, C8g, C8h, C8i) %>%  # add clad4
  pivot_longer(cols = -c(dom1, clad4, rip), names_to = "variable", values_to = "value")

long_data$clad4 <- factor(long_data$clad4, 
                          levels = c("cl1", "cl2", "cl3", "cl4"),
                          labels = c("Micro", "Small", "Medium", "Large"))

long_data$rip <- factor(long_data$rip, 
                          levels = c("ITC", "ITF", "ITG", "ITH", "ITI"),
                          labels = c("Northwest", "South", "Iisland", "Northeast", "Center"))



# Categorize the values
long_data$categorized <- case_when(
  is.na(long_data$value) ~ "Missing",
  long_data$value == 1 ~ "1",
  long_data$value == 0 ~ "0",
  TRUE ~ as.character(long_data$value)
)

# Plot with facets by company size
ggplot(long_data, aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Missing Data Distribution for Website Services by Sector, Segmented by Size", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_wrap(~ clad4, ncol = 1, scales = "free_x") +  # adjust ncol based on how many panels you want per row
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot with facets by company regions
ggplot(long_data, aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Missing Data Distribution for Website Services by Sector, Segmented by Region", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_wrap(~ rip, ncol = 1, scales = "free_x") +  # adjust ncol based on how many panels you want per row
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Summarize the data
summary_data <- long_data %>%
  group_by(dom1, clad4, categorized) %>%
  tally()

# Spread the data for table display
table_data <- summary_data %>%
  pivot_wider(names_from = categorized, values_from = n, values_fill = 0)

# Display the table with gt
table_display <- table_data %>%
  gt() %>%
  tab_header(
    title = "Count of Missing, 1s, and 0s by Economic Sector and Company Size"
  )

table_display



###############################################################################
################################## CLOUD COMPUTING USE ########################
###############################################################################



# Subset data for rows where any of the seven variables have missing values
missing_subset1 <- ict_14c %>% 
  filter(is.na(D2a) | is.na(D2b) | is.na(D2c) | is.na(D2d) | is.na(D2e) | is.na(D2f) | is.na(D2g))

# Descriptive statistics for economic sector and region in the subset
table(missing_subset1$dom1)
table(missing_subset1$rip)

# Visualization for economic sector of missing cases
ggplot(missing_subset1, aes(x = dom1)) + 
  geom_bar() + 
  ggtitle("Economic Sectors of Missing Cases") + 
  xlab("Economic Sector") + 
  ylab("Count")

# Visualization for regions of missing cases
ggplot(missing_subset1, aes(x = clad4)) + 
  geom_bar() + 
  ggtitle("Sizes of Missing Cases") + 
  xlab("sizes") + 
  ylab("Count")
# This code will help you identify any patterns or concentrations in the dom1 and rip variables among the observations with missing values in the seven ICT variables. The bar plots will visually present the distribution of missing cases across different sectors and regions.
# 
# If there are specific sectors or regions that have a noticeably higher count of missing cases, it could suggest a relationship between those characteristics and the missingness in the ICT variables.


# Melt the data
long_data1 <- ict_14c %>%
  select(dom1, D2a, D2a, D2b, D2c, D2d, D2e, D2g, clad4) %>%  # select relevant columns
  pivot_longer(cols = -dom1, names_to = "variable", values_to = "value")

# Categorize the values
long_data1$categorized <- case_when(
  is.na(long_data1$value) ~ "Missing",
  long_data1$value == 1 ~ "1",
  long_data1$value == 0 ~ "0",
  TRUE ~ as.character(long_data1$value)
)

# Plot
ggplot(long_data1, aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Missing, 1s, and 0s by Sector", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Continue from previous transformations
summary_data1 <- long_data1 %>%
  group_by(dom1, categorized) %>%
  tally()

# Spread the data for a wide format suitable for table display
table_data1 <- summary_data1 %>%
  pivot_wider(names_from = categorized1, values_from = n, values_fill = 0)

# Display table with gt
table_data1 %>%
  gt() %>%
  tab_header(
    title = "Count of Missing, 1s, and 0s by Economic Sector"
  )


# Melt the data
long_data1 <- ict_14c %>%
  select(dom1, clad4, rip, D2a, D2a, D2b, D2c, D2d, D2e, D2g,
         D3a, D3b) %>%  # add clad4
  pivot_longer(cols = -c(dom1, clad4, rip), names_to = "variable", values_to = "value")


long_data1$clad4 <- factor(long_data1$clad4, 
                          levels = c("cl1", "cl2", "cl3", "cl4"),
                          labels = c("Micro", "Small", "Medium", "Large"))

long_data1$rip <- factor(long_data1$rip, 
                        levels = c("ITC", "ITF", "ITG", "ITH", "ITI"),
                        labels = c("Northwest", "South", "Iisland", "Northeast", "Center"))


# Categorize the values
long_data1$categorized <- case_when(
  is.na(long_data1$value) ~ "Missing",
  long_data1$value == 1 ~ "1",
  long_data1$value == 0 ~ "0",
  TRUE ~ as.character(long_data1$value)
)

# Plot with facets by company size
ggplot(long_data1, aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Missing, 1s, and 0s by Sector", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_wrap(~ clad4, ncol = 1, scales = "free_x") +  # adjust ncol based on how many panels you want per row
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot with facets by company size
ggplot(long_data1, aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Missing, 1s, and 0s by Sector", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_wrap(~ rip, ncol = 1, scales = "free_x") +  # adjust ncol based on how many panels you want per row
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







# Summarize the data
summary_data <- long_data %>%
  group_by(dom1, clad4, categorized) %>%
  tally()

# Spread the data for table display
table_data <- summary_data %>%
  pivot_wider(names_from = categorized, values_from = n, values_fill = 0)

# Display the table with gt
table_display <- table_data %>%
  gt() %>%
  tab_header(
    title = "Count of Missing, 1s, and 0s by Economic Sector and Company Size"
  )

table_display



miss_by_sector <- ict_14c %>% group_by(dom1,clad4) %>% miss_var_summary()





