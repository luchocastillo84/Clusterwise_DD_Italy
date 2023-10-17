#### 0. Load the necessary packages ####
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(here)
library(git2r)
library(visdat)
library(ggplot2)
library(naniar)
library(chatgpt)
library(gptstudio)
require(devtools)
library(finalfit)
library(mice)
library(gridExtra)
library(FactoMineR)
library(factoextra)
library(ggmice)
library(ggmcmc)
library(nnet)
library(caret)
library(pROC)
library(fastDummies)

install_github("MichelNivard/gptstudio", force = TRUE)

##### 0.1. Load environment #####

load(here("Scripts", "Environments",  "missing_ict_14.RData"))



#### 1. Load Data ####

##### 1.1. Loads ATECO classification ####
ateco <- read_xlsx(here("Data", "Tags","ateco_survey.xlsx"))

ateco_b17 <- ateco %>% group_by(ateco_B17) %>% 
  summarise(details = paste(unique(details_sec1), collapse = "; "))

ateco_a17 <- ateco %>% group_by(ateco_A17) %>% 
  summarise(details = paste(unique(details_sec2), collapse = "; "))

##### 1.1. Loads sizes by No. of employees ##### 

size <- read_xlsx(here("Data", "Tags","size.xlsx"))

##### 1.2. Loads regions NUTS 1 ##### 

regions <- read_xlsx(here("Data", "Tags","regions_nuts1.xlsx"))

##### 1.3. Load the ICT usage survey ##### 
# This line reads an Excel file "ICT_Microdati_2014.xlsx", located in the "Data" folder, inside the project directory. 
# The "here" function is used to specify the relative path to the file. 
# The data from the Excel file is stored in a new R data frame called ICT_2014.
ICT_2014 <- read_xlsx(here("Data", "Raw", "ICT_Microdati_2014.xlsx"))
ict_14 <- read_xlsx(here("Data", "Raw", "ICT_Microdati_2014.xlsx"))

###### 1.3.1 Visualize data,  uncover NAs and add ateco17  ######

vis_miss(ict_14, warn_large_data = FALSE)

ict_14 <- ict_14 %>% replace_with_na_all(condition = ~. == ".") # converting "" into NA within the D
ict_14 <- ict_14 %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the D
ict_14 <- ict_14 %>% replace_with_na_all(condition = ~. == "NA") # converting "" into NA within the D


# creating a copy of ict_14c c meaning cleaned

ict_14c <- ict_14

# Grouping by 'clad4', 'dom1', and 'region'
levels_clad4 = c("cl1", "cl2", "cl3", "cl4")
labels_clad4 = c("Micro", "Small", "Medium", "Large")

ict_14c %>%
  group_by(clad4) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = factor(clad4, levels = levels_clad4, labels = labels_clad4), y = count)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-0.5) +
  labs(title = "Observations per Company Size", x = "Company Size", y = "Count") +
  theme_minimal()



ict_14c %>%
  group_by(ateco_1) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = ateco_1, y = count)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-0.5) +
  labs(title = "Observations per Secors", x = "Sectors", y = "Count") +
  theme_minimal()


levels_rip4 = c("ITC", "ITF", "ITG", "ITH", "ITI")
labels_rip4 = c("Northwest", "South", "Iisland", "Northeast", "Center")

ict_14c %>%
  group_by(rip) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = factor(rip, levels = levels_rip4, labels = labels_rip4), y = count)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-0.5) +
  labs(title = "Observations per Region", x = "Region", y = "Count") +
  theme_minimal()


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


options(scipen=999)
ict_14c$Revenue_K <- ict_14c$Ricavi/1000

###### 1.3.2. Remove variables with high missingness #####

# Convert variables in the ict_14c dataframe
ict_14c <- ict_14c %>%
  # Convert variable 1 to character
  mutate_at(vars(1), as.character) %>%
  
  # Convert variables 2 to 78 and 84 to 85 to numeric
  mutate_at(vars(4,13,18, 64:68, 71,72, 76,84,85), as.numeric) %>%
  
  # Convert variables 79 to 83 and 86 to 87 to factor
  mutate_at(vars(2,3,5:12,19:63, 69:70, 73:75, 77:83, 86:88), as.factor)


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

ict_14c0 <- ict_14c


###### 1.3.3. One-hot encoding of variables####

ict_14c <- dummy_cols(ict_14c, select_columns = c("dom1", "clad4", 
                                                  "rip","Ricavi"), 
                      remove_selected_columns = F, remove_first_dummy = T)

ict_14c[,c(79:124)] <- lapply(ict_14c[,c(79:124)], as.factor)




#### 2. ONE VAR ANALYSIS #####

#ict_14c_no_missing1
# ict_14c

# Melt the data
long_var <- ict_14c %>%
  select(Revenue_K, dom1, clad4, rip, C4) %>%  # add clad4
  pivot_longer(cols = -c(Revenue_K, dom1, clad4, rip), names_to = "variable", values_to = "value")

long_var$clad4 <- factor(long_var$clad4, 
                          levels = c("cl1", "cl2", "cl3", "cl4"),
                          labels = c("Micro", "Small", "Medium", "Large"))

long_var$rip <- factor(long_var$rip, 
                        levels = c("ITC", "ITF", "ITG", "ITH", "ITI"),
                        labels = c("Northwest", "South", "Iisland", "Northeast", "Center"))



# Categorize the values
long_var$categorized <- case_when(
  is.na(long_var$value) ~ "Missing",
  long_var$value == 1 ~ "1",
  long_var$value == 0 ~ "0",
  TRUE ~ as.character(long_var$value)
)

# Plot with facets by company size
ggplot(long_var, aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Missing", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_wrap(~ clad4, ncol = 1, scales = "free_x") +  # adjust ncol based on how many panels you want per row
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






#### 3. WEBSITE USE ####


# This code will help you identify any patterns or concentrations in the dom1 
# and rip variables among the observations with missing values in the seven ICT 
# variables. The bar plots will visually present the distribution of missing 
# cases across different sectors and regions.
# 
# If there are specific sectors or regions that have a noticeably higher count 
# of missing cases, it could suggest a relationship between those characteristics 
# and the missingness in the ICT variables.

# Companies services offered by the website.  
# Ricavi. Is the revenue by levels. There are 14 levels
# dom4. Represent the 27 economic sectors Ateco before 2017
# rip. Represent the 5 regions in Italy NUTS 1
# C8a. Possibility to place orders or reservations online (e.g. online shopping cart)
# C8b. Online traceability of the order
# C8c. Access to product catalogues or price lists
# C8d. Ability to customize site content for regular visitors
# C8e. Ability for site visitors to customize or design products
# C8f. Privacy Policy Notices, Privacy Certification Mark, or Site Security Certification
# C8g. Announcement of job vacancies or possibility of making job applications online
# C8h. Links or references to the company's social media profiles
# C8i. Possibility to submit complaints online (via email, web form, etc.)

##### 3.1.  Melt the data WS #####
long_data <- ict_14c %>%
  select(Revenue_K, dom1, clad4, rip, C8a, C8b, C8c, C8d, C8e, C8f, C8g, C8h, C8i) %>%  # add clad4
  pivot_longer(cols = -c(Revenue_K, dom1, clad4, rip), names_to = "variable", values_to = "value")

# Changing the name of the size of compnanies 
long_data$clad4 <- factor(long_data$clad4, 
                          levels = c("cl1", "cl2", "cl3", "cl4"),
                          labels = c("Micro", "Small", "Medium", "Large"))

# Changing the name of the regions in Italy
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

###### 3.1.1. Plot by size ###### 

# Plot with facets by company size
ggplot(long_data, aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Missing Data Distribution for Website Services by Sector, Segmented by Size", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_wrap(~ clad4, ncol = 1, scales = "free_x") +  # adjust ncol based on how many panels you want per row
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###### 3.1.2. Plot by region #####

# Plot with facets by company regions
ggplot(long_data, aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Missing Data Distribution for Website Services by Sector, Segmented by Region", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_wrap(~ rip, ncol = 1, scales = "free_x") +  # adjust ncol based on how many panels you want per row
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###### 3.1.3. Plot by revenue  #####

ggplot(long_data, aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Missing, 1s, and 0s by Sector", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_grid(Revenue_K ~ clad4, scales = "free_x") +  # Using facet_grid to facet by both revenue and company size
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




#### 4. CLOUD COMPUTING USE #####



# This code will help you identify any patterns or concentrations in the dom1 
# and rip variables among the observations with missing values in the seven ICT 
# variables. The bar plots will visually present the distribution of missing 
# cases across different sectors and regions.
# 
# If there are specific sectors or regions that have a noticeably higher count 
# of missing cases, it could suggest a relationship between those characteristics 
# and the missingness in the ICT variables.


##### 4.1. Melt the data CC ####
long_data1 <- ict_14c %>%
  select(Revenue_K, dom1, clad4, rip,  D2a, D2b, D2c, D2d, D2e, D2f, D2g,
         D3a, D3b) %>%  # add clad4
  pivot_longer(cols = -c(Revenue_K, dom1, clad4, rip), names_to = "variable", values_to = "value")


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

###### 4.1.1. Plot by size  ####

# Plot with facets by company size
ggplot(long_data1, aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Missing, 1s, and 0s by Sector", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_wrap(~ clad4, ncol = 1, scales = "free_x") +  # adjust ncol based on how many panels you want per row
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###### 4.1.2. Plot by region  ####

# Plot with facets by  region
ggplot(long_data1, aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Missing, 1s, and 0s by Sector", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_wrap(~ rip, ncol = 1, scales = "free_x") +  # adjust ncol based on how many panels you want per row
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###### 4.1.3. Plot by revenue  ####

# Plot with facets by  revenue level
ggplot(long_data1, aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Missing, 1s, and 0s by Sector", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_grid(Revenue_K ~ clad4, scales = "free_x") +  # Using facet_grid to facet by both revenue and company size
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### 4.2. Cloud computing category of benefits to companies ###################


# Melt the data
long_data1a <- ict_14c %>%
  select(Revenue_K, dom1, clad4, rip, D4a, D4b, D4c) %>%  # add clad4
  pivot_longer(cols = -c(Revenue_K, dom1, clad4, rip), names_to = "variable", values_to = "value")


long_data1a$clad4 <- factor(long_data1a$clad4, 
                           levels = c("cl1", "cl2", "cl3", "cl4"),
                           labels = c("Micro", "Small", "Medium", "Large"))

long_data1a$rip <- factor(long_data1a$rip, 
                         levels = c("ITC", "ITF", "ITG", "ITH", "ITI"),
                         labels = c("Northwest", "South", "Iisland", "Northeast", "Center"))


# Categorize the values
long_data1a$categorized <- case_when(
  long_data1a$value == 1 ~ "High benefit",
  long_data1a$value == 2 ~ "Medium benefit",
  long_data1a$value == 3 ~ "Low benefit",
  long_data1a$value == 4 ~ "No benefit",
  is.na(long_data1a$value) ~ "Missing",
  TRUE ~ as.character(long_data1a$value)
)

# Plot with facets by company size
ggplot(long_data1a, aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Missing, 1s, and 0s by Sector", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_wrap(~ clad4, ncol = 1, scales = "free_x") +  # adjust ncol based on how many panels you want per row
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




ggplot(long_data1a, aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Missing, 1s, and 0s by Sector", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_grid(Revenue_K ~ clad4, scales = "free_x") +  # Using facet_grid to facet by both revenue and company size
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Plot with facets by company size
ggplot(long_data1a, aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Missing, 1s, and 0s by Sector", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_wrap(~ rip, ncol = 1, scales = "free_x") +  # adjust ncol based on how many panels you want per row
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#### 5.TYPE OF INVOICES  #####

##### 5.1. Melt the data TI ####

# I2a. Percentage of invoices sent to other companies or PAs in standard electronic 
#     format suitable for automatic data processing (e.g. EDI, UBL, XML)	29.55
# I2b. Percentage of invoices sent to other businesses or PAs in electronic 
#     format not suitable for automatic processing (e.g. email, email attachments in PDF format)	29.55
# I2c	Percentage of invoices sent to other companies or PAs in paper format	29.55

# Melting data for better visualization
# Categorize the data into "Missing" and "Complete"
long_data2 <- ict_14c %>%
  select(Revenue_K, dom1, clad4, rip, I2a, I2b, I2c) %>%
  pivot_longer(cols = -c(Revenue_K, dom1, clad4, rip), names_to = "variable", values_to = "value") %>%
  mutate(categorized = ifelse(is.na(value), "Missing", "Complete"))


long_data2$clad4 <- factor(long_data2$clad4, 
                            levels = c("cl1", "cl2", "cl3", "cl4"),
                            labels = c("Micro", "Small", "Medium", "Large"))

long_data2$rip <- factor(long_data2$rip, 
                          levels = c("ITC", "ITF", "ITG", "ITH", "ITI"),
                          labels = c("Northwest", "South", "Iisland", "Northeast", "Center"))

# long_data2$variable <- factor(long_data2$variable, 
#                          levels = c("I2a", "I2b", "I2c"),
#                          labels = c("Auto_Process_Invo", "Manual_e_Invo", "Paper_Invo"))


###### 5.1.1. Plot by size I2a  ####
# Plot with facets by company size
long_data2 %>% filter(variable == "I2a") %>%  ggplot(aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Auto-Process Invoices by Sector and size", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_wrap(~ clad4 + variable, ncol = 1, scales = "free_x") +  # facet by both size and variable
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###### 5.1.2. Plot by size I2b  ####

# Plot with facets by company size
long_data2 %>% filter(variable == "I2b") %>%  ggplot(aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Manual e-Invoices by Sector and size", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_wrap(~ clad4 + variable, ncol = 1, scales = "free_x") +  # facet by both size and variable
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###### 5.1.3. Plot by size I2c  ####

# Plot with facets by company size
long_data2 %>% filter(variable == "I2c") %>%  ggplot(aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of paper in oinvoices by Sector and size", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_wrap(~ clad4 + variable, ncol = 1, scales = "free_x") +  # facet by both size and variable
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###### 5.1.4. Plot by region I2a  ####

# Plot with facets by company Region
long_data2 %>% filter(variable == "I2a") %>%  ggplot(aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Auto-Process Invoices by Sector and size", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_wrap(~ rip + variable, ncol = 1, scales = "free_x") +  # facet by both size and variable
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###### 5.1.5. Plot by region I2b  ####

# Plot with facets by company Region
long_data2 %>% filter(variable == "I2b") %>%  ggplot(aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Manual e-Invoices by Sector and size", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_wrap(~ rip + variable, ncol = 1, scales = "free_x") +  # facet by both size and variable
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###### 5.1.6. Plot by region I2c  ####

# Plot with facets by company Region
long_data2 %>% filter(variable == "I2c") %>%  ggplot(aes(x = dom1, fill = categorized)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of paper invoices by Sector and size", 
       x = "Economic Sector", y = "Count", fill = "Category") +
  facet_wrap(~ rip + variable, ncol = 1, scales = "free_x") +  # facet by both size and variable
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#### 6. IMPUTATON #####


##### 6.1 WEBSITE VARIABLES #####


# dom1, clad4, rip, C8a, C8b, C8c, C8d, C8e, C8f, C8g, C8h, C8i

# Companies services offered by the website.
# C8a. Possibility to place orders or reservations online (e.g. online shopping cart)
# C8b. Online traceability of the order
# C8c. Access to product catalogues or price lists
# C8d. Ability to customize site content for regular visitors
# C8e. Ability for site visitors to customize or design products
# C8f. Privacy Policy Notices, Privacy Certification Mark, or Site Security Certification
# C8g. Announcement of job vacancies or possibility of making job applications online
# C8h. Links or references to the company's social media profiles
# C8i. Possibility to submit complaints online (via email, web form, etc.)

# B5a Using IT specialists who are part of the business group
# B1 	Employment of IT specialists
# C9c SM usage by type: multimedia content sharing websites

###### 6.1.1. Model 1: Imputation with logreg #####

# Converting as factor the variables to impute
# ict_14c[,c(19:27)] <- lapply(ict_14c[,c(19:27)], as.factor)

var1 <- "J1"

# Choosing only the related variables to website use to impute

imp_model_ws1 <- mice(ict_14c[,c("C8a", "C8b", "C8c", "C8d", 
                                 "C8e", "C8f", "C8g", "C8h", 
                                 "C8i", "B1", "B5a", "C9c", 
                                 grep("dom1_", names(ict_14c), value = TRUE),
                                 grep("clad4_", names(ict_14c), value = TRUE),
                                 grep("rip_", names(ict_14c), value = TRUE))],
                      method = "logreg", 
                      m = 5, # Number of imputed datasets. 5 is a common choice.
                      maxit = 15) # Number of iterations

plot_trace(imp_model_ws1)

# Load the necessary library
library(mice)

# Specify the imputation method for each variable. 
# If 'C4' is the only variable with missing values, we specify its method. 
# Variables not named in the method list will not be imputed.
method_list <- make.method(ict_14c[, c("C8a", "C8b", "C8c", "C8d", 
                                       "C8e", "C8f", "C8g", "C8h", 
                                       "C8i", "B1", "B5a", "C9c", 
                                       grep("dom1_", names(ict_14c), value = TRUE),
                                       grep("clad4_", names(ict_14c), value = TRUE),
                                       grep("rip_", names(ict_14c), value = TRUE))])
variables <- c("C8a", "C8b", "C8c", "C8d", 
               "C8e", "C8f", "C8g", "C8h", 
               "C8i")

for (var in variables) {
  method_list[[var]] <- "logreg"
}

# Set predictorMatrix to default to start, can refine predictors later
predictorMatrix <- make.predictorMatrix(ict_14c[, c("C8a", "C8b", "C8c", "C8d", 
                                                    "C8e", "C8f", "C8g", "C8h", 
                                                    "C8i", "B1", "B5a", "C9c", 
                                                grep("dom1_", names(ict_14c), value = TRUE),
                                                grep("clad4_", names(ict_14c), value = TRUE),
                                                grep("rip_", names(ict_14c), value = TRUE))])


# Perform the imputation
imputed_data <- mice(ict_14c, method=method_list, predictorMatrix=predictorMatrix, m=5)

# Check the imputed data
summary(imputed_data)

# You can also complete the data and use it for analysis
completed_data <- complete(imputed_data, 1) #using the first imputed dataset as an example



plot_trace(imp_model_ws1)

# You can change action to view other imputed datasets
completed_data_ws1 <- complete(imp_model_ws1, action = 1) 


###### 6.1.2. Model 2: Imputation with pmm #####

# # Choosing only the related variables to website use to impute with other covarietes
# such size, sector, and region
# dom1 economic sectors
# clad4 size by No. of employees
# rip regions NUTS 1 for Italy


imp_model_ws2 <- mice(ict_14c[,c("C8a", "C8b", "C8c", "C8d", 
                                 "C8e", "C8f", "C8g", "C8h", 
                                 "C8i", "B1", "B5a", "C9c", 
                                 grep("dom1_", names(ict_14c), value = TRUE),
                                 grep("clad4_", names(ict_14c), value = TRUE),
                                 grep("rip_", names(ict_14c), value = TRUE))],
                      method = "pmm", 
                      m = 5, # Number of imputed datasets. 5 is a common choice.
                      maxit = 15) # Number of iterations



plot_trace(imp_model_ws2)

# You can change action to view other imputed datasets
completed_data_ws2 <- complete(imp_model_ws2, action = 1) 


###### 6.1.3. Model 3: Imputation with cart #####

# # Choosing only the related variables to website use to impute with other covarietes
# such size, sector, and region, access to computer and use of social media
### Covariates
# dom1 economic sectors
# clad4 size by No. of employees
# rip regions NUTS 1 for Italy
# A2 Percentage of employees use computers
# B1 Employment of IT specialists
# C9c SM usage by type: multimedia content sharing websites (e.g. YouTube, Flickr, Picasa, SlideShare)

imp_model_ws3 <- mice(ict_14c[,c("C8a", "C8b", "C8c", "C8d", 
                                 "C8e", "C8f", "C8g", "C8h", 
                                 "C8i", "B1", "B5a", "C9c", 
                                 grep("dom1_", names(ict_14c), value = TRUE),
                                 grep("clad4_", names(ict_14c), value = TRUE),
                                 grep("rip_", names(ict_14c), value = TRUE))],
                      method = "cart",
                      m = 5, maxit = 15) # Number of imputed datasets. 5 is a common choice.



plot_trace(imp_model_ws3)




###### 6.1.4. Bar plot panel by variable  #####

# C8a, C8b, C8c, C8d, C8e, C8f, C8g, C8h, C8i
var0 <- "C7"

plot_original <- ggplot(ict_14c, aes(x = !!sym(var0))) + geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  ggtitle("Original Data")

plot_imputed1 <- ggplot(completed_data1, aes(x = !!sym(var0))) + geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  ggtitle("Imputed Data model1")

plot_imputed2 <- ggplot(completed_data2, aes(x = !!sym(var0))) + geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  ggtitle("Imputed Data model2")

plot_imputed3 <- ggplot(completed_data3, aes(x = !!sym(var0))) + geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  ggtitle("Imputed Data model3")




grid.arrange(plot_original, plot_imputed1, plot_imputed2, plot_imputed3, ncol=2)


##### 6.1.5. IMPUTATION DIAGNOSTICS WS #####

###### 6.1.6. Model with original data ######
var1 <- "J1"
# Running the model with no missing values using data without imputing
ict_14c_no_missing <- na.omit(ict_14c[, c(var1, "C8a", "C8b", "C8c", 
                                          "C8d", "C8e", "C8f", "C8g", 
                                          "C8h", "C8i")])

# Run the logistic regression model on the data without missing values
ws_org1 <- glm(ict_14c_no_missing[[var1]] ~ C8a + C8b + C8c + C8d + C8e + C8f + C8g + C8h + C8i, 
               data = ict_14c_no_missing, 
               family = binomial())

# Display the summary of the model
summary(ws_org1)

# Predict using your models
predicted_prob_org_ws<- predict(ws_org1, type = "response")
predicted_class_org_ws <- ifelse(predicted_prob_org_ws > 0.5, 1, 0)  # Using 0.5 as threshold

# 1. Accuracy
acc_org_ws <- sum(predicted_class_org_ws == ict_14c_no_missing[[var1]]) / length(ict_14c_no_missing[[var1]])

# 2. Confusion Matrix
conf_matrix_org_ws <- confusionMatrix(as.factor(predicted_class_org_ws), as.factor(ict_14c_no_missing[[var1]]))

# 3. ROC & AUC
roc_obj_org_ws <- roc(ict_14c_no_missing[[var1]], predicted_prob_org_ws)
auc_org_ws <- auc(roc_obj_org_ws)

# Print the results
print(paste("Accuracy for Model Org:", acc_org_ws))
print(conf_matrix_org_ws)
print(paste("AUC for Model Org:", auc_org_ws))


###### 6.1.7. Model 1 performance metrics ######


# You can change action to view other imputed datasets
completed_data_ws1 <- complete(imp_model_ws1, action = 1)
# Add the dependent variable the the complete data
completed_data_ws1[[var1]] <-  ict_14c[[var1]]
# Using completed_data1 for the regression
# Fit the model on each imputed dataset for model1

ws_model1_mice <- with(data = completed_data_ws1, 
                       expr = glm(completed_data_ws1[[var1]] ~ C8a + C8b + C8c + C8d + C8e + C8f + C8g + C8h + C8i, family = binomial()))



summary(ws_model1_mice)

# Predict using your models
predicted_prob_ws1 <- predict(ws_model1_mice, type = "response")
predicted_class_ws1 <- ifelse(predicted_prob_ws1 > 0.5, 1, 0)  # Using 0.5 as threshold

# 1. Accuracy
acc_ws1 <- sum(predicted_class_ws1 == completed_data_ws1[[var1]]) / length(completed_data_ws1[[var1]])

# 2. Confusion Matrix
conf_matrix_ws1 <- confusionMatrix(as.factor(predicted_class_ws1), as.factor(completed_data_ws1[[var1]]))

# 3. ROC & AUC
roc_obj_ws1 <- roc(completed_data_ws1[[var1]], predicted_prob_ws1)
auc_ws1 <- auc(roc_obj_ws1)

# Print the results
print(paste("Accuracy for Model 1:", acc_ws1))
print(conf_matrix_ws1)
print(paste("AUC for Model 1:", auc_ws1))


###### 6.1.8. Model 2 performance metrics ######

# You can change action to view other imputed datasets
completed_data_ws2 <- complete(imp_model_ws2, action = 4)
# Add the dependent variable the the complete data
completed_data_ws2[[var1]] <-  ict_14c[[var1]]
# Fit the model on each imputed dataset for model2
ws_model2_mice <- with(data = completed_data_ws2, 
                       expr = glm(completed_data_ws2[[var1]] ~ C8a + C8b + C8c + C8d + C8e + C8f + C8g + C8h + C8i, family = binomial()))

# Display the pooled results

summary(ws_model2_mice)


# Predict using your models
predicted_prob_ws2 <- predict(ws_model2_mice, type = "response")
predicted_class_ws2 <- ifelse(predicted_prob_ws2 > 0.5, 1, 0)  # Using 0.5 as threshold

# 1. Accuracy
acc_ws2 <- sum(predicted_class_ws2 == completed_data_ws2[[var1]]) / length(completed_data_ws2[[var1]])

# 2. Confusion Matrix
conf_matrix_ws2 <- confusionMatrix(as.factor(predicted_class_ws2), as.factor(completed_data_ws2[[var1]]))

# 3. ROC & AUC
roc_obj_ws2 <- roc(completed_data_ws2[[var1]], predicted_prob_ws2)
auc_ws2 <- auc(roc_obj_ws2)

# Print the results
print(paste("Accuracy for Model 2:", acc_ws2))
print(conf_matrix_ws2)
print(paste("AUC for Model 2:", auc_ws2))


###### 6.1.9. Model 3 performance metrics ######

# You can change action to view other imputed datasets
completed_data_ws3 <- complete(imp_model_ws3, action = 1)
# Add the dependent variable the the complete data
completed_data_ws3[[var1]] <-  ict_14c[[var1]]

# Fit the model on each imputed dataset for model3
ws_model3_mice <- with(data = completed_data_ws3, 
                       expr = glm(completed_data_ws3[[var1]] ~ C8a + C8b + C8c + C8d + C8e + C8f + C8g + C8h + C8i, family = binomial()))

# Display the pooled results

summary(ws_model3_mice)


# Predict using your models
predicted_prob_ws3 <- predict(ws_model3_mice, type = "response")
predicted_class_ws3 <- ifelse(predicted_prob_ws3 > 0.5, 1, 0)  # Using 0.5 as threshold

# 1. Accuracy
acc_ws3 <- sum(predicted_class_ws3 == completed_data_ws3[[var1]]) / length(completed_data_ws3[[var1]])

# 2. Confusion Matrix
conf_matrix_ws3 <- confusionMatrix(as.factor(predicted_class_ws3), as.factor(completed_data_ws3[[var1]]))

# 3. ROC & AUC
roc_obj_ws3 <- roc(completed_data_ws3[[var1]], predicted_prob_ws3)
auc_ws3 <- auc(roc_obj_ws3)

# Print the results
print(paste("Accuracy for Model 3:", acc_ws3))
print(conf_matrix_ws3)
print(paste("AUC for Model 3:", auc_ws3))


###### 6.1.10. Summaries ######

# Store results in a list of lists
results <- list(
  list(conf_matrix = conf_matrix_org_ws, AUC = paste("AUC for Model Org:", auc_org_ws)),
  list(conf_matrix = conf_matrix_ws1, AUC = paste("AUC for Model 1:", auc_ws1)),
  list(conf_matrix = conf_matrix_ws2, AUC = paste("AUC for Model 2:", auc_ws2)),
  list(conf_matrix = conf_matrix_ws3, AUC = paste("AUC for Model 3:", auc_ws3))
)

# Print results
for (res in results) {
  print(res$conf_matrix)
  cat(res$AUC, "\n\n")
}



##### 6.2. CLOUD COMPUTING VARIABLES ######

# dom1, clad4, rip, D2a, D2a, D2b, D2c, D2d, D2e, D2g, D3a, D3b
# Which cloud computing services used on the Internet are purchased by the company:
# D2a. Email services
# D2b. Office software (e.g., word processors, spreadsheets)
# D2c. Enterprise database hosting
# D2d. File storage
# D2e. Finance and accounting software applications
# D2f. CRM (Customer Relationship Management) software applications to manage information relating to your customers
# D2g. Computing power to run the company's software

# How the cloud computing services used on the Internet purchased by the company are provided.
# D3a. Through cloud service provider servers that are not reserved exclusively for the company
# D3b. Through servers of the cloud service provider which are reserved exclusively for the company

# C7. Website
# B5a Using IT specialists who are part of the business group
# C9c SM usage by type: multimedia content sharing websites


## Converting these variables as factor 
# ict_14c[,c(33:41)] <- lapply(ict_14c[,c(33:41)], as.factor)

###### 6.2.1. Model 1: Imputation with logreg #####

# Choosing only the related variables to website use to impute


imp_model_cc1 <- mice(ict_14c[,c("D2a", "D2b", "D2c", "D2d", 
                                 "D2e", "D2f", "D2g", "D3a", 
                                 "D3b", "C7", "B5a", "C9c", 
                                 grep("dom1_", names(ict_14c), value = TRUE),
                                 grep("clad4_", names(ict_14c), value = TRUE),
                                grep("rip_", names(ict_14c), value = TRUE))],
                      method = "logreg", 
                      m = 7, maxit = 10 ) # Number of imputed datasets. 5 is a common choice.

# , 
# grep("dom1_", names(ict_14c), value = TRUE),
# grep("clad4_", names(ict_14c), value = TRUE),
# grep("rip_", names(ict_14c), value = TRUE),
# grep("Ricavi_", names(ict_14c), value = TRUE))],


plot_trace(imp_model_cc1)



###### 6.2.2. Model 2: Imputation with pmm #####

# # Choosing only the related variables to website use to impute with other covarietes
# such size, sector, and region
### Covariates
# dom1 economic sectors
# clad4 size by No. of employees
# rip regions NUTS 1 for Italy
imp_model_cc2 <- mice(ict_14c[,c("D2a", "D2b", "D2c", "D2d", 
                                 "D2e", "D2f", "D2g", "D3a", 
                                 "D3b", "C7", "B5a", "C9c", 
                                 grep("dom1_", names(ict_14c), value = TRUE),
                                 grep("clad4_", names(ict_14c), value = TRUE),
                                 grep("rip_", names(ict_14c), value = TRUE))], 
                      method = "rf", 
                      m = 7, maxit = 10) # Number of imputed datasets. 5 is a common choice.

plot_trace(imp_model_cc2)



###### 6.2.3. Model 3: Imputation with cart #####

# # Choosing only the related variables to website use to impute with other covarietes
# such size, sector, and region, access to computer and use of social media
### Covariates
# dom1 economic sectors
# clad4 size by No. of employees
# rip regions NUTS 1 for Italy
# A2 Percentage of employees use computers
# B1 Employment of IT specialists
# C9c SM usage by type: multimedia content sharing websites (e.g. YouTube, Flickr, Picasa, SlideShare)


imp_model_cc3 <- mice(ict_14c[,c("D2a", "D2b", "D2c", "D2d", 
                                 "D2e", "D2f", "D2g", "D3a", 
                                 "D3b", "C7", "B5a", "C9c", 
                                 grep("dom1_", names(ict_14c), value = TRUE),
                                 grep("clad4_", names(ict_14c), value = TRUE),
                                 grep("rip_", names(ict_14c), value = TRUE))], 
                      method = "cart", 
                      m = 7, maxit = 15) # Number of imputed datasets. 5 is a common choice.


plot_trace(imp_model_cc3)



###### 6.2.4. Bar plot panel by variable  #####


# "D2a", "D2b", "D2c", "D2d", "D2e", "D2f", "D2g", "D3a", "D3b"
var0 <- "D2a"

plot_original_cc <- ggplot(ict_14c, aes(x = !!sym(var0))) + geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  ggtitle("Original Data")

plot_imputed_cc1 <- ggplot(completed_data_cc1, aes(x = !!sym(var0))) + geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  ggtitle("Imputed Data model1")

plot_imputed_cc2 <- ggplot(completed_data_cc2, aes(x = !!sym(var0))) + geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  ggtitle("Imputed Data model2")

plot_imputed_cc3 <- ggplot(completed_data_cc3, aes(x = !!sym(var0))) + geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  ggtitle("Imputed Data model3")




grid.arrange(plot_original_cc, plot_imputed_cc1, 
             plot_imputed_cc2, plot_imputed_cc3, ncol=2)


##### 6.2.5. IMPUTATION DIAGNOSTICS CC #####

###### 6.2.6. Model with original data ######

var2 <- "B1"
# Running the model with no missing values using data without imputing
ict_14c_no_missing1 <- na.omit(ict_14c[, c(var2, "D2a", "D2b", "D2c", "D2d", 
                                           "D2e", "D2f", "D2g", "D3a", "D3b")])

# Run the logistic regression model on the data without missing values
cc_org1 <- glm(ict_14c_no_missing1[[var2]] ~ D2a + D2b + D2c + D2d + D2e + D2f + D2g + D3a + D3b, 
               data = ict_14c_no_missing1, 
               family = binomial())

# Display the summary of the model
summary(cc_org1)

# Predict using your models
predicted_prob_org_cc1<- predict(cc_org1, type = "response")
predicted_class_org_cc1 <- ifelse(predicted_prob_org_cc1 > 0.5, 1, 0)  # Using 0.5 as threshold

# 1. Accuracy
acc_org_cc1 <- sum(predicted_class_org_cc1 == ict_14c_no_missing1[[var2]]) / length(ict_14c_no_missing1[[var2]])

# 2. Confusion Matrix
conf_matrix_org_cc1 <- confusionMatrix(as.factor(predicted_class_org_cc1), as.factor(ict_14c_no_missing1[[var2]]))

# 3. ROC & AUC
roc_obj_org_cc1 <- roc(ict_14c_no_missing1[[var2]], predicted_prob_org_cc1)
auc_org_cc1 <- auc(roc_obj_org_cc1)

# Print the results
print(paste("Accuracy for Model Org:", acc_org_cc1))
print(conf_matrix_org_cc1)
print(paste("AUC for Model Org:", auc_org_cc1))


###### 6.2.7. Model 1 performance metrics ######


# You can change action to view other imputed datasets
completed_data_cc1 <- complete(imp_model_cc1, action = 4) 
completed_data_cc1[[var2]] <-  ict_14c[[var2]]

# Using completed_data1 for the regression
# Fit the model on each imputed dataset for model1

cc_model1_mice <- with(data = completed_data_cc1, 
                       expr = glm(completed_data_cc1[[var2]] ~ D2a + D2b + D2c + D2d + D2e + D2f + D2g + D3a + D3b, 
                                  family = binomial()))



summary(cc_model1_mice)

# Predict using your models
predicted_prob_cc1 <- predict(cc_model1_mice, type = "response")
predicted_class_cc1 <- ifelse(predicted_prob_cc1 > 0.5, 1, 0)  # Using 0.5 as threshold

# 1. Accuracy
acc_cc1 <- sum(predicted_class_cc1 == completed_data_cc1[[var2]]) / length(completed_data_cc1[[var2]])

# 2. Confusion Matrix
conf_matrix_cc1 <- confusionMatrix(as.factor(predicted_class_cc1), as.factor(completed_data_cc1[[var2]]))

# 3. ROC & AUC
roc_obj_cc1 <- roc(completed_data_cc1[[var2]], predicted_prob_cc1)
auc_cc1 <- auc(roc_obj_cc1)

# Print the results
print(paste("Accuracy for Model 1:", acc_cc1))
print(conf_matrix_cc1)
print(paste("AUC for Model 1:", auc_cc1))


###### 6.2.8. Model 2 performance metrics ######

# You can change action to view other imputed datasets
var2 <- "B1"
completed_data_cc2 <- complete(imp_model_cc2, action = 1) 
completed_data_cc2[[var2]] <-  ict_14c[[var2]]

# Fit the model on each imputed dataset for model2
cc_model2_mice <- with(data = completed_data_cc2, 
                       expr = glm(completed_data_cc2[[var2]] ~ D2a + D2b + D2c + D2d + D2e + D2f + D2g + D3a + D3b, 
                                  family = binomial()))



summary(cc_model2_mice)


var2 <- "B1"
# Predict using your models
predicted_prob_cc2 <- predict(cc_model2_mice, type = "response")
predicted_class_cc2 <- ifelse(predicted_prob_cc2 > 0.5, 1, 0)  # Using 0.5 as threshold

# 1. Accuracy
acc_cc2 <- sum(predicted_class_cc2 == completed_data_cc2[[var2]]) / length(completed_data_cc2[[var2]])

# 2. Confusion Matrix
conf_matrix_cc2 <- confusionMatrix(as.factor(predicted_class_cc2), as.factor(completed_data_cc2[[var2]]))

# 3. ROC & AUC
roc_obj_cc2 <- roc(completed_data_cc2[[var2]], predicted_prob_cc2)
auc_cc2 <- auc(roc_obj2)

# Print the results
print(paste("Accuracy for Model 2:", acc_cc2))
print(conf_matrix_cc2)
print(paste("AUC for Model 2:", auc_cc2))


###### 6.2.9. Model 3 performance metrics ######

# You can change action to view other imputed datasets
completed_data_cc3 <- complete(imp_model_cc3, action = 1) 
completed_data_cc3[[var2]] <-  ict_14c[[var2]]

# Fit the model on each imputed dataset for model3
cc_model3_mice <- with(data = completed_data_cc3, 
                       expr = glm(completed_data_cc3[[var2]] ~ D2a + D2b + D2c + D2d + D2e + D2f + D2g + D3a + D3b, 
                                  family = binomial()))



summary(cc_model3_mice)

# Predict using your models
predicted_prob_cc3 <- predict(cc_model3_mice, type = "response")
predicted_class_cc3 <- ifelse(predicted_prob_cc3 > 0.5, 1, 0)  # Using 0.5 as threshold

# 1. Accuracy
acc_cc3 <- sum(predicted_class_cc3 == completed_data_cc3[[var2]]) / length(completed_data_cc3[[var2]])

# 2. Confusion Matrix
conf_matrix_cc3 <- confusionMatrix(as.factor(predicted_class_cc3), as.factor(completed_data_cc3[[var2]]))

# 3. ROC & AUC
roc_obj_cc3 <- roc(completed_data_cc3[[var2]], predicted_prob_cc3)
auc_cc3 <- auc(roc_obj3)

# Print the results
print(paste("Accuracy for Model 3:", acc_cc3))
print(conf_matrix_cc3)
print(paste("AUC for Model 3:", auc_cc3))

###### 6.2.10. Summaries ######

# Store results in a list of lists
results <- list(
  list(conf_matrix = conf_matrix_org_cc1, AUC = paste("AUC for Model Org:", auc_org_cc1)),
  list(conf_matrix = conf_matrix_cc1, AUC = paste("AUC for Model 1:", auc_cc1)),
  list(conf_matrix = conf_matrix_cc2, AUC = paste("AUC for Model 2:", auc_cc2)),
  list(conf_matrix = conf_matrix_cc3, AUC = paste("AUC for Model 3:", auc_cc3))
)

# Print results
for (res in results) {
  print(res$conf_matrix)
  cat(res$AUC, "\n\n")
}

##### 6.3. TYPE OF INVIOCES VARIABLES ######

# dom1, clad4, rip, I2a, I2b, I2c
# Of all the invoices sent by the company during the year 2013 to other 
# companies or public administrations, indicate the percentage of the following types:
# I2a. electronic invoices sent in standard format or suitable for automatic processing (for example in EDI, UBL, XML format)
# I2b. invoices sent in an electronic format not suitable for automatic processing (for example emails or email attachments in PDF, TIF, JPEG or other format)
# I2c. paper invoices

###### 6.3.1. Model 1: Imputation with sample #####

corPlot(ict_14c[, c("I2a", "I2b", "I2c")])
var3 <- "J1"
# Choosing only the related variables to website use to impute
imp_model_ti1 <- mice(ict_14c[,c("I2a", "I2b", "C7", 
                                 "E2a", "C9c", 
                                 grep("dom1_", names(ict_14c), value = TRUE),
                                 grep("clad4_", names(ict_14c), value = TRUE),
                                 grep("rip_", names(ict_14c), value = TRUE))], 
                      method = "sample", 
                      m = 5, maxit = 15) # Number of imputed datasets. 5 is a common choice.

# Normalize using z-score normalization
normalize <- function(x) {
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}


ict_14c <- ict_14c[, -c(125:127)]
ict_14c$I2b_z <- normalize(ict_14c$I2b)
ict_14c$I2c_z <- normalize(ict_14c$I2c)

# Now, proceed with your imputation using the newly created normalized variables
imp_model_ti1 <- mice(ict_14c[,c("I2a_z", "I2b_z","I2c_z", "C7", 
                                 "E2a", "C9c", 
                                 grep("dom1_", names(ict_14c), value = TRUE),
                                 grep("clad4_", names(ict_14c), value = TRUE),
                                 grep("rip_", names(ict_14c), value = TRUE))], 
                      method = "sample", 
                      m = 5, maxit = 20)

# Now, proceed with your imputation using the newly created normalized variables
imp_model_ti1a <- mice(ict_14c[,c( "I2c", "C7", 
                                 "E2a", "C9c", 
                                 grep("dom1_", names(ict_14c), value = TRUE),
                                 grep("clad4_", names(ict_14c), value = TRUE),
                                 grep("rip_", names(ict_14c), value = TRUE))], 
                      method = "ri", 
                      m = 5, maxit = 15)


completed_data_ti1a <- complete(imp_model_ti1a, action = 3) 
plot_trace(imp_model_ti1)
plot_trace(imp_model_ti1a)


###### 6.3.2. Model 2: Imputation with pmm #####

# # Choosing only the related variables to website use to impute with other covarietes
# such size, sector, and region
### Covariates
# dom1 economic sectors
# clad4 size by No. of employees
# rip regions NUTS 1 for Italy
imp_model_ti2 <- mice(ict_14c[,c("I2a", "I2b", "C7", 
                                 "B5a", "C9c", 
                                 grep("dom1_", names(ict_14c), value = TRUE),
                                 grep("clad4_", names(ict_14c), value = TRUE),
                                 grep("rip_", names(ict_14c), value = TRUE))], 
                      method = "rf", 
                      m = 5, maxit = 15) # Number of imputed datasets. 5 is a common choice.

imp_model_ti2a <- mice(ict_14c[,c("I2b_z","I2c_z", "C7", 
                                 "B5a", "C9c", 
                                 grep("dom1_", names(ict_14c), value = TRUE),
                                 grep("clad4_", names(ict_14c), value = TRUE),
                                 grep("rip_", names(ict_14c), value = TRUE))], 
                      method = "rf", 
                      m = 5, maxit = 15) # Number of imputed datasets. 5 is a common choice.


plot_trace(imp_model_ti2)
plot_trace(imp_model_ti2a)



###### 6.3.3. Model 3: Imputation with cart #####

# # Choosing only the related variables to website use to impute with other covarietes
# such size, sector, and region, access to computer and use of social media
### Covariates
# dom1 economic sectors
# clad4 size by No. of employees
# rip regions NUTS 1 for Italy

imp_model_ti3 <- mice(ict_14c[,c("I2a", "I2b", "C7", 
                                 "B5a", "C9c", 
                                 grep("dom1_", names(ict_14c), value = TRUE),
                                 grep("clad4_", names(ict_14c), value = TRUE),
                                 grep("rip_", names(ict_14c), value = TRUE))], 
                      method = "cart", 
                      m = 5, maxit = 15) # Number of imputed datasets. 5 is a common choice.

imp_model_ti3a <- mice(ict_14c[,c("I2b_z","I2c_z", "C7", 
                                 "B5a", "C9c", 
                                 grep("dom1_", names(ict_14c), value = TRUE),
                                 grep("clad4_", names(ict_14c), value = TRUE),
                                 grep("rip_", names(ict_14c), value = TRUE))], 
                      method = "cart", 
                      m = 5, maxit = 15) # Number of imputed datasets. 5 is a common choice.

plot_trace(imp_model_ti3)
plot_trace(imp_model_ti3a)




###### 6.3.4. Bar plot panel by variable  #####


# "I2a", "I2b", "I2c"
var0 <- "I2a"

plot_original_ti <- ggplot(ict_14c, aes(x = !!sym(var0))) + geom_histogram() + 
    ggtitle("Original Data")

plot_imputed_ti1 <- ggplot(completed_data_ti1, aes(x = !!sym(var0))) + geom_histogram() + 
    ggtitle("Imputed Data model1")

plot_imputed_ti2 <- ggplot(completed_data_ti2, aes(x = !!sym(var0))) + geom_histogram() + 
    ggtitle("Imputed Data model2")

plot_imputed_ti3 <- ggplot(completed_data_ti3, aes(x = !!sym(var0))) + geom_histogram() + 
   ggtitle("Imputed Data model3")




grid.arrange(plot_original_ti, plot_imputed_ti1, 
             plot_imputed_ti2, plot_imputed_ti3, ncol=2)


##### 6.3.5. IMPUTATION DIAGNOSTICS TI #####

###### 6.3.6. Model with original data ######

var3 <- "J7"
# Running the model with no missing values using data without imputing
ict_14c_no_missing2 <- na.omit(ict_14c[, c(var3, "I2a_z", "I2b_z", "I2c_z")])

# Run the logistic regression model on the data without missing values
ti_org1 <- glm(ict_14c_no_missing2[[var3]] ~ I2a_z + I2b_z + I2c_z , 
               data = ict_14c_no_missing2, 
               family = binomial())

# Display the summary of the model
summary(ti_org1)

# Predict using your models
predicted_prob_org_ti<- predict(ti_org1, type = "response")
predicted_class_org_ti <- ifelse(predicted_prob_org_ti > 0.5, 1, 0)  # Using 0.5 as threshold


# 1. Accuracy
acc_org_ti <- sum(predicted_class_org_ti == ict_14c_no_missing2[[var3]]) / length(ict_14c_no_missing2[[var3]])

# 2. Confusion Matrix
conf_matrix_org_ti <- confusionMatrix(factor(predicted_class_org_ti, levels = c(0,1)),
                                      factor(ict_14c_no_missing2[[var3]], levels = c(0,1)))

# 3. ROC & AUC
roc_obj_org_ti <- roc(ict_14c_no_missing2[[var3]], predicted_prob_org_ti)
auc_org_ti <- auc(roc_obj_org_ti)

# Print the results
print(paste("Accuracy for Model Org:", acc_org_ti))
print(conf_matrix_org_ti)
print(paste("AUC for Model Org:", auc_org_ti))


###### 6.3.7. Model 1 performance metrics ######


# You can change action to view other imputed datasets
completed_data_ti1 <- complete(imp_model_ti1, action = 1) 
completed_data_ti1[[var3]] <-  ict_14c[[var3]]

# Using completed_data1 for the regression
# Fit the model on each imputed dataset for model1
ti_model1_mice <- with(data = completed_data_ti1, 
                       expr = glm(completed_data_ti1[[var3]] ~ I2a + I2b, 
                                  family = binomial()))



summary(ti_model1_mice)

# Predict using your models
predicted_prob_ti1 <- predict(ti_model1_mice, type = "response")
predicted_class_ti1 <- ifelse(predicted_prob_ti1 > 0.5, 1, 0)  # Using 0.5 as threshold

# 1. Accuracy
acc_ti1 <- sum(predicted_class_cc1 == completed_data_ti1[[var3]]) / length(completed_data_ti1[[var3]])

# 2. Confusion Matrix
conf_matrix_ti1 <- confusionMatrix(factor(predicted_class_ti1, levels = c(0,1)), 
                                   factor(completed_data_ti1[[var3]], levels = c(0,1)))

# 3. ROC & AUC
roc_obj_ti1 <- roc(completed_data_ti1[[var3]], predicted_prob_ti1)
auc_ti1 <- auc(roc_obj_cc1)

# Print the results
print(paste("Accuracy for Model 1:", acc_ti1))
print(conf_matrix_ti1)
print(paste("AUC for Model 1:", auc_ti1))


###### 6.3.8. Model 2 performance metrics ######

# You can change action to view other imputed datasets
completed_data_ti2 <- complete(imp_model_ti2, action = 1) 
completed_data_ti2[[var3]] <-  ict_14c[[var3]]
# Fit the model on each imputed dataset for model2
ti_model2_mice <- with(data = completed_data_ti2, 
                       expr = glm(completed_data_ti2[[var3]] ~ I2a + I2b , 
                                  family = binomial()))



summary(ti_model2_mice)

# Predict using your models
predicted_prob_ti2 <- predict(ti_model2_mice, type = "response")
predicted_class_ti2 <- ifelse(predicted_prob_ti2 > 0.5, 1, 0)  # Using 0.5 as threshold

# 1. Accuracy
ati_ti2 <- sum(predicted_class_ti2 == completed_data_ti2[[var3]]) / length(completed_data_ti2[[var3]])

# 2. Confusion Matrix
conf_matrix_ti2 <- confusionMatrix(factor(predicted_class_ti2, levels = c(0,1)), 
                                   factor(completed_data_ti2[[var3]], levels = c(0,1)))

# 3. ROC & AUC
roc_obj_ti2 <- roc(completed_data_ti2[[var3]], predicted_prob_ti2)
auc_ti2 <- auc(roc_obj2)

# Print the results
print(paste("Accuracy for Model 1:", ati_ti2))
print(conf_matrix_ti2)
print(paste("AUC for Model 1:", auc_ti2))


###### 6.3.9. Model 3 performance metrics ######

# You can change action to view other imputed datasets
completed_data_ti3 <- complete(imp_model_ti3, action = 3) 
completed_data_ti3[[var3]] <-  ict_14c[[var3]]
# Fit the model on each imputed dataset for model3
ti_model3_mice <- with(data = completed_data_ti3, 
                       expr = glm(completed_data_ti3[[var3]] ~ I2a  + I2b, 
                                  family = binomial()))



summary(ti_model3_mice)

# Predict using your models
predicted_prob_ti3 <- predict(ti_model3_mice, type = "response")
predicted_class_ti3 <- ifelse(predicted_prob_ti3 > 0.5, 1, 0)  # Using 0.5 as threshold

# 1. Accuracy
ati_ti3 <- sum(predicted_class_ti3 == completed_data_ti3[[var3]]) / length(completed_data_ti3[[var3]])

# 2. Confusion Matrix
conf_matrix_ti3 <- confusionMatrix(factor(predicted_class_ti3, levels = c(0,1)), 
                                   factor(completed_data_ti3[[var3]], levels = c(0,1)))

# 3. ROC & AUC
roc_obj_ti3 <- roc(completed_data_ti3[[var3]], predicted_prob_ti3)
auc_ti3 <- auc(roc_obj3)

# Print the results
print(paste("Accuracy for Model 1:", ati_ti3))
print(conf_matrix_ti3)
print(paste("AUC for Model 1:", auc_ti3))

###### 6.3.10. Summaries ######

# Store results in a list of lists
results <- list(
  list(conf_matrix = conf_matrix_org_ti, AUC = paste("AUC for Model Org:", auc_org_ti)),
  list(conf_matrix = conf_matrix_ti1, AUC = paste("AUC for Model 1:", auc_ti1)),
  list(conf_matrix = conf_matrix_ti2, AUC = paste("AUC for Model 2:", auc_ti2)),
  list(conf_matrix = conf_matrix_ti3, AUC = paste("AUC for Model 3:", auc_ti3))
)

# Print results
for (res in results) {
  print(res$conf_matrix)
  cat(res$AUC, "\n\n")
}










mca_result <- MCA(completed_data3[,c("C8a", "C8b", "C8c", "C8d", 
                                  "C8e", "C8f", "C8g", "C8h", "C8i")], graph=FALSE)


# Eigenvalues/variances
eig.val <- get_eigenvalue(mca_result)
barplot(eig.val[, 2], names.arg=1:nrow(eig.val), main="Eigenvalues", 
        col="steelblue", cex.names=0.7)


# Plotting the variables on dimensions 1 and 2
fviz_mca_var(mca_result, choice = "var.cat", axes = 1:2)

fviz_mca_var(mca_result, choice = "var", axes = 1:2)



