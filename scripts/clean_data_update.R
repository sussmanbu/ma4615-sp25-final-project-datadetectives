
# Note, you may end up creating more than one cleaned data set and saving that
# to separate files in order to work on different aspects of your project

# SELECTED COLUMNS -> R0000100 (Case ID), R0173600 (Sample ID), R0214700 (Race), R0214800 (Sex), T2272800 (Q3-4| Highest Grade Completed),...
#T3045300 (total income from wages and salary prev cal year), T3108400 (marital status), T3108700 (age at int. date (2010))
## 1b. DECODE COLUMNS R000010 (Case ID), R0173600 (Sample ID), R0214700 (Race), R0214800 (Sex), T2272800 (Q3-4| Highest Grade Completed),...
## T3045300 (total income from wages and salary prev cal year), T3108400 (marital status), T3108700 (age at int. date (2010)),
## 1c.remove non-interviews (-5)
### 2. DECODE COLUMNS
## a. race (1 - Hispanic, 2- Black, 3 - Non-Black, Non-Hispanic)

### b.sex (1 - male, 2 - female)


### c.highest grade completed (1 - 1ST GRADE, 2 - 2ND GRADE, 3 -3RD GRADE, 4 -4TH GRADE, ...

###       5 - 5TH GRADE, 6 - 6TH GRADE, 7 - 7TH GRADE, 8 - 8TH GRADE, 9 - 9TH GRADE, 10 - 10TH GRADE, ...
###       11 - 11TH GRADE, 12 - 12TH GRADE, 13 - 1ST YEAR COLLEGE, 14 - 2ND YEAR COLLEGE, 15 - 3RD YEAR COLLEGE, ...
###       16 - 4TH YEAR COLLEGE, 17 - 5TH YEAR COLLEGE, 18 - 6TH YEAR COLLEGE, 19 - 7TH YEAR COLLEGE, 20 - 8TH YEAR COLLEGE OR MORE, 95 - UNGRADED)

### d. Marital Status (0 - Never Married, 1 - Married, 2- Separated, 3- Divorced, 6 - Widowed)

# NOTE: Salary and wages are already decoded, Age at Interview Data already decoded

library(tidyverse)
library(here)

nls_data <- nls_data %>%
  mutate(
    R6909701 = na_if(R6909701, -1),
    R6909701 = ifelse(R6909701 < 0, NA, R6909701),
    R7607800 = na_if(R7607800, -1),
    R7607800 = ifelse(R7607800 < 0, NA, R7607800),
    R8316300 = na_if(R8316300, -1),
    R8316300 = ifelse(R8316300 < 0, NA, R8316300),
    T0912400 = na_if(T0912400, -1),
    T0912400 = ifelse(T0912400 < 0, NA, T0912400),
    T2076700 = na_if(T2076700, -1),
    T2076700 = ifelse(T2076700 < 0, NA, T2076700),
    T3045300 = na_if(T3045300, -1),
    T3045300 = ifelse(T3045300 < 0, NA, T3045300)
  ) %>%

  # Remove rows where any variable is -5
  filter(if_all(everything(), ~ . != -5))

# Debugging Step: Check if `nls_data` is created
print("nls_data successfully loaded and filtered.")
print(dim(nls_data))

# 2. Handle Missing Data and Negative Income
nls_data <- nls_data %>%
  mutate(
    R6909701 = na_if(R6909701, -1),
    R6909701 = ifelse(R6909701 < 0, NA, R6909701),
    R7607800 = na_if(R7607800, -1),
    R7607800 = ifelse(R7607800 < 0, NA, R7607800),
    R8316300 = na_if(R8316300, -1),
    R8316300 = ifelse(R8316300 < 0, NA, R8316300),
    T0912400 = na_if(T0912400, -1),
    T0912400 = ifelse(T0912400 < 0, NA, T0912400),
    T2076700 = na_if(T2076700, -1),
    T2076700 = ifelse(T2076700 < 0, NA, T2076700),
    T3045300 = na_if(T3045300, -1),
    T3045300 = ifelse(T3045300 < 0, NA, T3045300)
  ) %>%
  filter(if_all(everything(), ~ . != -5))



# Debugging Step: Check after missing value handling
print("Handled missing data and negative income.")

# 3. Define Grade Labels
grade_labels <- c(
  "1st Grade", "2nd Grade", "3rd Grade", "4th Grade", "5th Grade", 
  "6th Grade", "7th Grade", "8th Grade", "9th Grade", "10th Grade", 
  "11th Grade", "12th Grade", "1st Year College", "2nd Year College", 
  "3rd Year College", "4th Year College", "5th Year College", "6th Year College", 
  "7th Year College", "8th Year College or More", "UNGRADED"
)

# 4. Create Cleaned Dataset
nls_data_clean <- nls_data %>%
  mutate(
    Race = case_when(
      R0214700 == 1 ~ "Hispanic",
      R0214700 == 2 ~ "Black",
      R0214700 == 3 ~ "Non-Black/Non-Hispanic",
      TRUE ~ NA_character_
    ),
    Sex = case_when(
      R0214800 == 1 ~ "Male",
      R0214800 == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
      Highest_Grade_Completed = case_when(
        T2272800 == 1 ~ "1ST GRADE", 
        T2272800 == 2 ~ "2ND GRADE", 
        T2272800 == 3 ~ "3RD GRADE",
        T2272800 == 4 ~ "4TH GRADE", 
        T2272800 == 5 ~ "5TH GRADE", 
        T2272800 == 6 ~ "6TH GRADE", 
        T2272800 == 7 ~ "7TH GRADE", 
        T2272800 == 8 ~ "8TH GRADE", 
        T2272800 == 9 ~ "9TH GRADE", 
        T2272800 == 10 ~ "10TH GRADE", 
        T2272800 == 11 ~ "11TH GRADE", 
        T2272800 == 12 ~ "12TH GRADE", 
        T2272800 == 13 ~ "1ST YEAR COLLEGE", 
        T2272800 == 14 ~ "2ND YEAR COLLEGE", 
        T2272800 == 15 ~ "3RD YEAR COLLEGE", 
        T2272800 == 16 ~ "4TH YEAR COLLEGE", 
        T2272800 == 17 ~ "5TH YEAR COLLEGE", 
        T2272800 == 18 ~ "6TH YEAR COLLEGE", 
        T2272800 == 19 ~ "7TH YEAR COLLEGE", 
        T2272800 == 20 ~ "8TH YEAR COLLEGE OR MORE", 
        T2272800 == 95 ~ "UNGRADED",
        TRUE ~ NA_character_
      ),
  Marital_Status = case_when(
      T3108400 == 0 ~ "Never Married",
      T3108400 == 1 ~ "Married",
      T3108400 == 2 ~ "Separated",
      T3108400 == 3 ~ "Divorced",
      T3108400 == 6 ~ "Widowed",
      TRUE ~ NA_character_
    ),
  Region = case_when(
    T3108200 == 1 ~ "Northeast",
    T3108200 == 2 ~ "North Central",
    T3108200 == 3 ~ "South",
    T3108200 == 4 ~ "West",
    TRUE ~ NA_character_
  )
  ) %>%
  # Convert categorical variables to factors
  mutate(across(c(Race, Sex, Highest_Grade_Completed, Marital_Status, Region), as.factor)) %>%
  # Remove original numeric columns
  select(-c(R0214700, R0214800, T2272800, T3108400, T3108200)) %>%
  # Rename columns
  # Rename columns
  rename(
    Case_ID = R0000100,
    Sample_ID = R0173600,
    Age_2010 = T3108700,
    Income_2000 = R6909701,
    Income_2002 = R7607800,
    Income_2004 = R8316300,
    Income_2006 = T0912400,
    Income_2008 = T2076700,
    Income_2010 = T3045300
  )



# Debugging Step: Check if `nls_data_clean` is successfully created
print("nls_data_clean successfully created.")
print(dim(nls_data_clean))

# 5. Save Cleaned Data
write_rds(nls_data_clean, here("dataset", "nls_clean.rds"))
write.csv(nls_data_clean, here("dataset", "nls_clean.csv"))

# Verification
print(colSums(is.na(nls_data_clean)))
str(nls_data_clean)

# Load in another library to clean second dataset
library(lubridate)

# Now we clean the second Dataset which we loaded in 
rates <- read_csv("./dataset/long-term-rates-2000-2010.csv")

# Define a mode function (returns the first mode if multiple)
get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

#mutate data to have proper year date format (wil be easier to compute stat summaries with)
rates <- rates %>%
  mutate(Date = mdy(Date),
         Year = year(Date))

interest_rates_cleaned <- rates %>%
  group_by(Year) %>%
  summarise(
    mean_LT = mean(`LT COMPOSITE (>10 Yrs)`, na.rm = TRUE),
    min_LT = min(`LT COMPOSITE (>10 Yrs)`, na.rm = TRUE),
    max_LT = max(`LT COMPOSITE (>10 Yrs)`, na.rm = TRUE),
    sd_LT = sd(`LT COMPOSITE (>10 Yrs)`, na.rm = TRUE),
    median_LT = median(`LT COMPOSITE (>10 Yrs)`, na.rm = TRUE),
    mode_LT = get_mode(`LT COMPOSITE (>10 Yrs)`),
    
    mean_TREASURY = mean(`TREASURY 20-Yr CMT`, na.rm = TRUE),
    min_TREASURY = min(`TREASURY 20-Yr CMT`, na.rm = TRUE),
    max_TREASURY = max(`TREASURY 20-Yr CMT`, na.rm = TRUE),
    sd_TREASURY = sd(`TREASURY 20-Yr CMT`, na.rm = TRUE),
    median_TREASURY = median(`TREASURY 20-Yr CMT`, na.rm = TRUE),
    mode_TREASURY = get_mode(`TREASURY 20-Yr CMT`)
  ) %>%
  ungroup()

write_csv(interest_rates_cleaned, "interest_rates_cleaned.csv")
