
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

# 1. Load Data with Explicit Column Types
nls_data <- read_csv(
  here("dataset", "nls_initial_data_updated.csv"),
  col_types = cols(
    R0000100 = col_integer(),   # Case ID
    R0173600 = col_integer(),   # Sample ID
    R0214700 = col_integer(),   # Race
    R0214800 = col_integer(),   # Sex
    T2272800 = col_integer(),   # Highest Grade Completed
    T3045300 = col_double(),    # Total Income
    T3108400 = col_integer(),   # Marital Status
    T3108700 = col_integer()    # Age
  )
) %>%
  # Remove rows where any variable is -5
  filter(if_all(everything(), ~ . != -5))

# Debugging Step: Check if `nls_data` is created
print("nls_data successfully loaded and filtered.")
print(dim(nls_data))

# 2. Handle Missing Data and Negative Income
nls_data <- nls_data %>%
  mutate(
    T3045300 = na_if(T3045300, -1),  # Mark -1 as NA
    T3045300 = ifelse(T3045300 < 0, NA, T3045300)  # Negative income to NA
  )

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
    )
  ) %>%
  # Convert categorical variables to factors
  mutate(across(c(Race, Sex, Highest_Grade_Completed, Marital_Status), as.factor)) %>%
  # Remove original numeric columns
  select(-c(R0214700, R0214800, T2272800, T3108400)) %>%
  # Rename columns
  rename(
    Case_ID = R0000100,
    Sample_ID = R0173600,
    Total_Income_Prev_Year = T3045300,
    Age_2010 = T3108700
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
