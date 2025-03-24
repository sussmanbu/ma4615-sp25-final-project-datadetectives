
# Note, you may end up creating more than one cleaned data set and saving that
# to separate files in order to work on different aspects of your project

# SELECTED COLUMNS -> R000010 (Case ID), R0173600 (Sample ID), R0214700 (Race), R0214800 (Sex), T2272800 (Q3-4| Highest Grade Completed),...
#T3045300 (total income from wages and salary prev cal year), T3108400 (marital status), T3108700 (age at int. date (2010))

# NOTE: Salary and wages are already decoded, Age at Interview Data already decoded
library(tidyverse)

### 1. Read CSV
nls_data <- read_csv(here::here("dataset", "nls_initial_data.csv"))

### 2. Load data
nls_data_clean <- nls_data |>
  pivot_longer(2:5, names_to = "group", values_to = "...")

### 3. DECODE COLUMNS R000010 (Case ID), R0173600 (Sample ID), R0214700 (Race), R0214800 (Sex), T2272800 (Q3-4| Highest Grade Completed),...
#T3045300 (total income from wages and salary prev cal year), T3108400 (marital status), T3108700 (age at int. date (2010))

colnames(nls_data_clean) <- c(
  "Case_ID",
  "Sample_ID",
  "Race",
  "Sex",
  "Highest_Grade_Completed",
  "Total_Income_Prev_Year",
  "Marital_Status",
  "Age_2010"
)

nls_data_clean <- nls_data %>%
  select(
    R000010,   # Case ID
    R0173600,  # Sample ID
    R0214700,  # Race
    R0214800,  # Sex
    T2272800,  # Highest Grade Completed
    T3045300,  # Total Income from Wages
    T3108400,  # Marital Status
    T3108700   # Age at Interview (2010)
  ) %>%
  rename(
    Case_ID = R000010,
    Sample_ID = R0173600,
    Race = R0214700,
    Sex = R0214800,
    Highest_Grade_Completed = T2272800,
    Total_Income_Prev_Year = T3045300,
    Marital_Status = T3108400,
    Age_2010 = T3108700
  )

### 4. REMOVE non-interviews(-5)
nls_data_clean <- nls_data_clean %>%
  filter(
    Race >= 0,
    Sex >= 0,
    Highest_Grade_Completed >= 0,
    Marital_Status >= 0
  )

### 5. DECODE race (1 - Hispanic, 2- Black, 3 - Non-Black, Non-Hispanic)

### 6. DECODE sex (1 - male, 2 - female)

### 7. DECODE highest grade completed (1 - 1ST GRADE, 2 - 2ND GRADE, 3 -3RD GRADE, 4 -4TH GRADE, ...
###       5 - 5TH GRADE, 6 - 6TH GRADE, 7 - 7TH GRADE, 8 - 8TH GRADE, 9 - 9TH GRADE, 10 - 10TH GRADE, ...
###       11 - 11TH GRADE, 12 - 12TH GRADE, 13 - 1ST YEAR COLLEGE, 14 - 2ND YEAR COLLEGE, 15 - 3RD YEAR COLLEGE, ...
###       16 - 4TH YEAR COLLEGE, 17 - 5TH YEAR COLLEGE, 18 - 6TH YEAR COLLEGE, 19 - 7TH YEAR COLLEGE, 20 - 8TH YEAR COLLEGE OR MORE, 95 - UNGRADED)




### DECODE Marital Status (0 - Never Married, 1 - Married, 2- Separated, 3- Divorced, 6 - Widowed)

write_rds(loan_data_clean, file = here::here("dataset", "loan_refusal_clean.rds"))
