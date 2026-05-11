library(haven)
library(here)
library(tidyverse)

# Load data
path <- file.choose()
data <- read_xpt(path)

# Select needed variables
subdat <- data |>
  select(
    # Outcome
    MEDCOST1,
    # Demographic
    `_STATE`, `_URBSTAT`, SEXVAR, `_AGEG5YR`, `_IMPRACE`, 
    INCOME3, MARITAL, EDUCA, EMPLOY1,
    # Health status
    GENHLTH, PHYSHLTH, MENTHLTH, ADDEPEV3,
    DIABETE4, CVDINFR4, CVDCRHD4, CVDSTRK3, ASTHMA3, CHCCOPD3, CHCKDNY2, HAVARTH4,
    DEAF, BLIND, DECIDE, DIFFWALK, DIFFDRES, DIFFALON,
    # Healthcare access
    PRIMINS2, PERSDOC3, CHECKUP1,
    # Survey design
    `_PSU`, `_STSTR`, `_LLCPWT`
  )

# Helper function
yn_flag <- function(x) {
  x <- as.integer(x)
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 0,
    x %in% c(7, 9) ~ NA_integer_,
    is.na(x) ~ NA_integer_,
    TRUE ~ NA_integer_)
}

cleandat <- subdat |>
  # OUTCOME
  # Filter on valid OUTCOME and encode 1/0
  filter(MEDCOST1 %in% c(1, 2)) |>
  mutate(MEDCOST1 = if_else(MEDCOST1 == 1, 1, 0)) |>
  
  # Demographic
  # Collapse _STATE into 4 regions
  filter(!`_STATE` %in% c(66, 72, 78)) |>
  mutate(
    `_STATE` = as.integer(`_STATE`),
    region = case_when(
      `_STATE` %in% c(9,23,25,33,34,36,42,44,50) ~ "Northeast",
      `_STATE` %in% c(17,18,19,20,26,27,29,31,38,39,46,55) ~ "Midwest",
      `_STATE` %in% c(1,5,10,11,12,13,21,22,24,28,37,40,45,48,51,54) ~ "South",
      `_STATE` %in% c(2,4,6,8,15,16,30,32,35,41,49,53,56) ~ "West",
      TRUE ~ NA_character_),
    region = factor(region, levels = c("South","Northeast","Midwest","West"))
  ) |>
  
  # Encode _URBSTAT: Urban/Rural 
  mutate(rurality = case_when(
    `_URBSTAT` == 1 ~ "Urban",
    `_URBSTAT` == 2 ~ "Rural",
    TRUE ~ NA_character_),
    rurality = factor(rurality, levels = c("Urban", "Rural"))
  ) |>
  
  # Encode SEXVAR
  mutate(
    SEXVAR = as.integer(SEXVAR),
    sex = case_when(
      SEXVAR == 1 ~ "Male",
      SEXVAR == 2 ~ "Female",
      TRUE ~ NA_character_),
    sex = factor(sex, levels = c("Male", "Female"))
  ) |>
  
  # Encode _AGEG5YR
  mutate(
    `_AGEG5YR` = as.integer(`_AGEG5YR`),
    agegroup = case_when(
      `_AGEG5YR` %in% c(1, 2) ~ "18-29",
      `_AGEG5YR` %in% c(3, 4) ~ "30-39",
      `_AGEG5YR` %in% c(5, 6) ~ "40-49",
      `_AGEG5YR` %in% c(7, 8) ~ "50-59",
      `_AGEG5YR` %in% c(9, 10) ~ "60-69",
      `_AGEG5YR` %in% c(11, 12, 13) ~ "70+",
      `_AGEG5YR` == 14 ~ NA_character_,
      TRUE ~ NA_character_),
    agegroup = factor(agegroup, levels = c(
      "60-69", "18-29", "30-39", "40-49", "50-59", "70+"))
  ) |>
  
  # Encode _IMPRACE
  mutate(
    `_IMPRACE` = as.integer(`_IMPRACE`),
    race = case_when(
      `_IMPRACE` == 1 ~ "White_NH",
      `_IMPRACE` == 2 ~ "Black_NH",
      `_IMPRACE` == 3 ~ "Asian_NH",
      `_IMPRACE` == 4 ~ "AIAN_NH",
      `_IMPRACE` == 5 ~ "Hispanic",
      `_IMPRACE` == 6 ~ "Other_NH",
      TRUE ~ NA_character_),
    race = factor(race, levels = c("White_NH","Black_NH","Asian_NH",
                                   "AIAN_NH","Hispanic","Other_NH"))
  ) |>
  
  # Encode INCOME3
  mutate(
    INCOME3 = as.integer(INCOME3),
    income = case_when(
      INCOME3 %in% c(1, 2) ~ "Low",        # <$15k
      INCOME3 %in% c(3, 4) ~ "Low_mid",    # $15-25k
      INCOME3 %in% c(5, 6) ~ "Middle",     # $25-50k
      INCOME3 %in% c(7, 8) ~ "Upper_mid",  # $50-100k
      INCOME3 %in% c(9,10,11) ~ "High",    # $100k+
      INCOME3 %in% c(77, 99) ~ "Unknown",
      is.na(INCOME3) ~ "Unknown",
      TRUE ~ "Unknown"),
    income = factor(income, levels = c(
      "High", "Upper_mid", "Middle", "Low_mid", "Low", "Unknown"))
  ) |>
  
  # Encode MARITAL
  mutate(
    MARITAL = as.integer(MARITAL),
    maritalstatus = case_when(
      MARITAL == 1 ~ "Married",
      MARITAL %in% c(2,3,4,5,6) ~ "Currently_not_married",
      MARITAL == 9 ~ NA_character_,
      is.na(MARITAL) ~ NA_character_,
      TRUE~ NA_character_),
    maritalstatus = factor(maritalstatus, levels = c("Married", "Currently_not_married"))
  ) |>
  
  # Encode EDUCA
  mutate(
    EDUCA = as.integer(EDUCA),
    education = case_when(
      EDUCA %in% c(1, 2, 3) ~ "Less_than_HS",  
      EDUCA == 4 ~ "HS_grad",       
      EDUCA == 5 ~ "Some_college",    
      EDUCA == 6 ~ "College_grad",   
      EDUCA == 9 ~ NA_character_,
      is.na(EDUCA) ~ NA_character_,
      TRUE ~ NA_character_),
    education = factor(education, levels = c(
      "College_grad", "Some_college", "HS_grad", "Less_than_HS"))
  ) |>
  
  # Encode EMPLOY1
  mutate(
    EMPLOY1 = as.integer(EMPLOY1),
    employment = case_when(
      EMPLOY1 %in% c(1, 2) ~ "Employed",      
      EMPLOY1 %in% c(3, 4) ~ "Unemployed",     
      EMPLOY1 %in% c(5, 6) ~ "Not_in_labor",   
      EMPLOY1 == 7 ~ "Retired",       
      EMPLOY1 == 8 ~ "Unable_to_work", 
      EMPLOY1 == 9 ~ NA_character_,
      is.na(EMPLOY1) ~ NA_character_,
      TRUE ~ NA_character_),
    employment = factor(employment, levels = c(
      "Employed", "Unemployed", "Not_in_labor", "Retired", "Unable_to_work"))
  ) |>
  
  # Health status 
  # Encode GENHLTH
  mutate(
    GENHLTH = as.integer(GENHLTH),
    healthstatus = case_when(
      GENHLTH == 1 ~ "Excellent",
      GENHLTH == 2 ~ "Very_good",
      GENHLTH == 3 ~ "Good",
      GENHLTH == 4 ~ "Fair",
      GENHLTH == 5 ~ "Poor",
      GENHLTH %in% c(7, 9) ~ NA_character_,
      is.na(GENHLTH) ~ NA_character_,
      TRUE ~ NA_character_),
    healthstatus = factor(healthstatus, levels = c(
      "Excellent", "Very_good", "Good", "Fair", "Poor"))
  ) |>
  
  # Encode PHYSHLTH
  mutate( 
    PHYSHLTH = as.integer(PHYSHLTH),
    physhlth_days = case_when(
      PHYSHLTH == 88 ~ 0,       
      PHYSHLTH %in% c(77, 99) ~ NA_real_,
      is.na(PHYSHLTH) ~ NA_real_,
      TRUE ~ as.numeric(PHYSHLTH))
  ) |>
  
  # Encode MENTHLTH
  mutate( 
    MENTHLTH = as.integer(MENTHLTH),
    menthlth_days = case_when(
      MENTHLTH == 88 ~ 0,       
      MENTHLTH %in% c(77, 99) ~ NA_real_,
      is.na(MENTHLTH) ~ NA_real_,
      TRUE ~ as.numeric(MENTHLTH))
  ) |>
  
  # Encode ADDEPEV3
  mutate(
    ADDEPEV3 = as.integer(ADDEPEV3),
    depression = case_when(
      ADDEPEV3 == 1 ~ 1,
      ADDEPEV3 == 2 ~ 0,
      ADDEPEV3 %in% c(7, 9) ~ NA_integer_,
      is.na(ADDEPEV3) ~ NA_integer_,
      TRUE ~ NA_integer_)
  ) |>
  
  # Create chronic_count
  mutate(
    d_DIABETE4 = case_when(
      as.integer(DIABETE4) == 1 ~ 1,
      as.integer(DIABETE4) %in% c(2, 3, 4) ~ 0,
      as.integer(DIABETE4) %in% c(7, 9) ~ NA_integer_,
      is.na(DIABETE4) ~ NA_integer_,
      TRUE ~ NA_integer_),
    d_CVDINFR4 = yn_flag(CVDINFR4),
    d_CVDCRHD4 = yn_flag(CVDCRHD4),
    d_CVDSTRK3 = yn_flag(CVDSTRK3),
    d_ASTHMA3  = yn_flag(ASTHMA3),
    d_CHCCOPD3 = yn_flag(CHCCOPD3),
    d_CHCKDNY2 = yn_flag(CHCKDNY2),
    d_HAVARTH4 = yn_flag(HAVARTH4),
    
    chronic_count = as.integer(rowSums(
      across(c(d_DIABETE4, d_CVDINFR4, d_CVDCRHD4, d_CVDSTRK3,
               d_ASTHMA3,  d_CHCCOPD3, d_CHCKDNY2, d_HAVARTH4)), na.rm = TRUE))
  ) |>
  
  # Create any_disability 
  mutate(
    d_DEAF = yn_flag(DEAF),
    d_BLIND = yn_flag(BLIND),
    d_DECIDE = yn_flag(DECIDE),
    d_DIFFWALK = yn_flag(DIFFWALK),
    d_DIFFDRES = yn_flag(DIFFDRES),
    d_DIFFALON = yn_flag(DIFFALON),
    
    any_disability = case_when(
      d_DEAF == 1 | d_BLIND == 1 | d_DECIDE == 1 | 
        d_DIFFWALK == 1 | d_DIFFDRES == 1 | d_DIFFALON == 1 ~ 1,
      rowSums(is.na(across(c(d_DEAF, d_BLIND, d_DECIDE, d_DIFFWALK, 
                             d_DIFFDRES, d_DIFFALON)))) == 6 ~ NA_integer_,
      TRUE ~ 0)
  ) |>
  
  # Healthcare access
  # Encode PRIMINS2
  mutate(
    PRIMINS2 = as.integer(PRIMINS2),
    insurance = case_when(
      PRIMINS2 == 1 ~ "Employer",
      PRIMINS2 == 2 ~ "Private",
      PRIMINS2 %in% c(3, 4) ~ "Medicare",
      PRIMINS2 %in% c(5, 6) ~ "Medicaid_CHIP",
      PRIMINS2 == 7 ~ "Military_VA",
      PRIMINS2 %in% c(8,9,10) ~ "Other_govt",
      PRIMINS2 == 88 ~ "Uninsured",    
      PRIMINS2 %in% c(77, 99) ~ NA_character_,  
      is.na(PRIMINS2) ~ NA_character_,
      TRUE ~ NA_character_),
    insurance = factor(insurance, levels = c(
      "Employer", "Private", "Medicare", "Medicaid_CHIP", 
      "Military_VA", "Other_govt", "Uninsured"))
  ) |>
  
  # Encode PERSDOC3
  mutate(
    PERSDOC3 = as.integer(PERSDOC3),
    pers_doc = case_when(
      PERSDOC3 %in% c(1, 2) ~ "Yes",   
      PERSDOC3 == 3 ~ "No",   
      PERSDOC3 %in% c(7, 9) ~ NA_character_,
      is.na(PERSDOC3) ~ NA_character_,
      TRUE ~ NA_character_),
    pers_doc = factor(pers_doc, levels = c("Yes", "No"))
  ) |>
  
  # Encode CHECKUP1
  mutate(
    CHECKUP1 = as.integer(CHECKUP1),
    last_checkup = case_when(
      CHECKUP1 == 1 ~ "Within_1yr",    
      CHECKUP1 == 2 ~ "1_2yrs",        
      CHECKUP1 == 3 ~ "2_5yrs",     
      CHECKUP1 == 4 ~ "Over_5yrs", 
      CHECKUP1 == 8 ~ "Never",       
      CHECKUP1 %in% c(7, 9) ~ NA_character_,
      is.na(CHECKUP1) ~ NA_character_,
      TRUE ~ NA_character_),
    last_checkup = factor(last_checkup, levels = c(
      "Within_1yr", "1_2yrs", "2_5yrs", "Over_5yrs", "Never"))
  ) |>
  
  # Survey design
  rename(
    PSU = `_PSU`,
    STSTR  = `_STSTR`,
    WEIGHT = `_LLCPWT`
  ) |>
  
  # Select needed variables
  select(
    # Outcome
    MEDCOST1,
    # Demographic
    region, rurality, sex, agegroup, race, 
    income, maritalstatus, education, employment,
    # Health status
    healthstatus, physhlth_days, menthlth_days,
    depression, chronic_count, any_disability,
    # Healthcare access
    insurance, pers_doc, last_checkup,
    # Survey design
    PSU, STSTR, WEIGHT
  ) |>
  # Drop na directly as only < 5%
  drop_na(
    rurality, agegroup,maritalstatus, education, employment, healthstatus, 
    physhlth_days, menthlth_days, depression, any_disability, 
    insurance, pers_doc, last_checkup
  )

# Save clean dataset
saveRDS(cleandat, file = "data/brfss2024_clean.rds")
