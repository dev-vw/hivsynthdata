library(tidyverse)

# clean patient look-up ---------------------------------------------------

# import first and last names
first_names <- read.csv("data-raw/first_name.csv")
last_names <- read.csv("data-raw/last_name.csv")

first_names_swz <- first_names %>% filter(Country == "Eswatini")
last_names_swz <- last_names %>% filter(Country == "Eswatini")

# import patient look up
patient_lookup <- read.csv("data-raw/age_sex_preg.csv")
patient_lookup$prob <- patient_lookup$prob/100

# if male, then preg_status should be NA
patient_lookup <- patient_lookup %>% mutate(preg_status = ifelse(sex == "Male", NA, preg_status))

patient_lookup_swz <- patient_lookup[patient_lookup$operatingunit == "Eswatini", ]

# generate n pop 2500 -----------------------------------------------------

# Create an index for sampling based on probabilities
sample_indices <- sample(1:nrow(patient_lookup_swz),
                         size = 2500,
                         replace = TRUE,
                         prob = patient_lookup_swz$prob)

# Create the patient population
patient_data <- patient_lookup_swz[sample_indices, ] %>% select(-prob)

# reset rownames
rownames(patient_data) <- NULL


# generate attributes -----------------------------------------------------

patient_data$patient_id <- create_unique_ids(7, 2500)

patient_data$age <- calc_age(patient_data$agegroup)
patient_data$birthday <- calc_birthday(patient_data$age, "2024-12-31")
patient_data$birthday <- as.Date(patient_data$birthday,
                                 "1970-01-01")
patient_data <- patient_data %>%
  rowwise() %>%
  mutate(
    first_name = sample(first_names_swz$First_Name[first_names_swz$Sex == sex], 1),
    last_name = sample(last_names_swz$Last_Name, 1)
  ) %>%
  ungroup()

patient_data$age <- floor(patient_data$age)
patient_data$phone_number <- gen_phonenumber(2500)

# clean up output file ----------------------------------------------------

patient_data <- patient_data %>% select(operatingunit, patient_id,
                                        first_name, last_name, sex,
                                        agegroup, age, birthday, preg_status, phone_number)

write_csv(patient_data, "outputs/patient_data.csv")
View(patient_data)
