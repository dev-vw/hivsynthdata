# birthday generation
decimal_age <- 34.5678
anchor_date <- as.Date("2024-12-31")
whole_years <- floor(decimal_age)
fraction_years <- decimal_age - whole_years

birthday <- anchor_date - years(whole_years)
birthday <- birthday - days(round(365.2425 * fraction_years))

# interaction initialization test -----------------------------------------
patient_data <- init_population("Eswatini")

n_interactions_tbl <- patient_data |>
  select(patient_id, age_dec, dob, preg_status) |>
  rowwise() |>
  mutate(n_interactions = gen_n_interactions())

interactions_dates_list <- lapply(1:nrow(n_interactions_tbl), function(i) {
  gen_interaction_dates_ran(n_interactions = n_interactions_tbl$n_interactions[i])
})

interactions_dates_list <- setNames(interactions_list, n_interactions_tbl$patient_id)

# interactions <- lapply(names(interactions_list),
#                        function(patient_id) {
#                          data.frame(
#                            patient_id = patient_id,
#                            interaction_dat = unlist(interactions_list[[patient_id]])
#                          )
#                        })

interaction_dates_tbl <- do.call(rbind,
                            lapply(names(interactions_list),
                                   function(patient_id) {
                                     data.frame(
                                       patient_id = patient_id,
                                       interaction_dat = unlist(interactions_list[[patient_id]])
                                     )
                               }))

interactions <- interactions %>%
  mutate(
    interaction_type = ifelse(
      duplicated(interactions) | duplicated(interactions, fromLast = TRUE),
      "lab",
      NA
    )
  ) %>%
  rowwise() %>%
  mutate(
    interaction_type = ifelse(is.na(interaction_type),
                              sample(interaction_type_probs$interaction_type,
                                     size = 1,
                                     replace = TRUE,
                                     prob = interaction_type_probs$prob),
                              interaction_type
    )
  )


interactions$interaction_id <- create_unique_ids(8, nrow(interactions))

# interaction probability assignments -------------------------------------
patient_data <- init_population("Eswatini")
interactions <- init_interactions(patient_data)

# test the duplication
test <- interactions %>% select(patient_id, interaction_date, interaction_type)
test <- test[duplicated(test) | duplicated(test, fromLast = TRUE), ]
sum(test$interaction_type == "lab") == nrow(test)

# any negative ages?
View(interactions[which(interactions$interaction_age < 0), ])

# interaction age calc
start_date <- as.Date(interactions$dob)
end_date <- as.Date(interactions$interaction_date)


# preg status test --------------------------------------------------------
df <- interactions
mini_df <- read.csv("data-raw/pregnant_case.csv")

df <- df |>
  dplyr::group_by(patient_id) |>
  dplyr::arrange(patient_id, interaction_date)


# lab data generation START -----------------------------------------------
interactions <- readr::read_csv("outputs/interactions_11272024.csv") |>
  dplyr::select(-`...1`)


