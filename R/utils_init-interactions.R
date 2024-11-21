#' Generates random number of interactions. Not vectorized.
#'
#' @param mean int mean number of interactions for normal distribution
#' @param sd dbl sd of interactions
#' @param min int min interactions
#' @param max int max interactions
#'
#' @return n number of interactions that follow the desired normal distribution
gen_n_interactions <- function(mean = 8,
                               sd = 3,
                               min = 1,
                               max = 15) {
  n_interactions <- round(rnorm(1, mean = mean, sd = sd))

  while (n_interactions < min || n_interactions > max) {
    n_interactions <- round(rnorm(1, mean = mean, sd = sd))
  }

  return(n_interactions)
}

#' Generates a vector of interaction dates (for a given patient) at
#' regular intervals
#'
#' @param date_start String in format "YYYY-MM-DD"
#' @param date_end String in format "YYYY-MM-DD"
#' @param n_interactions int
#'
#' @return a vector, interaction_dates, that includes a series of Date objects
gen_interaction_dates_reg <- function(date_start = "2020-01-01",
                                      date_end = "2024-12-31",
                                      n_interactions) {

  date_start <- as.Date(date_start)
  date_end <- as.Date(date_end)

  interaction_dates <- seq.Date(from = date_start,
                                to = date_end,
                                length.out = n_interactions)

  return(interaction_dates)
}

#' Generates a vector of interaction dates (for a given patient) at
#' random intervals
#'
#' @param date_start String in format "YYYY-MM-DD"
#' @param date_end String in format "YYYY-MM-DD"
#' @param n_interactions int
#' @param max_repeats int
#'
#' @return a vector, interaction_dates, that includes a series of Date objects
gen_interaction_dates_ran <- function(date_start = "2020-01-01",
                                      date_end = "2024-12-31",
                                      n_interactions,
                                      max_repeats = 2) {

  date_start <- as.Date(date_start)
  date_end <- as.Date(date_end)

  interaction_dates <- c()

  while (length(interaction_dates) < n_interactions) {

    selected_date <- sample(x = seq.Date(from = date_start,
                                         to = date_end,
                                         by = "day"),
                            size = 1) # Select one random day within date range

    if (sum(interaction_dates == selected_date) < max_repeats) {
      interaction_dates <- c(interaction_dates, selected_date) # Add to the result
    }
  }

  interaction_dates <- as.Date(interaction_dates, "1970-01-01")

  # Simple solution: randomly selected interaction dates within date range
  #                  without replacements
  # interaction_dates <- sample(x = seq.Date(from = date_start,
  #                                          to = date_end,
  #                                          by = "day"),
  #                             size = n_interactions)

  return(interaction_dates)
}

#' Assigns interaction types and sources given n
#'
#' @importFrom dplyr mutate case_when
#' @param n int
#'
#' @return df
assign_interaction_type_source <- function(n) {
  interaction_type <- sample(interaction_type_probs$interaction_type,
                             size = n,
                             replace = TRUE,
                             prob = interaction_type_probs$prob)

  types_sources <- data.frame(
    interaction_type = interaction_type
  )

  types_sources <- types_sources |>
    mutate(interaction_source = case_when(
      interaction_type %in% c("clinic", "rx-pickup") ~ "EMR",
      interaction_type == "lab" ~ "LIS",
      TRUE ~ "CHIS"
    ))

  return(types_sources)
}
