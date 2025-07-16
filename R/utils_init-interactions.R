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

#' Assigns interaction types to an input interaction df
#'
#' @importFrom dplyr mutate case_when rowwise
#' @param df a data frame
#'
#' @return df
assign_interaction_type <- function(df) {

  df <- df |>
    dplyr::mutate(
      interaction_type = ifelse(
        duplicated(df) | duplicated(df, fromLast = TRUE),
        "lab",
        NA
      )
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      interaction_type = ifelse(is.na(interaction_type),
                                sample(interaction_type_probs$interaction_type,
                                       size = 1,
                                       replace = TRUE,
                                       prob = interaction_type_probs$prob),
                                interaction_type
      )
    )

  return(df)
}

#' Assigns interaction sources to an input interaction df
#'
#' @importFrom dplyr mutate case_when
#' @param df a data frame
#'
#' @return df
assign_interaction_source <- function(df) {
  df <- df |>
    dplyr::mutate(interaction_source = dplyr::case_when(
      interaction_type %in% c("clinic", "rx-pickup") ~ "EMR",
      interaction_type == "lab" ~ "LIS",
      TRUE ~ "CHIS"
    ))

  return(df)
}

#' Calculates age give date anchor
#'
#' @param interaction_dates a vector of Dates
#' @param dobs a vector of dob
#'
#' @return a vector of decimal ages
calc_age <- function(interaction_dates, dobs) {

  start_date <- as.Date(dobs)
  end_date <- as.Date(interaction_dates)

  age_at_interaction <- round(as.numeric(difftime(end_date,
                                            start_date,
                                            units = "days") / 365.25),
                              digits = 1)

  return(age_at_interaction)
}

#' Assigns agegroup
#'
#' @param df an interaction df
#'
#' @return df
assign_agegroup <- function(df) {
  df <- df |>
    dplyr::mutate(
      agegroup_interaction = dplyr::case_when(
        interaction_age < 1 ~ "<01",
        interaction_age >= 1 & interaction_age < 5 ~ "1-4",
        interaction_age >= 5 & interaction_age < 10 ~ "5-9",
        interaction_age >= 10 & interaction_age < 15 ~ "10-14",
        interaction_age >= 15 & interaction_age < 20 ~ "15-19",
        interaction_age >= 20 & interaction_age < 25 ~ "20-24",
        interaction_age >= 25 & interaction_age < 30 ~ "25-29",
        interaction_age >= 30 & interaction_age < 35 ~ "30-34",
        interaction_age >= 35 & interaction_age < 40 ~ "35-39",
        interaction_age >= 40 & interaction_age < 45 ~ "40-44",
        interaction_age >= 45 & interaction_age < 50 ~ "45-49",
        interaction_age >= 50 & interaction_age < 55 ~ "50-54",
        interaction_age >= 55 & interaction_age < 60 ~ "55-59",
        interaction_age >= 60 & interaction_age < 65 ~ "60-64",
        interaction_age >= 65 ~ "65+",
        TRUE ~ NA
      )
    )

  return(df)
}

#' Assigns pregnancy status for each interaction
#'
#' @param df an interaction df
#'
#' @return a df
assign_pregstatus <- function(df) {

  df <- df |>
    dplyr::group_by(patient_id) |>
    dplyr::group_modify(~ update_pregstatus("Eswatini", .)) |>
    dplyr::ungroup()
  return(df)
}

#' Updates pregnancy status
#'
#' @param df a df
#' @param country a string
#'
#' @importFrom dplyr arrange
#' @return a updated df
update_pregstatus <- function(country, df) {
  # ensure reverse chronological order in this df
  df <- df |>
    dplyr::arrange(desc(interaction_date))

  # initialize rb and start preg dates
  df$rb_pregdate <- NA
  df$start_pregdate <- NA

  if (unique(df$sex) == "Female" & nrow(df) > 1) {
    # initial preg status
    anchor_pregstatus <- df[1, ]$preg_status
    anchor_interaction_date <- df[1, ]$interaction_date

    # initial prior_pregdate
    preg_bounds <- calc_pregdate_bounds(anchor_pregstatus,
                                        anchor_interaction_date)

    df[1, ]$rb_pregdate <- format(preg_bounds$rb_pregdate, "%Y-%m-%d")
    df[1, ]$start_pregdate <- format(preg_bounds$start_pregdate, "%Y-%m-%d")

    for (i in 2:nrow(df)) {
      int_date <- df[i, ]$interaction_date
      start_date <- df[i-1, ]$start_pregdate
      rb_date <- df[i-1, ]$rb_pregdate

      if (df[i-1, ]$preg_status == "pregnant") {
        # if woman was pregnant at prior interaction

        if (int_date > start_date) {
          # she is still pregnant, preg_status reflects that
          df[i, ]$preg_status <- "pregnant"
          df[i, ]$start_pregdate <- start_date
          df[i, ]$rb_pregdate <- rb_date
        } else if (int_date < start_date & int_date > rb_date) {
          # she isn't pregnant, reassign preg_status and start_pregdate
          df[i, ]$preg_status <- "not pregnant"
          df[i, ]$start_pregdate <- NA
          df[i, ]$rb_pregdate <- rb_date
        } else {
          # woman was not pregnant
          ag <- df[i, ]$agegroup_interaction

          # new lookup table
          preg_probs <- age_sex_preg |>
            dplyr::filter(operatingunit == country, sex == "Female") |>
            dplyr::group_by(agegroup) |>
            dplyr::reframe(preg_status = preg_status, prob = prob/sum(prob)) |>
            dplyr::ungroup() |>
            dplyr::filter(agegroup == ag)

          # roll the dice on pregnancy
          is_preg <- sample(x = preg_probs$preg_status,
                            size = 1,
                            prob = preg_probs$prob)

          # is_preg <- sample(x = c("pregnant", "not pregnant"),
          #                   size = 1,
          #                   prob = c(0.5, 0.5))

          df[i, ]$preg_status <- is_preg
          start_gest <- lubridate::interval(int_date,
                                            df[i-1, ]$interaction_date) %/% months(1)

          start_gest <- ifelse(start_gest > 9, 9, start_gest)
          preg_bounds <- calc_pregdate_bounds(is_preg,
                                              int_date,
                                              start_gest)

          df[i, ]$start_pregdate <- format(preg_bounds$start_pregdate, "%Y-%m-%d")
          df[i, ]$rb_pregdate <- format(preg_bounds$rb_pregdate, "%Y-%m-%d")

        }
      } else {
        # woman was not pregnant at prior interaction
        ag <- df[i, ]$agegroup_interaction

        # new lookup table
        preg_probs <- age_sex_preg |>
          dplyr::filter(operatingunit == country, sex == "Female") |>
          dplyr::group_by(agegroup) |>
          dplyr::reframe(preg_status = preg_status, prob = prob/sum(prob)) |>
          dplyr::ungroup() |>
          dplyr::filter(agegroup == ag)

        # roll the dice on pregnancy
        is_preg <- sample(x = preg_probs$preg_status,
                          size = 1,
                          prob = preg_probs$prob)

        # is_preg <- sample(x = c("pregnant", "not pregnant"),
        #                   size = 1,
        #                   prob = c(0.5, 0.5))

        df[i, ]$preg_status <- is_preg
        start_gest <- lubridate::interval(int_date,
                                          df[i-1, ]$interaction_date) %/% months(1)

        start_gest <- ifelse(start_gest > 9, 9, start_gest)
        preg_bounds <- calc_pregdate_bounds(is_preg,
                                            int_date,
                                            start_gest)

        df[i, ]$start_pregdate <- format(preg_bounds$start_pregdate, "%Y-%m-%d")
        df[i, ]$rb_pregdate <- format(preg_bounds$rb_pregdate, "%Y-%m-%d")
      }
    }
  }
  return(df)
}

#' Calculates preg date bounds
#'
#' @param anchor_pregstatus a date
#' @param anchor_interaction_date a date
#' @param start_gest an int
#'
#' @return a list of two vars
calc_pregdate_bounds <- function(anchor_pregstatus,
                                 anchor_interaction_date,
                                 start_gest = 1) {
  interpreg_months <- 18 # set as constant; 18 months between pregnancies
  gest_months <- ifelse(anchor_pregstatus == "pregnant", sample(start_gest:9, 1), NA)

  # - subtract gest_months to determine the start date of pregnancy
  # - will only have a non-NA value if pregnant
  # (approximate as 9 * 30.4375 days)
  start_pregdate <- as.Date(anchor_interaction_date) - (gest_months * 30.4375)
  rb_pregdate <- start_pregdate - (interpreg_months * 30.4375)

  return(list(start_pregdate = start_pregdate, rb_pregdate = rb_pregdate))
}
