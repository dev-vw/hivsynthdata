% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_init-interactions.R
\name{gen_interaction_dates_ran}
\alias{gen_interaction_dates_ran}
\title{Generates a vector of interaction dates (for a given patient) at
random intervals}
\usage{
gen_interaction_dates_ran(
  date_start = "2020-01-01",
  date_end = "2024-12-31",
  n_interactions,
  max_repeats = 2
)
}
\arguments{
\item{date_start}{String in format "YYYY-MM-DD"}

\item{date_end}{String in format "YYYY-MM-DD"}

\item{n_interactions}{int}

\item{max_repeats}{int}
}
\value{
a vector, interaction_dates, that includes a series of Date objects
}
\description{
Generates a vector of interaction dates (for a given patient) at
random intervals
}
