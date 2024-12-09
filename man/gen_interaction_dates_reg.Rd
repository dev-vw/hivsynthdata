% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_init-interactions.R
\name{gen_interaction_dates_reg}
\alias{gen_interaction_dates_reg}
\title{Generates a vector of interaction dates (for a given patient) at
regular intervals}
\usage{
gen_interaction_dates_reg(
  date_start = "2020-01-01",
  date_end = "2024-12-31",
  n_interactions
)
}
\arguments{
\item{date_start}{String in format "YYYY-MM-DD"}

\item{date_end}{String in format "YYYY-MM-DD"}

\item{n_interactions}{int}
}
\value{
a vector, interaction_dates, that includes a series of Date objects
}
\description{
Generates a vector of interaction dates (for a given patient) at
regular intervals
}
