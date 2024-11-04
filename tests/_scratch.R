# birthday generation
decimal_age <- 34.5678
anchor_date <- as.Date("2024-12-31")
whole_years <- floor(decimal_age)
fraction_years <- decimal_age - whole_years

birthday <- anchor_date - years(whole_years)
birthday <- birthday - days(round(365.2425 * fraction_years))
