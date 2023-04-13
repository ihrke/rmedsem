## code to prepare `workout` dataset goes here
workout <- haven::read_dta("data-raw/workout.dta")

usethis::use_data(workout, overwrite = TRUE)
