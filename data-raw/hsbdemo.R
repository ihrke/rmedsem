## code to prepare `hsbdemo` dataset goes here
hsbdemo <- readr::read_csv("https://raw.githubusercontent.com/rlowrance/re/master/hsbdemo.csv")
hsbdemo <- hsbdemo |> dplyr::select(-1)
readr::write_csv(hsbdemo, "data-raw/hsbdemo.csv")
usethis::use_data(hsbdemo, overwrite = TRUE)
