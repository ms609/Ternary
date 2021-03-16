holdridge <- read.csv(system.file('data-raw', 'Holdridge_data.csv',
                                  package = 'Ternary'), row.names = 1)
usethis::use_data(holdridge, overwrite = TRUE)
