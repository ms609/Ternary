holdridge <- read.csv(system.file('data-raw', 'Holdridge_data.csv',
                                  package = 'Ternary'), row.names = 1)
usethis::use_data(holdridge, overwrite = TRUE)

holdridgeClasses <- c(
  "Polar desert",
  "Subpolar dry tundra",
  "Subpolar moist tundra",
  "Subpolar wet tundra",
  "Subpolar rain tundra",
  "Boreal desert",
  "Boreal dry scrub",
  "Boreal moist forest",
  "Boreal wet forest",
  "Boreal rain forest",
  "Cool temperate desert",
  "Cool temperate desert scrub",
  "Cool temperate steppe",
  "Cool temperate moist forest",
  "Cool temperate wet forest",
  "Cool temperate rain forest",
  "Warm temperate desert",
  "Warm temperate desert scrub",
  "Warm temperate thorn scrub",
  "Warm temperate dry forest",
  "Warm temperate moist forest",
  "Warm temperate wet forest",
  "Warm temperate rain forest",
  "Subtropical desert",
  "Subtropical desert scrub",
  "Subtropical thorn woodland",
  "Subtropical dry forest",
  "Subtropical moist forest",
  "Subtropical wet forest",
  "Subtropical rain forest",
  "Tropical desert",
  "Tropical desert scrub",
  "Tropical thorn woodland",
  "Tropical very dry forest",
  "Tropical dry forest",
  "Tropical moist forest",
  "Tropical wet forest",
  "Tropical rain forest"
)
usethis::use_data(holdridgeClasses, overwrite = TRUE)

holdridgeClassesUp <- gsub(" ", "\n", holdridgeClasses, fixed = TRUE)
usethis::use_data(holdridgeClassesUp, overwrite = TRUE)

holdridgeLifeZones <- c(rep('Polar', 3),
                        paste(c('Dry', 'Moist', 'Wet', 'Rain'), 'tundra'),
                        'Desert', 'Dry bush', 'Moist furest', 'Wet forest', 'Rain forest',
                        'Desert', 'Desert scrub', 'Steppe', 'Moist forest', 'Wet forest', 'Rain forest',
                        'Desert', 'Desert scrub', 'Thorn steppe', 'Dry forest', 'Moist forest', 'Wet forest', 'Rain forest',
                        'Desert', 'Desert scrub', 'Thorn woodland', 'Very dry forest', 'Dry forest', 'Moist forest', 'Wet forest', 'Rain forest')
usethis::use_data(holdridgeLifeZones, overwrite = TRUE)

holdridgeLifeZonesUp <- gsub(" ", "\n", holdridgeLifeZones, fixed = TRUE)
usethis::use_data(holdridgeLifeZonesUp, overwrite = TRUE)
