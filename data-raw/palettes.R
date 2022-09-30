cbPalette8 <- c( # "Okabe-Ito"
  "#000000", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)
cbPalette15 <- c(
  rgb(000, 000, 000, maxColorValue = 255),
  rgb(000, 073, 073, maxColorValue = 255),
  rgb(000, 146, 146, maxColorValue = 255),
  rgb(255, 109, 182, maxColorValue = 255),
  rgb(255, 182, 219, maxColorValue = 255),
  rgb(073, 000, 146, maxColorValue = 255),
  rgb(000, 109, 219, maxColorValue = 255),
  rgb(182, 109, 255, maxColorValue = 255),
  rgb(109, 182, 255, maxColorValue = 255),
  rgb(182, 219, 255, maxColorValue = 255),
  rgb(146, 000, 000, maxColorValue = 255),
  rgb(146, 073, 000, maxColorValue = 255),
  rgb(219, 109, 000, maxColorValue = 255),
  rgb(036, 255, 036, maxColorValue = 255),
  rgb(255, 255, 109, maxColorValue = 255)
)

cbPalette13 <- cbPalette15[-c(4, 7)]

usethis::use_data(cbPalette8, cbPalette13, cbPalette15,
  overwrite = TRUE,
  compress = "gzip"
)
