library(showtext)
library(MoreyHoekstra2019)

## Set up lato font
font_paths(
  system.file("font/", package = "MoreyHoekstra2019")
)

font_add("lato",
         regular = "Lato-Light.ttf",
         italic = "Lato-LightItalic.ttf",
         bold = "Lato-Semibold.ttf",
         bolditalic = "Lato-SemiboldItalic.ttf",
         symbol = "Lato-LightItalic.ttf")

# Set default font
knitr::knit_hooks$set(
  MH2019 = function(before, options, envir)
    if (before) par(family = MoreyHoekstra2019::pkg_options("base_family"))
)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.showtext=TRUE,
  dpi=300,
  ### This sets the font use
  MH2019 = TRUE 
)
