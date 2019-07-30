library(showtext)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.showtext=TRUE,
  dpi=300
)

font_paths(
  system.file("font/", package = "MoreyHoekstra2019")
)

font_add("lato",
         regular = "Lato-Light.ttf",
         italic = "Lato-LightItalic.ttf",
         bold = "Lato-Semibold.ttf",
         bolditalic = "Lato-SemiboldItalic.ttf",
         symbol = "Lato-LightItalic.ttf")

knitr::knit_hooks$set(
  lato = function(before, options, envir)
    if (before) par(family = "lato")
)

knitr::opts_chunk$set(lato = TRUE)
