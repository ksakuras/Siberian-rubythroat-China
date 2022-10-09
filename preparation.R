library(devtools)
devtools::install_github("Rafnuss/GeoPressureR")
devtools::install()

library(GeoPressureR)

# Only used for some visualization. The code to compute the light position is included in `GeoPressureR`
library(GeoLocTools)
setupGeolocation()

# ERA5 data download library
library(ecmwfr)

# Graph library
library(igraph)

# Plotting library
library(ggplot2)
library(gridExtra)
library(plotly)
library(RColorBrewer)

# Interactif figure library
library(leaflet)
library(leaflet.extras)
