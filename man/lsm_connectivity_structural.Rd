% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lsm_connectivity_strutural.R
\name{lsm_connectivity_structural}
\alias{lsm_connectivity_structural}
\title{Calculate structural connectivity}
\usage{
lsm_connectivity_structural(
  input,
  output = NULL,
  zero_as_null = FALSE,
  id_direction = 8,
  area_round_digit = 0,
  area_unit = "ha",
  map_connec_struct = TRUE,
  map_connec_struct_area = FALSE,
  nprocs = 1,
  memory = 300
)
}
\arguments{
\item{input}{\verb{[character]} Habitat map (binary classification: e.g., 1/0 or 1/NA) in GRASS.}

\item{output}{\verb{[character]} Output map base name in GRASS.}

\item{zero_as_null}{\verb{[logical]} If TRUE, non-habitat (0) cells are converted to NULL.}

\item{id_direction}{\verb{[numeric]} Neighborhood for clumping (4 or 8).}

\item{area_round_digit}{\verb{[integer]} Decimal digits for area rounding.}

\item{area_unit}{\verb{[character]} Area unit: \code{"ha"}, \code{"m2"}, or \code{"km2"}.}

\item{map_id}{\verb{[logical]} Keep fragment ID raster?}

\item{map_ncell}{\verb{[logical]} Output raster with fragment cell counts?}

\item{map_area}{\verb{[logical]} Output raster with fragment area?}
}
\description{
Identifies patch and calculates area in hectare.
}
\examples{
library(lsmetrics)
library(terra)

# read habitat data
r <- lsmetrics::lsm_toy_landscape(proj_type = "meters")

# plot
plot(r, legend = FALSE, axes = FALSE, main = "Binary habitat")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r)

# find grass
path_grass <- system("grass --config path", inter = TRUE) # windows users need to find the grass gis path installation, e.g. "C:/Program Files/GRASS GIS 8.3"

# create grassdb
rgrass::initGRASS(gisBase = path_grass,
                  SG = r,
                  gisDbase = "grassdb",
                  location = "newLocation",
                  mapset = "PERMANENT",
                  override = TRUE)


# import raster from r to grass
rgrass::write_RAST(x = r, flags = c("o", "overwrite", "quiet"), vname = "r", verbose = FALSE)

# structural connectivity
lsmetrics::lsm_connectivity_structural(input = "r",
                                       zero_as_null = FALSE,
                                       map_connec_struct = TRUE,
                                       map_connec_struct_area = TRUE)

# files
rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_struct_connec_area <- rgrass::read_RAST("r_struct_connec_area", flags = "quiet", return_format = "terra")

plot(r_struct_connec_area, legend = FALSE, axes = FALSE, main = "Structural connected area (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_struct_connec_area)

# import from grass to r
r_struct_connec <- rgrass::read_RAST("r_struct_connec", flags = "quiet", return_format = "terra")

plot(r_struct_connec, legend = FALSE, axes = FALSE, main = "Structural connectivity (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_struct_connec)

# delete grassdb
unlink("grassdb", recursive = TRUE)

}
