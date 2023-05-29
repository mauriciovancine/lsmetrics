library(lsmetrics)
library(terra)

# read habitat data
f <- system.file("raster/toy_landscape_habitat.tif", package = "lsmetrics")
r <- terra::rast(f)

# plot
plot(r, legend = FALSE, axes = FALSE, main = "Binary habitat")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r)

# find grass
path_grass <- as.character(link2GI::findGRASS()[1]) # windows users need to find, e.g. "C:/Program Files/GRASS GIS 8.2"

# create grassdb
rgrass::initGRASS(gisBase = path_grass,
                  SG = r,
                  gisDbase = "grassdb",
                  location = "newLocation",
                  mapset = "PERMANENT",
                  override = TRUE)

# import raster from r to grass
rgrass::write_RAST(x = r, flags = c("o", "overwrite", "quiet"), vname = "r")

# inside distance
lsm_distance(input = "r", zero_as_na = FALSE, type = "outside")

# functional connectivity
lsm_functional_connectivity(input = "r",
                            zero_as_na = FALSE,
                            input_distance_outside = "r_distance_outside",
                            gap_crossing = 100)

# files
# rgrass::execGRASS(cmd = "g.list", type = "raster")

# import do r
r_confun100_pid <- rgrass::read_RAST("r_confun100_pid", flags = "quiet", return_format = "terra")

plot(r_confun100_pid, legend = FALSE, axes = FALSE, main = "Patch id")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_confun100_pid)

# import habitat patch area to r
r_confun100_area <- rgrass::read_RAST("r_confun100_area_ha", flags = "quiet", return_format = "terra")

plot(r_confun100_area, legend = FALSE, axes = FALSE, main = "Functional connectivity (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_confun100_area)

# delete grassdb
unlink("grassdb", recursive = TRUE)
