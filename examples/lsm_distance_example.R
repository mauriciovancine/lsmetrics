library(lsmetrics)
library(terra)
library(sp)

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

# distance
lsm_distance(input = "r", zero_as_na = FALSE, type = "inside")
lsm_distance(input = "r", zero_as_na = FALSE, type = "outside")

# files
# rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_dist_in <- terra::rast(rgrass::read_RAST("r_distance_inside", flags = "quiet", return_format = "SGDF"))
r_dist_out <- terra::rast(rgrass::read_RAST("r_distance_outside", flags = "quiet", return_format = "SGDF"))

# plot
plot(r_dist_in, legend = FALSE, axes = FALSE, main = "Distance inside (m)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_dist_in, cex = .5)

plot(r_dist_out, legend = FALSE, axes = FALSE, main = "Distance outside (m)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_dist_out, cex = .5)

# delete grassdb
unlink("grassdb", recursive = TRUE)
