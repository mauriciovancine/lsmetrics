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
rgrass::write_RAST(x = r, flags = c("o", "overwrite", "quiet"), vname = "r", verbose = FALSE)

# perimeter
lsmetrics::lsm_perimeter(input = "r")

# files
# rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_perimeter <- terra::rast(rgrass::read_RAST("r_perimeter", flags = "quiet", return_format = "SGDF"))

# plot
plot(r_perimeter, legend = FALSE, axes = FALSE, main = "Perimeter (m)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_perimeter, cex = .5)

# import from grass to r
r_perimeter_area_ratio <- terra::rast(rgrass::read_RAST("r_perimeter_area_ratio", flags = "quiet", return_format = "SGDF"))

# plot
plot(r_perimeter_area_ratio, legend = FALSE, axes = FALSE, main = "Perimeter area ratio")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_perimeter_area_ratio, digits = 3, cex = .4)

# delete grassdb
unlink("grassdb", recursive = TRUE)

