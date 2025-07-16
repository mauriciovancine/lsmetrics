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
path_grass <- system("grass --config path", inter = TRUE) # windows users need to find the grass gis path installation, e.g. "C:/Program Files/GRASS GIS 8.3"

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
lsmetrics::lsm_enn_distance(input = "r", zero_as_na = FALSE)

# files
rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_enn_distance <- terra::rast(rgrass::read_RAST("r_enn_distance", flags = "quiet", return_format = "SGDF"))

# plot
plot(r_enn_distance, legend = FALSE, axes = FALSE, main = "Euclidean nearest neighbor distance (m)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_enn_distance, cex = .5)

# delete grassdb
unlink("grassdb", recursive = TRUE)
