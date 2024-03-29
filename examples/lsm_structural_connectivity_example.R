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
rgrass::write_RAST(x = r, flags = c("o", "overwrite", "quiet"), vname = "r", verbose = FALSE)

# structural connectivity
lsmetrics::lsm_structural_connectivity(input = "r")

# files
# rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_structural_connected_area <- rgrass::read_RAST("r_structural_connected_area", flags = "quiet", return_format = "terra")

plot(r_structural_connected_area, legend = FALSE, axes = FALSE, main = "Structural connected area (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_structural_connected_area)

# import from grass to r
r_structural_connectivity <- rgrass::read_RAST("r_structural_connectivity", flags = "quiet", return_format = "terra")

plot(r_structural_connectivity, legend = FALSE, axes = FALSE, main = "Structural connectivity (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_structural_connectivity)

# delete grassdb
unlink("grassdb", recursive = TRUE)
