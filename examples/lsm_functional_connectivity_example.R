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

# functional connectivity
lsmetrics::lsm_functional_connectivity(input = "r", gap_crossing = 100, id = TRUE, dilation = TRUE)

# files
# rgrass::execGRASS(cmd = "g.list", type = "raster")

# import do r
r_functional_connected_area200_id <- rgrass::read_RAST("r_functional_connected_area200_id", flags = "quiet", return_format = "terra")
r_functional_connectivity_dilation200_null <- rgrass::read_RAST("r_functional_connectivity_dilation200_null", flags = "quiet", return_format = "terra")

plot(r_functional_connectivity_dilation200_null, legend = FALSE, axes = FALSE,
     main = "Functional connected area id (200 m)")
plot(r_functional_connected_area200_id, legend = FALSE, axes = FALSE, add = TRUE)
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_functional_connected_area200_id)

# import to r
r_functional_connected_area200 <- rgrass::read_RAST("r_functional_connected_area200", flags = "quiet", return_format = "terra")

plot(r_functional_connectivity_dilation200_null, legend = FALSE, axes = FALSE,
     main = "Functional connected area (ha) (200 m)")
plot(r_functional_connected_area200, legend = FALSE, axes = FALSE, add = TRUE)
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_functional_connected_area200)

# import to r
r_functional_connectivity200 <- rgrass::read_RAST("r_functional_connectivity200", flags = "quiet", return_format = "terra")

plot(r_functional_connectivity_dilation200_null, legend = FALSE, axes = FALSE,
     main = "Functional connectivity (ha) (200 m)")
plot(r_functional_connectivity200, legend = FALSE, axes = FALSE, add = TRUE)
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_functional_connectivity200)

# delete grassdb
unlink("grassdb", recursive = TRUE)
