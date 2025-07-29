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
r_connec_struct_area <- rgrass::read_RAST("r_connec_struct_area", flags = "quiet", return_format = "terra")

plot(r_connec_struct_area, legend = FALSE, axes = FALSE, main = "Structural connected area (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_connec_struct_area)

# import from grass to r
r_connec_struct <- rgrass::read_RAST("r_connec_struct", flags = "quiet", return_format = "terra")

plot(r_connec_struct, legend = FALSE, axes = FALSE, main = "Structural connectivity (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_connec_struct)

# delete grassdb
unlink("grassdb", recursive = TRUE)
