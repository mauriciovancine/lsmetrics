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
rgrass::write_RAST(x = r, flags = c("o", "overwrite", "quiet"), vname = "r")

# functional connectivity
lsmetrics::lsm_connectivity_functional(input = "r",
                                       gap_crossing_value = 100,
                                       map_func_connec_area =  TRUE,
                                       map_func_connec_id = TRUE,
                                       map_func_connec_dilation = TRUE)

# files
rgrass::execGRASS(cmd = "g.list", type = "raster")

# import do r
r_func_connec200_id <- rgrass::read_RAST("r_func_connec200_id", flags = "quiet", return_format = "terra")
r_func_connec200_dilation <- rgrass::read_RAST("r_func_connec200_dilation", flags = "quiet", return_format = "terra")

plot(r_func_connec200_dilation, legend = FALSE, axes = FALSE,main = "Functional connected area id (200 m)")
plot(r_func_connec200_id, legend = FALSE, axes = FALSE, add = TRUE)
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_func_connec200_id)

# import to r
r_func_connec200_area <- rgrass::read_RAST("r_func_connec200_area", flags = "quiet", return_format = "terra")

plot(r_func_connec200_dilation, legend = FALSE, axes = FALSE, main = "Functional connected area (ha) (200 m)")
plot(r_func_connec200_area, legend = FALSE, axes = FALSE, add = TRUE)
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_func_connec200_area)

# import to r
r_func_connec200 <- rgrass::read_RAST("r_func_connec200", flags = "quiet", return_format = "terra")

plot(r_func_connec200_dilation, legend = FALSE, axes = FALSE, main = "Functional connectivity (ha) (200 m)")
plot(r_func_connec200, legend = FALSE, axes = FALSE, add = TRUE)
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_func_connec200)

# delete grassdb
unlink("grassdb", recursive = TRUE)
