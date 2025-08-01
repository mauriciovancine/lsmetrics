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

# fill holes
lsmetrics::lsm_aux_fill_hole(input = "r")

# files
rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_aux_fill_hole <- rgrass::read_RAST("r_aux_fill_hole", flags = "quiet", return_format = "terra")

# plot
plot(r_aux_fill_hole, legend = FALSE, axes = FALSE, main = "Fragment fill hole")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_aux_fill_hole)

# delete grassdb
unlink("grassdb", recursive = TRUE)

