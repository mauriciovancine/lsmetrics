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
rgrass::write_RAST(x = r, flags = c("o", "overwrite", "quiet"), vname = "r-raster-landcape.tif", verbose = FALSE)

# files
rgrass::execGRASS(cmd = "g.list", type = "raster")

# fix names
input <- lsmetrics::lsm_aux_fix_names(input = "r-raster-landcape.tif")
input

# files
rgrass::execGRASS(cmd = "g.list", type = "raster")

# delete grassdb
unlink("grassdb", recursive = TRUE)
