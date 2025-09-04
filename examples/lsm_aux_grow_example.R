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

# clump
rgrass::execGRASS(cmd = "r.mapcalc", expression = "r_null = if(r == 1, 1, null())")
rgrass::execGRASS(cmd = "r.clump", flags = c("d", "overwrite"), input = "r_null", output = "r_id")

# grow
lsmetrics::lsm_aux_grow(input = "r_id", output = "r_id_grow")

# files
rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_id <- rgrass::read_RAST("r_id", flags = "quiet", return_format = "terra")

# plot
plot(r_id, legend = FALSE, axes = FALSE, main = "Fragment id")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_id)

# import from grass to r
r_id_grow <- rgrass::read_RAST("r_id_grow", flags = "quiet", return_format = "terra")

plot(r_id_grow, legend = FALSE, axes = FALSE, main = "Fragment grow")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_id_grow)

# delete grassdb
unlink("grassdb", recursive = TRUE)

