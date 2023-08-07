library(lsmetrics)
library(terra)

# read habitat data
r <- lsm_toy_landscape(type = "binary")

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
rgrass::write_RAST(x = r, flags = c("o", "overwrite"), vname = "r")

# percentage
lsmetrics::lsm_percentage_parallel(input = "r", buffer_radius = 100, grid_size = 500, grid_delete = FALSE, nprocs = 5)

# files
# rgrass::execGRASS(cmd = "g.list", type = "raster")
# rgrass::execGRASS(cmd = "g.list", type = "vector")

# import from grass to r
v <- rgrass::read_VECT("grid", flags = "quiet")
r_pct_buf100 <- rgrass::read_RAST("r_pct_buf100", flags = "quiet", return_format = "terra")

# plot
plot(r_pct_buf100, legend = FALSE, axes = FALSE, main = "Habitat percentage (buffer 100 m)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
plot(v, lwd = 3, border = "blue", add = TRUE)
text(v, cex = 3, col = "blue")
text(r_pct_buf100, cex = .75)

# delete grassdb
unlink("grassdb", recursive = TRUE)
