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
rgrass::write_RAST(x = r, flags = c("o", "overwrite"), vname = "r")

# percentage
lsmetrics::lsm_percentage(input = "r", buffer_radius = 100)

# files
rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_pct_buf100 <- rgrass::read_RAST("r_pct_buf100", flags = "quiet", return_format = "terra")

# plot
plot(r_pct_buf100, legend = FALSE, axes = FALSE, main = "Habitat percentage (buffer 100 m)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_pct_buf100, cex = .75)

# percentage 300 m
lsmetrics::lsm_percentage(input = "r", buffer_radius = 300)

# files
rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_pct_buf300 <- rgrass::read_RAST("r_pct_buf300", flags = "quiet", return_format = "terra")

# plot
plot(r_pct_buf300, legend = FALSE, axes = FALSE, main = "Habitat percentage (buffer 300 m)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_pct_buf300, cex = .75)

# delete grassdb
unlink("grassdb", recursive = TRUE)

