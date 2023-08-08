library(lsmetrics)
library(terra)

# read habitat data
r <- lsmetrics::lsm_toy_landscape(type = "class")

# plot
plot(r, legend = FALSE, axes = FALSE, main = "Classes")
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

# diversity
lsmetrics::lsm_diversity_parallel(input = "r", index = "shannon", buffer_radius = 100, grid_size = 1000, nprocs = 5, grid_delete = FALSE)

# files
# rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
v <- rgrass::read_VECT("grid", flags = "quiet")
r_div_buf100 <- rgrass::read_RAST("r_diversity_shannon_buffer100", flags = "quiet", return_format = "terra")

# plot
plot(r_div_buf100, legend = FALSE, axes = FALSE, main = "Landscape diversity (Shannon) (buffer 100 m)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(v, lwd = 3, border = "blue", add = TRUE)
text(v, cex = 3, col = "blue")
text(r_div_buf100, digits = 1, cex = .75)

# delete grassdb
unlink("grassdb", recursive = TRUE)
