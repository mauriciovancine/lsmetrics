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
lsmetrics::lsm_percentage_table(input = "r")

# files
rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
da_pct <- readr::read_csv("r_percentage.csv")
da_pct

# delete file
unlink("r_percentage.csv")

# delete grassdb
unlink("grassdb", recursive = TRUE)

