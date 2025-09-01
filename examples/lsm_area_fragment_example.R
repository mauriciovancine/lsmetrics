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

# area
lsmetrics::lsm_area_fragment(input = "r", map_fragment_id = TRUE, table_fragment_area = TRUE)

# files
rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_fragment_id <- rgrass::read_RAST("r_fragment_id", flags = "quiet", return_format = "terra")

# plot
plot(r_fragment_id, legend = FALSE, axes = FALSE, main = "Fragment id")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_fragment_id)

# import from grass to r
r_fragment_area <- rgrass::read_RAST("r_fragment_area", flags = "quiet", return_format = "terra")

plot(r_fragment_area, legend = FALSE, axes = FALSE, main = "Fragment area (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_fragment_area)

# table
da_fragment_area <- vroom::vroom("r_fragment_table_area.csv", show_col_types = FALSE)
da_fragment_area

# delete grassdb
unlink("r_fragment_table_area.csv")
unlink("grassdb", recursive = TRUE)
