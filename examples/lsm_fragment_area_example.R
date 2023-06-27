library(lsmetrics)
library(terra)

# read habitat data
f <- system.file("raster/toy_landscape_habitat.tif", package = "lsmetrics")
r <- terra::rast(f)

# plot
plot(r, legend = FALSE, axes = FALSE, main = "Binary habitat")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r)

# find grass
path_grass <- as.character(link2GI::findGRASS()[1]) # windows users need to find, e.g. "C:/Program Files/GRASS GIS 8.2"

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
lsmetrics::lsm_fragment_area(input = "r", id = TRUE, table = TRUE)

# files
# rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_fragment_id <- rgrass::read_RAST("r_fragment_id", flags = "quiet", return_format = "terra")

# plot
plot(r_fragment_id, legend = FALSE, axes = FALSE, main = "Fragment id")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_fragment_id)

# import from grass to r
r_fragment_area <- rgrass::read_RAST("r_fragment_area_ha", flags = "quiet", return_format = "terra")

plot(r_fragment_area, legend = FALSE, axes = FALSE, main = "Fragment area (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_fragment_area)

# table
da_fragment <- readr::read_csv("r_fragment.csv", show_col_types = FALSE)
da_fragment

da_fragment_summary <- readr::read_csv("r_fragment_summary.csv", show_col_types = FALSE)
da_fragment_summary

# delete grassdb
unlink("grassdb", recursive = TRUE)
unlink("r_fragment.csv")
unlink("r_fragment_summary.csv")

