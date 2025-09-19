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

# morphological_segmentation
lsmetrics::lsm_morphological_segmentation(input = "r", table_morphological_segmentation = TRUE)

# files
rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_morphological_segmentation <- rgrass::read_RAST("r_morphological_segmentation", flags = "quiet", return_format = "terra")

# plot
plot(r_morphological_segmentation, legend = FALSE, axes = FALSE, main = "Morphological segmentation")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_morphological_segmentation)

# table
table_morphological_segmentation <- readr::read_csv("r_table_morphological_segmentation.csv", show_col_types = FALSE)
table_morphological_segmentation

# delete grassdb
unlink("r_table_morphological_segmentation.csv")
unlink("grassdb", recursive = TRUE)
