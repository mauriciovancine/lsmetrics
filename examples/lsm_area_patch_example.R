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

# patch area
lsmetrics::lsm_area_patch(input = "r",
                          map_patch_id = TRUE,
                          map_patch_area_original = TRUE,
                          map_patch_number_original = TRUE)

# files
rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_patch_id <- rgrass::read_RAST("r_patch_id", flags = "quiet", return_format = "terra")

plot(r_patch_id, legend = FALSE, axes = FALSE, main = "Patch id")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_patch_id)

r_patch_area <- rgrass::read_RAST("r_patch_area", flags = "quiet", return_format = "terra")

plot(r_patch_area, legend = FALSE, axes = FALSE, main = "Patch area (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_patch_area)

r_patch_area_original <- rgrass::read_RAST("r_patch_area_original", flags = "quiet", return_format = "terra")

plot(r_patch_area_original, legend = FALSE, axes = FALSE, main = "Patch area original (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_patch_area_original)

r_patch_number_original <- rgrass::read_RAST("r_patch_number_original", flags = "quiet", return_format = "terra")

plot(r_patch_number_original, legend = FALSE, axes = FALSE, main = "Number of patch (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_patch_number_original)

# delete grassdb
unlink("grassdb", recursive = TRUE)

