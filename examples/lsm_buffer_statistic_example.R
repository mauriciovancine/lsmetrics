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

# points
p <- terra::vect(data.frame(x = c(235150, 234450, 235150),
                            y = c(7525250, 7524850, 7524350)),
                 geom = c("x", "y"),
                 crs = "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs +type=crs")

# plot
plot(r, legend = FALSE, axes = FALSE, main = "Points")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
plot(p, cex = 3, add = TRUE)

# import vector from r to grass
rgrass::write_VECT(x = p, flags = c("o", "overwrite", "quiet"), vname = "p")

# area
lsmetrics::lsm_fragment_area(input = "r")

# buffer statistics
lsmetrics::lsm_buffer_statistic(input = "r",
                                landscape_metric = "r_fragment_area_ha",
                                landscape_metric_has_null = TRUE,
                                point = "p",
                                distance = 200,
                                column_prefix = "area",
                                method = "average")

# files
# rgrass::execGRASS(cmd = "g.list", type = "vector")

# import from grass to r
r_fragment_area <- rgrass::read_RAST("r_fragment_area_ha", flags = "quiet", return_format = "terra")

# import buffer
v_buffer <- rgrass::read_VECT("r_p_buffer200", flags = "quiet")

# plot
plot(r_fragment_area, legend = FALSE, axes = FALSE, main = "Fragment area (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_fragment_area)
plot(v_buffer, "area_average", legend = FALSE, alpha = .5, add = TRUE)
plot(p, col = "gray30", cex = 3, add = TRUE)
text(v_buffer, col = "white", labels = "area_average", cex = .7)

# delete grassdb
unlink("grassdb", recursive = TRUE)
