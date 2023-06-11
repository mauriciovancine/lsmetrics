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
lsmetrics::lsm_fragment_area(input = "r", zero_as_na = FALSE)

# files
# rgrass::execGRASS(cmd = "g.list", type = "raster")

# import r
r_fragment_area_ha <- rgrass::read_RAST("r_fragment_area_ha", flags = "quiet")

# plot
plot(r_fragment_area_ha, legend = FALSE, axes = FALSE, main = "Fragment area (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_fragment_area_ha)

# grid
lsmetrics::lsm_grid_statistics(input = "r",
                               landscape_metric = "r_fragment_area_ha",
                               landscape_metric_has_null = TRUE,
                               size = 200,
                               hexagon = TRUE,
                               column_prefix = "area",
                               method = "average")

# files
# rgrass::execGRASS(cmd = "g.list", type = "vector")

# import r
r_grid <- rgrass::read_VECT("r_grid200", flags = "quiet")

# plot
r_grid <- r_grid[is.na(r_grid$area_average) == FALSE, ]
plot(r_grid, "area_average", legend = FALSE, axes = FALSE, main = "Area average (ha)")
text(r_grid, labels = "area_average", cex = .7)

# delete grassdb
unlink("grassdb", recursive = TRUE)
