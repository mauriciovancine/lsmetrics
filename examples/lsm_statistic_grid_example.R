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
lsmetrics::lsm_area_fragment(input = "r")

# files
rgrass::execGRASS(cmd = "g.list", type = "raster")

# import r
r_fragment_area <- rgrass::read_RAST("r_fragment_area", flags = "quiet")

# plot
plot(r_fragment_area, legend = FALSE, axes = FALSE, main = "Fragment area (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_fragment_area)

# grid
lsmetrics::lsm_statistic_grid(input = "r",
                              landscape_metric = "r_fragment_area",
                              landscape_metric_has_null = TRUE,
                              grid_size = 200,
                              hexagon = TRUE,
                              column_prefix = "area",
                              method = "average")

# files
rgrass::execGRASS(cmd = "g.list", type = "vector")

# import r
r_grid <- rgrass::read_VECT("r_grid200", flags = "quiet")

# plot
r_grid <- r_grid[is.na(r_grid$area_average) == FALSE, ]
plot(r_grid, "area_average", legend = FALSE, axes = FALSE, main = "Area average (ha)")
text(r_grid, labels = "area_average", cex = .7)
plot(as.polygons(r), col = c(adjustcolor("white", 0), adjustcolor("gray", .5)), add = TRUE)

# delete grassdb
unlink("grassdb", recursive = TRUE)
