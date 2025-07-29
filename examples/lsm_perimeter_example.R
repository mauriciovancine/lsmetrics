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

# perimeter
lsmetrics::lsm_perimeter(input = "r",
                         map_perimeter_area_ratio_index = TRUE,
                         map_shape_index = TRUE,
                         map_fractal_index = TRUE)

# files
rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_perimeter <- terra::rast(rgrass::read_RAST("r_perimeter", flags = "quiet", return_format = "SGDF"))

# plot
plot(r_perimeter, legend = FALSE, axes = FALSE, main = "Perimeter (m)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_perimeter, cex = .5)

# import from grass to r
r_perimeter_area_ratio <- terra::rast(rgrass::read_RAST("r_perimeter_area_ratio", flags = "quiet", return_format = "SGDF"))

# plot
plot(r_perimeter_area_ratio, legend = FALSE, axes = FALSE, main = "Perimeter-area ratio index")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_perimeter_area_ratio, digits = 3, cex = .4)

# import from grass to r
r_shape <- terra::rast(rgrass::read_RAST("r_shape", flags = "quiet", return_format = "SGDF"))

# plot
plot(r_shape, legend = FALSE, axes = FALSE, main = "Shape index")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_shape, digits = 3, cex = .4)

# import from grass to r
r_fractal <- terra::rast(rgrass::read_RAST("r_fractal", flags = "quiet", return_format = "SGDF"))

# plot
plot(r_fractal, legend = FALSE, axes = FALSE, main = "Fractal dimension index")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_fractal, digits = 3, cex = .4)

# delete grassdb
unlink("grassdb", recursive = TRUE)

