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
path_grass <- system("grass --config path", inter = TRUE) # windows users need to find the grass gis path installation, e.g. "C:/Program Files/GRASS GIS 8.3"

# create grassdb
rgrass::initGRASS(gisBase = path_grass,
                  SG = r,
                  gisDbase = "grassdb",
                  location = "newLocation",
                  mapset = "PERMANENT",
                  override = TRUE)

# import raster from r to grass
rgrass::write_RAST(x = r, flags = c("o", "overwrite", "quiet"), vname = "r")

# core and edge
lsmetrics::lsm_core_edge(input = "r",
                         edge_depth = 100,
                         core_edge_type = "both",
                         id = TRUE,
                         core_number = TRUE,
                         core_edge_original = TRUE,
                         calculate_area = TRUE,
                         calculate_percentage = TRUE,
                         buffer_radius = 100,
                         buffer_circular = FALSE)

# files
# rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_core100 <- rgrass::read_RAST("r_core100", flags = "quiet", return_format = "terra")
r_core100_id <- rgrass::read_RAST("r_core100_id", flags = "quiet", return_format = "terra")
r_core100_area_ha <- rgrass::read_RAST("r_core100_area_ha", flags = "quiet", return_format = "terra")
r_core100_area_ha_original <- rgrass::read_RAST("r_core100_area_ha_original", flags = "quiet", return_format = "terra")
r_core100_core_number <- rgrass::read_RAST("r_core100_core_number_original", flags = "quiet", return_format = "terra")
r_core100_pct_buf100 <- rgrass::read_RAST("r_core100_pct_buf100", flags = "quiet", return_format = "terra")

r_edge100 <- rgrass::read_RAST("r_edge100", flags = "quiet", return_format = "terra")
r_edge100_id <- rgrass::read_RAST("r_edge100_id", flags = "quiet", return_format = "terra")
r_edge100_area_ha <- rgrass::read_RAST("r_edge100_area_ha", flags = "quiet", return_format = "terra")
r_edge100_area_ha_original <- rgrass::read_RAST("r_edge100_area_ha_original", flags = "quiet", return_format = "terra")
r_edge100_pct_buf100 <- rgrass::read_RAST("r_edge100_pct_buf100", flags = "quiet", return_format = "terra")

# plot
plot(r_core100, legend = FALSE, axes = FALSE, main = "Core")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_core100)

plot(r_core100_id, legend = FALSE, axes = FALSE, main = "Core id")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_core100_id)

plot(r_core100_area_ha, legend = FALSE, axes = FALSE, main = "Core area (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_core100_area_ha)

plot(r_core100_area_ha_original, legend = FALSE, axes = FALSE, main = "Core area original (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_core100_area_ha_original)

plot(r_core100_core_number, legend = FALSE, axes = FALSE, main = "Number of cores")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_core100_core_number)

plot(r_core100_pct_buf100, legend = FALSE, axes = FALSE, main = "Core percentage (buffer 100 m)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_core100_pct_buf100, cex = .75)


plot(r_edge100, legend = FALSE, axes = FALSE, main = "Edge")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_edge100)

plot(r_edge100_id, legend = FALSE, axes = FALSE, main = "Edge id")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_edge100_id)

plot(r_edge100_area_ha, legend = FALSE, axes = FALSE, main = "Edge area (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_edge100_area_ha)

plot(r_edge100_area_ha_original, legend = FALSE, axes = FALSE, main = "Edge area original (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_edge100_area_ha_original)

plot(r_edge100_pct_buf100, legend = FALSE, axes = FALSE, main = "Edge percentage (buffer 100 m)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_edge100_pct_buf100, cex = .75)

# delete grassdb
unlink("grassdb", recursive = TRUE)
