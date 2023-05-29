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
path_grass <- as.character(link2GI::findGRASS()[1])
path_grass

# create grassdb
rgrass::initGRASS(gisBase = path_grass,
                  SG = r,
                  gisDbase = "grassdb",
                  location = "newLocation",
                  mapset = "PERMANENT",
                  override = TRUE)

# import raster from r to grass
rgrass::write_RAST(x = r, flags = c("o", "overwrite"), vname = "r")

# area
lsmetrics::lsm_area(input = "r", zero_as_na = FALSE)

# inside distance
lsm_distance(input = "r", zero_as_na = FALSE, type = "inside")

# core and edge
lsm_core_edge(input = "r",
              input_distance_inside = "r_distance_inside",
              edge_depth = 100,
              type = "all",
              calculate_area = TRUE,
              original_pid = "r_pid",
              calculate_percentage = TRUE,
              buffer_radius = 100)

# files
rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_core100 <- rgrass::read_RAST("r_core100", return_format = "terra")
r_core100

r_core100_area_ha <- rgrass::read_RAST("r_core100_area_ha", return_format = "terra")
r_core100_area_ha

r_core100_area_ha_original <- rgrass::read_RAST("r_core100_area_ha_original", return_format = "terra")
r_core100_area_ha_original

r_core100_pct_buf100 <- rgrass::read_RAST("r_core100_pct_buf100", return_format = "terra")
r_core100_pct_buf100


r_edge100 <- rgrass::read_RAST("r_edge100", return_format = "terra")
r_edge100

r_edge100_area_ha <- rgrass::read_RAST("r_edge100_area_ha", return_format = "terra")
r_edge100_area_ha

r_edge100_area_ha_original <- rgrass::read_RAST("r_edge100_area_ha_original", return_format = "terra")
r_edge100_area_ha_original

r_edge100_pct_buf100 <- rgrass::read_RAST("r_edge100_pct_buf100", return_format = "terra")
r_edge100_pct_buf100

# plot
plot(r_core100, legend = FALSE, axes = FALSE, main = "Core")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_core100)

plot(r_core100_area_ha, legend = FALSE, axes = FALSE, main = "Core area (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_core100_area_ha)

plot(r_core100_area_ha_original, legend = FALSE, axes = FALSE, main = "Core area original (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_core100_area_ha_original)

plot(r_core100_pct_buf100, legend = FALSE, axes = FALSE, main = "Core percentage (buffer 100 m)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_core100_pct_buf100)


plot(r_edge100, legend = FALSE, axes = FALSE, main = "Edge")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_edge100)

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
text(r_edge100_pct_buf100)


