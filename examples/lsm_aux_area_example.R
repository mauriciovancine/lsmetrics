library(lsmetrics)
library(terra)

# read habitat data
r <- lsmetrics::lsm_toy_landscape(proj_type = "degrees")

# plot
plot(r, col = c("white", "forestgreen"), legend = FALSE, axes = FALSE, main = "Binary habitat")
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

# null
rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "r_null = if(r == 1, 1, null())")

# clump
rgrass::execGRASS(cmd = "r.clump", flags = "overwrite", input = "r_null", output = "r_id")

# area
lsmetrics::lsm_aux_area(input_null = "r_null",
                        input_id = "r_id",
                        area_round_digit = 1,
                        area_unit = "ha",
                        map_ncell = TRUE,
                        table_export = TRUE)

# files
rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_id <- terra::rast(rgrass::read_RAST("r_id", flags = "quiet", return_format = "SGDF"))
r_ncell <- terra::rast(rgrass::read_RAST("r_ncell", flags = "quiet", return_format = "SGDF"))
r_area <- terra::rast(rgrass::read_RAST("r_area", flags = "quiet", return_format = "SGDF"))

# plot
plot(r_id, legend = FALSE, axes = FALSE, main = "Fragment ID")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_id)

plot(r_ncell, legend = FALSE, axes = FALSE, main = "Number of cells")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_ncell)

plot(r_area, legend = FALSE, axes = FALSE, main = "Area (ha)")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_area, cex = .7, digits = 1)

# table
r_table_area <- vroom::vroom("r_table_area.csv", show_col_types = FALSE)
r_table_area

# delete table and grassdb
unlink("r_table_area.csv")
unlink("grassdb", recursive = TRUE)

