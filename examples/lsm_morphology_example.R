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

# morphology
lsmetrics::lsm_morphology(input = "r", zero_as_na = FALSE)

# files
# rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_mophology <- rgrass::read_RAST("r_morphology", flags = "quiet", return_format = "terra")
r_core <- rgrass::read_RAST("r_core", flags = "quiet", return_format = "terra")
r_edge <- rgrass::read_RAST("r_edge", flags = "quiet", return_format = "terra")
r_corridor <- rgrass::read_RAST("r_corridor", flags = "quiet", return_format = "terra")
r_branch <- rgrass::read_RAST("r_branch", flags = "quiet", return_format = "terra")
r_stepping_stone <- rgrass::read_RAST("r_stepping_stone", flags = "quiet", return_format = "terra")
r_perforation <- rgrass::read_RAST("r_perforation", flags = "quiet", return_format = "terra")

# plot
plot(r_mophology, legend = FALSE, axes = FALSE, main = "Morphology")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_mophology)

plot(r_core, legend = FALSE, axes = FALSE, main = "Core")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_core)

plot(r_edge, legend = FALSE, axes = FALSE, main = "Edge")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_edge)

plot(r_corridor, legend = FALSE, axes = FALSE, main = "Corridor")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_corridor)

plot(r_branch, legend = FALSE, axes = FALSE, main = "Branch")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_branch)

plot(r_stepping_stone, legend = FALSE, axes = FALSE, main = "Stepping stone")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_stepping_stone)

plot(r_perforation, legend = FALSE, axes = FALSE, main = "Perforation")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_perforation)

# delete grassdb
unlink("grassdb", recursive = TRUE)
