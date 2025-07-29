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

# morphology
lsmetrics::lsm_morphology(input = "r")

# files
rgrass::execGRASS(cmd = "g.list", type = "raster")

# import from grass to r
r_morphology <- rgrass::read_RAST("r_morphology", flags = "quiet", return_format = "terra")
r_morphology_matrix <- rgrass::read_RAST("r_morphology_matrix", flags = "quiet", return_format = "terra")
r_morphology_core <- rgrass::read_RAST("r_morphology_core", flags = "quiet", return_format = "terra")
r_morphology_edge <- rgrass::read_RAST("r_morphology_edge", flags = "quiet", return_format = "terra")
r_morphology_corridor <- rgrass::read_RAST("r_morphology_corridor", flags = "quiet", return_format = "terra")
r_morphology_branch <- rgrass::read_RAST("r_morphology_branch", flags = "quiet", return_format = "terra")
r_morphology_stepping_stone <- rgrass::read_RAST("r_morphology_stepping_stone", flags = "quiet", return_format = "terra")
r_morphology_perforation <- rgrass::read_RAST("r_morphology_perforation", flags = "quiet", return_format = "terra")

# plot
plot(r_morphology, legend = FALSE, axes = FALSE, main = "Morphology")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_morphology)

plot(r_morphology_matrix, legend = FALSE, axes = FALSE, main = "Matrix")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_morphology_matrix)

plot(r_morphology_core, legend = FALSE, axes = FALSE, main = "Core")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_morphology_core)

plot(r_morphology_edge, legend = FALSE, axes = FALSE, main = "Edge")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_morphology_edge)

plot(r_morphology_corridor, legend = FALSE, axes = FALSE, main = "Corridor")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_morphology_corridor)

plot(r_morphology_branch, legend = FALSE, axes = FALSE, main = "Branch")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_morphology_branch)

plot(r_morphology_stepping_stone, legend = FALSE, axes = FALSE, main = "Stepping stone")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_morphology_stepping_stone)

plot(r_morphology_perforation, legend = FALSE, axes = FALSE, main = "Perforation")
plot(as.polygons(r, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(r), add = TRUE)
text(r_morphology_perforation)

# delete grassdb
unlink("grassdb", recursive = TRUE)

