library(lsmetrics)
library(terra)

# toy landscape
toy_landscape <- lsm_toy_landscape()

# plot
plot(toy_landscape, legend = FALSE, axes = FALSE, main = "Toy landscape")
plot(as.polygons(toy_landscape, dissolve = FALSE), lwd = .1, add = TRUE)
plot(as.polygons(toy_landscape), add = TRUE)
text(toy_landscape)
