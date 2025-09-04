#!/usr/bin/env Rscript

############################################################################
#
# MODULE:    r.grow
# AUTHOR(S): Glynn Clements
# PURPOSE:   Fast replacement for r.grow using r.grow.distance
#
# COPYRIGHT: (C) 2008 by Glynn Clements
#
#   This program is free software under the GNU General Public
#   License (>=v2). Read the file COPYING that comes with GRASS
#   for details.
#
#############################################################################

library(rgrass)

# Cleanup function
cleanup <- function(temp_maps) {
    for (map in temp_maps) {
        if (!is.null(map)) {
            try(
                execGRASS("g.remove", flags = c("f", "b"), type = "rast",
                          name = map, intern = TRUE, ignore.stderr = TRUE),
                silent = TRUE
            )
        }
    }
}

lsm_aux_grow <- function(input,
                         output,
                         flags,
                         radius = -1.01,
                         metric = "euclidean",
                         old = "",
                         new = "",
                         mapunits = FALSE) {

    tmp <- Sys.getpid()

    temp_dist <- paste0("r.grow.tmp.", tmp, ".dist")

    shrink <- FALSE
    if (radius < 0) {
        shrink <- TRUE
        radius <- -radius
    }

    if (new == "" && !shrink) {
        temp_val <- paste0("r.grow.tmp.", tmp, ".val")
        new <- temp_val
    }else{
        temp_val <- paste0("r.grow.tmp.", tmp, ".val")
    }

    if (old == "") {
        old <- input
    }

    if (!mapunits) {
        kv <- execGRASS("g.region", flags = "g", intern = TRUE)
        kv <- strsplit(kv, "=")
        kv <- setNames(sapply(kv, `[`, 2), sapply(kv, `[`, 1))
        scale <- sqrt(as.numeric(kv["nsres"]) * as.numeric(kv["ewres"]))
        radius <- (radius + .01) * scale
    }

    if (metric == "euclidean") {
        metric <- "squared"
        radius <- radius * radius
    }

    # check if input exists
    if (execGRASS("g.findfile", element = "cell", file = input, intern = TRUE)[1] == "") {
        stop(paste("Raster map <", input, "> not found"))
    }

    # growing
    if (!shrink) {
        execGRASS("r.grow.distance",
                  flags = flags,
                  input = input,
                  metric = metric,
                  distance = temp_dist,
                  value = temp_val)

        expr <- paste0(output,
                       " = if(!isnull(", input, "),", old,
                       ",if(", temp_dist, " < ", radius, ",", new, ",null()))")

        execGRASS("r.mapcalc", flags = flags, expression = expr)

    } else {
        # shrinking
        execGRASS("r.grow.distance",
                  input = input,
                  metric = metric,
                  distance = temp_dist,
                  value = temp_val,
                  flags = c("n", flags))

        expr <- paste0(output,
                       " = if(isnull(", temp_dist, "),", old,
                       ",if(", temp_dist, " < ", radius, ",null(),", old, "))")

        execGRASS("r.mapcalc", flags = flags, expression = expr)
    }

    # apply colors
    execGRASS("r.colors", map = output, raster = input)

    # add history
    execGRASS("r.support", map = output, history = paste("Created by r_grow wrapper"))

    # cleanup
    cleanup(c(temp_dist, temp_val))
}
