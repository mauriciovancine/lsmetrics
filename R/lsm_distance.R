#' Calculate fragment distance
#'
#' Calculate distance inside and outside of fragments in meters from [r.grow.distance] GRASS GIS module.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param output `[character=""]` \cr fragment area map name inside GRASS Data Base.
#' @param zero_as_null `[logical=""]` \cr
#' @param distance_type `[character=""]` \cr
#' @param distance_metric `[character=""]` \cr
#'
#' @example examples/lsm_distance_example.R
#'
#' @name lsm_distance
#' @export
lsm_distance <- function(input,
                         output = NULL,
                         zero_as_null = FALSE,
                         region_input = FALSE,
                         distance_type){

    # region ----
    if(region_input){
        rgrass::execGRASS("g.region", flags = "a", raster = input)
    }

    # binary ----
    if(zero_as_null){

        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_distance_null = ", input))

    } else{
        rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_distance_null = if(", input, " == 1, 1, null())"))
    }

    # projection ----
    proj_info <- rgrass::execGRASS("g.proj", flags = c("g", "quiet"), intern = TRUE)
    proj_unit <- tolower(sub("units=", "", proj_info[grepl("^units=", proj_info)]))

    if(proj_unit == "meters"){

        distance_metric <- "euclidean"

    } else if (proj_unit == "degrees") {

        distance_metric <- "geodesic"

    } else {
        warning(paste("Units:", projunits, "not currently supported"))
    }

    # type inside ----
    if(distance_type == "inside" | distance_type == "both"){

        # create raster
        rgrass::execGRASS(cmd = "g.message", message = "Creating raster inverse")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_inverse = if(isnull(",
                                              input, output, "_distance_null), 1, null())"))

        # distance
        rgrass::execGRASS(cmd = "g.message", message = "Calculating distance")
        rgrass::execGRASS(cmd = "r.grow.distance",
                          flags = c("overwrite", "quiet"),
                          input = paste0(input, output, "_inverse"),
                          distance = paste0(input, output, "_distance_inside"),
                          metric = distance_metric)

        # integer
        rgrass::execGRASS(cmd = "g.message", message = "Transforming raster to integer")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_distance_inside = round(",
                                              input, output, "_distance_inside)"))

        # color
        rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")
        rgrass::execGRASS(cmd = "r.colors",
                          flags = "quiet",
                          map = paste0(input, output, "_distance_inside"),
                          color = "viridis")

    }

    # type outside ----
    if(distance_type == "outside" | distance_type == "both"){

        # distance
        rgrass::execGRASS(cmd = "g.message", message = "Calculating distance")
        rgrass::execGRASS(cmd = "r.grow.distance",
                          flags = c("overwrite", "quiet"),
                          input = paste0(input, output, "_distance_null"),
                          distance = paste0(input, output, "_distance_outside"),
                          metric = distance_metric)

        # integer
        rgrass::execGRASS(cmd = "g.message", message = "Transforming raster to integer")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_distance_outside = round(",
                                              input, output, "_distance_outside)"))

        # color
        rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")
        rgrass::execGRASS(cmd = "r.colors",
                          flags = "quiet",
                          map = paste0(input, output, "_distance_outside"),
                          color = "viridis")
    }

    # clean
    rgrass::execGRASS(cmd = "g.message", message = "Cleaning data")

    suppressWarnings(
        rgrass::execGRASS(cmd = "g.remove",
                          flags = c("b", "f", "quiet"),
                          type = "raster",
                          name = paste0(input, output, "_distance_null"))
    )
}
