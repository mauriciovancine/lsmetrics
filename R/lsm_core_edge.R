#' Identify the core and edge of landscape elements
#'
#' Identify the core and edge of landscape elements for each pixel. Calculate
#' area, original area and percentage of core and edge.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param output `[character=""]` \cr Map name output inside GRASS Data Base.
#' @param zero_as_na `[logical(1)=FALSE]` \cr If `TRUE`, the function treats
#' non-habitat cells as null; if `FALSE`, the function converts non-habitat zero
#' cells to null cells.
#' @param edge_depth `[numeric]` \cr Integer indicating edge distance in meters
#' considered adjacent to form a patch.
#' @param type `[character=""]` \cr
#' @param calculate_area `[logical(1)=FALSE]` \cr
#' @param calculate_percentage `[logical(1)=FALSE]` \cr
#' @param buffer_radius `[numeric]` \cr

#' @example examples/lsm_core_edge_example.R
#'
#' @name lsm_core_edge
#' @export
lsm_core_edge <- function(input,
                          output = NULL,
                          zero_as_na = FALSE,
                          edge_depth,
                          type = "all",
                          calculate_area = FALSE,
                          original_pid,
                          calculate_percentage = FALSE,
                          buffer_radius = NULL){

    # edge depth ----
    res <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))

    if(edge_depth/res >= 1){
        window <- 2 * round(edge_depth/res, 0) + 1
    }else{
        stop("Edge depth is smaller than map resolution. Choose a higher value for the edge depth.")
    }

    # binary
    if(zero_as_na){

        rgrass::execGRASS(cmd = "g.message", message = "Converting null as zero")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, " = if(", input, " == 1, 1, 0)"))
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_null = if(", input, " == 1, 1, null())"))
        rgrass::execGRASS(cmd = "r.clump",
                          flags = c("d", "quiet", "overwrite"),
                          input = paste0(input, output, "_null"),
                          output = paste0(input, output, "_pid"))

    } else{

        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, " = ", input))
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, " = if(", input, " == 1, 1, 0)"))
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_null = if(", input, " == 1, 1, null())"))
        rgrass::execGRASS(cmd = "r.clump",
                          flags = c("d", "quiet", "overwrite"),
                          input = paste0(input, output, "_null"),
                          output = paste0(input, output, "_pid"))
    }

    # core ----
    if(type == "all" | type == "core"){

        # core
        rgrass::execGRASS(cmd = "g.message", message = "Calculating core")
        rgrass::execGRASS(cmd = "r.neighbors",
                          flags = c("overwrite"),
                          input = paste0(input, output),
                          selection = paste0(input, output),
                          output = paste0(input, output, "_core", edge_depth),
                          method = "min",
                          size = window)

        rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_core", edge_depth), color = "blues")

        # core area
        if(calculate_area == TRUE){

            rgrass::execGRASS(cmd = "g.message", message = "Calculating core area")

            lsm_area(input = paste0(input, output, "_core", edge_depth))

            rgrass::execGRASS(cmd = "r.stats.zonal",
                              flags = c("overwrite"),
                              base = paste0(input, output, "_pid"),
                              cover = paste0(input, output, "_core", edge_depth, "_null"),
                              method = "count",
                              output = paste0(input, output, "_core", edge_depth, "_area_ncell_original"))

            area_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))^2/1e4
            rgrass::execGRASS(cmd = "r.mapcalc",
                              flags = c("overwrite"),
                              expression = paste0(input, output, "_core", edge_depth, "_area_ha_original=", input, output, "_core", edge_depth, "_area_ncell_original * ", area_pixel))
            rgrass::execGRASS(cmd = "r.mapcalc",
                              flags = c("overwrite"),
                              expression = paste0(input, output, "_core", edge_depth, "_area_ha_original_int=if(", input, output, "_core", edge_depth, "_area_ha_original < 1, 1, ", input, output, "_core", edge_depth, "_area_ha_original)"))
            rgrass::execGRASS(cmd = "r.mapcalc",
                              flags = c("overwrite"),
                              expression = paste0(input, output, "_core", edge_depth, "_area_ha_original_int=round(", input, output, "_core", edge_depth, "_area_ha_original)"))

            rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_core", edge_depth, "_area_ncell_original"), color = "ryg")
            rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_core", edge_depth, "_area_ha_original"), color = "ryg")
            rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_core", edge_depth, "_area_ha_original_int"), color = "ryg")

        }

        # core percentage
        if(calculate_percentage == TRUE){

            rgrass::execGRASS(cmd = "g.message", message = "Calculating core percentage")

            lsm_percentage(input = paste0(input, output, "_core", edge_depth),
                           buffer_radius = buffer_radius)

            rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_core", edge_depth, "_pct_buf", buffer_radius), color = "blues")

        }

    }

    # edge ----
    if(type == "all" | type == "edge"){

        # edge
        rgrass::execGRASS(cmd = "g.message", message = "Calculating edge")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = "overwrite",
                          expression = paste0(input, output, "_edge", edge_depth, "=", input, output, " - ", input, output, "_core", edge_depth))

        rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_edge", edge_depth), color = "oranges")

        # edge area
        if(calculate_area == TRUE){

            rgrass::execGRASS(cmd = "g.message", message = "Calculating edge area")

            lsm_area(input = paste0(input, output, "_edge", edge_depth))

            rgrass::execGRASS(cmd = "r.stats.zonal",
                              flags = c("overwrite"),
                              base = paste0(input, output, "_pid"),
                              cover = paste0(input, output, "_edge", edge_depth, "_null"),
                              method = "count",
                              output = paste0(input, output, "_edge", edge_depth, "_area_ncell_original"))

            area_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))^2/1e4
            rgrass::execGRASS(cmd = "r.mapcalc",
                              flags = c("overwrite"),
                              expression = paste0(input, output, "_edge", edge_depth, "_area_ha_original=", input, output, "_edge", edge_depth, "_area_ncell_original * ", area_pixel))

            rgrass::execGRASS(cmd = "r.mapcalc",
                              flags = c("overwrite"),
                              expression = paste0(input, output, "_edge", edge_depth, "_area_ha_original_int=if(", input, output, "_edge", edge_depth, "_area_ha_original < 1, 1, ", input, output, "_edge", edge_depth, "_area_ha_original)"))

            rgrass::execGRASS(cmd = "r.mapcalc",
                              flags = c("overwrite"),
                              expression = paste0(input, output, "_edge", edge_depth, "_area_ha_original_int=round(", input, output, "_edge", edge_depth, "_area_ha_original_int)"))

            rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_edge", edge_depth, "_area_ncell_original"), color = "ryg")
            rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_edge", edge_depth, "_area_ha_original"), color = "ryg")
            rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_edge", edge_depth, "_area_ha_original_int"), color = "ryg")

        }

        # edge percentage
        if(calculate_percentage == TRUE){

            rgrass::execGRASS(cmd = "g.message", message = "Calculating edge percentage")

            lsm_percentage(input = paste0(input, output, "_edge", edge_depth),
                           buffer_radius = buffer_radius)

            rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_edge", edge_depth, "_pct_buf", buffer_radius), color = "oranges")

        }

    }

}
