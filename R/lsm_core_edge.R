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
#' @param edge_contraction `[character=""]` \cr
#' @param core_edge_type `[character=""]` \cr
#' @param ncell `[logical(1)=FALSE]` \cr If `TRUE`
#' @param area_integer `[logical(1)=FALSE]` \cr If `TRUE`
#' @param calculate_area `[logical(1)=FALSE]` \cr
#' @param calculate_percentage `[logical(1)=FALSE]` \cr
#' @param core_number `[logical(1)=FALSE]` \cr
#' @param buffer_radius `[numeric]` \cr
#' @param buffer_circular `[logical(1)=FALSE]` \cr
#' @param nprocs `[numeric()]` \cr
#' @param memory `[numeric()]` \cr
#'
#' @example examples/lsm_core_edge_example.R
#'
#' @name lsm_core_edge
#' @export
lsm_core_edge <- function(input,
                          output = NULL,
                          zero_as_na = FALSE,
                          edge_depth,
                          edge_contraction = "maximum",
                          core_edge_type = "both",
                          id = FALSE,
                          ncell = FALSE,
                          area_integer = FALSE,
                          calculate_area = FALSE,
                          core_edge_original = FALSE,
                          calculate_percentage = FALSE,
                          core_number = FALSE,
                          buffer_radius = NULL,
                          buffer_circular = FALSE,
                          nprocs = 1,
                          memory = 300){

    # edge depth ----
    res <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern = TRUE), value = TRUE)))

    if(edge_depth/res >= 1){
        window <- 2 * round(edge_depth/res, 0) + 1
    }else{
        stop("Edge depth is smaller than map resolution. Choose a higher value for the edge depth.")
    }

    # binary
    if(zero_as_na == TRUE){

        rgrass::execGRASS(cmd = "g.message", message = "Converting null and zero")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_core_edge_binary = if(isnull(", input, "), 0, 1)"))
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_core_edge_null = ", input))

    } else{

        rgrass::execGRASS(cmd = "g.message", message = "Converting null and zero")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_core_edge_binary = ", input))
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_core_edge_null = if(", input, " == 1, 1, null())"))
    }

    # id ----
    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "quiet", "overwrite"),
                      input = paste0(input, output, "_core_edge_null"),
                      output = paste0(input, output, "_core_edge_id"))

    # core ----
    if(core_edge_type == "both" | core_edge_type == "core"){

        # core
        rgrass::execGRASS(cmd = "g.message", message = "Calculating core")
        rgrass::execGRASS(cmd = "r.neighbors",
                          flags = c("overwrite", "quiet"),
                          input = paste0(input, output, "_core_edge_binary"),
                          selection = paste0(input, output, "_core_edge_binary"),
                          output = paste0(input, output, "_core", edge_depth),
                          method = "min",
                          size = window,
                          nprocs = nprocs,
                          memory = memory)

        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_core", edge_depth, "_null = if(", input, output, "_core", edge_depth, " == 1, 1, null())"))

        rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_core", edge_depth), color = "blues")

        # core area
        if(calculate_area == TRUE){

            rgrass::execGRASS(cmd = "g.message", message = "Calculating core area")

            lsmetrics::lsm_fragment_area(input = paste0(input, output, "_core", edge_depth), id = id | core_number, ncell = ncell, area_integer = area_integer)

            rgrass::execGRASS(cmd = "g.rename", flags = "quiet", raster = paste0(input, output, "_core", edge_depth, "_fragment_area_ha,", input, output, "_core", edge_depth, "_area_ha"))

            if(id | core_number == TRUE){
                rgrass::execGRASS(cmd = "g.rename", flags = "quiet", raster = paste0(input, output, "_core", edge_depth, "_fragment_id,", input, output, "_core", edge_depth, "_id"))
                rgrass::execGRASS(cmd = "r.colors", flags = c("quiet"), map = paste0(input, output, "_core", edge_depth, "_id"), color = "random")
            }else{
                rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_core", edge_depth, "_id"))
            }

            if(ncell == TRUE){
                rgrass::execGRASS(cmd = "g.rename", flags = "quiet", raster = paste0(input, output, "_core", edge_depth, "_fragment_area_ncell,", input, output, "_core", edge_depth, "_area_ncell"))
            }

            if(core_edge_original == TRUE){

                rgrass::execGRASS(cmd = "r.stats.zonal",
                                  flags = c("overwrite", "quiet"),
                                  base = paste0(input, output, "_core_edge_id"),
                                  cover = paste0(input, output, "_core", edge_depth, "_null"),
                                  method = "count",
                                  output = paste0(input, output, "_core", edge_depth, "_area_ncell_original"))

                area_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))^2/1e4

                rgrass::execGRASS(cmd = "r.mapcalc",
                                  flags = c("overwrite", "quiet"),
                                  expression = paste0(input, output, "_core", edge_depth, "_area_ha_original=", input, output, "_core", edge_depth, "_area_ncell_original * ", area_pixel))
                rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_core", edge_depth, "_area_ha_original"), color = "ryg")

                if(area_integer == TRUE){
                    rgrass::execGRASS(cmd = "r.mapcalc",
                                      flags = c("overwrite", "quiet"),
                                      expression = paste0(input, output, "_core", edge_depth, "_area_ha_original = round(", input, output, "_core", edge_depth, "_area_ha_original)"))
                    rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_core", edge_depth, "_area_ha_original"), color = "ryg")
                }

                # ncell
                if(ncell == TRUE){

                    rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_core", edge_depth, "_area_ncell_original"), color = "ryg")

                }else{

                    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_core", edge_depth, "_area_ncell_original"))

                }
            }
        }

        # core percentage
        if(calculate_percentage == TRUE){

            rgrass::execGRASS(cmd = "g.message", message = "Calculating core percentage")

            lsmetrics::lsm_percentage(input = paste0(input, output, "_core", edge_depth),
                                      buffer_radius = buffer_radius,
                                      buffer_circular = buffer_circular)

            rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_core", edge_depth, "_pct_buf", buffer_radius), color = "blues")

        }

        # core number
        if(core_number == TRUE){

            rgrass::execGRASS(cmd = "r.stats",
                              flags = c("N", "overwrite"),
                              separator = ",",
                              input = paste0(input, output, "_core_edge_id,", input, output, "_core", edge_depth, "_id"),
                              output = paste0(input, output, "_core_id.txt"))

            readr::write_delim(dplyr::mutate(dplyr::count(readr::read_csv(paste0(input, output, "_core_id.txt"), show_col_types = FALSE, col_names = c("id", "n_core_ids")), id), n = n - 1),
                               paste0(input, output, "_core_id.txt"), delim = "=", col_names = FALSE)

            rgrass::execGRASS(cmd = "r.reclass",
                              flags = c("overwrite", "quiet"),
                              input = paste0(input, output, "_core_edge_id"),
                              output = paste0(input, output, "_core", edge_depth, "_core_number_original_temp"),
                              rules = paste0(input, output, "_core_id.txt"))

            rgrass::execGRASS(cmd = "r.mapcalc",
                              flags = c("overwrite", "quiet"),
                              expression = paste0(input, output, "_core", edge_depth, "_core_number_original = ", input, output, "_core", edge_depth, "_core_number_original_temp"))

            unlink(paste0(input, output, "_core_id.txt"))

            rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_core", edge_depth, "_core_number_original_temp"))

        }

        # clean
        rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_core", edge_depth, "_null"))

    }

    # edge ----
    if(core_edge_type == "both" | core_edge_type == "edge"){

        if(core_edge_type == "edge"){

            rgrass::execGRASS(cmd = "r.neighbors",
                              flags = c("overwrite", "quiet"),
                              input = paste0(input, output, "_core_edge_binary"),
                              selection = paste0(input, output, "_core_edge_binary"),
                              output = paste0(input, output, "_core", edge_depth),
                              method = "min",
                              size = window,
                              nprocs = nprocs,
                              memory = memory)

            # edge
            rgrass::execGRASS(cmd = "g.message", message = "Calculating edge")
            rgrass::execGRASS(cmd = "r.mapcalc",
                              flags = c("overwrite", "quiet"),
                              expression = paste0(input, output, "_edge", edge_depth, "=", input, output, "_core_edge_binary - ", input, output, "_core", edge_depth))

            rgrass::execGRASS(cmd = "r.mapcalc",
                              flags = c("overwrite", "quiet"),
                              expression = paste0(input, output, "_edge", edge_depth, "_null = if(", input, output, "_edge", edge_depth, " == 1, 1, null())"))

            rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_edge", edge_depth), color = "oranges")

            rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_core", edge_depth))

        } else{

            # edge
            rgrass::execGRASS(cmd = "g.message", message = "Calculating edge")
            rgrass::execGRASS(cmd = "r.mapcalc",
                              flags = c("overwrite", "quiet"),
                              expression = paste0(input, output, "_edge", edge_depth, "=", input, output, "_core_edge_binary - ", input, output, "_core", edge_depth))

            rgrass::execGRASS(cmd = "r.mapcalc",
                              flags = c("overwrite", "quiet"),
                              expression = paste0(input, output, "_edge", edge_depth, "_null = if(", input, output, "_edge", edge_depth, " == 1, 1, null())"))

            rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_edge", edge_depth), color = "oranges")

        }

        # edge area
        if(calculate_area == TRUE){

            rgrass::execGRASS(cmd = "g.message", message = "Calculating edge area")

            lsmetrics::lsm_fragment_area(input = paste0(input, output, "_edge", edge_depth), id = id, ncell = ncell, area_integer = area_integer)

            rgrass::execGRASS(cmd = "g.rename", flags = "quiet", raster = paste0(input, output, "_edge", edge_depth, "_fragment_area_ha,", input, output, "_edge", edge_depth, "_area_ha"))

            if(ncell == TRUE){
                rgrass::execGRASS(cmd = "g.rename", flags = "quiet", raster = paste0(input, output, "_edge", edge_depth, "_fragment_area_ncell,", input, output, "_edge", edge_depth, "_area_ncell"))
            }

            if(core_edge_original == TRUE){
                rgrass::execGRASS(cmd = "r.stats.zonal",
                                  flags = c("overwrite", "quiet"),
                                  base = paste0(input, output, "_core_edge_id"),
                                  cover = paste0(input, output, "_edge", edge_depth, "_null"),
                                  method = "count",
                                  output = paste0(input, output, "_edge", edge_depth, "_area_ncell_original"))

                area_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))^2/1e4

                rgrass::execGRASS(cmd = "r.mapcalc",
                                  flags = c("overwrite", "quiet"),
                                  expression = paste0(input, output, "_edge", edge_depth, "_area_ha_original=", input, output, "_edge", edge_depth, "_area_ncell_original * ", area_pixel))
                rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_edge", edge_depth, "_area_ha_original"), color = "ryg")

                if(area_integer == TRUE){

                    rgrass::execGRASS(cmd = "r.mapcalc",
                                      flags = c("overwrite", "quiet"),
                                      expression = paste0(input, output, "_edge", edge_depth, "_area_ha_original = round(", input, output, "_edge", edge_depth, "_area_ha_original)"))
                }

                # id
                if(id == FALSE){
                    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_edge", edge_depth, "_id"))
                    rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_edge", edge_depth, "_id"), color = "random")
                } else{
                    rgrass::execGRASS(cmd = "g.rename", flags = "quiet", raster = paste0(input, output, "_edge", edge_depth, "_fragment_id,", input, output, "_edge", edge_depth, "_id"))
                }

                # ncell
                if(ncell == TRUE){
                    rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_edge", edge_depth, "_area_ncell_original"), color = "ryg")
                }else{
                    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_edge", edge_depth, "_area_ncell_original"))
                }

            }

        }

        # edge percentage
        if(calculate_percentage == TRUE){

            rgrass::execGRASS(cmd = "g.message", message = "Calculating edge percentage")
            lsmetrics::lsm_percentage(input = paste0(input, output, "_edge", edge_depth),
                                      buffer_radius = buffer_radius,
                                      buffer_circular = buffer_circular)

            rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_edge", edge_depth, "_pct_buf", buffer_radius), color = "oranges")

        }

        # clean
        rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_edge", edge_depth, "_null"))

    }

    # clean
    rgrass::execGRASS(cmd = "g.message", message = "Cleaning rasters")
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_core_edge_binary"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_core_edge_null"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_core_edge_id"))

}
