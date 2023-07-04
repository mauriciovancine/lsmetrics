#' Calculate functional connectivity
#'
#' Identifies functional fragmentes connected and calculate area in hectare.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param output `[character=""]` \cr Habitat area map name output GRASS Data Base
#' @param zero_as_na `[logical(1)=FALSE]` \cr If `TRUE`, the function treats
#' non-habitat cells as null; if `FALSE`, the function converts non-habitat zero
#' cells to null cells.
#' @param id `[logical(1)=FALSE]` \cr If `TRUE`
#' @param ncell `[logical(1)=FALSE]` \cr If `TRUE`
#' @param area_integer `[logical(1)=FALSE]` \cr If `TRUE`
#' @param gap_crossing `[numeric]` \cr Integer indicating gap crossing distance.
#' @param dilation `[logical(1)=FALSE]` \cr If `TRUE`
#' @param dilation_type `[character=""]` \cr If
#' @param nprocs `[numeric()]` \cr
#' @param memory `[numeric()]` \cr
#'
#' @example examples/lsm_functional_connectivity_example.R
#'
#' @name lsm_functional_connectivity
#' @export
lsm_functional_connectivity <- function(input,
                                        output = NULL,
                                        zero_as_na = FALSE,
                                        gap_crossing,
                                        id = FALSE,
                                        ncell = FALSE,
                                        area_integer = FALSE,
                                        dilation = FALSE,
                                        dilation_type = "minimum",
                                        nprocs = 1,
                                        memory = 300){

    # gap crossing
    res <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))

    if(gap_crossing/res >= 1){
        window <- 2 * round(gap_crossing/res, 0) + 1
    }else{
        stop("Gap crossing is smaller than map resolution. Choose a higher value for the gap crossing.")
    }

    # gap crossing name
    gap_crossing_name <- gap_crossing * 2

    # binary
    if(zero_as_na == TRUE){

        rgrass::execGRASS(cmd = "g.message", message = "Converting null as zero")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_functional_connectivity_binary = if(isnull(", input, "), 0, 1)"))
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_functional_connectivity_null = ",  input))

    } else{

        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_functional_connectivity_binary = ", input))
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_functional_connectivity_null = if(", input, " == 1, 1, null())"))
    }

    # dilation ----
    rgrass::execGRASS(cmd = "g.message", message = "Dilation pixels")
    if(dilation_type == "minimum"){

        rgrass::execGRASS(cmd = "r.neighbors",
                          flags = c("c", "overwrite"),
                          input = paste0(input, output, "_functional_connectivity_binary"),
                          selection = paste0(input, output, "_functional_connectivity_binary"),
                          output = paste0(input, output, "_functional_connectivity_dilation", gap_crossing_name),
                          method = "max",
                          size = window,
                          nprocs = nprocs,
                          memory = memory)

    }

    if(dilation_type == "maximum"){

        rgrass::execGRASS(cmd = "r.neighbors",
                          flags = "overwrite",
                          input = paste0(input, output, "_functional_connectivity_binary"),
                          selection = paste0(input, output, "_functional_connectivity_binary"),
                          output = paste0(input, output, "_functional_connectivity_dilation", gap_crossing_name),
                          method = "max",
                          size = window,
                          nprocs = nprocs,
                          memory = memory)

    }

    rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_functional_connectivity_dilation", gap_crossing_name, "_null = if(", input, output, "_functional_connectivity_dilation", gap_crossing_name, " == 1, 1, null())"))

    rgrass::execGRASS(cmd = "g.message", message = "Identifying the fragmentes for gap crossing")
    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "quiet", "overwrite"),
                      input = paste0(input, output, "_functional_connectivity_dilation", gap_crossing_name, "_null"),
                      output = paste0(input, output, "_functional_connectivity_dilation", gap_crossing_name, "_id"))

    rgrass::execGRASS(cmd = "g.message", message = "Multipling id by original habitat")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_functional_connected_area", gap_crossing_name, "_id = int(", input, output, "_functional_connectivity_dilation", gap_crossing_name, "_id * ", input, output, "_functional_connectivity_null)"))

    rgrass::execGRASS(cmd = "g.message", message = "Counting the number of fragmentes")
    rgrass::execGRASS(cmd = "r.stats.zonal",
                      flags = c("overwrite"),
                      base = paste0(input, output, "_functional_connected_area", gap_crossing_name, "_id"),
                      cover = paste0(input, output, "_functional_connectivity_null"),
                      method = "count",
                      output = paste0(input, output, "_functional_connected_area", gap_crossing_name, "_ncell"))

    # area
    rgrass::execGRASS(cmd = "g.message", message = "Calculating the functional connected area")
    area_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))^2/1e4
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_functional_connected_area", gap_crossing_name, " = ", input, output, "_functional_connected_area", gap_crossing_name, "_ncell * ", area_pixel))
    rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_functional_connected_area", gap_crossing_name), color = "ryg")

    if(area_integer == TRUE){
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = "overwrite",
                          expression = paste0(input, output, "_functional_connected_area", gap_crossing_name, " = round(", input, output, "_functional_connected_area", gap_crossing_name, ")"))
    }

    # id ----
    if(id == FALSE){

        rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = paste0(input, output, "_functional_connected_area", gap_crossing_name, "_id"))

    }

    # ncell ----
    if(ncell == FALSE){

        rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_functional_connected_area", gap_crossing_name, "_ncell"))

    }

    # functional connectivity ----
    # fragment area
    lsmetrics::lsm_fragment_area(input = input, output = output, zero_as_na = zero_as_na, id = FALSE, ncell = ncell, area_integer = area_integer)

    # functional connectivity
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_functional_connectivity", gap_crossing_name, " = ", input, output, "_functional_connected_area", gap_crossing_name, " - ", input, output, "_fragment_area_ha"))

    # dilation ----
    if(dilation == FALSE){
        rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = paste0(input, output, "_functional_connectivity_dilation", gap_crossing_name, "_null"))
    } else{
        rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_functional_connectivity_dilation", gap_crossing_name, "_null"), color = "grey")
    }

    # clean
    rgrass::execGRASS(cmd = "g.message", message = "Removing extra rasters")
    rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = paste0(input, output, "_functional_connectivity_dilation", gap_crossing_name))
    rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = paste0(input, output, "_functional_connectivity_dilation", gap_crossing_name, "_id"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = paste0(input, output, "_functional_connectivity_binary"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = paste0(input, output, "_functional_connectivity_null"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = paste0(input, output, "_fragment_area_ha"))
}
