#' Calculate functional connectivity
#'
#' Identifies functional patches connected.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param output `[character=""]` \cr Patch area map name inside GRASS Data Base
#' @param resolution `[numeric(1)=4]{4,8}` \cr Integer indicating which cells are
#' @param gap_crossing `[numeric(1)=4]{4,8}` \cr Integer indicating which cells are
#'
#' @example examples/lsm_functional_connectivity_example.R
#'
#' @name lsm_functional_connectivity
#' @export
lsm_functional_connectivity <- function(input,
                                        output = NULL,
                                        zero_as_na = FALSE,
                                        input_distance_outside,
                                        gap_crossing){

    # gap crossing
    res <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))

    if(gap_crossing < res){
        stop("Gap crossing is smaller than map resolution. Choose a higher value for the gap crossing.")
    }

    # binary
    if(zero_as_na){

        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_null = ", input))


    } else{
        rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_null = if(", input, " == 1, 1, null())"))
    }

    # distance outside ----
    files <- rgrass::stringexecGRASS("g.list type=rast", intern=TRUE)

    if(!input_distance_outside %in% files){
        stop("Input distance outside does not exist. Use the lsm_distance() function with the argument `type = 'outside'` to create it.")
    }

    # gap crossing ----
    res <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))
    if(gap_crossing < res){
        stop("Gap crossing is smaller than map resolution. Choose a higher value for the gap crossing.")
    }

    # functional connectivity ----
    rgrass::execGRASS(cmd = "g.message", message = "Selecting gap crossing pixels")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_outside_distance_under", gap_crossing, "=if(", input_distance_outside, "> 0 && ", input_distance_outside, "<=", gap_crossing, ", 1, 0)"))

    rgrass::execGRASS(cmd = "g.message", message = "Summing gap crossing pixels to habitat raster")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_outside_distance_under", gap_crossing, "_sum =", input, " + ", input, output, "_outside_distance_under", gap_crossing))

    rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_outside_distance_under", gap_crossing, "_sum_null=if(", input, output, "_outside_distance_under", gap_crossing, "_sum==0, null(), 1)"))

    rgrass::execGRASS(cmd = "g.message", message = "Identifying the patches")
    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "quiet", "overwrite"),
                      input = paste0(input, output, "_outside_distance_under", gap_crossing, "_sum_null"),
                      output = paste0(input, output, "_outside_distance_under", gap_crossing, "_pid"))

    rgrass::execGRASS(cmd = "g.message", message = "Multipling pid by original habitat")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_confun", gap_crossing, "_pid = ", input, output, "_outside_distance_under", gap_crossing, "_pid * ", input, output, "_null"))

    rgrass::execGRASS(cmd = "g.message", message = "Counting the number of patches")
    rgrass::execGRASS(cmd = "r.stats.zonal",
                      flags = c("overwrite"),
                      base = paste0(input, output, "_confun", gap_crossing, "_pid"),
                      cover = paste0(input, output, "_null"),
                      method = "count",
                      output = paste0(input, output, "_confun", gap_crossing, "_area_ncell"))

    rgrass::execGRASS(cmd = "g.message", message = "Calculating the area")
    area_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))^2/1e4
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_confun", gap_crossing, "_area_ha=", input, output, "_confun", gap_crossing, "_area_ncell * ", area_pixel))
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_confun", gap_crossing, "_area_ha_int=if(", input, output, "_confun", gap_crossing, "_area_ha < 1, 1, ", input, output, "_confun", gap_crossing, "_area_ha)"))
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_confun", gap_crossing, "_area_ha_int=round(", input, output, "_confun", gap_crossing, "_area_ha)"))


    # color
    rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")

    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_confun", gap_crossing, "_pid"),
                      color = "random")

    rgrass::execGRASS(cmd = "r.colors",
                      flags = c("g", "quiet"),
                      map = paste0(input, output, "_confun", gap_crossing, "_area_ncell"),
                      color = "ryg")

    rgrass::execGRASS(cmd = "r.colors",
                      flags = c("g", "quiet"),
                      map = paste0(input, output, "_confun", gap_crossing, "_area_ha"),
                      color = "ryg")

    rgrass::execGRASS(cmd = "r.colors",
                      flags = c("g", "quiet"),
                      map = paste0(input, output, "_confun", gap_crossing, "_area_ha_int"),
                      color = "ryg")

    # delete
    rgrass::execGRASS(cmd = "g.message", message = "Removing extra rasters")
    rgrass::execGRASS(cmd = "g.remove",
                      flags = c("f", "quiet"),
                      type = "raster",
                      pattern =  "*outside_distance_under*")

}
