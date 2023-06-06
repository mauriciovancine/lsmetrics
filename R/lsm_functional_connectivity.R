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
#' @param gap_crossing `[numeric]` \cr Integer indicating gap crossing distance.
#'
#' @example examples/lsm_functional_connectivity_example.R
#'
#' @name lsm_functional_connectivity
#' @export
lsm_functional_connectivity <- function(input,
                                        output = NULL,
                                        zero_as_na = FALSE,
                                        id = FALSE,
                                        ncell = FALSE,
                                        area_integer = FALSE,
                                        gap_crossing){

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
                          expression = paste0(input, output, " = if(", input, " == 1, 1, 0)"))


    } else{

        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, " = ", input, output))
    }

    # functional connectivity ----
    rgrass::execGRASS(cmd = "g.message", message = "Selecting gap crossing pixels")
    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = c("c", "overwrite"),
                      input = paste0(input, output),
                      selection = paste0(input, output),
                      output = paste0(input, output, "_dilation", gap_crossing_name),
                      method = "max",
                      size = window)

    rgrass::execGRASS(cmd = "g.message", message = "Summing gap crossing pixels to habitat raster")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_dilation", gap_crossing_name, "_sum =", input, output, " + ", input, output, "_dilation", gap_crossing_name))

    rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_dilation", gap_crossing_name, "_sum_null = if(", input, output, "_dilation", gap_crossing_name, "_sum > 0, 1, null())"))

    rgrass::execGRASS(cmd = "g.message", message = "Identifying the fragmentes")
    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "quiet", "overwrite"),
                      input = paste0(input, output, "_dilation", gap_crossing_name, "_sum_null"),
                      output = paste0(input, output, "_dilation", gap_crossing_name, "_id"))

    rgrass::execGRASS(cmd = "g.message", message = "Multipling id by original habitat")
    rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                      expression = paste0(input, output, "_null = if(", input, " == 1, 1, null())"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_confun", gap_crossing_name, "_id = ", input, output, "_dilation", gap_crossing_name, "_id * ", input, output, "_null"))

    rgrass::execGRASS(cmd = "g.message", message = "Counting the number of fragmentes")
    rgrass::execGRASS(cmd = "r.stats.zonal",
                      flags = c("overwrite"),
                      base = paste0(input, output, "_confun", gap_crossing_name, "_id"),
                      cover = paste0(input, output, "_null"),
                      method = "count",
                      output = paste0(input, output, "_confun", gap_crossing_name, "_area_ncell"))


    if(area_integer == FALSE){
        rgrass::execGRASS(cmd = "g.message", message = "Calculating the area")
        area_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))^2/1e4
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = "overwrite",
                          expression = paste0(input, output, "_confun", gap_crossing_name, "_area_ha = ", input, output, "_confun", gap_crossing_name, "_area_ncell * ", area_pixel))

    }else{

        rgrass::execGRASS(cmd = "g.message", message = "Calculating the area")
        area_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))^2/1e4
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = "overwrite",
                          expression = paste0(input, output, "_confun", gap_crossing_name, "_area_ha = ", input, output, "_confun", gap_crossing_name, "_area_ncell * ", area_pixel))
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = "overwrite",
                          expression = paste0(input, output, "_confun", gap_crossing_name, "_area_ha = round(", input, output, "_confun", gap_crossing_name, "_area_ha)"))

    }

    # color
    rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")

    rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_confun", gap_crossing_name, "_id"), color = "random")
    rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_confun", gap_crossing_name, "_area_ncell"), color = "ryg")
    rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_confun", gap_crossing_name, "_area_ha"), color = "ryg")

    # id ----
    if(id == FALSE){

        rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = paste0(input, output, "_dilation", gap_crossing_name, "_id"))

    }

    # ncell ----
    if(ncell == FALSE){

        rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_confun", gap_crossing_name, "_area_ncell"))

    }else{

        rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_confun", gap_crossing_name, "_area_ncell"), color = "ryg")

    }

    # clena
    rgrass::execGRASS(cmd = "g.message", message = "Removing extra rasters")
    rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = paste0(input, output, "_dilation", gap_crossing_name))
    rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = paste0(input, output, "_dilation", gap_crossing_name, "_id"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = paste0(input, output, "_dilation", gap_crossing_name, "_sum"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = paste0(input, output, "_dilation", gap_crossing_name, "_sum_null"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = paste0(input, output, "_null"))

}
