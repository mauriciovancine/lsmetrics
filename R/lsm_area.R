#' Calculate area
#'
#' Identifies patches and calculates area using the r.area GRASS module.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param output `[character=""]` \cr Patch area map name inside GRASS Data Base.
#' @param zero_as_na `[logical(1)=FALSE]` \cr If `TRUE`, the function treats
#' cells that are zero as if they were null (non-habitat); if `FALSE`, the function
#' converts zero cells to null cells.
#'
#' @example examples/lsm_area_example.R
#'
#' @name lsm_area
#' @export
lsm_area <- function(input,
                     output = NULL,
                     zero_as_na = FALSE){

    # binary
    if(zero_as_na){

        # patch id
        rgrass::execGRASS(cmd = "g.message", message = "Identifying the patches")
        rgrass::execGRASS(cmd = "r.clump",
                          flags = c("d", "quiet", "overwrite"),
                          input = input,
                          output = paste0(input, output, "_pid"))

    } else{

        # null
        rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_null = if(", input, " == 1, 1, null())"))

        # patch id
        rgrass::execGRASS(cmd = "g.message", message = "Identifying the patches")
        rgrass::execGRASS(cmd = "r.clump",
                          flags = c("d", "quiet", "overwrite"),
                          input = paste0(input, output, "_null"),
                          output = paste0(input, output, "_pid"))

    }

    # ncell
    rgrass::execGRASS(cmd = "g.message", message = "Counting the cell number of patches")
    rgrass::execGRASS(cmd = "r.stats.zonal",
                      flags = c("overwrite"),
                      base = paste0(input, output, "_pid"),
                      cover = paste0(input, output, "_null"),
                      method = "count",
                      output = paste0(input, output, "_area_ncell"))

    # area
    rgrass::execGRASS(cmd = "g.message", message = "Calculating the area of patches")
    area_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))^2/1e4
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite"),
                      expression = paste0(input, output, "_area_ha=", input, output, "_area_ncell * ", area_pixel))
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite"),
                      expression = paste0(input, output, "_area_ha_int=if(", input, output, "_area_ha < 1, 1, ", input, output, "_area_ha)"))
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite"),
                      expression = paste0(input, output, "_area_ha_int=round(", input, output, "_area_ha_int)"))

    # color
    rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")
    rgrass::execGRASS(cmd = "r.colors",
                      flags = c("g", "quiet"),
                      map = paste0(input, output, "_area_ncell"),
                      color = "ryg")
    rgrass::execGRASS(cmd = "r.colors",
                      flags = c("g", "quiet"),
                      map = paste0(input, output, "_area_ha"),
                      color = "ryg")
    rgrass::execGRASS(cmd = "r.colors",
                      flags = c("g", "quiet"),
                      map = paste0(input, output, "_area_ha_int"),
                      color = "ryg")

}
