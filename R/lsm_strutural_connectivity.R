#' Calculate structural connectivity
#'
#' Identifies patch and calculates area in hectare.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat) inside GRASS Data Base.
#' @param output `[character=""]` \cr Map name output inside GRASS Data Base.
#' @param zero_as_na `[logical(1)=FALSE]` \cr If `TRUE`, the function treats
#' non-habitat cells as null; if `FALSE`, the function converts non-habitat zero
#' cells to null cells.
#'
#' @example examples/lsm_structural_connectivity_example.R
#'
#' @name lsm_structural_connectivity
#' @export
lsm_structural_connectivity <- function(input,
                                        output = NULL,
                                        ncell = FALSE,
                                        area_integer = FALSE,
                                        zero_as_na = FALSE){

    # binary
    if(zero_as_na){

        # null
        rgrass::execGRASS(cmd = "g.message", message = "Identifying the patches")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_structural_connectivity_null =", input, output))

    } else{

        # null
        rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_structural_connectivity_null = if(", input, " == 1, 1, null())"))

    }

    # fragment area ----
    lsmetrics::lsm_fragment_area(input = input, output = output, zero_as_na = zero_as_na, id = FALSE, ncell = ncell, area_integer = area_integer)

    # patch area ----
    lsmetrics::lsm_patch_area(input = input, output = output, zero_as_na = zero_as_na, id = FALSE, ncell = ncell, area_integer = area_integer, patch_original = FALSE, patch_number = FALSE)

    # structural connectivity ----
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_structural_connectivity = ", input, output, "_fragment_area_ha - ", input, output, "_patch_area_ha"))

   # clean ----
   rgrass::execGRASS(cmd = "g.message", message = "Cleaning rasters")
   rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_structural_connectivity_null"))
   rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_fragment_area_ha"))
   rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_patch_area_ha"))

}
