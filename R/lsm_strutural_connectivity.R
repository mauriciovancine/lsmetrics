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
#' @param id `[logical(1)=FALSE]` \cr If `TRUE`
#' @param ncell `[logical(1)=FALSE]` \cr If `TRUE`
#' @param area_integer `[logical(1)=FALSE]` \cr If `TRUE`
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
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = "overwrite",
                          expression = paste0(input, output, "_structural_connectivity_null =", input, output))
        rgrass::execGRASS(cmd = "r.clump",
                          flags = "overwrite",
                          input = paste0(input, output, "_structural_connectivity_null"),
                          output = paste0(input, output, "_structural_connectivity_id"))

    } else{

        # null
        rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_structural_connectivity_null = if(", input, " == 1, 1, null())"))
        rgrass::execGRASS(cmd = "r.clump",
                          flags = "overwrite",
                          input = paste0(input, output, "_structural_connectivity_null"),
                          output = paste0(input, output, "_structural_connectivity_id"))
    }

    # fragment area ----
    lsmetrics::lsm_fragment_area(input = input, output = output, zero_as_na = zero_as_na, id = FALSE, ncell = ncell, area_integer = area_integer)

    # patch area ----
    lsmetrics::lsm_patch_area(input = input, output = output, zero_as_na = zero_as_na, id = FALSE, ncell = ncell, area_integer = area_integer, patch_original = FALSE, patch_number = FALSE)

    # structural connectivity ----
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_structural_connectivity = ", input, output, "_fragment_area_ha - ", input, output, "_patch_area_ha"))

    rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_structural_connectivity"), color = "ryg")

    # structural connected area ----
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_structural_connectivity_int = int(", input, output, "_structural_connectivity)"))

    rgrass::execGRASS(cmd = "r.stats",
                      flags = c("N", "overwrite"),
                      separator = ",",
                      input = paste0(input, output, "_structural_connectivity_id,", input, output, "_structural_connectivity_int"),
                      output = paste0(input, output, "_structural_connected_area.txt"))

    readr::write_delim(dplyr::summarise(dplyr::group_by(dplyr::mutate(readr::read_csv(paste0(input, output, "_structural_connected_area.txt"), show_col_types = FALSE, col_names = c("id", "area")), area = as.numeric(ifelse(area == "*", 0, area))), id), sum = sum(area)),
                       paste0(input, output, "_structural_connected_area.txt"), delim = "=", col_names = FALSE)

    rgrass::execGRASS(cmd = "r.reclass",
                      flags = "overwrite",
                      input = paste0(input, output, "_structural_connectivity_id"),
                      output = paste0(input, output, "_structural_connected_area_temp"),
                      rules = paste0(input, output, "_structural_connected_area.txt"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_structural_connected_area = ", input, output, "_structural_connected_area_temp"))

    rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_structural_connected_area"), color = "ryg")

    unlink(paste0(input, output, "_structural_connected_area.txt"))

    # clean ----
    rgrass::execGRASS(cmd = "g.message", message = "Cleaning rasters")
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_structural_connectivity_null"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_structural_connected_area_temp"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_structural_connectivity_id"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_structural_connectivity_int"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_fragment_area_ha"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_patch_area_ha"))

}
