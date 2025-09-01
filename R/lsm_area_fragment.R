#' Calculate fragment area
#'
#' Identifies fragments and calculates area in a selected unit.
#'
#' @param input `[character]` Habitat map (binary classification: e.g., 1/0 or 1/NA) in GRASS.
#' @param output `[character]` Output map base name in GRASS.
#' @param zero_as_null `[logical]` If TRUE, non-habitat (0) cells are converted to NULL.
#' @param region_input `[logical]`
#' @param id_direction `[numeric]` Neighborhood for clumping (4 or 8).
#' @param area_round_digit `[integer]` Decimal digits for area rounding.
#' @param area_unit `[character]` Area unit: `"ha"`, `"m2"`, or `"km2"`.
#' @param map_fragment_id `[logical]` Keep fragment ID raster?
#' @param map_fragment_ncell `[logical]` Output raster with fragment cell counts?
#' @param map_fragment_area `[logical]` Output raster with fragment area?
#' @param table_fragment_area `[logical]` Output CSV with fragment area and summary?
#'
#' @example examples/lsm_area_fragment_example.R
#'
#' @name lsm_area_fragment
#' @export
lsm_area_fragment <- function(input,
                              output = NULL,
                              zero_as_null = FALSE,
                              region_input = FALSE,
                              id_direction = 8,
                              area_round_digit = 0,
                              area_unit = "ha",
                              map_fragment_id = FALSE,
                              map_fragment_ncell = FALSE,
                              map_fragment_area = TRUE,
                              table_fragment_area = FALSE){

    # fix names ----
    input <- lsmetrics::lsm_aux_fix_names(input)

    # region ----
    if(region_input){
        rgrass::execGRASS("g.region", flags = "a", raster = input)
    }

    # binary ----
    if(zero_as_null) {

        rgrass::execGRASS("r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_fragment_null = ", input)
        )
    } else {

        rgrass::execGRASS("g.message", message = "Converting zeros to null")
        rgrass::execGRASS("r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_fragment_null = if(", input, " == 1, 1, null())")
        )
    }

    # clump ----
    rgrass::execGRASS("g.message", message = "Identifying fragments")
    if (!id_direction %in% c(4, 8)) stop("Clump `id_direction` must be 4 or 8.")
    clump_flags <- c("quiet", "overwrite")
    if (id_direction == 8) clump_flags <- c("d", clump_flags)

    rgrass::execGRASS(cmd = "r.clump",
                      flags = clump_flags,
                      input = paste0(input, output, "_fragment_null"),
                      output = paste0(input, output, "_fragment_id")
    )

    # calculate area ----
    rgrass::execGRASS("g.message", message = "Calculation area")

    lsmetrics::lsm_aux_area(input_null = paste0(input, output, "_fragment_null"),
                            input_id = paste0(input, output, "_fragment_id"),
                            area_round_digit = area_round_digit,
                            area_unit = area_unit,
                            map_ncell = map_fragment_ncell,
                            table_export = table_fragment_area)

    # cleaning
    rgrass::execGRASS("g.message", message = "Cleaning data")

    if (!map_fragment_id) {
        suppressWarnings(
            rgrass::execGRASS("g.remove",
                              flags = c("b", "f", "quiet"),
                              type = "raster",
                              name = paste0(input, output, "_fragment_id"))
        )
    }

    suppressWarnings(
        rgrass::execGRASS("g.remove",
                          flags = c("b", "f", "quiet"),
                          type = "raster",
                          name = paste0(input, output, "_fragment_null"))
    )

}
