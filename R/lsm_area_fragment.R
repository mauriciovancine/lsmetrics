#' Calculate fragment area
#'
#' Identifies fragments and calculates area in a selected unit.
#'
#' @param input `[character]` Habitat map (binary classification: e.g., 1/0 or 1/NA) in GRASS.
#' @param output `[character]` Output map base name in GRASS.
#' @param zero_as_null `[logical]` If TRUE, non-habitat (0) cells are converted to NULL.
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
                              id_direction = 8,
                              area_round_digit = 0,
                              area_unit = "ha",
                              map_fragment_id = FALSE,
                              map_fragment_ncell = FALSE,
                              map_fragment_area = TRUE,
                              table_fragment_area = FALSE) {

    # region ----
    rgrass::execGRASS("g.region", flags = "a", raster = input)

    # binary ----
    if (zero_as_null) {
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
    if (!id_direction %in% c(4, 8)) stop("Clump `id_direction` must be 4 or 8.")
    clump_flags <- c("quiet", "overwrite")
    if (id_direction == 8) clump_flags <- c("d", clump_flags)

    rgrass::execGRASS("g.message", message = "Identifying fragments")
    rgrass::execGRASS("r.clump",
                      flags = clump_flags,
                      input = paste0(input, output, "_fragment_null"),
                      output = paste0(input, output, "_fragment_id")
    )

    # calculate area ----
    rgrass::execGRASS("g.message", message = "Calculation area")

    if (map_fragment_id || map_fragment_ncell || map_fragment_area || table_area) {

        fragment_area <- rgrass::execGRASS("r.stats",
                                           flags = c("a", "c", "n", "overwrite", "quiet"),
                                           separator = ",",
                                           input = paste0(input, output, "_fragment_id"),
                                           intern = TRUE) %>%
            tibble::as_tibble() %>%
            tidyr::separate(value,
                            into = c("fid", "area", "ncell"),
                            sep = ",", convert = TRUE)

        # area unit ----
        if (area_unit == "ha") {
            fragment_area_unit_rounded <- fragment_area %>%
                dplyr::mutate(area = round(area / 1e4, area_round_digit))
        } else if (area_unit == "m2") {
            fragment_area_unit_rounded <- fragment_area %>%
                dplyr::mutate(area = round(area, area_round_digit))
        } else if (area_unit == "km2") {
            fragment_area_unit_rounded <- fragment_area %>%
                dplyr::mutate(area = round(area / 1e6, area_round_digit))
        } else {
            stop("Choose a valid area_unit: 'm2', 'km2', or 'ha'")
        }

        # assign area ----
        rgrass::execGRASS("g.message", message = "Assigning area")
        if (map_fragment_area) {
            fragment_area_unit_rounded %>%
                dplyr::mutate(fid2 = fid) %>%
                dplyr::select(fid, fid2, area) %>%
                readr::write_delim(paste0(input, output, "_fragment_area.txt"),
                                   delim = ":", col_names = FALSE)

            rgrass::execGRASS("r.recode",
                              flags = "overwrite",
                              input = paste0(input, output, "_fragment_id"),
                              output = paste0(input, output, "_fragment_area"),
                              rules = paste0(input, output, "_fragment_area.txt"))

            rgrass::execGRASS("r.colors",
                              flags = c("g", "quiet"),
                              map = paste0(input, output, "_fragment_area"),
                              color = "ryg"
            )

            unlink(paste0(input, output, "_fragment_area.txt"))
        }

        # ncell ----
        if (map_fragment_ncell) {

            fragment_area_unit_rounded %>%
                dplyr::mutate(fid2 = fid) %>%
                dplyr::select(fid, fid2, ncell) %>%
                readr::write_delim(paste0(input, output, "_fragment_ncell.txt"),
                                   delim = ":", col_names = FALSE)

            rgrass::execGRASS("r.recode",
                              flags = "overwrite",
                              input = paste0(input, output, "_fragment_id"),
                              output = paste0(input, output, "_fragment_ncell"),
                              rules = paste0(input, output, "_fragment_ncell.txt"))

            unlink(paste0(input, output, "_fragment_ncell.txt"))
        }

        # table ----
        if (table_fragment_area) {

            rgrass::execGRASS("g.message", message = "Exporting table")

            fragment_area_unit_rounded %>%
                readr::write_csv(paste0(input, output, "_fragment_area.csv"))

            fragment_area_unit_rounded %>%
                dplyr::summarise(
                    n_frag = dplyr::n(),
                    area_mean = round(mean(area), 2),
                    area_sd = round(sd(area), 2),
                    area_cv = round(area_sd/area_mean * 100, 2),
                    area_max = max(area),
                    area_min = min(area)) %>%
                readr::write_csv(paste0(input, output, "_fragment_area_summary.csv"))
        }
    }

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
