#' Calculate structural connectivity
#'
#' Identifies patch and calculates area in hectare.
#'
#' @param input `[character]` Habitat map (binary classification: e.g., 1/0 or 1/NA) in GRASS.
#' @param output `[character]` Output map base name in GRASS.
#' @param zero_as_null `[logical]` If TRUE, non-habitat (0) cells are converted to NULL.
#' @param id_direction `[numeric]` Neighborhood for clumping (4 or 8).
#' @param area_round_digit `[integer]` Decimal digits for area rounding.
#' @param area_unit `[character]` Area unit: `"ha"`, `"m2"`, or `"km2"`.
#' @param map_id `[logical]` Keep fragment ID raster?
#' @param map_ncell `[logical]` Output raster with fragment cell counts?
#' @param map_area `[logical]` Output raster with fragment area?
#'
#' @example examples/lsm_connectivity_structural_example.R
#'
#' @name lsm_connectivity_structural
#' @export
lsm_connectivity_structural <- function(input,
                                        output = NULL,
                                        zero_as_null = FALSE,
                                        id_direction = 8,
                                        area_round_digit = 0,
                                        area_unit = "ha",
                                        map_connec_struct = TRUE,
                                        map_connec_struct_area = FALSE,
                                        nprocs = 1,
                                        memory = 300){

    # region ----
    rgrass::execGRASS("g.region", flags = "a", raster = input)

    # proj units ----
    proj_info <- rgrass::execGRASS("g.proj", flags = "g", intern = TRUE)
    proj_unit <- tolower(sub("units=", "", proj_info[grepl("^units=", proj_info)]))

    if(proj_unit %in% c("meters", "degrees")){

        # binary ----
        if(zero_as_null){

            rgrass::execGRASS(cmd = "r.mapcalc",
                              flags = c("overwrite", "quiet"),
                              expression = paste0(input, output, "_connec_struct_null =", input))

        } else{

            rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
            rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite", "quiet"),
                              expression = paste0(input, output, "_connec_struct_null = if(", input, " == 1, 1, null())"))

        }

        # clump ----
        if (!id_direction %in% c(4, 8)) stop("Clump `id_direction` must be 4 or 8.")
        clump_flags <- c("quiet", "overwrite")
        if (id_direction == 8) clump_flags <- c("d", clump_flags)
        rgrass::execGRASS("g.message", message = "Identifying fragments")
        rgrass::execGRASS(cmd = "r.clump",
                          flags = clump_flags,
                          input = paste0(input, output, "_connec_struct_null"),
                          output = paste0(input, output, "_connec_struct_id"))

        # fragment area ----
        lsmetrics::lsm_area_fragment(input = paste0(input, output, "_connec_struct_null"),
                                     zero_as_null = TRUE,
                                     id_direction = id_direction,
                                     area_round_digit = area_round_digit,
                                     area_unit = area_unit)

        # patch area ----
        lsmetrics::lsm_area_patch(input = paste0(input, output, "_connec_struct_null"),
                                  zero_as_null = TRUE,
                                  area_round_digit = area_round_digit,
                                  area_unit = area_unit,
                                  nprocs = nprocs,
                                  memory = memory)

        # structural connectivity ----
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_connec_struct = ",
                                              input, output, "_connec_struct_null_fragment_area - ",
                                              input, output, "_connec_struct_null_patch_area"))

        rgrass::execGRASS(cmd = "r.colors",
                          flags = c("g", "quiet"),
                          map = paste0(input, output, "_connec_struct"),
                          color = "ryg")

        # structural connected area ----
        if(map_connec_struct_area){

            rgrass::execGRASS(cmd = "r.stats",
                              flags = c("1", "N", "overwrite"),
                              input = paste0(input, output, "_connec_struct_id,",
                                             input, output, "_connec_struct"),
                              output = paste0(input, output, "_connec_struct_area.txt"),
                              separator = ",")

            readr::read_csv(paste0(input, output, "_connec_struct_area.txt"),
                            show_col_types = FALSE, col_names = c("fid", "str_con")) %>%
                dplyr::distinct() %>%
                dplyr::mutate(str_con = as.numeric(ifelse(str_con == "*", 0, str_con))) %>%
                dplyr::group_by(fid) %>%
                dplyr::summarise(str_con = sum(str_con)) %>%
                dplyr::mutate(fid2 = fid) %>%
                dplyr::select(fid, fid2, str_con) %>%
                readr::write_delim(paste0(input, output, "_connec_struct_area.txt"),
                                   delim = ":", col_names = FALSE)

            rgrass::execGRASS(cmd = "r.recode",
                              flags = c("overwrite", "quiet"),
                              input = paste0(input, output, "_connec_struct_id"),
                              output = paste0(input, output, "_connec_struct_area"),
                              rules = paste0(input, output, "_connec_struct_area.txt"))

            rgrass::execGRASS(cmd = "r.colors",
                              flags = c("g", "quiet"),
                              map = paste0(input, output, "_connec_struct_area"),
                              color = "ryg")

            unlink(paste0(input, output, "_connec_struct_area.txt"))

        }

        # clean ----
        rgrass::execGRASS(cmd = "g.message", message = "Cleaning data")

        suppressWarnings(
            rgrass::execGRASS(cmd = "g.remove",
                              flags = c("b", "f", "quiet"),
                              type = "raster",
                              name = c(paste0(input, output, "_connec_struct_null"),
                                       paste0(input, output, "_connec_struct_id"),
                                       paste0(input, output, "_connec_struct_null_fragment_area"),
                                       paste0(input, output, "_connec_struct_null_patch_area")))
        )

    } else {
        warning(paste("Units:", proj_unit, "not currently supported!"))
    }


}
