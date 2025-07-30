#' Calculate patch area
#'
#' Identifies patch and calculates area in hectare.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification (e.g. values 1,0 or 1,NA for habitat,non-habitat) inside GRASS Data Base.
#' @param output `[character=""]` \cr Map name output inside GRASS Data Base.
#' @param zero_as_null `[logical(1)=FALSE]` \cr If `TRUE`, the function treats non-habitat cells as null; if `FALSE`, the function converts non-habitat zero cells to null cells.
#' @param direction `[numeric]` Neighborhood for clumping (4 or 8).
#' @param area_round_digit `[logical(1)=FALSE]` \cr If `TRUE`
#' @param map_id `[logical(1)=FALSE]` \cr If `TRUE`
#' @param map_ncell `[logical(1)=FALSE]` \cr If `TRUE`
#' @param map_patch_original `[logical(1)=FALSE]` \cr If `TRUE`
#' @param map_patch_number `[logical(1)=FALSE]` \cr If `TRUE`
#' @param nprocs `[numeric()]` \cr
#' @param memory `[numeric()]` \cr
#'
#' @example examples/lsm_area_patch_example.R
#'
#' @name lsm_area_patch
#' @export
lsm_area_patch <- function(input,
                           output = NULL,
                           zero_as_null = FALSE,
                           area_round_digit = 0,
                           area_unit = "ha",
                           map_patch_id = FALSE,
                           map_patch_area = TRUE,
                           map_patch_ncell = FALSE,
                           map_patch_area_original = FALSE,
                           map_patch_number_original = FALSE,
                           nprocs = 1,
                           memory = 300){

    # region ----
    rgrass::execGRASS("g.region", flags = "a", raster = input)

    # binary and null ----
    if(zero_as_null){

        # null
        rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_patch_null =", input))

        # binary
        rgrass::execGRASS("g.message", message = "Converting null to zero")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_patch_binary = if(isnull(", input, "), 0, 1)"))

    } else{

        # null
        rgrass::execGRASS("g.message", message = "Converting zeros to null")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_patch_null = if(", input, " == 1, 1, null())"))

        # binary
        rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_patch_binary = ", input))

    }

    # clump ----
    rgrass::execGRASS(cmd = "g.message", message = "Identifying the fragments")
    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "quiet", "overwrite"),
                      input = paste0(input, output, "_patch_null"),
                      output = paste0(input, output, "_patch_fid"))

    # fill hole ----
    lsmetrics::lsm_aux_fill_hole(input = paste0(input, output, "_patch_binary"),
                                 nprocs = nprocs,
                                 memory = memory)

    # patch id ----
    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = c("overwrite", "quiet"),
                      input = paste0(input, output, "_patch_binary_aux_fill_hole"),
                      selection = input,
                      output = paste0(input, output, "_patch_binary_contr"),
                      size = 3,
                      method = "min",
                      nprocs = nprocs,
                      memory = memory)

    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = c("overwrite", "quiet"),
                      input = paste0(input, output, "_patch_binary_contr"),
                      selection = input,
                      output = paste0(input, output, "_patch_binary_contr_dila"),
                      size = 3,
                      method = "max",
                      nprocs = nprocs,
                      memory = memory)

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_patch_binary_contr_dila_null = if(",
                                          input, output, "_patch_binary_contr_dila == 1, 1, null())"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_patch_binary_contr_dila_patch = ",
                                          input, output, "_patch_binary_contr_dila_null * ",
                                          input, output, "_patch_null"))

    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("overwrite", "quiet"),
                      input = paste0(input, output, "_patch_binary_contr_dila_patch"),
                      output = paste0(input, output, "_patch_id"))

    # calculate area ----
    rgrass::execGRASS("g.message", message = "Calculation area")

    if (map_patch_area || map_patch_id || map_patch_ncell || map_patch_area_original || map_patch_number_original) {
        patch_area <- rgrass::execGRASS("r.stats",
                                        flags = c("a", "c", "n", "overwrite", "quiet"),
                                        separator = ",",
                                        input = paste0(input, output, "_patch_id"),
                                        intern = TRUE
        ) %>%
            tibble::as_tibble() %>%
            tidyr::separate(value,
                            into = c("pid", "area", "ncell"),
                            sep = ",", convert = TRUE
            )

        # area unit ----
        if (area_unit == "ha") {
            patch_area_unit_rounded <- patch_area %>%
                dplyr::mutate(area = round(area / 1e4, area_round_digit))
        } else if (area_unit == "m2") {
            patch_area_unit_rounded <- patch_area %>%
                dplyr::mutate(area = round(area, area_round_digit))
        } else if (area_unit == "km2") {
            patch_area_unit_rounded <- patch_area %>%
                dplyr::mutate(area = round(area / 1e6, area_round_digit))
        } else {
            stop("Choose a valid area_unit: 'm2', 'km2', or 'ha'")
        }

        # assign area ----
        rgrass::execGRASS("g.message", message = "Assigning area")
        if (map_patch_area) {
            patch_area_unit_rounded %>%
                dplyr::mutate(pid2 = pid) %>%
                dplyr::select(pid, pid2, area) %>%
                readr::write_delim(paste0(input, output, "_patch_area.txt"), delim = ":", col_names = FALSE)

            rgrass::execGRASS("r.recode",
                              flags = "overwrite",
                              input = paste0(input, output, "_patch_id"),
                              output = paste0(input, output, "_patch_area"),
                              rules = paste0(input, output, "_patch_area.txt")
            )

            rgrass::execGRASS("r.colors",
                              flags = c("g", "quiet"),
                              map = paste0(input, output, "_patch_area"),
                              color = "ryg"
            )
        }

        # ncell ----
        if (map_patch_ncell) {
            patch_area_unit_rounded %>%
                dplyr::mutate(pid2 = pid) %>%
                dplyr::select(pid, pid2, ncell) %>%
                readr::write_delim(paste0(input, output, "_patch_ncell.txt"),
                                   delim = ":", col_names = FALSE)

            rgrass::execGRASS("r.recode",
                              flags = "overwrite",
                              input = paste0(input, output, "_patch_id"),
                              output = paste0(input, output, "_patch_ncell"),
                              rules = paste0(input, output, "_patch_ncell.txt")
            )

        }


        # patch area and patch number original ----
        if(map_patch_area_original || map_patch_number_original){

            rgrass::execGRASS(cmd = "r.stats",
                              flags = c("a", "c", "N", "overwrite", "quiet"),
                              separator = ",",
                              input = paste0(input, output, "_patch_fid,",
                                             input, output, "_patch_id"),
                              output = paste0(input, output, "_patch_area_number.txt"))

            # patch area ----
            if(map_patch_area_original) {

                patch_area_original <- readr::read_csv(paste0(input, output, "_patch_area_number.txt"),
                                                       show_col_types = FALSE, col_names = c("fid", "pid", "area", "ncell")) %>%
                    dplyr::mutate(area = ifelse(pid == "*", 0, area)) %>%
                    dplyr::group_by(fid) %>%
                    dplyr::summarise(area = sum(area))

                if (area_unit == "ha") {
                    patch_area_original_unit_rounded <- patch_area_original %>%
                        dplyr::mutate(area = round(area / 1e4, area_round_digit))
                } else if (area_unit == "m2") {
                    patch_area_original_unit_rounded <- patch_area_original %>%
                        dplyr::mutate(area = round(area, area_round_digit))
                } else if (area_unit == "km2") {
                    patch_area_original_unit_rounded <- patch_area_original %>%
                        dplyr::mutate(area = round(area / 1e6, area_round_digit))
                } else {
                    stop("Choose a valid area_unit: 'm2', 'km2', or 'ha'")
                }

                rgrass::execGRASS("g.message", message = "Assigning area")
                patch_area_original_unit_rounded %>%
                    dplyr::mutate(fid2 = fid) %>%
                    dplyr::select(fid, fid2, area) %>%
                    readr::write_delim(paste0(input, output, "_patch_area_original.txt"), delim = ":", col_names = FALSE)

                rgrass::execGRASS("r.recode",
                                  flags = "overwrite",
                                  input = paste0(input, output, "_patch_fid"),
                                  output = paste0(input, output, "_patch_area_original"),
                                  rules = paste0(input, output, "_patch_area_original.txt")
                )

                rgrass::execGRASS("r.colors",
                                  flags = c("g", "quiet"),
                                  map = paste0(input, output, "_patch_area_original"),
                                  color = "ryg"
                )

                unlink(paste0(input, output, "_patch_area_original.txt"))
            }

            # number patch original ----
            if(map_patch_number_original) {

                readr::read_csv(paste0(input, output, "_patch_area_number.txt"),
                                show_col_types = FALSE, col_names = c("fid", "pid", "area", "ncell")) %>%
                    dplyr::mutate(pid = as.numeric(ifelse(pid == "*", 0, pid)),
                                  pid = ifelse(pid > 0, 1, 0)) %>%
                    dplyr::group_by(fid) %>%
                    dplyr::summarise(npid = sum(pid)) %>%
                    dplyr::mutate(fid2 = fid) %>%
                    dplyr::select(fid, fid2, npid) %>%
                    readr::write_delim(paste0(input, output, "_patch_number_original.txt"), delim = ":", col_names = FALSE)

                rgrass::execGRASS(cmd = "r.recode",
                                  flags = c("overwrite", "quiet"),
                                  input = paste0(input, output, "_patch_fid"),
                                  output = paste0(input, output, "_patch_number_original"),
                                  rules = paste0(input, output, "_patch_number_original.txt"))

                unlink(paste0(input, output, "_patch_number_original.txt"))

            }

            unlink(paste0(input, output, "_patch_area_number.txt"))

        }

        # clean
        rgrass::execGRASS(cmd = "g.message", message = "Cleaning data")

        unlink(paste0(input, output, "_patch_area.txt"))
        unlink(paste0(input, output, "_patch_ncell.txt"))

        if(!map_patch_id){

            suppressWarnings(
                rgrass::execGRASS(cmd = "g.remove",
                                  flags = c("b", "f", "quiet"),
                                  type = "raster",
                                  name = paste0(input, output, "_patch_id"))
            )
        }

        suppressWarnings(
            rgrass::execGRASS(cmd = "g.remove",
                              flags = c("b", "f", "quiet"),
                              type = "raster",
                              name = c(
                                  paste0(input, output, "_patch_binary"),
                                  paste0(input, output, "_patch_null"),
                                  paste0(input, output, "_patch_fid"),
                                  paste0(input, output, "_patch_binary_aux_fill_hole"),
                                  paste0(input, output, "_patch_binary_contr"),
                                  paste0(input, output, "_patch_binary_contr_dila"),
                                  paste0(input, output, "_patch_binary_contr_dila_null"),
                                  paste0(input, output, "_patch_binary_contr_dila_patch")))
        )

    }

}
