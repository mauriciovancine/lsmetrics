#' Calculate functional connectivity
#'
#' Identifies functional fragments connected and calculate area in hectare.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param output `[character=""]` \cr Habitat area map name output GRASS Data Base
#' @param zero_as_null `[logical(1)=FALSE]` \cr If `TRUE`, the function treats non-habitat cells as null; if `FALSE`, the function converts non-habitat zero cells to null cells.
#' @param gap_crossing_value `[numeric]` \cr Integer indicating gap crossing distance.
#' @param dilation_type `[character=""]` \cr If
#' @param map_id `[logical(1)=FALSE]` \cr If `TRUE`
#' @param map_ncell `[logical(1)=FALSE]` \cr If `TRUE`

#' @param map_dilation `[logical(1)=FALSE]` \cr If `TRUE`
#' @param nprocs `[numeric()]` \cr
#' @param memory `[numeric()]` \cr
#'
#' @example examples/lsm_connectivity_functional_example.R
#'
#' @name lsm_connectivity_functional
#' @export
lsm_connectivity_functional <- function(input,
                                        output = NULL,
                                        zero_as_null = FALSE,
                                        gap_crossing_value,
                                        dilation_type = "minimum",
                                        id_direction = 8,
                                        area_round_digit = 0,
                                        area_unit = "ha",
                                        map_func_connec = TRUE,
                                        map_func_connec_id = FALSE,
                                        map_func_connec_area = FALSE,
                                        map_func_connec_ncell = FALSE,
                                        map_func_connec_dilation = FALSE,
                                        nprocs = 1,
                                        memory = 300){

    # region ----
    rgrass::execGRASS("g.region", flags = "a", raster = input)

    # proj units ----
    proj_info <- rgrass::execGRASS("g.proj", flags = "g", intern = TRUE)
    proj_unit <- tolower(sub("units=", "", proj_info[grepl("^units=", proj_info)]))

    # gap crossing ----
    if(proj_unit == "meters"){

        res <- rgrass::stringexecGRASS("g.region -p", intern = TRUE) %>%
            stringr::str_subset("nsres") %>%
            stringr::str_extract("\\d+") %>%
            as.numeric()

        if(gap_crossing_value/res >= 1){
            window <- 2 * round(gap_crossing_value/res, 0) + 1
        }else{
            stop("Gap crossing is smaller than map resolution. Choose a higher value for the gap crossing.")
        }

    } else if (proj_unit == "degrees") {

        res <- rgrass::stringexecGRASS("g.region -p", intern = TRUE) %>%
            stringr::str_subset("nsres") %>%
            stringr::str_extract_all("\\d+") %>%
            unlist() %>%
            as.numeric() %>%
            {\(x) (x[1] + x[2]/60 + as.numeric(paste0(x[3], ".", x[4]))/3600) * 111320}()

        if(gap_crossing_value/res >= 1){
            window <- 2 * round(gap_crossing_value/res, 0) + 1
        }else{
            stop("Gap crossing is smaller than map resolution. Choose a higher value for the gap crossing.")
        }

    } else {
        warning(paste("Units:", proj_unit, "not currently supported"))
    }

    # gap crossing name
    gap_crossing_name <- gap_crossing_value * 2

    # binary ----
    if(zero_as_null){

        rgrass::execGRASS(cmd = "g.message", message = "Converting null as zero")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_func_connec_binary = if(isnull(", input, "), 0, 1)"))
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_func_connec_null = ",  input))

    } else{

        rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_func_connec_binary = ", input))
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_func_connec_null = if(", input, " == 1, 1, null())"))
    }

    # dilation ----
    if (!dilation_type %in% c("minimum", "maximum")) stop("`dilation_type` must be minimum or maximum.")
    dilation_flags <- c("overwrite", "quiet")
    if (dilation_type == "minimum") dilation_flags <- c("c", dilation_flags)
    rgrass::execGRASS(cmd = "g.message", message = "Dilation of fragments")
    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = dilation_flags,
                      input = paste0(input, output, "_func_connec_binary"),
                      selection = paste0(input, output, "_func_connec_binary"),
                      output = paste0(input, output, "_func_connec_dilation", gap_crossing_name),
                      method = "max",
                      size = window,
                      nprocs = nprocs,
                      memory = memory)

    # dilation null ----
    rgrass::execGRASS(cmd = "g.message", message = "Converting dilation as null")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_func_connec_dilation", gap_crossing_name, "_null = if(", input, output, "_func_connec_dilation", gap_crossing_name, " == 1, 1, null())"))

    # dilation clump ----
    rgrass::execGRASS(cmd = "g.message", message = "Identifying the fragments for gap crossing")
    if (!id_direction %in% c(4, 8)) stop("Clump `id_direction` must be 4 or 8.")
    clump_flags <- c("quiet", "overwrite")
    if (id_direction == 8) clump_flags <- c("d", clump_flags)
    rgrass::execGRASS(cmd = "r.clump",
                      flags = clump_flags,
                      input = paste0(input, output, "_func_connec_dilation", gap_crossing_name, "_null"),
                      output = paste0(input, output, "_func_connec_dilation", gap_crossing_name, "_id"))

    # clump original ----
    rgrass::execGRASS(cmd = "g.message", message = "Multipling clump by original habitat")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_func_connec", gap_crossing_name, "_id = int(", input, output, "_func_connec_dilation", gap_crossing_name, "_id * ", input, output, "_func_connec_null)"))

    # functional connected area ----
    rgrass::execGRASS(cmd = "g.message", message = "Calculating the functional connected area")

    func_connec_area <- rgrass::execGRASS("r.stats",
                                          flags = c("a", "c", "n", "overwrite", "quiet"),
                                          separator = ",",
                                          input = paste0(input, output, "_func_connec", gap_crossing_name, "_id"),
                                          intern = TRUE
    ) %>%
        tibble::as_tibble() %>%
        tidyr::separate(value,
                        into = c("id", "area", "ncell"),
                        sep = ",", convert = TRUE
        )

    ## area unit ----
    if (area_unit == "ha") {
        func_connec_area_unit_rounded <- func_connec_area %>%
            dplyr::mutate(area = round(area / 1e4, area_round_digit))
    } else if (area_unit == "m2") {
        func_connec_area_unit_rounded <- func_connec_area %>%
            dplyr::mutate(area = round(area, area_round_digit))
    } else if (area_unit == "km2") {
        func_connec_area_unit_rounded <- func_connec_area %>%
            dplyr::mutate(area = round(area / 1e6, area_round_digit))
    } else {
        stop("Choose a valid area_unit: 'm2', 'km2', or 'ha'")
    }

    ## assign area ----
    rgrass::execGRASS("g.message", message = "Assigning functional connected area")
    if (map_func_connec_area) {
        func_connec_area_unit_rounded %>%
            dplyr::mutate(id2 = id) %>%
            dplyr::select(id, id2, area) %>%
            readr::write_delim("connec_func_area.txt", delim = ":", col_names = FALSE)

        rgrass::execGRASS("r.recode",
                          flags = "overwrite",
                          input = paste0(input, output, "_func_connec", gap_crossing_name, "_id"),
                          output = paste0(input, output, "_func_connec", gap_crossing_name, "_area"),
                          rules = "connec_func_area.txt"
        )

        rgrass::execGRASS("r.colors",
                          flags = c("g", "quiet"),
                          map = paste0(input, output, "_func_connec", gap_crossing_name, "_area"),
                          color = "ryg"
        )
    }

    # functional connected ncell ----
    if (map_func_connec_ncell) {
        func_connec_area_unit_rounded %>%
            dplyr::mutate(id2 = id) %>%
            dplyr::select(id, id2, ncell) %>%
            readr::write_delim("fragment_ncell.txt", delim = ":", col_names = FALSE)

        rgrass::execGRASS("r.recode",
                          flags = "overwrite",
                          input = paste0(input, output, "_func_connec", gap_crossing_name, "_id"),
                          output = paste0(input, output, "_func_connec", gap_crossing_name, "_ncell"),
                          rules = "fragment_ncell.txt"
        )

        unlink("fragment_ncell.txt")
    }

    # functional connected dilation ----
    if(map_func_connec_dilation){

        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_func_connec", gap_crossing_name, "_dilation = ", input, output, "_func_connec_dilation", gap_crossing_name, "_null"))

        tibble::tibble(values = 0:1,
                       colors = c("white", "#cacaca")) %>%
            readr::write_delim("table_color_func_connec_dilation.txt",
                               delim = " ",
                               col_names = FALSE)
        rgrass::execGRASS(cmd = "r.colors",
                          flags = c("g", "quiet"),
                          map = paste0(input, output, "_func_connec", gap_crossing_name, "_dilation"),
                          rules = "table_color_func_connec_dilation.txt")

        unlink("table_color_func_connec_dilation.txt")
    }

    # functional connected id ----
    if(!map_func_connec_id){

        rgrass::execGRASS(cmd = "g.remove",
                          flags = c("f", "quiet"),
                          type = "raster",
                          name = paste0(input, output, "_func_connec", gap_crossing_name, "_id"))
    }

    # functional connectivity ----
    if(map_func_connec){
        lsmetrics::lsm_area_fragment(input = input,
                                     output = "_func_connec",
                                     zero_as_null = zero_as_null)

        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_func_connec", gap_crossing_name, " = ", input, output, "_func_connec", gap_crossing_name, "_area - ", input, "_func_connec_fragment_area"))

        rgrass::execGRASS(cmd = "r.colors",
                          flags = c("g", "quiet"),
                          map = paste0(input, output, "_func_connec", gap_crossing_name),
                          color = "ryg")

    }

    # clean ----
    rgrass::execGRASS("g.message", message = "Cleaning data")

    unlink("connec_func_area.txt")

    suppressWarnings(
        rgrass::execGRASS(cmd = "g.remove",
                          flags = c("f", "quiet"),
                          type = "raster",
                          name = c(paste0(input, output, "_func_connec_binary"),
                                   paste0(input, output, "_func_connec_null"),
                                   paste0(input, output, "_func_connec_fragment_area"),
                                   paste0(input, output, "_func_connec_dilation", gap_crossing_name),
                                   paste0(input, output, "_func_connec_dilation", gap_crossing_name, "_id"),
                                   paste0(input, output, "_func_connec_dilation", gap_crossing_name, "_null")))
        )
}
