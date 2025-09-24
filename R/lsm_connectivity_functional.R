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
                                        region_input = FALSE,
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
    if(region_input){
        rgrass::execGRASS("g.region", flags = "a", raster = input)
    }

    # proj units ----
    proj_info <- rgrass::execGRASS("g.proj", flags = c("g", "quiet"), intern = TRUE)
    proj_unit <- tolower(sub("units=", "", proj_info[grepl("^units=", proj_info)]))

    # gap crossing ----
    if(proj_unit == "meters"){

        res <- rgrass::stringexecGRASS("g.region -p --quiet", intern = TRUE) %>%
            stringr::str_subset("nsres") %>%
            stringr::str_extract("\\d+") %>%
            as.numeric()

        if(gap_crossing_value/res >= 1){
            window <- 2 * round(gap_crossing_value/res, 0) + 1
        }else{
            stop("Gap crossing is smaller than map resolution. Choose a higher value for the gap crossing.")
        }

    } else if (proj_unit == "degrees") {

        res <- rgrass::stringexecGRASS("g.region -p --quiet", intern = TRUE) %>%
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
                          expression = paste0(input, output, "_func_connec", gap_crossing_name, "_binary = if(isnull(", input, "), 0, 1)"))
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_func_connec", gap_crossing_name, "_null = ",  input))

    } else{

        rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_func_connec", gap_crossing_name, "_binary = ", input))
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_func_connec", gap_crossing_name, "_null = if(", input, " == 1, 1, null())"))
    }

    # dilation ----
    if (!dilation_type %in% c("minimum", "maximum")) stop("`dilation_type` must be minimum or maximum.")
    dilation_flags <- c("overwrite", "quiet")
    if (dilation_type == "minimum") dilation_flags <- c("c", dilation_flags)
    rgrass::execGRASS(cmd = "g.message", message = "Dilation of fragments")
    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = dilation_flags,
                      input = paste0(input, output, "_func_connec", gap_crossing_name, "_binary"),
                      selection = paste0(input, output, "_func_connec", gap_crossing_name, "_binary"),
                      output = paste0(input, output, "_func_connec_dilation", gap_crossing_name),
                      method = "max",
                      size = window,
                      nprocs = nprocs,
                      memory = memory)

    # dilation null ----
    rgrass::execGRASS(cmd = "g.message", message = "Converting dilation as null")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_func_connec_dilation", gap_crossing_name, "_null = if(",
                                          input, output, "_func_connec_dilation", gap_crossing_name, " == 1, 1, null())"))

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
                      expression = paste0(input, output, "_func_connec", gap_crossing_name, "_id = int(",
                                          input, output, "_func_connec_dilation", gap_crossing_name, "_id * ",
                                          input, output, "_func_connec", gap_crossing_name, "_null)"))

    # functional connected area ----
    rgrass::execGRASS(cmd = "g.message", message = "Calculating the functional connected area")

    lsmetrics::lsm_aux_area(input_null = paste0(input, output, "_func_connec", gap_crossing_name, "_null"),
                            input_id = paste0(input, output, "_func_connec", gap_crossing_name, "_id"),
                            area_round_digit = area_round_digit,
                            area_unit = area_unit,
                            map_ncell = map_func_connec_ncell,
                            table_export = FALSE)

    rgrass::execGRASS("g.rename", raster = paste0(input, output, "_func_connec", gap_crossing_name, "_null_area,",
                                                  input, output, "_func_connec", gap_crossing_name, "_area"))

    # functional connected dilation ----
    if(map_func_connec_dilation){

        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_func_connec", gap_crossing_name, "_dilation = ",
                                              input, output, "_func_connec_dilation", gap_crossing_name, "_null"))

        tibble::tibble(values = 0:1,
                       colors = c("white", "#cacaca")) %>%
            readr::write_delim(paste0(input, output, "_color_func_connec_dilation.txt"),
                               delim = " ",
                               col_names = FALSE)
        rgrass::execGRASS(cmd = "r.colors",
                          flags = c("g", "quiet"),
                          map = paste0(input, output, "_func_connec", gap_crossing_name, "_dilation"),
                          rules = paste0(input, output, "_color_func_connec_dilation.txt"))

        unlink(paste0(input, output, "_color_func_connec_dilation.txt"))
    }

    # functional connected id ----
    if(!map_func_connec_id){

        rgrass::execGRASS(cmd = "g.remove",
                          flags = c("f", "quiet"),
                          type = "raster",
                          name = paste0(input, output, "_func_connec", gap_crossing_name, "_id"))
    }

    # functional connectivity ----
    lsmetrics::lsm_area_fragment(input = input,
                                 output = paste0("_func_connec", gap_crossing_name),
                                 zero_as_null = zero_as_null,
                                 region_input = region_input,
                                 area_round_digit = area_round_digit,
                                 area_unit = area_unit,
                                 id_direction = id_direction)

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_func_connec", gap_crossing_name, " = ",
                                          input, output, "_func_connec", gap_crossing_name, "_area - ",
                                          input, "_func_connec", gap_crossing_name, "_fragment_area"))

    rgrass::execGRASS(cmd = "r.colors",
                      flags = c("g", "quiet"),
                      map = paste0(input, output, "_func_connec", gap_crossing_name),
                      color = "ryg")


    # clean ----
    rgrass::execGRASS("g.message", message = "Cleaning data")

    unlink(paste0(input, output, "_connec_func_area.txt"))

    suppressWarnings(
        rgrass::execGRASS(cmd = "g.remove",
                          flags = c("f", "quiet"),
                          type = "raster",
                          name = c(paste0(input, output, "_func_connec", gap_crossing_name, "_binary"),
                                   paste0(input, output, "_func_connec", gap_crossing_name, "_null"),
                                   paste0(input, output, "_func_connec", gap_crossing_name, "_fragment_area"),
                                   paste0(input, output, "_func_connec_dilation", gap_crossing_name),
                                   paste0(input, output, "_func_connec_dilation", gap_crossing_name, "_id"),
                                   paste0(input, output, "_func_connec_dilation", gap_crossing_name, "_null")))
    )

}
