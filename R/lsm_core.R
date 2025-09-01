#' Identify the core of landscape elements
#'
#' Identify the core of landscape elements for each pixel. Calculate area, original area and percentage.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param output `[character=""]` \cr Map name output inside GRASS Data Base.
#' @param zero_as_null `[logical(1)=FALSE]` \cr If `TRUE`, the function treats non-habitat cells as null; if `FALSE`, the function converts non-habitat zero cells to null cells.
#' @param edge_depth `[numeric]` \cr Integer indicating edge distance in meters considered adjacent to form a patch.
#' @param edge_contraction `[character=""]` \cr
#' @param area_round_digits `[logical(1)=FALSE]` \cr If `TRUE`
#' @param area_unit `[logical(1)=FALSE]` \cr If `TRUE`
#' @param map_core_id `[logical(1)=FALSE]` \cr
#' @param map_core_ncell `[logical(1)=FALSE]` \cr
#' @param map_core_area `[logical(1)=FALSE]` \cr
#' @param map_core_area_index `[logical(1)=FALSE]` \cr
#' @param map_core_id_original `[logical(1)=FALSE]` \cr
#' @param map_core_ncell_original `[logical(1)=FALSE]` \cr
#' @param map_core_area_original `[logical(1)=FALSE]` \cr
#' @param map_core_percentage `[logical(1)=FALSE]` \cr
#' @param buffer_radius `[numeric]` \cr
#' @param buffer_circular `[logical(1)=FALSE]` \cr
#' @param nprocs `[numeric()]` \cr
#' @param memory `[numeric()]` \cr
#'
#' @example examples/lsm_core_example.R
#'
#' @name lsm_core
#' @export
lsm_core <- function(input,
                          output = NULL,
                          zero_as_null = FALSE,
                          edge_depth,
                          edge_contraction = "maximum",
                          area_round_digits = 0,
                          area_unit = "ha",
                          map_core_id = FALSE,
                          map_core_ncell = FALSE,
                          map_core_area = FALSE,
                          map_core_number = FALSE,
                          map_core_area_index = FALSE,
                          map_core_id_original = FALSE,
                          map_core_ncell_original = FALSE,
                          map_core_area_original = FALSE,
                          map_core_percentage = FALSE,
                          buffer_radius = NULL,
                          buffer_circular = FALSE,
                          nprocs = 1,
                          memory = 300){

    # region ----
    if(region_input){
        rgrass::execGRASS("g.region", flags = "a", raster = input)
    }

    # edge depth ----
    proj_info <- rgrass::execGRASS("g.proj", flags = "g", intern = TRUE)
    proj_unit <- tolower(sub("units=", "", proj_info[grepl("^units=", proj_info)]))

    if(proj_unit == "meters"){

        res <- rgrass::stringexecGRASS("g.region -p", intern = TRUE) %>%
            stringr::str_subset("nsres") %>%
            stringr::str_extract("\\d+") %>%
            as.numeric()

        if(edge_depth/res >= 1){
            window <- 2 * round(edge_depth/res, 0) + 1
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

        if(edge_depth/res >= 1){
            window <- 2 * round(edge_depth/res, 0) + 1
        }else{
            stop("Gap crossing is smaller than map resolution. Choose a higher value for the gap crossing.")
        }

    } else {
        warning(paste("Units:", proj_unit, "not currently supported!"))
    }

    # binary
    if(zero_as_null == TRUE){

        rgrass::execGRASS(cmd = "g.message", message = "Converting null and zero")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_core_binary = if(isnull(", input, "), 0, 1)"))
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_core_null = ", input))

    } else{

        rgrass::execGRASS(cmd = "g.message", message = "Converting null and zero")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_core_binary = ", input))
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_core_null = if(", input, " == 1, 1, null())"))
    }

    # id ----
    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "quiet", "overwrite"),
                      input = paste0(input, output, "_core_null"),
                      output = paste0(input, output, "_core_id"))

    # core ----
    if(core_edge_type == "both" | core_edge_type == "core"){

        # core
        rgrass::execGRASS(cmd = "g.message", message = "Calculating core")
        rgrass::execGRASS(cmd = "r.neighbors",
                          flags = c("overwrite", "quiet"),
                          input = paste0(input, output, "_core_binary"),
                          selection = paste0(input, output, "_core_binary"),
                          output = paste0(input, output, "_core", edge_depth),
                          method = "min",
                          size = window,
                          nprocs = nprocs,
                          memory = memory)

        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_core", edge_depth, "_null = if(", input, output, "_core", edge_depth, " == 1, 1, null())"))

        rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_core", edge_depth), color = "blues")

        # core area
        if(calculate_area == TRUE){

            rgrass::execGRASS(cmd = "g.message", message = "Calculating core area")
            lsmetrics::lsm_area_fragment(input = paste0(input, output, "_core", edge_depth),
                                         map_fragment_id = map_id | map_core_number,
                                         map_fragment_ncell = map_ncell,
                                         area_round_digit = 0,
                                         area_unit = "ha",)

            rgrass::execGRASS(cmd = "g.rename",
                              flags = "quiet",
                              raster = paste0(input, output, "_core", edge_depth, "_fragment_area,", input, output, "_core", edge_depth, "_area"))

            if(map_id | map_core_number == TRUE){
                rgrass::execGRASS(cmd = "g.rename",
                                  flags = "quiet",
                                  raster = paste0(input, output, "_core", edge_depth, "_fragment_id,",
                                                  input, output, "_core", edge_depth, "_id"))
                rgrass::execGRASS(cmd = "r.colors",
                                  flags = "quiet",
                                  map = paste0(input, output, "_core", edge_depth, "_id"),
                                  color = "random")
            }else{
                rgrass::execGRASS(cmd = "g.remove",
                                  flags = c("b", "f", "quiet"),
                                  type = "raster",
                                  name = paste0(input, output, "_core", edge_depth, "_id"))
            }

            if(map_ncell == TRUE){
                rgrass::execGRASS(cmd = "g.rename",
                                  flags = "quiet",
                                  raster = paste0(input, output, "_core", edge_depth, "_fragment_ncell,",
                                                  input, output, "_core", edge_depth, "_area_ncell"))
            }

            if(core_edge_original == TRUE){

                rgrass::execGRASS(cmd = "r.stats.zonal",
                                  flags = c("overwrite", "quiet"),
                                  base = paste0(input, output, "_core_id"),
                                  cover = paste0(input, output, "_core", edge_depth, "_null"),
                                  method = "count",
                                  output = paste0(input, output, "_core", edge_depth, "_ncell_original"))

                area_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))^2/1e4

                rgrass::execGRASS(cmd = "r.mapcalc",
                                  flags = c("overwrite", "quiet"),
                                  expression = paste0(input, output, "_core", edge_depth, "_area_ha_original=", input, output, "_core", edge_depth, "_area_ncell_original * ", area_pixel))
                rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_core", edge_depth, "_area_ha_original"), color = "ryg")

                if(area_integer == TRUE){
                    rgrass::execGRASS(cmd = "r.mapcalc",
                                      flags = c("overwrite", "quiet"),
                                      expression = paste0(input, output, "_core", edge_depth, "_area_ha_original = round(", input, output, "_core", edge_depth, "_area_ha_original)"))
                    rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_core", edge_depth, "_area_ha_original"), color = "ryg")
                }

                # ncell
                if(ncell == TRUE){

                    rgrass::execGRASS(cmd = "r.colors",
                                      flags = c("g", "quiet"),
                                      map = paste0(input, output, "_core", edge_depth, "_area_ncell_original"),
                                      color = "ryg")

                }else{

                    rgrass::execGRASS(cmd = "g.remove",
                                      flags = c("b", "f", "quiet"),
                                      type = "raster",
                                      name = paste0(input, output, "_core", edge_depth, "_ncell_original"))

                }
            }
        }

        # core percentage
        if(calculate_percentage == TRUE){

            rgrass::execGRASS(cmd = "g.message", message = "Calculating core percentage")

            lsmetrics::lsm_percentage(input = paste0(input, output, "_core", edge_depth),
                                      buffer_radius = buffer_radius,
                                      buffer_circular = buffer_circular)

            rgrass::execGRASS(cmd = "r.colors",
                              flags = "quiet",
                              map = paste0(input, output, "_core", edge_depth, "_pct_buf", buffer_radius),
                              color = "blues")

        }

        # core number
        if(core_number == TRUE){

            rgrass::execGRASS(cmd = "r.stats",
                              flags = c("N", "overwrite"),
                              separator = ",",
                              input = paste0(input, output, "_core_id,", input, output, "_core", edge_depth, "_id"),
                              output = paste0(input, output, "_core_id.txt"))

            readr::write_delim(dplyr::mutate(dplyr::count(readr::read_csv(paste0(input, output, "_core_id.txt"), show_col_types = FALSE, col_names = c("id", "n_core_ids")), id), n = n - 1),
                               paste0(input, output, "_core_id.txt"), delim = "=", col_names = FALSE)

            rgrass::execGRASS(cmd = "r.reclass",
                              flags = c("overwrite", "quiet"),
                              input = paste0(input, output, "_core_id"),
                              output = paste0(input, output, "_core", edge_depth, "_core_number_original_temp"),
                              rules = paste0(input, output, "_core_id.txt"))

            rgrass::execGRASS(cmd = "r.mapcalc",
                              flags = c("overwrite", "quiet"),
                              expression = paste0(input, output, "_core", edge_depth, "_core_number_original = ", input, output, "_core", edge_depth, "_core_number_original_temp"))

            unlink(paste0(input, output, "_core_id.txt"))

            rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_core", edge_depth, "_core_number_original_temp"))

        }

        if(map_core_area_index == TRUE){

        }

        # clean
        rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_core", edge_depth, "_null"))

    }

    # clean ----
    rgrass::execGRASS(cmd = "g.message", message = "Cleaning data")

    suppressWarnings(
        rgrass::execGRASS(cmd = "g.remove",
                          flags = c("b", "f", "quiet"),
                          type = "raster",
                          name = c(
                              paste0(input, output, "_core_binary"),
                              paste0(input, output, "_core_null"),
                              paste0(input, output, "_core_id")))
    )

}
