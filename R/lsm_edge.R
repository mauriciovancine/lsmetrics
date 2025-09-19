#' Identify edge of landscape elements
#'
#' Identify edge of landscape elements for each pixel. Calculate area, original area and percentage.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param output `[character=""]` \cr Map name output inside GRASS Data Base.
#' @param zero_as_null `[logical(1)=FALSE]` \cr If `TRUE`, the function treats non-habitat cells as null; if `FALSE`, the function converts non-habitat zero cells to null cells.
#' @param edge_depth `[numeric]` \cr Integer indicating edge distance in meters considered adjacent to form a patch.
#' @param edge_contraction `[character=""]` \cr
#' @param area_round_digits `[logical(1)=FALSE]` \cr If `TRUE`
#' @param area_unit `[logical(1)=FALSE]` \cr If `TRUE`
#' @param map_edge_id `[logical(1)=FALSE]` \cr
#' @param map_edge_ncell `[logical(1)=FALSE]` \cr
#' @param map_edge_area `[logical(1)=FALSE]` \cr
#' @param map_edge_id_original `[logical(1)=FALSE]` \cr
#' @param map_edge_ncell_original `[logical(1)=FALSE]` \cr
#' @param map_edge_area_original `[logical(1)=FALSE]` \cr
#' @param map_edge_percentage `[logical(1)=FALSE]` \cr
#' @param buffer_radius `[numeric]` \cr
#' @param buffer_circular `[logical(1)=FALSE]` \cr
#' @param nprocs `[numeric()]` \cr
#' @param memory `[numeric()]` \cr
#'
#' @example examples/lsm_edge_example.R
#'
#' @name lsm_edge
#' @export
lsm_edge <- function(input,
                     output = NULL,
                     zero_as_null = FALSE,
                     region_input = FALSE,
                     edge_depth,
                     edge_contraction = "maximum",
                     area_round_digits = 0,
                     area_unit = "ha",
                     map_edge_id = FALSE,
                     map_edge_ncell = FALSE,
                     map_edge_area = FALSE,
                     map_edge_id_original = FALSE,
                     map_edge_ncell_original = FALSE,
                     map_edge_area_original = FALSE,
                     nprocs = 1,
                     memory = 300){

    # region ----
    if(region_input){
        rgrass::execGRASS("g.region", flags = "a", raster = input)
    }

    # edge depth ----
    proj_info <- rgrass::execGRASS("g.proj", flags = c("g", "quiet"), intern = TRUE)
    proj_unit <- tolower(sub("units=", "", proj_info[grepl("^units=", proj_info)]))

    if(proj_unit == "meters"){

        res <- rgrass::stringexecGRASS("g.region -p --quiet", intern = TRUE) %>%
            stringr::str_subset("nsres") %>%
            stringr::str_extract("\\d+") %>%
            as.numeric()

        if(edge_depth/res >= 1){
            window <- 2 * round(edge_depth/res, 0) + 1
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

        if(edge_depth/res >= 1){
            window <- 2 * round(edge_depth/res, 0) + 1
        }else{
            stop("Gap crossing is smaller than map resolution. Choose a higher value for the gap crossing.")
        }

    } else {
        warning(paste("Units:", proj_unit, "not currently supported!"))
    }

    # binary ----
    if(zero_as_null){

        rgrass::execGRASS(cmd = "g.message", message = "Converting null and zero")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_edge_binary = if(isnull(",
                                              input, "), 0, 1)"))
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_edge_null = ", input))

    } else{

        rgrass::execGRASS(cmd = "g.message", message = "Converting zero and null")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_edge_binary = ", input))
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_edge_null = if(",
                                              input, " == 1, 1, null())"))
    }

    # id ----
    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "quiet", "overwrite"),
                      input = paste0(input, output, "_edge_null"),
                      output = paste0(input, output, "_edge_id"))

    # edge ----
    rgrass::execGRASS(cmd = "g.message", message = "Calculating edge")

    # core
    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = c("overwrite", "quiet"),
                      input = paste0(input, output, "_edge_binary"),
                      selection = paste0(input, output, "_edge_binary"),
                      output = paste0(input, output, "_edge_core", edge_depth),
                      method = "min",
                      size = window,
                      nprocs = nprocs,
                      memory = memory)

    # edge
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_edge", edge_depth, "=",
                                          input, output, "_edge_binary - ",
                                          input, output, "_edge_core", edge_depth))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_edge", edge_depth, "_null = if(",
                                          input, output, "_edge", edge_depth, " == 1, 1, null())"))

    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_edge", edge_depth),
                      color = "oranges")

    # edge area ----
    if(map_edge_id || map_edge_ncell || map_edge_area){

        rgrass::execGRASS(cmd = "g.message", message = "Calculating edge area")

        lsmetrics::lsm_area_fragment(input = paste0(input, output, "_edge", edge_depth),
                                     area_round_digit = area_round_digit,
                                     area_unit = area_unit,
                                     map_fragment_id = map_edge_id,
                                     map_fragment_ncell = map_edge_ncell)

        rgrass::execGRASS(cmd = "g.rename",
                          flags = "quiet",
                          raster = paste0(input, output, "_edge", edge_depth, "_fragment_area,",
                                          input, output, "_edge", edge_depth, "_area"))

        if(map_edge_id){

            rgrass::execGRASS(cmd = "g.rename",
                              flags = "quiet",
                              raster = paste0(input, output, "_edge", edge_depth, "_fragment_id,",
                                              input, output, "_edge", edge_depth, "_id"))
        }

        if(map_edge_ncell){

            rgrass::execGRASS(cmd = "g.rename",
                              flags = "quiet",
                              raster = paste0(input, output, "_edge", edge_depth, "_fragment_ncell,",
                                              input, output, "_edge", edge_depth, "_ncell"))
        }

        # original ----
        if(map_edge_id_original || map_edge_ncell_original || map_edge_area_original){

            rgrass::execGRASS(cmd = "g.message", message = "Calculating edge area original")

            if(map_edge_id_original){

                rgrass::execGRASS(cmd = "r.stats.zonal",
                                  flags = c("overwrite", "quiet"),
                                  base = paste0(input, output, "_edge_id"),
                                  cover = paste0(input, output, "_edge", edge_depth, "_id"),
                                  method = "average",
                                  output = paste0(input, output, "_edge", edge_depth, "_id_original"))


            }

            rgrass::execGRASS(cmd = "r.stats.zonal",
                              flags = c("overwrite", "quiet"),
                              base = paste0(input, output, "_edge_id"),
                              cover = paste0(input, output, "_edge", edge_depth, "_area"),
                              method = "average",
                              output = paste0(input, output, "_edge", edge_depth, "_area_original"))

            rgrass::execGRASS(cmd = "r.colors",
                              flags = c("g", "quiet"),
                              map = paste0(input, output, "_edge", edge_depth, "_area_original"),
                              color = "ryg")

            # clean
            rgrass::execGRASS(cmd = "g.message", message = "Cleaning data")

            suppressWarnings(
                rgrass::execGRASS(cmd = "g.remove",
                                  flags = c("b", "f", "quiet"),
                                  type = "raster",
                                  name = c(
                                      paste0(input, output, "_edge_binary"),
                                      paste0(input, output, "_edge_null"),
                                      paste0(input, output, "_edge_id")))
            )

        }
    }

}
