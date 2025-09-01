#' Calculate perimeter
#'
#' Calculate perimeter in meters and shape index.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification (e.g. values 1,0 or 1,NA for habitat,non-habitat) inside GRASS Data Base.
#' @param output `[character=""]` \cr Map name output inside GRASS Data Base.
#' @param zero_as_null `[logical(1)=FALSE]` \cr If `TRUE`, the function treats non-habitat cells as null; if `FALSE`, the function converts non-habitat zero cells to null cells.
#' @param map_perimeter_area_ratio_index `[character=""]` \cr If `TRUE`, the function treats.
#' @param map_shape_index `[character=""]` \cr If `TRUE`, the function treats.
#' @param map_fractal_index `[character=""]` \cr If `TRUE`, the function treats.#' @param nprocs `[numeric()]` \cr
#' @param nprocs `[numeric()]` \cr
#' @param memory `[numeric()]` \cr
#'
#' @example examples/lsm_perimeter_example.R
#'
#' @name lsm_perimeter
#' @export
lsm_perimeter <- function(input,
                          output = NULL,
                          zero_as_null = FALSE,
                          perimeter_round_digit = 0,
                          map_perimeter_area_ratio_index = FALSE,
                          map_shape_index = FALSE,
                          map_fractal_index = FALSE,
                          nprocs = 1,
                          memory = 300){

    # region ----
    if(region_input){
        rgrass::execGRASS("g.region", flags = "a", raster = input)
    }

    # proj units ----
    proj_info <- rgrass::execGRASS("g.proj", flags = "g", intern = TRUE)
    proj_unit <- tolower(sub("units=", "", proj_info[grepl("^units=", proj_info)]))

    if(proj_unit %in% c("meters", "degrees")){

        # region ----
        rgrass::execGRASS("g.region", flags = "a", raster = input)

        region <- rgrass::execGRASS("g.region", flags = "g", intern = TRUE) %>%
            stringr::str_split("=", simplify = TRUE) %>%
            as.data.frame() %>%
            setNames(c("var", "value")) %>%
            dplyr::mutate(value = as.numeric(value)) %>%
            tidyr::pivot_wider(names_from = var, values_from = value)

        res <- region$nsres  # ou ewres
        rgrass::execGRASS("g.region",
                          flags = "a",
                          n = as.character(region$n + res),
                          s = as.character(region$s - res),
                          e = as.character(region$e + res),
                          w = as.character(region$w - res))

        # binary ----
        if(zero_as_null == TRUE){

            rgrass::execGRASS(cmd = "g.message", message = "Converting null as zero")
            rgrass::execGRASS(cmd = "r.mapcalc", flags= c("overwrite", "quiet"),
                              expression = paste0(input, output, "_perimeter_binary = if(isnull(", input, "), 0, 1)"))
            rgrass::execGRASS(cmd = "r.mapcalc", flags= c("overwrite", "quiet"),
                              expression = paste0(input, output, "_perimeter_null =", input))

        } else{

            rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
            rgrass::execGRASS(cmd = "r.mapcalc", flags= c("overwrite", "quiet"),
                              expression = paste0(input, output, "_perimeter_binary =", input))
            rgrass::execGRASS(cmd = "r.mapcalc", flags= c("overwrite", "quiet"),
                              expression = paste0(input, output, "_perimeter_null = if(", input, " == 1, 1, null())"))
        }

        # matrix ----
        rgrass::execGRASS(cmd = "g.message", message = "Calculation matrix")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags= c("overwrite", "quiet"),
                          expression = paste0(input, output, "_perimeter_matrix = if(",
                                              input, output, "_perimeter_binary == 1, 0, 1)"))

        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags= c("overwrite", "quiet"),
                          expression = paste0(input, output, "_perimeter_matrix = if(isnull(",
                                              input, output, "_perimeter_matrix), 1,",
                                              input, output, "_perimeter_matrix)"))

        # count edge to matrix ----
        rgrass::execGRASS(cmd = "g.message", message = "Counting edges to matrix")
        rgrass::execGRASS(cmd = "r.neighbors",
                          flags = c("c", "overwrite", "quiet"),
                          input = paste0(input, output, "_perimeter_matrix"),
                          # selection = input,
                          output = paste0(input, output, "_perimeter_neighbors"),
                          size = 3,
                          method = "sum",
                          nprocs = nprocs,
                          memory = memory)

        # resolution  ----
        if(proj_unit == "meters"){

            res <- rgrass::stringexecGRASS("g.region -p", intern = TRUE) %>%
                stringr::str_subset("nsres") %>%
                stringr::str_extract("\\d+") %>%
                as.numeric()

        } else if (proj_unit == "degrees") {

            res <- rgrass::stringexecGRASS("g.region -p", intern = TRUE) %>%
                stringr::str_subset("nsres") %>%
                stringr::str_extract_all("\\d+") %>%
                unlist() %>%
                as.numeric() %>%
                {\(x) (x[1] + x[2]/60 + as.numeric(paste0(x[3], ".", x[4]))/3600) * 111320}()
        }

        # count edges ----
        rgrass::execGRASS(cmd = "g.message", message = "Calculating perimeter")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags= c("overwrite", "quiet"),
                          expression = paste0(input, output, "_perimeter_count_edges = ",
                                              input, output, "_perimeter_neighbors * ",
                                              round(res, perimeter_round_digit), " * ",
                                              input, output, "_perimeter_null"))

        # clump ----
        rgrass::execGRASS(cmd = "r.clump",
                          flags = c("d", "quiet", "overwrite"),
                          input = paste0(input, output, "_perimeter_null"),
                          output = paste0(input, output, "_perimeter_id"))

        # perimeter ----
        rgrass::execGRASS(cmd = "r.stats.zonal",
                          flags = c("overwrite", "quiet"),
                          base = paste0(input, output, "_perimeter_id"),
                          cover = paste0(input, output, "_perimeter_count_edges"),
                          method = "sum",
                          output = paste0(input, output, "_perimeter"))

        rgrass::execGRASS(cmd = "r.colors",
                          flags = c("quiet"),
                          map = paste0(input, output, "_perimeter"),
                          color = "bgyr")

        # region ----
        rgrass::execGRASS("g.region", flags = "a", raster = input)

        # index ----
        if(map_perimeter_area_ratio_index || map_shape_index || map_fractal_index){

            ## fragment area ----
            lsmetrics::lsm_area_fragment(input = input,
                                         output = "_perimeter",
                                         zero_as_null = zero_as_null,
                                         area_unit = "m2")

            ## perimeter area ratio index ----
            if(map_perimeter_area_ratio_index){
                rgrass::execGRASS(cmd = "g.message", message = "Calculating perimeter area ratio index")

                rgrass::execGRASS(cmd = "r.mapcalc",
                                  flags= c("overwrite", "quiet"),
                                  expression = paste0(input, output, "_perimeter_area_ratio_index = ",
                                                      input, output, "_perimeter/(",
                                                      input, output, "_perimeter_fragment_area)"))

                # color
                rgrass::execGRASS(cmd = "r.colors",
                                  flags = c("quiet"),
                                  map = paste0(input, output, "_perimeter_area_ratio_index"),
                                  color = "bcyr")
            }

            ## shape index ----
            if(map_shape_index){

                rgrass::execGRASS(cmd = "g.message", message = "Calculating shape index")
                rgrass::execGRASS(cmd = "r.mapcalc",
                                  flags= c("overwrite", "quiet"),
                                  expression = paste0(input, output, "_shape_index = 0.25 *",
                                                      input, output, "_perimeter/(sqrt(",
                                                      input, output, "_perimeter_fragment_area))"))

                # color
                rgrass::execGRASS(cmd = "r.colors",
                                  flags = c("quiet"),
                                  map = paste0(input, output, "_shape_index"),
                                  color = "bcyr")

            }

            ## fractal index ----
            if(map_fractal_index){

                rgrass::execGRASS(cmd = "g.message", message = "Calculating fractal index")
                rgrass::execGRASS(cmd = "r.mapcalc",
                                  flags= c("overwrite", "quiet"),
                                  expression = paste0(input, output, "_fractal_index = 2 * log(0.25 * ",
                                                      input, output, "_perimeter)/(log(",
                                                      input, output, "_perimeter_fragment_area))"))

                # color
                rgrass::execGRASS(cmd = "r.colors",
                                  flags = c("quiet"),
                                  map = paste0(input, output, "_fractal_index"),
                                  color = "bcyr")
            }

            ## clean ----
            rgrass::execGRASS(cmd = "g.remove",
                              flags = c("b", "f", "quiet"),
                              type = "raster",
                              name = paste0(input, output, "_perimeter_fragment_area"))

        }

        # clean ----
        rgrass::execGRASS(cmd = "g.message", message = "Cleaning data")

        suppressWarnings(
            rgrass::execGRASS(cmd = "g.remove",
                              flags = c("b", "f", "quiet"),
                              type = "raster",
                              name = c(
                                  paste0(input, output, "_perimeter_binary"),
                                  paste0(input, output, "_perimeter_null"),
                                  paste0(input, output, "_perimeter_matrix"),
                                  paste0(input, output, "_perimeter_neighbors"),
                                  paste0(input, output, "_perimeter_id"),
                                  paste0(input, output, "_perimeter_count_edges"),
                                  paste0(input, output, "_perimeter_fragment_area")))
        )

    } else {

        warning(paste("Units:", proj_unit, "not currently supported!"))

    }

}
