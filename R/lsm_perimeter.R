#' Calculate perimeter
#'
#' Calculate perimeter in meter.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat) inside GRASS Data Base.
#' @param output `[character=""]` \cr Map name output inside GRASS Data Base.
#' @param zero_as_na `[logical(1)=FALSE]` \cr If `TRUE`, the function treats
#' non-habitat cells as null; if `FALSE`, the function converts non-habitat zero
#' cells to null cells.
#' @param perimeter_area_ratio `[character=""]` \cr If `TRUE`, the function treats.
#' @param nprocs `[numeric()]` \cr
#' @param memory `[numeric()]` \cr
#'
#' @example examples/lsm_perimeter_example.R
#'
#' @name lsm_perimeter
#' @export
lsm_perimeter <- function(input,
                          output = NULL,
                          zero_as_na = FALSE,
                          perimeter_area_ratio = FALSE,
                          nprocs = 1,
                          memory = 300){

    # binary
    if(zero_as_na == TRUE){

        # patch id
        rgrass::execGRASS(cmd = "g.message", message = "Identifying the patches")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_perimeter_binary = if(isnull(", input, "), 0, 1)"))
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_perimeter_null =", input))

    } else{

        # null
        rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_perimeter_binary =", input))
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_perimeter_null = if(", input, " == 1, 1, null())"))
    }

    # matrix ----
    rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                      expression = paste0(input, output, "_perimeter_matrix = if(", input, output, "_perimeter_binary == 1, 0, 1)"))

    # count edge to matrix ----
    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = c("c", "overwrite"),
                      input = paste0(input, output, "_perimeter_matrix"),
                      selection = input,
                      output = paste0(input, output, "_perimeter_count_edges"),
                      size = 3,
                      method = "sum",
                      nprocs = nprocs,
                      memory = memory)

    res_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_perimeter_count_edges = ", input, output, "_perimeter_count_edges * ", res_pixel, " * ", input, output, "_perimeter_null"))

    # id ----
    rgrass::execGRASS(cmd = "g.message", message = "Identifying the patches")
    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "quiet", "overwrite"),
                      input = paste0(input, output, "_perimeter_null"),
                      output = paste0(input, output, "_perimeter_id"))

   # perimeter ----
    rgrass::execGRASS(cmd = "r.stats.zonal",
                      flags = c("overwrite"),
                      base = paste0(input, output, "_perimeter_id"),
                      cover = paste0(input, output, "_perimeter_count_edges"),
                      method = "sum",
                      output = paste0(input, output, "_perimeter"))

    # perimeter area ratio ----
    if(perimeter_area_ratio == TRUE){

    ## perimeter area ----
    lsmetrics::lsm_fragment_area(input = input, output = output, zero_as_na = zero_as_na, id = FALSE, ncell = FALSE, area_integer = FALSE)

    # perimeter area ratio ----
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_perimeter_area_ratio = ", input, output, "_perimeter/(", input, output, "_fragment_area_ha * 10000)"))

    # color
    rgrass::execGRASS(cmd = "r.colors", flags = c("quiet"), map = paste0(input, output, "_perimeter"), color = "bgyr")
    rgrass::execGRASS(cmd = "r.colors", flags = c("quiet"), map = paste0(input, output, "_perimeter_area_ratio"), color = "bcyr")

    # clean
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_fragment_area_ha"))

    }

    # clean
    rgrass::execGRASS(cmd = "g.message", message = "Cleaning rasters")
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_perimeter_id"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_perimeter_null"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_perimeter_binary"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_perimeter_count_edges"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_perimeter_matrix"))

}
