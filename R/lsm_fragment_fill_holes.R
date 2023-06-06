#' Fill holes in fragments
#'
#' Fill holes in fragments
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat) inside GRASS Data Base.
#' @param output `[character=""]` \cr Map name output inside GRASS Data Base.
#' @param zero_as_na `[logical(1)=FALSE]` \cr If `TRUE`, the function treats
#' non-habitat cells as null; if `FALSE`, the function converts non-habitat zero
#' cells to null cells.
#'
#' @example examples/lsm_fragment_fill_holes_example.R
#'
#' @name lsm_fragment_fill_holes
#' @export
lsm_fragment_fill_holes <- function(input,
                                 output = NULL,
                                 zero_as_na = FALSE){

    # binary
    if(zero_as_na){

        # fragment id
        rgrass::execGRASS(cmd = "g.message", message = "Identifying the fragments")
        rgrass::execGRASS(cmd = "r.clump",
                          flags = c("d", "quiet", "overwrite"),
                          input = input,
                          output = paste0(input, output, "_fragment_fill_holes_id"))

    } else{

        # null
        rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_null = if(", input, " == 1, 1, null())"))

        # fragment id
        rgrass::execGRASS(cmd = "g.message", message = "Identifying the fragments")
        rgrass::execGRASS(cmd = "r.clump",
                          flags = c("d", "quiet", "overwrite"),
                          input = paste0(input, output, "_null"),
                          output = paste0(input, output, "_fragment_fill_holes_id"))

    }

    # matrix ----
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_matrix = if(", input, output, " == 1, 0, 1)"))

    # fill holes ----
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_matrix_null = if(", input, output, "_matrix == 1, 1, null())"))

    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "overwrite"),
                      input = paste0(input, output, "_matrix_null"),
                      output = paste0(input, output, "_matrix_id"))

    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = "overwrite",
                      input = paste0(input, output, "_fragment_fill_holes_id"),
                      selection = input,
                      output = paste0(input, output, "_id_dilation"),
                      size = 3,
                      method = "max")

    rgrass::execGRASS(cmd = "r.stats",
                      flags = c("n", "overwrite"),
                      separator = ",",
                      input = paste0(input, output, "_matrix_id,", input, output, "_id_dilation"),
                      output = paste0(input, output, "_perforation.txt"))

    readr::write_delim(dplyr::count(readr::read_csv(paste0(input, output, "_perforation.txt"), show_col_types = FALSE, col_names = c("id", "n_ids")), id),
                       paste0(input, output, "_perforation.txt"), delim = "=", col_names = FALSE)

    rgrass::execGRASS(cmd = "r.reclass",
                      flags = "overwrite",
                      input = paste0(input, output, "_matrix_id"),
                      output = paste0(input, output, "_matrix_id_fill"),
                      rules = paste0(input, output, "_perforation.txt"))

    unlink(paste0(input, output, "_perforation.txt"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_matrix_fill = ", input, output, "_matrix_id_fill == 1"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_matrix_fill = if(isnull(", input, output, "_matrix_fill), 0, ", input, output, "_matrix_fill)"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_fragment_fill_holes = ", input, output, "+", input, output, "_matrix_fill"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_fragment_fill_holes_null = if(", input, output, "_fragment_fill_holes == 0, null(), 1)"))

    # color
    rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")
    readr::write_delim(x = tibble::tibble(values = 0:1, colors = c("white", "#33964a")),
                       file = "table_color.txt", delim = " ", col_names = FALSE)
    rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_fragment_fill_holes"), rules = "table_color.txt")
    rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_fragment_fill_holes_null"), rules = "table_color.txt")
    unlink("table_color.txt")

    # clean
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_matrix"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_matrix_fill"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_matrix_null"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_matrix_id_fill"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_matrix_id"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_null"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_fragment_fill_holes_id"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_id_dilation"))

}
