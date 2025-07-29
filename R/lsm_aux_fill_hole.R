#' Fill holes
#'
#' Fill holes
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification (e.g. values 1,0 or 1,NA for habitat,non-habitat) inside GRASS Data Base.
#' @param output `[character=""]` \cr Map name output inside GRASS Data Base.
#' @param zero_as_null `[logical(1)=FALSE]` \cr If `TRUE`, the function treats non-habitat cells as null; if `FALSE`, the function converts non-habitat zero cells to null cells.
#' @param nprocs `[numeric()]` \cr
#' @param memory `[numeric()]` \cr
#'
#' @example examples/lsm_aux_fill_hole_example.R
#'
#' @name lsm_aux_fill_hole
#' @export
lsm_aux_fill_hole <- function(input,
                              output = NULL,
                              zero_as_null = FALSE,
                              nprocs = 1,
                              memory = 300){

    # region ----
    rgrass::execGRASS("g.region", flags = "a", raster = input)

    # binary ----
    if(zero_as_null){

        # null
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_aux_fill_hole_zero_as_null = ", input))

        # binary
        rgrass::execGRASS(cmd = "g.message", message = "Converting null as zero")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_aux_fill_hole_zero_as_binary = if(isnull(", input, "), 0, 1)"))

    } else{

        # null
        rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_aux_fill_hole_zero_as_null = if(", input, " == 1, 1, null())"))

        # binary
        rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_aux_fill_hole_zero_as_binary = ", input))

    }

    # clump ----
    rgrass::execGRASS(cmd = "g.message", message = "Identifying fragments")
    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "quiet", "overwrite"),
                      input = paste0(input, output, "_aux_fill_hole_zero_as_null"),
                      output = paste0(input, output, "_aux_fill_hole_id"))

    # matrix ----
    rgrass::execGRASS(cmd = "g.message", message = "Creating matrix")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_aux_fill_hole_matrix = if(isnull(", input, output, "_aux_fill_hole_zero_as_null), 1, 0)"))

    # fill hole ----
    rgrass::execGRASS(cmd = "g.message", message = "Filling holes")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_aux_fill_hole_matrix_null = if(", input, output, "_aux_fill_hole_matrix == 1, 1, null())"))

    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "overwrite", "quiet"),
                      input = paste0(input, output, "_aux_fill_hole_matrix_null"),
                      output = paste0(input, output, "_aux_fill_hole_matrix_id"))

    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = c("overwrite", "quiet"),
                      input = paste0(input, output, "_aux_fill_hole_id"),
                      selection = paste0(input, output, "_aux_fill_hole_zero_as_binary"),
                      output = paste0(input, output, "_aux_fill_hole_id_dilation"),
                      size = 3,
                      method = "max",
                      nprocs = nprocs,
                      memory = memory)

    rgrass::execGRASS(cmd = "r.stats",
                      flags = c("n", "overwrite", "quiet"),
                      separator = ",",
                      input = paste0(input, output, "_aux_fill_hole_matrix_id,",
                                     input, output, "_aux_fill_hole_id_dilation"),
                      output = paste0(input, output, "_aux_fill_hole.txt"))

    readr::read_csv(paste0(input, output, "_aux_fill_hole.txt"), show_col_types = FALSE, col_names = c("id", "n_ids")) %>%
        dplyr::count(id) %>%
        readr::write_delim(paste0(input, output, "_aux_fill_hole.txt"), delim = "=", col_names = FALSE)

    rgrass::execGRASS(cmd = "r.reclass",
                      flags = c("overwrite", "quiet"),
                      input = paste0(input, output, "_aux_fill_hole_matrix_id"),
                      output = paste0(input, output, "_aux_fill_hole_matrix_id_fill"),
                      rules = paste0(input, output, "_aux_fill_hole.txt"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_aux_fill_hole_matrix_fill = ", input, output, "_aux_fill_hole_matrix_id_fill == 1"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_aux_fill_hole_matrix_fill = if(isnull(", input, output, "_aux_fill_hole_matrix_fill), 0, ", input, output, "_aux_fill_hole_matrix_fill)"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_aux_fill_hole = ", input, output, "_aux_fill_hole_zero_as_binary + ", input, output, "_aux_fill_hole_matrix_fill"))

    if(zero_as_null){

        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_aux_fill_hole = if(", input, output, "_aux_fill_hole_binary == 0, null(), 1)"))

    }

    # clean ----
    rgrass::execGRASS("g.message", message = "Cleaning data")

    unlink(paste0(input, output, "_aux_fill_hole.txt"))

    suppressWarnings(
        rgrass::execGRASS(cmd = "g.remove",
                          flags = c("b", "f", "quiet"),
                          type = "raster",
                          name = c(paste0(input, output, "_aux_fill_hole_zero_as_null"),
                                   paste0(input, output, "_aux_fill_hole_zero_as_binary"),
                                   paste0(input, output, "_aux_fill_hole_matrix"),
                                   paste0(input, output, "_aux_fill_hole_matrix_fill"),
                                   paste0(input, output, "_aux_fill_hole_matrix_null"),
                                   paste0(input, output, "_aux_fill_hole_matrix_id_fill"),
                                   paste0(input, output, "_aux_fill_hole_matrix_id"),
                                   paste0(input, output, "_aux_fill_hole_id"),
                                   paste0(input, output, "_aux_fill_hole_id_dilation")))
    )

}
