#' Identifies landscape morphologies
#'
#' Identifies landscape morphologies: matrix, core, edge, corridor,
#' stepping stone, branch and perforation
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat) inside GRASS Data Base.
#' @param output `[character=""]` \cr Map name output inside GRASS Data Base.
#' @param input_distance_inside `[character=""]` \cr Distance inside map created
#' using the lsmetrics::lsm_distance() function with `type = "inside"`.
#' @param morphology `[character=""]` \cr
#' @param zero_as_na `[logical(1)=FALSE]` \cr If `TRUE`, the function treats
#' non-habitat cells as null; if `FALSE`, the function converts non-habitat zero
#' cells to null cells.
#'
#' @example examples/lsm_morphology_example.R
#'
#' @name lsm_morphology
#' @export
lsm_morphology <- function(input,
                           output = NULL,
                           morphology = "all",
                           zero_as_na = FALSE){

    # binary ----
    if(zero_as_na){

        # null
        rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_morphology_null = ", input))

        # binary
        rgrass::execGRASS(cmd = "g.message", message = "Converting null as zero")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_morphology_binary = if(isnull(", input, "), 0, 1)"))

        # patch id
        rgrass::execGRASS(cmd = "g.message", message = "Identifying the patches")
        rgrass::execGRASS(cmd = "r.clump",
                          flags = c("d", "quiet", "overwrite"),
                          input = paste0(input, output, "_morphology_null"),
                          output = paste0(input, output, "_morphology_pid"))

    } else{

        # null
        rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_morphology_null = if(", input, " == 1, 1, null())"))

        # binary
        rgrass::execGRASS(cmd = "g.message", message = "Converting null as zero")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_morphology_binary = ", input))

        # patch id
        rgrass::execGRASS(cmd = "g.message", message = "Identifying the patches")
        rgrass::execGRASS(cmd = "r.clump",
                          flags = c("d", "quiet", "overwrite"),
                          input = paste0(input, output, "_morphology_null"),
                          output = paste0(input, output, "_morphology_pid"))

    }

    # matrix ----
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_matrix = if(", input, output, "_morphology_binary == 1, 0, 1)"))

    # fill ----
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_matrix_null = if(", input, output, "_matrix == 1, 1, null())"))

    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "overwrite"),
                      input = paste0(input, output, "_matrix_null"),
                      output = paste0(input, output, "_matrix_pid"))

    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = "overwrite",
                      input = paste0(input, output, "_morphology_pid"),
                      selection = paste0(input, output, "_morphology_binary"),
                      output = paste0(input, output, "_pid_dilation"),
                      size = 3,
                      method = "max")

    rgrass::execGRASS(cmd = "r.stats",
                      flags = c("n", "overwrite"),
                      separator = ",",
                      input = paste0(input, output, "_matrix_pid,", input, output, "_pid_dilation"),
                      output = paste0(input, output, "_perforation.txt"))

    readr::write_delim(dplyr::count(readr::read_csv(paste0(input, output, "_perforation.txt"), show_col_types = FALSE, col_names = c("pid", "n_pids")), pid),
                       paste0(input, output, "_perforation.txt"), delim = "=", col_names = FALSE)

    rgrass::execGRASS(cmd = "r.reclass",
                      flags = "overwrite",
                      input = paste0(input, output, "_matrix_pid"),
                      output = paste0(input, output, "_matrix_pid_fill"),
                      rules = paste0(input, output, "_perforation.txt"))

    unlink(paste0(input, output, "_perforation.txt"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_matrix_fill = ", input, output, "_matrix_pid_fill == 1"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_matrix_fill = if(isnull(", input, output, "_matrix_fill), 0, ", input, output, "_matrix_fill)"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_fill = ", input, output, "_morphology_binary + ", input, output, "_matrix_fill"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_fill_null = if(", input, output, "_fill == 0, null(), 1)"))

    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "overwrite"),
                      input = paste0(input, output, "_fill_null"),
                      output = paste0(input, output, "_fill_pid"))

    # core ----
    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = "overwrite",
                      input = paste0(input, output, "_morphology_binary"),
                      selection = paste0(input, output, "_morphology_binary"),
                      output = paste0(input, output, "_core"),
                      size = 3,
                      method = "min")

    # edge, branch and corridor
    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = "overwrite",
                      input = paste0(input, output, "_fill"),
                      selection = paste0(input, output, "_morphology_binary"),
                      output = paste0(input, output, "_fill_contraction"),
                      size = 3,
                      method = "min")

    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = "overwrite",
                      input = paste0(input, output, "_fill_contraction"),
                      selection = paste0(input, output, "_morphology_binary"),
                      output = paste0(input, output, "_fill_contraction_dilation"),
                      size = 3,
                      method = "max")

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_fill_contraction_dilation_null = if(", input, output, "_fill_contraction_dilation == 1, 0, null())"))

    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "overwrite"),
                      input = paste0(input, output, "_fill_contraction_dilation_null"),
                      output = paste0(input, output, "_fill_contraction_dilation_pid"))

    # stepping stone ----
    rgrass::execGRASS(cmd = "r.stats.zonal",
                      flags = c("overwrite"),
                      base = paste0(input, output, "_fill_pid"),
                      cover = paste0(input, output, "_fill_contraction_dilation_null"),
                      method = "count",
                      output = paste0(input, output, "_zonal_stepping_stone"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_stepping_stone = if(", input, output, "_zonal_stepping_stone > 0, 0, 1)"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_stepping_stone = if(isnull(", input, output, "_stepping_stone), 0, ", input, output, "_stepping_stone)"))

    # perforation ----
    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = "overwrite",
                      input = paste0(input, output, "_matrix_fill"),
                      selection = paste0(input, output, "_morphology_binary"),
                      output = paste0(input, output, "_matrix_fill_dilation"),
                      size = 3,
                      method = "max")

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_perforation = ", input, output, "_matrix_fill_dilation - ", input, output, "_matrix_fill"))

    # edge, branch and corridor
    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = "overwrite",
                      input = paste0(input, output, "_fill"),
                      selection = paste0(input, output, "_morphology_binary"),
                      output = paste0(input, output, "_fill_contraction"),
                      size = 3,
                      method = "min")

    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = "overwrite",
                      input = paste0(input, output, "_fill_contraction"),
                      selection = paste0(input, output, "_morphology_binary"),
                      output = paste0(input, output, "_fill_contraction_dilation"),
                      size = 3,
                      method = "max")

    # edge ----
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_edge = ", input, output, "_fill_contraction_dilation - ", input, output, "_fill_contraction"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_edge_null = if(", input, output, "_edge == 1, 0, null())"))

    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "overwrite"),
                      input = paste0(input, output, "_edge_null"),
                      output = paste0(input, output, "_edge_pid"))

    # branch and corridor
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_branch_corridor = ", input, output, "_fill - ", input, output, "_fill_contraction_dilation - ", input, output, "_stepping_stone"))

    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = c("c", "overwrite"),
                      input = paste0(input, output, "_branch_corridor"),
                      selection = paste0(input, output, "_edge_null"),
                      output = paste0(input, output, "_branch_corridor_dilation"),
                      size = 3,
                      method = "max")

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_branch_corridor_dilation_null = if(", input, output, "_branch_corridor_dilation == 1, 0, null())"))

    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "overwrite"),
                      input = paste0(input, output, "_branch_corridor_dilation_null"),
                      output = paste0(input, output, "_branch_corridor_dilation_pid"))

    rgrass::execGRASS(cmd = "r.stats",
                      flags = c("n", "overwrite"),
                      separator = ",",
                      input = paste0(input, output, "_branch_corridor_dilation_pid,", input, output, "_edge_pid"),
                      output = paste0(input, output, "_branch_corridor.txt"))

    readr::write_delim(dplyr::count(readr::read_csv(paste0(input, output, "_branch_corridor.txt"), show_col_types = FALSE, col_names = c("pid", "n_patches")), pid),
                       paste0(input, output, "_branch_corridor.txt"), delim = "=", col_names = FALSE)

    rgrass::execGRASS(cmd = "r.reclass",
                      flags = "overwrite",
                      input = paste0(input, output, "_branch_corridor_dilation_pid"),
                      output = paste0(input, output, "_branch_corridor"),
                      rules = paste0(input, output, "_branch_corridor.txt"))

    unlink(paste0(input, output, "_branch_corridor.txt"))

    # branch ----
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_branch = if(", input, output, "_branch_corridor == 1, 1, null())"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_branch = if(isnull(", input, output, "_branch), 0, 1)"))

    # corridor ----
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_corridor = if(", input, output, "_branch_corridor > 1, 1, null())"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_corridor = if(isnull(", input, output, "_corridor), 0, 1)"))

    # edge ----
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_edge = ", input, output, "_edge - ", input, output, "_branch - ", input, output, "_corridor - ", input, output, "_perforation"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_edge = if(", input, output, "_edge < 0, 0, ", input, output, "_edge)"))

    # morphology ----
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_morphology = ", input, output, "_core * 1 + ", input, output, "_edge * 2 + ", input, output, "_corridor * 3 + ", input, output, "_branch * 4 + ", input, output, "_stepping_stone * 5 + ", input, output, "_perforation * 6"))

    # color
    rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")
    readr::write_delim(x = tibble::tibble(values = 0:6, colors = c("white", "#33964a", "#9ed4b1", "#50aab3", "#ffcd24", "#f6b1cf", "#aed9e7")),
                       file = "table_color.txt", delim = " ", col_names = FALSE)
    rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_morphology"), rules = "table_color.txt")
    readr::write_delim(x = tibble::tibble(values = 0:1, colors = c("white", "#33964a")),
                       file = "table_color.txt", delim = " ", col_names = FALSE)
    rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_core"), rules = "table_color.txt")
    readr::write_delim(x = tibble::tibble(values = 0:1, colors = c("white", "#9ed4b1")),
                       file = "table_color.txt", delim = " ", col_names = FALSE)
    rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_edge"), rules = "table_color.txt")
    readr::write_delim(x = tibble::tibble(values = 0:1, colors = c("white", "#50aab3")),
                       file = "table_color.txt", delim = " ", col_names = FALSE)
    rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_corridor"), rules = "table_color.txt")
    readr::write_delim(x = tibble::tibble(values = 0:1, colors = c("white", "#ffcd24")),
                       file = "table_color.txt", delim = " ", col_names = FALSE)
    rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_branch"), rules = "table_color.txt")
    readr::write_delim(x = tibble::tibble(values = 0:1, colors = c("white", "#f6b1cf")),
                       file = "table_color.txt", delim = " ", col_names = FALSE)
    rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_stepping_stone"), rules = "table_color.txt")
    readr::write_delim(x = tibble::tibble(values = 0:1, colors = c("white", "#aed9e7")),
                       file = "table_color.txt", delim = " ", col_names = FALSE)
    rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_perforation"), rules = "table_color.txt")
    unlink("table_color.txt")

    # clean
    rgrass::execGRASS(cmd = "g.message", message = "Cleaning rasters")
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_branch_corridor"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_branch_corridor_dilation"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_branch_corridor_dilation_null"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_branch_corridor_dilation_pid"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_edge_null"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_edge_pid"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_fill"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_fill_contraction"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_fill_contraction_dilation"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_fill_contraction_dilation_null"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_fill_contraction_dilation_pid"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_fill_null"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_fill_pid"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_matrix"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_matrix_fill"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_matrix_fill_dilation"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_matrix_null"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_matrix_pid_fill"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_matrix_pid"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_morphology_null"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_morphology_pid"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_pid_dilation"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_zonal_stepping_stone"))

    }
