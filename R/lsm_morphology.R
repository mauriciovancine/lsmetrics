#' Identifies landscape morphological
#'
#' Identifies landscape morphological: matrix, core, edge, corridor, stepping stone, branch, and perforation
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification  (e.g. values 1,0 or 1,NA for habitat,non-habitat) inside GRASS Data Base.
#' @param output `[character=""]` \cr Map name output inside GRASS Data Base.
#' @param morphology `[character=""]` \cr
#' @param zero_as_na `[logical(1)=FALSE]` \cr If `TRUE`, the function treats non-habitat cells as null; if `FALSE`, the function converts non-habitat zero cells to null cells.
#' @param nprocs `[numeric()]` \cr
#' @param memory `[numeric()]` \cr
#'
#' @example examples/lsm_morphology_example.R
#'
#' @name lsm_morphology
#' @export
lsm_morphology <- function(input,
                           output = NULL,
                           zero_as_null = FALSE,
                           region_input = FALSE,
                           morphology = "all",
                           nprocs = 1,
                           memory = 300){

    # region ----
    if(region_input){
        rgrass::execGRASS("g.region", flags = "a", raster = input)
    }

    # binary ----
    if(zero_as_null){

        # null
        rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_morphology_null = ",
                                              input))

        # binary
        rgrass::execGRASS(cmd = "g.message", message = "Converting null as zero")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_morphology_binary = if(isnull(",
                                              input, "), 0, 1)"))

    } else{

        # null
        rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_morphology_null = if(",
                                              input, " == 1, 1, null())"))

        # binary
        rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_morphology_binary = ",
                                              input))

    }

    # clump ----
    rgrass::execGRASS(cmd = "g.message", message = "Identifying fragments")
    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "quiet", "overwrite"),
                      input = paste0(input, output, "_morphology_null"),
                      output = paste0(input, output, "_morphology_id"))

    # matrix ----
    rgrass::execGRASS(cmd = "g.message", message = "Identifying matrix")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_matrix = if(",
                                          input, output, "_morphology_binary == 1, 0, 1)"))

    # core ----
    rgrass::execGRASS(cmd = "g.message", message = "Identifying core")
    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = c("overwrite", "quiet"),
                      input = paste0(input, output, "_morphology_binary"),
                      selection = paste0(input, output, "_morphology_binary"),
                      output = paste0(input, output, "_morphology_core"),
                      size = 3,
                      method = "min",
                      nprocs = nprocs,
                      memory = memory)

    # stepping stone ----
    rgrass::execGRASS(cmd = "g.message", message = "Identifying stepping stone")
    rgrass::execGRASS(cmd = "r.stats.zonal",
                      flags = c("overwrite", "quiet"),
                      base = paste0(input, output, "_morphology_id"),
                      cover = paste0(input, output, "_morphology_core"),
                      method = "sum",
                      output = paste0(input, output, "_morphology_stepping_stone"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_stepping_stone = if(",
                                          input, output, "_morphology_stepping_stone == 0, 1, null())"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_stepping_stone = if(isnull(",
                                          input, output, "_morphology_stepping_stone), 0, 1)"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_stepping_stone = ",
                                          input, output, "_morphology_stepping_stone *",
                                          input, output, "_morphology_binary"))

    # edge ----
    rgrass::execGRASS(cmd = "g.message", message = "Identifying edge")
    lsmetrics::lsm_aux_fill_hole(input = paste0(input, output, "_morphology_binary"),
                                 zero_as_null = FALSE,
                                 nprocs = nprocs,
                                 memory = memory)

    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = c("overwrite", "quiet"),
                      input = paste0(input, output, "_morphology_binary_aux_fill_hole"),
                      selection = input,
                      output = paste0(input, output, "_morphology_binary_fill_hole_contr"),
                      size = 3,
                      method = "min",
                      nprocs = nprocs,
                      memory = memory)

    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = c("overwrite", "quiet"),
                      input = paste0(input, output, "_morphology_binary_fill_hole_contr"),
                      selection = input,
                      output = paste0(input, output, "_morphology_patch_binary"),
                      size = 3,
                      method = "max",
                      nprocs = nprocs,
                      memory = memory)

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_edge = ",
                                          input, output, "_morphology_patch_binary - ",
                                          input, output, "_morphology_binary_fill_hole_contr"))

    # branch, corridor and perforation ----
    rgrass::execGRASS(cmd = "g.message", message = "Identifying branch, corridor and perforation")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_branch_corridor_perforation = ",
                                          input, output, " - ",
                                          input, output, "_morphology_patch_binary - ",
                                          input, output, "_morphology_stepping_stone"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_branch_corridor_binary = if(",
                                          input, output, "_morphology_branch_corridor_perforation == 1, 1, 0)"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_branch_corridor_null = if(",
                                          input, output, "_morphology_branch_corridor_binary == 1, 1, null())"))

    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "quiet", "overwrite"),
                      input = paste0(input, output, "_morphology_branch_corridor_null"),
                      output = paste0(input, output, "_morphology_branch_corridor_id"))

    lsmetrics::lsm_aux_grow(flags = c("quiet", "overwrite"),
                            input = paste0(input, output, "_morphology_branch_corridor_id"),
                            output = paste0(input, output, "_morphology_branch_corridor_id_dila"),
                            metric = "maximum")

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_branch_corridor_id_dila = int(",
                                          input, output, "_morphology_branch_corridor_id_dila)"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_patch_null = if(",
                                          input, output, "_morphology_patch_binary == 1, 1, null())"))

    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "quiet", "overwrite"),
                      input = paste0(input, output, "_morphology_patch_null"),
                      output = paste0(input, output, "_morphology_patch_id"))

    rgrass::execGRASS(cmd = "r.stats.zonal",
                      flags = c("c", "overwrite", "quiet"),
                      base = paste0(input, output, "_morphology_branch_corridor_id_dila"),
                      cover = paste0(input, output, "_morphology_patch_id"),
                      method = "range",
                      output = paste0(input, output, "_morphology_branch_corridor_id_dila_count"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_branch_corridor_id_dila_count_adj = ",
                                          input, output, "_morphology_branch_corridor_id_dila_count * ",
                                          input, output, "_morphology_branch_corridor_null"))

    # perforation ----
    rgrass::execGRASS(cmd = "g.message", message = "Identifying perforation")

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_perforation = if(",
                                          input, output, "_morphology_branch_corridor_perforation < 0, 1, 0)"))

    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = c("overwrite", "quiet"),
                      input = paste0(input, output, "_morphology_perforation"),
                      selection = input,
                      output = paste0(input, output, "_morphology_perforation_dila"),
                      size = 3,
                      method = "max",
                      nprocs = nprocs,
                      memory = memory)

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_perforation = ",
                                          input, output, "_morphology_perforation_dila * ",
                                          input, output, "_morphology_null"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_perforation = ",
                                          input, output, "_morphology_perforation - ",
                                          input, output, "_morphology_edge"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_perforation = if(",
                                          input, output, "_morphology_perforation == -1, 0,",
                                          input, output, "_morphology_perforation)"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_perforation = if(isnull(",
                                          input, output, "_morphology_perforation), 0,",
                                          input, output, "_morphology_perforation)"))

    # corridor ----
    rgrass::execGRASS(cmd = "g.message", message = "Identifying corridor")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_corridor = if(",
                                          input, output, "_morphology_branch_corridor_id_dila_count_adj > 0, 1, null())"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_corridor = if(isnull(",
                                          input, output, "_morphology_corridor), 0, 1)"))

    # branch ----
    rgrass::execGRASS(cmd = "g.message", message = "Identifying branch")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_branch = if(",
                                          input, output, "_morphology_branch_corridor_id_dila_count_adj == 0, 1, null())"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology_branch = if(isnull(",
                                          input, output, "_morphology_branch), 0, 1)"))

    # morphology ----
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_morphology = ",
                                          input, output, "_morphology_core * 1 + ",
                                          input, output, "_morphology_edge * 2 + ",
                                          input, output, "_morphology_corridor * 3 + ",
                                          input, output, "_morphology_branch * 4 + ",
                                          input, output, "_morphology_stepping_stone * 5 + ",
                                          input, output, "_morphology_perforation * 6"))

    # color
    rgrass::execGRASS(cmd = "g.message", message = "Changing raster color")

    tibble::tibble(values = 0:6, colors = c("#cacaca", "#33964a", "#9ed4b1", "#50aab3", "#ffcd24", "#f6b1cf", "#aed9e7")) %>%
        readr::write_delim(paste0(input, output, "_color_morphology.txt"),
                           delim = " ", col_names = FALSE)
    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_morphology"),
                      rules = paste0(input, output, "_color_morphology.txt"))

    tibble::tibble(values = 0:1, colors = c("white", "#cacaca")) %>%
        readr::write_delim(paste0(input, output, "_color_morphology_matrix.txt"),
                           delim = " ", col_names = FALSE)
    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_morphology_matrix"),
                      rules = paste0(input, output, "_color_morphology_matrix.txt"))

    tibble::tibble(values = 0:1, colors = c("white", "#33964a")) %>%
        readr::write_delim(paste0(input, output, "_color_morphology_core.txt"),
                           delim = " ", col_names = FALSE)
    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_morphology_core"),
                      rules = paste0(input, output, "_color_morphology_core.txt"))

    tibble::tibble(values = 0:1, colors = c("white", "#9ed4b1")) %>%
        readr::write_delim(paste0(input, output, "_color_morphology_edge.txt"),
                           delim = " ", col_names = FALSE)
    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_morphology_edge"),
                      rules = paste0(input, output, "_color_morphology_edge.txt"))

    tibble::tibble(values = 0:1, colors = c("white", "#50aab3")) %>%
        readr::write_delim(paste0(input, output, "_color_morphology_corridor.txt"),
                           delim = " ", col_names = FALSE)
    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_morphology_corridor"),
                      rules = paste0(input, output, "_color_morphology_corridor.txt"))

    tibble::tibble(values = 0:1, colors = c("white", "#ffcd24")) %>%
        readr::write_delim(paste0(input, output, "_color_morphology_branch.txt"),
                           delim = " ", col_names = FALSE)
    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_morphology_branch"),
                      rules = paste0(input, output, "_color_morphology_branch.txt"))

    tibble::tibble(values = 0:1, colors = c("white", "#f6b1cf")) %>%
        readr::write_delim(paste0(input, output, "_color_morphology_stepping_stone.txt"),
                           delim = " ", col_names = FALSE)
    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_morphology_stepping_stone"),
                      rules = paste0(input, output, "_color_morphology_stepping_stone.txt"))

    tibble::tibble(values = 0:1, colors = c("white", "#aed9e7")) %>%
        readr::write_delim(paste0(input, output, "_color_morphology_perforation.txt"), delim = " ", col_names = FALSE)
    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_morphology_perforation"),
                      rules = paste0(input, output, "_color_morphology_perforation.txt"))

    # clean ----
    rgrass::execGRASS(cmd = "g.message", message = "Cleaning data")

    unlink(paste0(input, output, "_color_morphology.txt"))
    unlink(paste0(input, output, "_color_morphology_matrix.txt"))
    unlink(paste0(input, output, "_color_morphology_core.txt"))
    unlink(paste0(input, output, "_color_morphology_edge.txt"))
    unlink(paste0(input, output, "_color_morphology_corridor.txt"))
    unlink(paste0(input, output, "_color_morphology_branch.txt"))
    unlink(paste0(input, output, "_color_morphology_stepping_stone.txt"))
    unlink(paste0(input, output, "_color_morphology_perforation.txt"))

    suppressWarnings(
        rgrass::execGRASS(cmd = "g.remove",
                          flags = c("b", "f", "quiet"),
                          type = "raster",
                          name = c(
                              paste0(input, output, "_morphology_binary"),
                              paste0(input, output, "_morphology_null"),
                              paste0(input, output, "_morphology_id"),
                              paste0(input, output, "_morphology_binary_aux_fill_hole"),
                              paste0(input, output, "_morphology_binary_fill_hole_contr"),
                              paste0(input, output, "_morphology_binary_aux_fill_hole_contr_dila"),
                              paste0(input, output, "_morphology_patch_binary"),
                              paste0(input, output, "_morphology_branch_corridor_perforation"),
                              paste0(input, output, "_morphology_branch_corridor_binary"),
                              paste0(input, output, "_morphology_branch_corridor_null"),
                              paste0(input, output, "_morphology_branch_corridor_id"),
                              paste0(input, output, "_morphology_branch_corridor_id_dila"),
                              paste0(input, output, "_morphology_branch_corridor_id_dila_count"),
                              paste0(input, output, "_morphology_branch_corridor_id_dila_count_adj"),
                              paste0(input, output, "_morphology_patch_null"),
                              paste0(input, output, "_morphology_patch_id"),
                              paste0(input, output, "_morphology_perforation_dila"),
                              paste0(input, output, "_morphology_branch_corridor_binary_dila_null")))
    )

}
