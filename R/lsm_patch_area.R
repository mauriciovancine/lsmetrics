#' Calculate patch area
#'
#' Identifies patch and calculates area in hectare.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat) inside GRASS Data Base.
#' @param output `[character=""]` \cr Map name output inside GRASS Data Base.
#' @param zero_as_na `[logical(1)=FALSE]` \cr If `TRUE`, the function treats
#' non-habitat cells as null; if `FALSE`, the function converts non-habitat zero
#' cells to null cells.
#' @param id `[logical(1)=FALSE]` \cr If `TRUE`
#' @param ncell `[logical(1)=FALSE]` \cr If `TRUE`
#' @param area_integer `[logical(1)=FALSE]` \cr If `TRUE`
#' @param patch_original `[logical(1)=FALSE]` \cr If `TRUE`
#' @param patch_number `[logical(1)=FALSE]` \cr If `TRUE`
#' @param nprocs `[numeric()]` \cr
#' @param memory `[numeric()]` \cr
#'
#' @example examples/lsm_patch_area_example.R
#'
#' @name lsm_patch_area
#' @export
lsm_patch_area <- function(input,
                           output = NULL,
                           zero_as_na = FALSE,
                           id = FALSE,
                           ncell = FALSE,
                           area_integer = FALSE,
                           patch_original = FALSE,
                           patch_number = FALSE,
                           nprocs = 1,
                           memory = 300){

    # binary
    if(zero_as_na == TRUE){

        # null
        rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_patch_area_null =", input))

        # binary
        rgrass::execGRASS(cmd = "g.message", message = "Converting null as zero")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_patch_area_binary = if(isnull(", input, "), 0, 1)"))

        # patch id
        rgrass::execGRASS(cmd = "g.message", message = "Identifying the patches")
        rgrass::execGRASS(cmd = "r.clump",
                          flags = c("d", "quiet", "overwrite"),
                          input = paste0(input, output, "_patch_area_null"),
                          output = paste0(input, output, "_patch_area_id"))

    } else{

        # null
        rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_patch_area_null = if(", input, " == 1, 1, null())"))

        # binary
        rgrass::execGRASS(cmd = "g.message", message = "Converting null as zero")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_patch_area_binary = ", input))

        # id
        rgrass::execGRASS(cmd = "g.message", message = "Identifying the patches")
        rgrass::execGRASS(cmd = "r.clump",
                          flags = c("d", "quiet", "overwrite"),
                          input = paste0(input, output, "_patch_area_null"),
                          output = paste0(input, output, "_patch_area_id"))

    }

    # fill hole ----
    lsmetrics::lsm_fragment_fill_hole(input = paste0(input, output, "_patch_area_binary"))

    # patch ----
    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = "overwrite",
                      input = paste0(input, output, "_patch_area_binary_fragment_fill_hole"),
                      selection = input,
                      output = paste0(input, output, "_fill_contraction"),
                      size = 3,
                      method = "min",
                      nprocs = nprocs,
                      memory = memory)

    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = "overwrite",
                      input = paste0(input, output, "_fill_contraction"),
                      selection = input,
                      output = paste0(input, output, "_patch"),
                      size = 3,
                      method = "max",
                      nprocs = nprocs,
                      memory = memory)

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_patch_null = if(", input, output, "_patch == 1, 1, null())"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_patch_null = ", input, output, "_patch_null * ", input, output, "_patch_area_null"))

    rgrass::execGRASS(cmd = "r.clump",
                      flags = "overwrite",
                      input = paste0(input, output, "_patch_null"),
                      output = paste0(input, output, "_patch_id"))

    # ncell
    rgrass::execGRASS(cmd = "g.message", message = "Counting the cell number of patches")
    rgrass::execGRASS(cmd = "r.stats.zonal",
                      flags = c("overwrite"),
                      base = paste0(input, output, "_patch_id"),
                      cover = paste0(input, output, "_patch_null"),
                      method = "count",
                      output = paste0(input, output, "_patch_area_ncell"))

    # area ----
    if(area_integer == FALSE){

        # area
        rgrass::execGRASS(cmd = "g.message", message = "Calculating the area of patches")
        area_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))^2/1e4
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite"),
                          expression = paste0(input, output, "_patch_area_ha=", input, output, "_patch_area_ncell * ", area_pixel))

        # color
        rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")
        readr::write_delim(x = tibble::tibble(values = 0:1, colors = c("white", "#33964a")),
                           file = "table_color.txt", delim = " ", col_names = FALSE)
        rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_patch"), rules = "table_color.txt")
        rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_patch_null"), rules = "table_color.txt")
        unlink("table_color.txt")
        rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_patch_area_ha"), color = "ryg")

    }else{

        # area
        rgrass::execGRASS(cmd = "g.message", message = "Calculating the area of patches")
        area_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))^2/1e4
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite"),
                          expression = paste0(input, output, "_patch_area_ha=", input, output, "_patch_area_ncell * ", area_pixel))

        # area integer
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite"),
                          expression = paste0(input, output, "_patch_area_ha=round(", input, output, "_patch_area_ha)"))

        # color
        rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")
        readr::write_delim(x = tibble::tibble(values = 0:1, colors = c("white", "#33964a")),
                           file = "table_color.txt", delim = " ", col_names = FALSE)
        rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_patch"), rules = "table_color.txt")
        rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_patch_null"), rules = "table_color.txt")
        unlink("table_color.txt")
        rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_patch_area_ha"), color = "ryg")

    }


    # patch original ----
    if(patch_original == TRUE & area_integer == FALSE){

        rgrass::execGRASS(cmd = "r.stats.zonal",
                          flags = c("overwrite"),
                          base = paste0(input, output, "_patch_area_id"),
                          cover = paste0(input, output, "_patch_null"),
                          method = "count",
                          output = paste0(input, output, "_patch_area_ncell_original"))

        area_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))^2/1e4
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite"),
                          expression = paste0(input, output, "_patch_area_ha_original =", input, output, "_patch_area_ncell_original * ", area_pixel))

        rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_patch_area_ha_original"), color = "ryg")

    }

    if(patch_original == TRUE & area_integer == TRUE){

        rgrass::execGRASS(cmd = "r.stats.zonal",
                          flags = c("overwrite"),
                          base = paste0(input, output, "_patch_area_id"),
                          cover = paste0(input, output, "_patch_null"),
                          method = "count",
                          output = paste0(input, output, "_patch_area_ncell_original"))

        area_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))^2/1e4
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite"),
                          expression = paste0(input, output, "_patch_area_ha_original =", input, output, "_patch_area_ncell_original * ", area_pixel))

        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite"),
                          expression = paste0(input, output, "_patch_area_ha_original = round(", input, output, "_patch_area_ha_original)"))

        rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_patch_area_ha_original"), color = "ryg")

    }


    # ncell ----
    if(ncell == FALSE){

        rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_patch_area_ncell"))

    }else{

        rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_patch_area_ncell"), color = "ryg")

    }

    if(patch_original == TRUE & ncell == FALSE){

        rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_patch_area_ncell_original"))

    }

    if(patch_original == TRUE & ncell == TRUE){

        rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_patch_area_ncell_original"), color = "ryg")

    }


    # patch number ----
    if(patch_number == TRUE){

        rgrass::execGRASS(cmd = "r.stats",
                          flags = c("N", "overwrite"),
                          separator = ",",
                          input = paste0(input, output, "_patch_area_id,", input, output, "_patch_id"),
                          output = paste0(input, output, "_patch_id.txt"))

        readr::write_delim(dplyr::summarise(dplyr::group_by(dplyr::mutate(dplyr::mutate(readr::read_csv(paste0(input, output, "_patch_id.txt"), show_col_types = FALSE, col_names = c("id", "n_core_ids")), n_core_ids = as.numeric(ifelse(n_core_ids == "*", 0, n_core_ids))), n_core_ids = ifelse(n_core_ids > 0, 1, 0)), id), sum = sum(n_core_ids)),
                           paste0(input, output, "_patch_id.txt"), delim = "=", col_names = FALSE)

        rgrass::execGRASS(cmd = "r.reclass",
                          flags = "overwrite",
                          input = paste0(input, output, "_patch_area_id"),
                          output = paste0(input, output, "_patch_number_original_temp"),
                          rules = paste0(input, output, "_patch_id.txt"))

        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite"),
                          expression = paste0(input, output, "_patch_number_original = ", input, output, "_patch_number_original_temp"))

        unlink(paste0(input, output, "_patch_id.txt"))

        rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_patch_number_original_temp"))

    }

    # fragment id ----
    if(id == FALSE){

        rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_patch_id"))

    }

    # clean
    rgrass::execGRASS(cmd = "g.message", message = "Cleaning rasters")
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_patch"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_patch_null"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_patch_area_null"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_patch_area_binary"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_patch_area_id"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_fill_contraction"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_patch_area_binary_fragment_fill_hole"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_patch_area_binary_fragment_fill_hole_null"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_patch_area_binary_fragment_fill_hole_binary"))

}
