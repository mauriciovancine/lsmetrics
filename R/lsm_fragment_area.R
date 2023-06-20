#' Calculate fragment area
#'
#' Identifies fragmentes and calculates area in hectare.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat) inside GRASS Data Base.
#' @param output `[character=""]` \cr Map name output inside GRASS Data Base.
#' @param zero_as_na `[logical(1)=FALSE]` \cr If `TRUE`, the function treats
#' non-habitat cells as null; if `FALSE`, the function converts non-habitat zero
#' cells to null cells.
#' @param id `[logical(1)=FALSE]` \cr
#' @param ncell `[logical(1)=FALSE]` \cr
#' @param area_integer `[logical(1)=FALSE]` \cr#'
#' @param raster `[logical(1)=FALSE]` \cr#'
#' @param table `[logical(1)=FALSE]` \cr#'
#'
#' @example examples/lsm_fragment_area_example.R
#'
#' @name lsm_fragment_area
#' @export
lsm_fragment_area <- function(input,
                              output = NULL,
                              zero_as_na = FALSE,
                              id = FALSE,
                              ncell = FALSE,
                              area_integer = FALSE,
                              raster = TRUE,
                              table = FALSE){

    # binary ----
    if(zero_as_na){

        # null
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = "overwrite",
                          expression = paste0(input, output, "_fragment_null = ", input))

        # fragment id
        rgrass::execGRASS(cmd = "g.message", message = "Identifying the fragmentes")
        rgrass::execGRASS(cmd = "r.clump",
                          flags = c("d", "quiet", "overwrite"),
                          input = paste0(input, output, "_fragment_null"),
                          output = paste0(input, output, "_fragment_id"))

    } else{

        # null
        rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                          expression = paste0(input, output, "_fragment_null = if(", input, " == 1, 1, null())"))

        # fragment id
        rgrass::execGRASS(cmd = "g.message", message = "Identifying the fragmentes")
        rgrass::execGRASS(cmd = "r.clump",
                          flags = c("d", "quiet", "overwrite"),
                          input = paste0(input, output, "_fragment_null"),
                          output = paste0(input, output, "_fragment_id"))


    }

    # raster ----
    if(raster == TRUE){

        # ncell
        rgrass::execGRASS(cmd = "g.message", message = "Counting the cell number of fragmentes")
        rgrass::execGRASS(cmd = "r.stats.zonal",
                          flags = c("overwrite"),
                          base = paste0(input, output, "_fragment_id"),
                          cover = paste0(input, output, "_fragment_null"),
                          method = "count",
                          output = paste0(input, output, "_fragment_area_ncell"))

        # fragment id ----
        if(!id){

            rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_fragment_id"))

        }

        if(!area_integer){

            # area ----
            rgrass::execGRASS(cmd = "g.message", message = "Calculating the area of fragmentes")
            area_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))^2/1e4
            rgrass::execGRASS(cmd = "r.mapcalc",
                              flags = c("overwrite"),
                              expression = paste0(input, output, "_fragment_area_ha=", input, output, "_fragment_area_ncell * ", area_pixel))
            rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_fragment_area_ha"), color = "ryg")

        } else{

            # area integer ----

            rgrass::execGRASS(cmd = "g.message", message = "Calculating the area of fragmentes")
            area_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))^2/1e4
            rgrass::execGRASS(cmd = "r.mapcalc",
                              flags = c("overwrite"),
                              expression = paste0(input, output, "_fragment_area_ha=", input, output, "_fragment_area_ncell * ", area_pixel))
            rgrass::execGRASS(cmd = "r.mapcalc",
                              flags = c("overwrite"),
                              expression = paste0(input, output, "_fragment_area_ha=round(", input, output, "_fragment_area_ha)"))
            rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_fragment_area_ha"), color = "ryg")

        }

        # ncell ----
        if(!ncell){

            rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_fragment_area_ncell"))

        } else{

            rgrass::execGRASS(cmd = "r.colors", flags = c("g", "quiet"), map = paste0(input, output, "_fragment_area_ncell"), color = "ryg")

        }
    }

    # table ----
    if(table == TRUE){

        rgrass::execGRASS(cmd = "g.message", message = "Calculating the area of fragmentes in tables")
        rgrass::execGRASS(cmd = "r.stats",
                          flags = c("a", "n", "overwrite", "quiet"),
                          separator = ",",
                          input = paste0(input, output, "_fragment_id"),
                          output = paste0(input, output, "_fragment.csv"))

        readr::write_csv(dplyr::mutate(readr::read_csv(paste0(input, output, "_fragment.csv"), col_names = c("id", "area_ha"), show_col_types = FALSE), area_ha = area_ha/1e4), paste0(input, output, "_fragment.csv"))

        readr::write_csv(dplyr::mutate(dplyr::summarise(dplyr::group_by(readr::read_csv(paste0(input, output, "_fragment.csv"), show_col_types = FALSE)),
                                                    area_ha_mean = round(mean(area_ha), 2),
                                                    area_ha_sd = round(sd(area_ha), 2),
                                                    area_ha_cv = round(area_ha_sd/area_ha_mean*100, 2),
                                                    area_ha_max = max(area_ha),
                                                    area_ha_min = min(area_ha)),
                                                 n_frag = nrow(readr::read_csv(paste0(input, output, "_fragment.csv"), show_col_types = FALSE)), .before = 1),
                         paste0(input, output, "_fragment_resume.csv"))

    }

    # clear ----
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_fragment_null"))

}
