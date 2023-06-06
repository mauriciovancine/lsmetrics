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
#' @param fragment_id `[logical(1)=FALSE]` \cr
#' @param fragment_ncell `[logical(1)=FALSE]` \cr
#' @param fragment_area_integer `[logical(1)=FALSE]` \cr#'
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
                              area_integer = FALSE){

    # binary
    if(zero_as_na){

        # fragment id
        rgrass::execGRASS(cmd = "g.message", message = "Identifying the fragmentes")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = "overwrite",
                          expression = paste0(input, output, "_fragment_null = ", input))
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

    # clear ----
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_fragment_null"))

}
