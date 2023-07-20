#' Calculate fragment percentage
#'
#' Calculate focal ("moving window") values for each cell using the mean
#' from [r.neighbors] module and multiplies by 100.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param output `[character=""]` \cr Habitat map output name inside GRASS Data Base.
#' @param zero_as_na `[logical=""]` \cr
#' @param buffer_radius `[numeric()]` \cr Integer indicating window size.
#' @param buffer_cirular `[logical=""]` \cr
#' @param nprocs `[numeric()]` \cr
#' @param memory `[numeric()]` \cr
#'
#' @example examples/lsm_percentage_example.R
#'
#' @name lsm_percentage
#' @export
lsm_percentage <- function(input,
                           output = NULL,
                           zero_as_na = FALSE,
                           buffer_radius,
                           buffer_circular = FALSE,
                           nprocs = 1,
                           memory = 300){

    # window
    res <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))

    if(buffer_radius/res >= 1){
        window <- 2 * round(buffer_radius/res, 0) + 1
    }else{
        stop("Buffer radius is smaller than map resolution. Choose a higher value for the buffer radius.")
    }

    # binary
    if(zero_as_na == TRUE){

        rgrass::execGRASS(cmd = "g.message", message = "Converting null as zero")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_percentage_binary = if(isnull(", input, "), 0, 1)"))
    } else{


        rgrass::execGRASS(cmd = "g.message", message = "Calculating proportion")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_percentage_binary = ", input))
    }

    # proportion
    if(buffer_circular == FALSE){

        rgrass::execGRASS(cmd = "g.message", message = "Calculating proportion")
        rgrass::execGRASS(cmd = "r.neighbors",
                          flags = c("overwrite", "quiet"),
                          input = paste0(input, output, "_percentage_binary"),
                          selection = paste0(input, output, "_percentage_binary"),
                          output = paste0(input, output, "_pct_buf", buffer_radius),
                          size = window,
                          nprocs = nprocs,
                          memory = memory)

    }else{

        rgrass::execGRASS(cmd = "g.message", message = "Calculating proportion")
        rgrass::execGRASS(cmd = "r.neighbors",
                          flags = c("c", "overwrite", "quiet"),
                          input = paste0(input, output, "_percentage_binary"),
                          selection = paste0(input, output, "_percentage_binary"),
                          output = paste0(input, output, "_pct_buf", buffer_radius),
                          size = window,
                          nprocs = nprocs,
                          memory = memory)

    }


    # percentage
    rgrass::execGRASS(cmd = "g.message", message = "Calculating percentage")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_pct_buf", buffer_radius, "= round(",
                                          input, output, "_pct_buf", buffer_radius, "*100)"))

    # color
    rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")
    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_pct_buf", buffer_radius),
                      color = "forest_cover")

    # clean
    rgrass::execGRASS(cmd = "g.message", message = "Cleaning rasters")
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_percentage_binary"))

}
