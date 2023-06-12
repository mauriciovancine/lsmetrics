#' Create grids and calculate statistics
#'
#' Create grids and calculate statistics
#'
#' @param input `[character=""]` \cr
#' @param output `[character=""]` \cr Map name output inside GRASS Data Base.
#' @param landscape_metric `[character=""]` \cr
#' @param landscape_metric_has_null `[character=""]` \cr
#' @param size `[character=""]` \cr
#' @param hexagon `[character=""]` \cr
#' @param column_prefix `[character=""]` \cr
#' @param method `[character=""]` \cr Univariate statistics: number, null_cells,
#' minimum ,maximum, range, average, stddev, variance, coeff_var, sum, first_quartile
#' ,median, third_quartile, percentile
#' @param percentile `[character=""]` \cr
#'
#' @example examples/lsm_grid_statistics_example.R
#'
#' @name lsm_grid_statistics
#' @export
lsm_grid_statistics <- function(input,
                                output = NULL,
                                landscape_metric,
                                landscape_metric_has_null = FALSE,
                                size,
                                hexagon = FALSE,
                                column_prefix,
                                method = "average",
                                percentile = NULL){

    # turn null values to zero ----
    if(landscape_metric_has_null == TRUE){

        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = "overwrite",
                          expression = paste0(landscape_metric, "_zero = if(isnull(", landscape_metric, "), 0, ", landscape_metric, ")"))

    } else{

        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = "overwrite",
                          expression = paste0(landscape_metric, "_zero = ", landscape_metric))
    }


    # region ----
    res <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))

    if(size >= res + (res * .5)){

        rgrass::execGRASS(cmd = "g.region", flags = "a", raster = landscape_metric)
        box <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("north|south|west|east", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))
        rgrass::execGRASS(cmd = "g.region",
                          flags = "a",
                          n = as.character(box[1] + as.numeric(size)),
                          s = as.character(box[2] - as.numeric(size)),
                          w = as.character(box[3] - as.numeric(size)),
                          e = as.character(box[4] + as.numeric(size)),
                          res = as.character(size))

    }else{

        stop("Grid size is smaller or very similar to current landscape resolution. Choose a larger value for the grid size.")

    }

    # grid ----
    if(hexagon == FALSE){

        rgrass::execGRASS(cmd = "v.mkgrid", flags = c("overwrite"), map = paste0(input, output, "_grid", size))

    } else{

        rgrass::execGRASS(cmd = "v.mkgrid", flags = c("h", "overwrite", "verbose"), map = paste0(input, output, "_grid", size))

    }

    # stats ----
    rgrass::execGRASS(cmd = "v.rast.stats",
                      flags = c("c", "verbose"),
                      map = paste0(input, output, "_grid", size),
                      raster = paste0(landscape_metric, "_zero"),
                      column_prefix = column_prefix,
                      method = paste0(method, collapse = ","))

    # colors ----
    rgrass::execGRASS(cmd = "v.colors",
                      map = paste0(input, output, "_grid", size),
                      use = "attr",
                      column = paste0(column_prefix, "_", method[1]),
                      color = "viridis")

    # region back ----
    rgrass::execGRASS(cmd = "g.region", flags = "a", raster = landscape_metric)

}
