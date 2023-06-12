#' Create buffers and calculate statistics
#'
#' Create buffers and calculate statistics
#'
#' @param input `[character=""]` \cr
#' @param output `[character=""]` \cr Map name output inside GRASS Data Base.
#' @param landscape_metric `[character=""]` \cr
#' @param landscape_metric_has_null `[character=""]` \cr
#' @param points `[character=""]` \cr
#' @param distance `[character=""]` \cr
#' @param column_prefix `[character=""]` \cr
#' @param method `[character=""]` \cr Univariate statistics: number, null_cells,
#' minimum ,maximum, range, average, stddev, variance, coeff_var, sum, first_quartile
#' ,median, third_quartile, percentile
#' @param percentile `[character=""]` \cr
#'
#' @example examples/lsm_buffer_statistic_example.R
#'
#' @name lsm_buffer_statistic
#' @export
lsm_buffer_statistic <- function(input,
                                 output = NULL,
                                 landscape_metric,
                                 landscape_metric_has_null = FALSE,
                                 point,
                                 distance,
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

    # buffer ----
    rgrass::execGRASS(cmd = "v.buffer",
                      flags = c("t", "overwrite"),
                      input = point,
                      output = paste0(input, output, "_", point, "_buffer", as.character(distance)),
                      distance = as.numeric(distance))

    # stats ----
    rgrass::execGRASS(cmd = "v.rast.stats",
                      flags = c("c", "verbose"),
                      map = paste0(input, output, "_", point, "_buffer", as.character(distance)),
                      raster = paste0(landscape_metric, "_zero"),
                      column_prefix = column_prefix,
                      method = paste0(method, collapse = ","))

    # colors ----
    rgrass::execGRASS(cmd = "v.colors",
                      map = paste0(input, output, "_", point, "_buffer", as.character(distance)),
                      use = "attr",
                      column = paste0(column_prefix, "_", method[1]),
                      color = "viridis")

}
