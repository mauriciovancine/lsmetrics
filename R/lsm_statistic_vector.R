#' Calculate statistics for vectors
#'
#' Calculate statistics for vectors
#'
#' @param input `[character=""]` \cr
#' @param output `[character=""]` \cr Map name output inside GRASS Data Base.
#' @param landscape_metric `[character=""]` \cr
#' @param landscape_metric_has_null `[character=""]` \cr
#' @param vector `[character=""]` \cr
#' @param type `[character=""]` \cr
#' @param column_prefix `[character=""]` \cr
#' @param method `[character=""]` \cr Univariate statistics: number, null_cells,
#' minimum ,maximum, range, average, stddev, variance, coeff_var, sum, first_quartile
#' ,median, third_quartile, percentile
#' @param percentile `[character=""]` \cr
#'
#' @example examples/lsm_statistic_vector_example.R
#'
#' @name lsm_statistic_vector
#' @export
lsm_statistic_vector <- function(input,
                                 output = NULL,
                                 region_input = FALSE,
                                 landscape_metric,
                                 landscape_metric_has_null = FALSE,
                                 vector,
                                 type,
                                 column_prefix,
                                 method = "average",
                                 percentile = NULL){

    # region ----
    if(region_input){
        rgrass::execGRASS("g.region", flags = "a", raster = input)
    }

    # turn null values to zero ----
    if(landscape_metric_has_null == TRUE){

        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(landscape_metric, "_binary = if(isnull(",
                                              landscape_metric, "), 0, ",
                                              landscape_metric, ")"))

    } else{

        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(landscape_metric, "_binary = ",
                                              landscape_metric))
    }

    # stats ----
    rgrass::execGRASS(cmd = "v.rast.stats",
                      flags = c("c", "verbose", "quiet"),
                      map = vector,
                      type = type,
                      raster = paste0(landscape_metric, "_binary"),
                      column_prefix = column_prefix,
                      method = paste0(method, collapse = ","))

    # colors ----
    rgrass::execGRASS(cmd = "v.colors",
                      flags = "quiet",
                      map = vector,
                      use = "attr",
                      column = paste0(column_prefix, "_", method[1]),
                      color = "viridis")

    # clean ----
    rgrass::execGRASS(cmd = "g.remove",
                      flags = c("b", "f", "quiet"),
                      type = "raster",
                      name = paste0(landscape_metric, "_binary"))

}
