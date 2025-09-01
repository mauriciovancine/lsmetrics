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
#' @example examples/lsm_statistic_buffer_example.R
#'
#' @name lsm_statistic_buffer
#' @export
lsm_statistic_buffer <- function(input,
                                 output = NULL,
                                 landscape_metric,
                                 landscape_metric_has_null = FALSE,
                                 point,
                                 buffer_distance,
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
                          expression = paste0(landscape_metric, "_zero = if(isnull(",
                                              landscape_metric, "), 0, ", landscape_metric, ")"))

    } else{

        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(landscape_metric, "_zero = ", landscape_metric))
    }

    # buffer ----
    ## proj units ----
    proj_info <- rgrass::execGRASS("g.proj", flags = "g", intern = TRUE)
    proj_unit <- tolower(sub("units=", "", proj_info[grepl("^units=", proj_info)]))

    ## buffer ----
    if(proj_unit == "meters"){

        res <- rgrass::stringexecGRASS("g.region -p", intern = TRUE) %>%
            stringr::str_subset("nsres") %>%
            stringr::str_extract("\\d+") %>%
            as.numeric()

    } else if (proj_unit == "degrees") {

        res <- rgrass::stringexecGRASS("g.region -p", intern = TRUE) %>%
            stringr::str_subset("nsres") %>%
            stringr::str_extract_all("\\d+") %>%
            unlist() %>%
            as.numeric() %>%
            {\(x) (x[1] + x[2]/60 + as.numeric(paste0(x[3], ".", x[4]))/3600) * 111320}()

    } else {
        warning(paste("Units:", projunits, "not currently supported"))
    }

    rgrass::execGRASS(cmd = "v.buffer",
                      flags = c("t", "overwrite"),
                      input = point,
                      output = paste0(input, output, "_", point, "_buffer", as.character(buffer_distance)),
                      distance = as.numeric(buffer_distance))

    # stats ----
    rgrass::execGRASS(cmd = "v.rast.stats",
                      flags = c("c", "verbose", "quiet"),
                      map = paste0(input, output, "_", point, "_buffer", as.character(buffer_distance)),
                      raster = paste0(landscape_metric, "_zero"),
                      column_prefix = column_prefix,
                      method = paste0(method, collapse = ","))

    # colors ----
    rgrass::execGRASS(cmd = "v.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_", point, "_buffer", as.character(buffer_distance)),
                      use = "attr",
                      column = paste0(column_prefix, "_", method[1]),
                      color = "viridis")

}
