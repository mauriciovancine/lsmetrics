#' Create grids and calculate statistics
#'
#' Create grids and calculate statistics
#'
#' @param input `[character=""]` \cr
#' @param output `[character=""]` \cr Map name output inside GRASS Data Base.
#' @param landscape_metric `[character=""]` \cr
#' @param landscape_metric_has_null `[character=""]` \cr
#' @param grid_size `[character=""]` \cr
#' @param hexagon `[character=""]` \cr
#' @param column_prefix `[character=""]` \cr
#' @param method `[character=""]` \cr Univariate statistics: number, null_cells,
#' minimum ,maximum, range, average, stddev, variance, coeff_var, sum, first_quartile
#' ,median, third_quartile, percentile
#' @param percentile `[character=""]` \cr
#'
#' @example examples/lsm_statistic_grid_example.R
#'
#' @name lsm_statistic_grid
#' @export
lsm_statistic_grid <- function(input,
                               output = NULL,
                               region_input = FALSE,
                               landscape_metric,
                               landscape_metric_has_null = FALSE,
                               grid_size,
                               hexagon = FALSE,
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
                                              landscape_metric, "), 0, ",
                                              landscape_metric, ")"))

    } else{

        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(landscape_metric, "_zero = ",
                                              landscape_metric))
    }

    # region ----
    proj_info <- rgrass::execGRASS("g.proj", flags = c("g", "quiet"), intern = TRUE)
    proj_unit <- tolower(sub("units=", "", proj_info[grepl("^units=", proj_info)]))

    if(proj_unit %in% c("meters", "degress")){

        if(proj_unit == "meters"){

            res <- rgrass::stringexecGRASS("g.region -p --quiet", intern = TRUE) %>%
                stringr::str_subset("nsres") %>%
                stringr::str_extract("\\d+") %>%
                as.numeric()

            grid_size <- grid_size

        } else if (proj_unit == "degrees") {

            res <- rgrass::stringexecGRASS("g.region -p --quiet", intern = TRUE) %>%
                stringr::str_subset("nsres") %>%
                stringr::str_extract_all("\\d+") %>%
                unlist() %>%
                as.numeric() %>%
                {\(x) (x[1] + x[2]/60 + as.numeric(paste0(x[3], ".", x[4]))/3600) * 111320}()

            grid_size <- grid_size/111320

        }

        if(grid_size >= res + (res * .5)){

            rgrass::execGRASS(cmd = "g.region", flags = "a", raster = landscape_metric)
            box <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("north|south|west|east",
                                                                rgrass::stringexecGRASS("g.region -p",
                                                                                        intern=TRUE),
                                                                value = TRUE)))
            rgrass::execGRASS(cmd = "g.region",
                              flags = "a",
                              n = as.character(box[1] + as.numeric(grid_size)),
                              s = as.character(box[2] - as.numeric(grid_size)),
                              w = as.character(box[3] - as.numeric(grid_size)),
                              e = as.character(box[4] + as.numeric(grid_size)),
                              res = as.character(grid_size))


        }else{

            stop("Grid grid_size is smaller or very similar to current landscape resolution. Choose a larger value for the grid grid_size.")

        }

        # grid ----
        if(hexagon == FALSE){

            rgrass::execGRASS(cmd = "v.mkgrid",
                              flags = c("overwrite", "quiet"),
                              map = paste0(input, output, "_grid", grid_size))

        } else{

            rgrass::execGRASS(cmd = "v.mkgrid",
                              flags = c("h", "overwrite", "quiet", "verbose"),
                              map = paste0(input, output, "_grid", grid_size))

        }

        # stats ----
        rgrass::execGRASS(cmd = "v.rast.stats",
                          flags = c("c", "quiet"),
                          map = paste0(input, output, "_grid", grid_size),
                          raster = paste0(landscape_metric, "_zero"),
                          column_prefix = column_prefix,
                          method = paste0(method, collapse = ","))

        # colors ----
        rgrass::execGRASS(cmd = "v.colors",
                          flags = "quiet",
                          map = paste0(input, output, "_grid", grid_size),
                          use = "attr",
                          column = paste0(column_prefix, "_", method[1]),
                          color = "viridis")

        # region back ----
        rgrass::execGRASS(cmd = "g.region", flags = "a", raster = landscape_metric)

    } else {
        warning(paste("Units:", proj_unit, "not currently supported!"))
    }

}

