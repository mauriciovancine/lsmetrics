#' Create a toy landscape
#'
#' Create a toy landscape.
#'
#' @param proj_type `[character=""]` \cr
#' @param values_type `[character=""]` \cr
#' @param values_random `[character=""]` \cr
#' @param values_random_multiclass_parameter `[numeric()]` \cr
#'
#' @example examples/lsm_toy_landscape_example.R
#'
#' @name lsm_toy_landscape
#' @export

lsm_toy_landscape <- function(proj_type = "meters",
                              values_type = "binary",
                              values_random = FALSE,
                              values_random_multiclass_parameter = 1){

    if(proj_type == "meters"){

    toy_landscape <- terra::rast(ncols = 16,
                                 nrows = 16,
                                 xmin = 234000,
                                 xmax = 235600,
                                 ymin = 7524000,
                                 ymax = 7525600,
                                 crs = "EPSG:32723")

    } else if(proj_type == "degrees"){

        toy_landscape <- terra::rast(ncols = 16,
                                     nrows = 16,
                                     xmin = -47.58312,
                                     xmax = -47.56830,
                                     ymin = -22.36995,
                                     ymax = -22.35514,
                                     res = 100/111320,
                                     crs = "EPSG:4326")
    }

    if(values_random){

        if(values_type == "binary"){

            toy_landscape[] <- rbinom(n = terra::ncell(toy_landscape),
                                      size = 1,
                                      prob = .5)

        } else if(values_type == "multiclass"){

            toy_landscape[] <- rpois(n = terra::ncell(toy_landscape),
                                     lambda = values_random_multiclass_parameter)

            }

    } else{

        if(values_type == "binary"){
            terra::values(toy_landscape) <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
                                              0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1,
                                              0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1,
                                              0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0,
                                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                              1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1,
                                              1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1,
                                              1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
                                              1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1,
                                              1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,
                                              1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
                                              1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1,
                                              1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
                                              0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)

        } else if(values_type == "multiclass"){
            terra::values(toy_landscape) <- c(2, 2, 2, 2, 3, 3, 3, 3, 3, 5, 5, 5, 5, 1, 1, 1,
                                              2, 1, 1, 1, 3, 3, 3, 1, 1, 1, 1, 5, 5, 1, 1, 1,
                                              2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 1, 1, 1,
                                              2, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 5, 5, 5, 5, 5,
                                              2, 2, 2, 0, 0, 0, 0, 4, 3, 3, 3, 4, 4, 4, 4, 4,
                                              2, 2, 2, 0, 0, 0, 0, 4, 3, 3, 3, 3, 4, 4, 4, 4,
                                              2, 2, 2, 0, 0, 0, 4, 4, 3, 3, 3, 4, 4, 4, 4, 4,
                                              1, 1, 1, 1, 1, 1, 2, 2, 1, 0, 0, 0, 1, 1, 1, 1,
                                              1, 1, 1, 1, 1, 1, 2, 2, 1, 0, 0, 0, 1, 1, 1, 1,
                                              1, 5, 5, 5, 1, 1, 2, 2, 4, 0, 0, 0, 1, 1, 1, 1,
                                              1, 5, 5, 5, 1, 2, 2, 2, 4, 0, 0, 1, 1, 1, 5, 1,
                                              1, 1, 1, 1, 1, 2, 2, 2, 4, 4, 1, 1, 1, 1, 1, 1,
                                              1, 1, 1, 1, 1, 3, 3, 3, 4, 4, 4, 4, 1, 1, 1, 1,
                                              1, 1, 1, 1, 1, 1, 1, 3, 5, 5, 5, 5, 1, 1, 1, 1,
                                              1, 1, 1, 1, 1, 5, 3, 3, 5, 5, 4, 4, 1, 1, 1, 1,
                                              5, 5, 5, 5, 5, 5, 3, 3, 5, 1, 4, 4, 4, 4, 4, 4)

        }

    }

    names(toy_landscape) <- "toy_landscape"

    return(toy_landscape)
}
