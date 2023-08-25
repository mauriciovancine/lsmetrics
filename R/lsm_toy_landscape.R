#' Create a toy landscape
#'
#' Create a toy landscape.
#'
#' @param type `[character=""]` \cr
#' @param random `[character=""]` \cr
#' @param labda `[numeric()]` \cr
#'
#' @example examples/lsm_toy_landscape_example.R
#'
#' @name lsm_toy_landscape
#' @export

lsm_toy_landscape <- function(type = "binary",
                              random = FALSE,
                              lambda = 1){

    toy_landscape <- terra::rast(ncols = 16, nrows = 16, xmin = 234000, xmax = 235600, ymin = 7524000, ymax = 7525600,
                                 crs = "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs ")

    if(random){

        if(type == "binary"){

            toy_landscape[] <- rbinom(n = terra::ncell(toy_landscape), size = 1, prob = .5)

        } else if(type == "multiclass"){

            toy_landscape[] <- rpois(n = terra::ncell(toy_landscape), lambda = lambda)

            }

    } else{

        if(type == "binary"){
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

        } else if(type == "multiclass"){
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
