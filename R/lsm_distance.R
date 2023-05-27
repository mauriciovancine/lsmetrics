#' Calculate patch distance
#'
#' Calculate focal ("moving window") values for each cell using the mean
#' from [r.neighbors] module and multiplies by 100.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param output `[character=""]` \cr Patch area map name inside GRASS Data Base.
#' @param zero_as_na `[logical=""]` \cr
#' @param type `[character=""]` \cr
#'
#' @example examples/lsm_grass_distance_example.R
#'
#' @name lsm_grass_distance
#' @export
lsm_grass_distance <- function(input,
                               output = NULL,
                               zero_as_na = FALSE,
                               type){

  # binary
  if(zero_as_na){

  } else{
    rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
    rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite",
                      expression = paste0(input, output, "_null = if(", input, " == 1, 1, null())"))
  }

  # type inside
  if(type == "inside"){


    if(zero_as_na){

      # create raster
      rgrass::execGRASS(cmd = "g.message", message = "Creating raster inverse")
      rgrass::execGRASS(cmd = "r.mapcalc",
                        flags = "overwrite",
                        expression = paste0(input, output, "_inside = if(isnull(", input, "), 1, null())"))

    } else{

      # create raster
      rgrass::execGRASS(cmd = "g.message", message = "Creating raster inverse")
      rgrass::execGRASS(cmd = "r.mapcalc",
                        flags = "overwrite",
                        expression = paste0(input, output, "_inside = if(isnull(", input, "_null), 1, null())"))

    }

    # distance
    rgrass::execGRASS(cmd = "g.message", message = "Calculation distance")
    rgrass::execGRASS(cmd = "r.grow.distance",
                      flags = "overwrite",
                      input = paste0(input, output, "_inside"),
                      distance = paste0(input, output, "_distance_inside"))

    # integer
    rgrass::execGRASS(cmd = "g.message", message = "Transforming raster to integer")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_distance_inside = round(", input, output, "_distance_inside)"))

    # color
    rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")
    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_distance_inside"),
                      color = "viridis")

    # delete
    rgrass::execGRASS(cmd = "g.remove",
                      flags = c("f", "quiet"),
                      type = "raster",
                      name = paste0(input, output, "_inside"))



  } else{

    if(zero_as_na){

      # distance
      rgrass::execGRASS(cmd = "g.message", message = "Calculation distance")
      rgrass::execGRASS(cmd = "r.grow.distance",
                        flags = c("overwrite"),
                        input = input,
                        distance = paste0(input, output, "_distance_outside"))

    } else{

      # distance
      rgrass::execGRASS(cmd = "g.message", message = "Calculation distance")
      rgrass::execGRASS(cmd = "r.grow.distance",
                        flags = c("overwrite"),
                        input = paste0(input, "_null"),
                        distance = paste0(input, output, "_distance_outside"))

    }

    # integer
    rgrass::execGRASS(cmd = "g.message", message = "Transforming raster to integer")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite"),
                      expression = paste0(input, output, "_distance_outside = round(", input, output, "_distance_outside)"))

    # color
    rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")
    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_distance_outside"),
                      color = "viridis")
  }

}
