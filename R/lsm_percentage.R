#' Calculate patch percentage
#'
#' Calculate focal ("moving window") values for each cell using the mean
#' from [r.neighbors] module and multiplies by 100.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param output `[character=""]` \cr Patch area map name inside GRASS Data Base.
#' @param zero_as_na `[logical=""]` \cr
#' @param buffer_radius `[numeric(1)=3]{3,...}` \cr Integer indicating window size. The window
#' can be defined as one (for a square) or two numbers (row, col); or with an
#' odd-sized weights matrix.
#'
#' @example examples/lsm_grass_percentage_example.R
#'
#' @name lsm_grass_percentage
#' @export
lsm_grass_percentage <- function(input,
                                 output = NULL,
                                 zero_as_na = FALSE,
                                 buffer_radius){

  # window
  res <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))

  if(buffer_radius/res >= 1){
    window <- 2 * round(buffer_radius/res, 0) + 1
  }else{
    stop("Buffer radius is smaller than map resolution. Choose a higher value for the buffer radius.")
  }

  # binary
  if(zero_as_na){

    rgrass::execGRASS(cmd = "g.message", message = "Converting null as zero")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_bin = if(isnull(", input, "), 0, 1)"))

    # proportion
    rgrass::execGRASS(cmd = "g.message", message = "Calculating proportion")
    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = "overwrite",
                      input = paste0(input, output, "_bin"),
                      selection = paste0(input, output, "_bin"),
                      output = paste0(input, output, "_pct_buf", buffer_radius),
                      size = window)

  } else{

    # proportion
    rgrass::execGRASS(cmd = "g.message", message = "Calculating proportion")
    rgrass::execGRASS(cmd = "r.neighbors",
                      flags = "overwrite",
                      input = input,
                      selection = input,
                      output = paste0(input, output, "_pct_buf", buffer_radius),
                      size = window)
  }

  # percentage
  rgrass::execGRASS(cmd = "g.message", message = "Calculating percentage")
  rgrass::execGRASS(cmd = "r.mapcalc",
                    flags = "overwrite",
                    expression = paste0(input, output, "_pct_buf", buffer_radius, "=int(",
                                        input, output, "_pct_buf", buffer_radius, "*100)"))

  # color
  rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")

  if(is.null(color)){
    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_pct_buf", buffer_radius),
                      color = "forest_cover")

  } else{
    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_pct_buf", buffer_radius),
                      color = color)
  }

}