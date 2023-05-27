#' Identify the edge of landscape elements
#'
#' Identify the edge of landscape elements for each pixel (matrix, core,
#' edge, patch (core + edge), corridor, branch, stepping stone and perforation)
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param output `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param input_distance_inside `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param edge_dist `[numeric]` \cr Integer indicating edge distance in meters
#' considered adjacent to form a patch. Should be 8 (Queen's case) or 4 (Rook's case).
#' @param type `[numeric]` \cr Integer indicating edge distance in meters
#' considered adjacent to form a patch. Should be 8 (Queen's case) or 4 (Rook's case).

#' @example examples/lsm_grass_core_edge_example.R
#'
#' @name lsm_grass_core_edge
#' @export
lsm_grass_core_edge <- function(input,
                                output = NULL,
                                input_distance_inside,
                                edge_depth,
                                type = "all",
                                calculate_area = FALSE,
                                original_pid,
                                calculate_percentage = FALSE,
                                buffer_radius = NULL){

  # distance inside ----
  files <- rgrass::stringexecGRASS("g.list type=rast", intern=TRUE)

  if(!input_distance_inside %in% files){
    stop("Input distance inside does not exist. Use the lsm_grass_distance() function with the argument `type = 'inside'` to create it.")
  }

  # edge depth ----
  res <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))
  if(edge_depth < res){
    stop("Edge depth is smaller than map resolution. Choose a higher value for the edge depth.")
  }

  # core ----
  if(type == "all" | type == "core"){

    # core
    rgrass::execGRASS(cmd = "g.message", message = "Calculating core")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_core", edge_depth, "=if(", input_distance_inside, ">", edge_depth, ", 1, 0)"))

    # core area
    if(calculate_area == TRUE){

      lsm_grass_area(input = paste0(input, output, "_core", edge_depth))

      rgrass::execGRASS(cmd = "r.stats.zonal",
                        flags = c("overwrite"),
                        base = original_pid,
                        cover = paste0(input, output, "_core", edge_depth, "_null"),
                        method = "count",
                        output = paste0(input, output, "_core", edge_depth, "_area_ncell_original"))

      area_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))^2/1e4
      rgrass::execGRASS(cmd = "r.mapcalc",
                        flags = c("overwrite"),
                        expression = paste0(input, output, "_core", edge_depth, "_area_ha_original=", input, output, "_core", edge_depth, "_area_ncell_original * ", area_pixel))
      rgrass::execGRASS(cmd = "r.mapcalc",
                        flags = c("overwrite"),
                        expression = paste0(input, output, "_core", edge_depth, "_area_ha_original_int=if(", input, output, "_core", edge_depth, "_area_ha_original < 1, 1, ", input, output, "_core", edge_depth, "_area_ha_original)"))
      rgrass::execGRASS(cmd = "r.mapcalc",
                        flags = c("overwrite"),
                        expression = paste0(input, output, "_core", edge_depth, "_area_ha_original_int=round(", input, output, "_core", edge_depth, "_area_ha_original)"))

      rgrass::execGRASS(cmd = "r.colors",
                        flags = c("g", "quiet"),
                        map = paste0(input, output, "_core", edge_depth, "_area_ncell_original"),
                        color = "ryg")

      rgrass::execGRASS(cmd = "r.colors",
                        flags = c("g", "quiet"),
                        map = paste0(input, output, "_core", edge_depth, "_area_ha_original"),
                        color = "ryg")

      rgrass::execGRASS(cmd = "r.colors",
                        flags = c("g", "quiet"),
                        map = paste0(input, output, "_core", edge_depth, "_area_ha_original_int"),
                        color = "ryg")

    }

    # core percentage
    if(calculate_percentage == TRUE){

      lsm_grass_percentage(input = paste0(input, output, "_core", edge_depth),
                           buffer_radius = buffer_radius)

      rgrass::execGRASS(cmd = "r.colors",
                        flags = "quiet",
                        map = paste0(input, output, "_core", edge_depth, "_pct_buf", buffer_radius),
                        color = "blues")

    }

    # color
    rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")
    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_core", edge_depth),
                      color = "blues")
  }

  # edge ----
  if(type == "all" | type == "edge"){

    # edge
    rgrass::execGRASS(cmd = "g.message", message = "Calculating edge")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_edge", edge_depth, "=if(", input_distance_inside, "> 0 &&", input_distance_inside, "<=", edge_depth, ", 1, 0)"))

    # edge area
    if(calculate_area == TRUE){

      lsm_grass_area(input = paste0(input, output, "_edge", edge_depth))

      rgrass::execGRASS(cmd = "r.stats.zonal",
                        flags = c("overwrite"),
                        base = original_pid,
                        cover = paste0(input, output, "_edge", edge_depth, "_null"),
                        method = "count",
                        output = paste0(input, output, "_edge", edge_depth, "_area_ncell_original"))

      area_pixel <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))^2/1e4
      rgrass::execGRASS(cmd = "r.mapcalc",
                        flags = c("overwrite"),
                        expression = paste0(input, output, "_edge", edge_depth, "_area_ha_original=", input, output, "_edge", edge_depth, "_area_ncell_original * ", area_pixel))

      rgrass::execGRASS(cmd = "r.mapcalc",
                        flags = c("overwrite"),
                        expression = paste0(input, output, "_edge", edge_depth, "_area_ha_original_int=if(", input, output, "_edge", edge_depth, "_area_ha_original < 1, 1, ", input, output, "_edge", edge_depth, "_area_ha_original)"))

      rgrass::execGRASS(cmd = "r.mapcalc",
                        flags = c("overwrite"),
                        expression = paste0(input, output, "_edge", edge_depth, "_area_ha_original_int=round(", input, output, "_edge", edge_depth, "_area_ha_original_int)"))

      rgrass::execGRASS(cmd = "r.colors",
                        flags = c("g", "quiet"),
                        map = paste0(input, output, "_edge", edge_depth, "_area_ncell_original"),
                        color = "ryg")

      rgrass::execGRASS(cmd = "r.colors",
                        flags = c("g", "quiet"),
                        map = paste0(input, output, "_edge", edge_depth, "_area_ha_original"),
                        color = "ryg")

      rgrass::execGRASS(cmd = "r.colors",
                        flags = c("g", "quiet"),
                        map = paste0(input, output, "_edge", edge_depth, "_area_ha_original_int"),
                        color = "ryg")

    }

    # edge percentage
    if(calculate_percentage == TRUE){

      lsm_grass_percentage(input = paste0(input, output, "_edge", edge_depth),
                           buffer_radius = buffer_radius,
                           color = "oranges")

    }

    # color
    rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")
    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_edge", edge_depth),
                      color = "oranges")

  }

}