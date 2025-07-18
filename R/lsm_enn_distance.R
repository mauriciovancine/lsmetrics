#' Calculate fragment euclidean nearest neighbor distance
#'
#' Calculate euclidean nearest neighbor distance among fragments in meters using
#' [r.clump] and [r.distance] GRASS GIS module.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param output `[character=""]` \cr Fragment fragment euclidean nearest neighbor distance map name inside GRASS Data Base.
#' @param zero_as_na `[logical=""]` \cr
#' @param directions `[numerical=""]` \cr
#' @param export_table `[logical=""]` \cr
#'
#' @example examples/lsm_enn_distance_example.R
#'
#' @name lsm_enn_distance
#' @export
lsm_enn_distance <- function(input,
                             output = NULL,
                             directions = 8,
                             zero_as_na = FALSE,
                             export_table = FALSE){

    # region ----
    rgrass::execGRASS(cmd = "g.region",
                      flags = "a",
                      raster = input)

    # null ----
    if(zero_as_na){
        rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_enn_distance_null = ", input))

    } else{
        rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_enn_distance_null = if(", input, " == 1, 1, null())"))
    }

    # clump ----
    rgrass::execGRASS(cmd = "g.message", message = "Identifying fragments")
    if(directions == 8){
        rgrass::execGRASS(cmd = "r.clump",
                          flags = c("d", "overwrite", "quiet"),
                          input = paste0(input, output, "_enn_distance_null"),
                          output = paste0(input, output, "_enn_distance_clump"))
    } else{
        rgrass::execGRASS(cmd = "r.clump",
                          flags = c("overwrite", "quiet"),
                          input = paste0(input, output, "_enn_distance_null"),
                          output = paste0(input, output, "_enn_distance_clump"))
    }

    # distance ----
    rgrass::execGRASS(cmd = "g.message", message = "Calculating distance")
    dist_enn <- rgrass::execGRASS(cmd = "r.distance",
                                  map = paste0(input, output, "_enn_distance_clump,",input, output, "_enn_distance_clump"),
                                  separator = "comma",
                                  intern = TRUE) %>%
        tibble::as_tibble() %>%
        tidyr::separate(value,
                        into = c("fid", "fid2", "dist", "x1", "y1", "x2", "y2"),
                        sep = ",",
                        convert = TRUE) %>%
        dplyr::mutate(dist = ceiling(dist),
                      x1 = as.numeric(x1),
                      y1 = as.numeric(y1),
                      x2 = as.numeric(x2),
                      y2 = as.numeric(y2)) %>%
        dplyr::filter(!fid == fid2) %>%
        dplyr::group_by(fid) %>%
        dplyr::slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup()

    # export ----
    dist_enn %>%
        dplyr::select(fid, dist) %>%
        readr::write_delim("dist.txt", delim = "=", col_names = FALSE)

    # assign ----
    rgrass::execGRASS(cmd = "r.reclass",
                      flags = "overwrite",
                      input = paste0(input, output, "_enn_distance_clump"),
                      output = paste0(input, output, "_enn_distance_temp"),
                      rules = "dist.txt")

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_enn_distance = ", input, output, "_enn_distance_temp"))

    # color ----
    rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")
    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_enn_distance"),
                      color = "viridis")

    # export table ----
    if(export_table == TRUE){
        readr::write_csv(dist_enn, paste0(input, output, "_enn_distance.csv"))
    }

    # clean ----
    rgrass::execGRASS(cmd = "g.message", message = "Cleaning files")
    suppressWarnings(
        rgrass::execGRASS(cmd = "g.remove",
                          flags = c("b", "f", "quiet"),
                          type = "raster",
                          name = c(paste0(input, output, "_enn_distance_null"),
                                   paste0(input, output, "_enn_distance_clump"),
                                   paste0(input, output, "_enn_distance_temp")))
    )
    rm(dist_enn)
    unlink("dist.txt")

}
