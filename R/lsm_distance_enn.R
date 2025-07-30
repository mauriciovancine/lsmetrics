#' Calculate fragment euclidean nearest neighbor distance
#'
#' Calculate euclidean nearest neighbor distance among fragments in meters using
#' [r.clump] and [r.distance] GRASS GIS module.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param output `[character=""]` \cr Fragment fragment euclidean nearest neighbor distance map name inside GRASS Data Base.
#' @param zero_as_null `[logical=""]` \cr
#' @param id_directions `[numerical=""]` \cr
#' @param table_distance `[logical=""]` \cr
#'
#' @example examples/lsm_distance_enn_example.R
#'
#' @name lsm_distance_enn
#' @export
lsm_distance_enn <- function(input,
                             output = NULL,
                             zero_as_null = FALSE,
                             id_direction = 8,
                             distance_round_digit = 0,
                             grid_size = 10000,
                             distance_radius = 2000,
                             table_distance = FALSE){

    # region ----
    rgrass::execGRASS(cmd = "g.region",
                      flags = "a",
                      raster = input)

    # null ----
    if(zero_as_null){

        rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_distance_enn_null = ", input))

    } else{

        rgrass::execGRASS(cmd = "g.message", message = "Converting zero as null")
        rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_distance_enn_null = if(", input, " == 1, 1, null())"))

    }

    # clump ----
    rgrass::execGRASS("g.message", message = "Identifying fragments")
    if (!id_direction %in% c(4, 8)) stop("Clump `id_direction` must be 4 or 8.")
    clump_flags <- c("quiet", "overwrite")
    if (id_direction == 8) clump_flags <- c("d", clump_flags)
    rgrass::execGRASS("r.clump",
                      flags = clump_flags,
                      input = paste0(input, output, "_distance_enn_null"),
                      output = paste0(input, output, "_distance_enn_id"))

    # distance ----
    rgrass::execGRASS(cmd = "g.message", message = "Calculating distance")

    # proj units ----
    proj_info <- rgrass::execGRASS("g.proj", flags = "g", intern = TRUE)
    proj_unit <- tolower(sub("units=", "", proj_info[grepl("^units=", proj_info)]))

    # resolution  ----
    if(proj_unit == "meters"){

        res <- rgrass::stringexecGRASS("g.region -p", intern = TRUE) %>%
            stringr::str_subset("nsres") %>%
            stringr::str_extract("\\d+") %>%
            as.numeric()

        distance_buffer <- distance_radius

    } else if (proj_unit == "degrees") {

        dd_to_dms <- function(dd) {
            g <- floor(dd)
            dec_m <- (dd - g) * 60
            m <- floor(dec_m)
            s <- (dec_m - m) * 60
            return(sprintf("%d:%02d:%06.3f", g, m, s))
        }

        grid_size <- dd_to_dms(grid_size/111320)

        res <- rgrass::stringexecGRASS("g.region -p", intern = TRUE) %>%
            stringr::str_subset("nsres") %>%
            stringr::str_extract("\\d+:\\d{2}:\\d{2}\\.\\d+")

        distance_buffer <- distance_radius/111320

    } else {
        warning(paste("Units:", proj_unit, "not currently supported"))
    }

    # grid ----
    rgrass::execGRASS(cmd = "g.region",
                      flags = "a",
                      raster = input,
                      res = as.character(grid_size))

    rgrass::execGRASS(cmd = "v.mkgrid",
                      flags = c("overwrite", "quiet"),
                      map = paste0(input, output, "_distance_enn_grid"))

    # region ----
    rgrass::execGRASS(cmd = "g.region",
                      flags = "a",
                      raster = input)

    # import grid ----
    v <- rgrass::read_VECT(vname = paste0(input, output, "_distance_enn_grid"),
                           flags = "quiet")

    # calculate diversity ----
    dist_enn <- NULL
    for(i in v$cat){

        # information
        print(paste0(i, " of ", v[nrow(v), ]$cat))

        # selection
        rgrass::execGRASS(cmd = "v.extract",
                          flags = c("overwrite", "quiet"),
                          input = paste0(input, output, "_distance_enn_grid"),
                          output = paste0(input, output, "_distance_enn_grid_temp", i),
                          where = paste0("cat = '", i, "'"))

        # region
        rgrass::execGRASS(cmd = "v.buffer",
                          flags = c("overwrite", "quiet"),
                          input = paste0(input, output, "_distance_enn_grid_temp", i),
                          output = paste0(input, output, "_distance_enn_grid_temp", i, "_buffer"),
                          distance = distance_buffer)

        rgrass::execGRASS(cmd = "g.region",
                          flags = "a",
                          vector = paste0(input, output, "_distance_enn_grid_temp", i, "_buffer"),
                          align = input)

        # distance
        dist_enn_i <- rgrass::execGRASS(cmd = "r.distance",
                                        flags = "verbose",
                                        map = paste0(input, output, "_distance_enn_id,",
                                                     input, output, "_distance_enn_id"),
                                        separator = "comma",
                                        intern = TRUE) %>%
            tibble::as_tibble() %>%
            tidyr::separate(value,
                            into = c("id", "id2", "dist", "x1", "y1", "x2", "y2"),
                            sep = ",",
                            convert = TRUE) %>%
            dplyr::mutate(dist = round(dist, distance_round_digit),
                          x1 = as.numeric(x1),
                          y1 = as.numeric(y1),
                          x2 = as.numeric(x2),
                          y2 = as.numeric(y2)) %>%
            dplyr::filter(!id == id2) %>%
            dplyr::group_by(id) %>%
            dplyr::slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
            dplyr::ungroup()

        dist_enn <- dist_enn %>%
            dplyr::bind_rows(., dist_enn_i) %>%
            dplyr::arrange(id)
    }

    # region ----
    rgrass::execGRASS(cmd = "g.region",
                      flags = "a",
                      raster = input)

    # export ----
    dist_enn %>%
        dplyr::group_by(id) %>%
        dplyr::slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(id2 = id) %>%
        dplyr::select(id, id2, dist) %>%
        readr::write_delim(paste0(input, output, "_dist_enn.txt"),
                           delim = ":", col_names = FALSE)

    # assign ----
    rgrass::execGRASS(cmd = "r.recode",
                      flags = "overwrite",
                      input = paste0(input, output, "_distance_enn_id"),
                      output = paste0(input, output, "_distance_enn_temp"),
                      rules = paste0(input, output, "_dist_enn.txt"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, output, "_distance_enn = ",
                                          input, output, "_distance_enn_temp"))

    # color ----
    rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")
    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_distance_enn"),
                      color = "viridis")

    # export table ----
    if(table_distance){

        readr::write_csv(dist_enn, paste0(input, output, "distance_enn.csv"))

    }

    # clean ----
    rgrass::execGRASS(cmd = "g.message", message = "Cleaning data")

    rm(dist_enn)
    unlink(paste0(input, output, "_dist_enn.txt"))

    suppressWarnings(
        rgrass::execGRASS(cmd = "g.remove",
                          flags = c("b", "f", "quiet"),
                          type = c("vector", "raster"),
                          name = c(
                              paste0(input, output, "_distance_enn_grid"),
                              paste0(input, output, "_distance_enn_grid_sel"),
                              paste0(input, output, "_distance_enn_grid_temp", v$cat),
                              paste0(input, output, "_distance_enn_grid_temp", v$cat, "_buffer"),
                              paste0(input, output, "_distance_enn_null"),
                              paste0(input, output, "_distance_enn_id"),
                              paste0(input, output, "_distance_enn_temp")))
    )

}
