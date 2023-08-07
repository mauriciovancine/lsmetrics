#' Calculate landscape diversity
#'
#' Calculate landscape diversity using [r.diversity] module.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param output `[character=""]` \cr Habitat map output name inside GRASS Data Base.
#' @param buffer_radius `[numeric()]` \cr Integer indicating window size.
#' @param index `[character=""]` \cr Integer indicating window size.
#' @param grid_size `[numeric()]` \cr Integer indicating window size.
#' @param grid_delete `[numeric()]` \cr Integer indicating window size.
#' @param install_rdiversity `[numeric()]` \cr Integer indicating window size.
#' @param nprocs `[numeric()]` \cr
#' @param memory `[numeric()]` \cr
#'
#' @example examples/lsm_diversity_parallel_example.R
#'
#' @name lsm_diversity_parallel
#' @export
lsm_diversity_parallel <- function(input,
                                   output = NULL,
                                   buffer_radius,
                                   index,
                                   alpha = NULL,
                                   grid_size,
                                   grid_delete = TRUE,
                                   install_rdiversity = FALSE,
                                   nprocs = 1,
                                   memory = 300){

    # install r.diversity
    if(install_rdiversity){
        system("sudo -Sk grass --exec g.extension r.diversity", input = rstudioapi::askForPassword("sudo password"))
    }

    # window
    res <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))

    if(buffer_radius/res >= 1){
        window <- 2 * round(buffer_radius/res, 0) + 1
    }else{
        stop("Buffer radius is smaller than map resolution. Choose a higher value for the buffer radius.")
    }

    # grid
    rgrass::execGRASS(cmd = "g.region", flags = "a", raster = input, res = as.character(grid_size))
    rgrass::execGRASS(cmd = "v.mkgrid", flags = c("overwrite", "quiet"), map = "grid")
    rgrass::execGRASS(cmd = "g.region", flags = "a", raster = input, res = as.character(res))

    # import grid
    v <- rgrass::read_VECT(vname = "grid", flags = "quiet")

    for(i in v$cat){

        # information
        print(paste0(i, " of ", max(v$cat)))

        # selection
        rgrass::execGRASS(cmd = "v.extract",
                          flags = c("overwrite", "quiet"),
                          input = "grid",
                          output = paste0("grid_temp", i),
                          where = paste0("cat = '", i, "'"))

        # buffer
        rgrass::execGRASS(cmd = "v.buffer",
                          flags = c("s", "overwrite", "quiet"),
                          input = paste0("grid_temp", i),
                          output = paste0("grid_temp_buf", i),
                          distance = buffer_radius)


        # region
        rgrass::execGRASS(cmd = "g.region", flags = "a", vector = paste0("grid_temp_buf", i))

        # diversity
        if(is.null(alpha)){

            rgrass::execGRASS(cmd = "r.diversity",
                              flags = c("overwrite"),
                              input = input,
                              prefix = paste0(input, output, "_diversity", i),
                              size = window,
                              method = index)

        } else{

            rgrass::execGRASS(cmd = "r.diversity",
                              flags = c("t", "overwrite"),
                              input = input,
                              prefix =paste0(input, output, "_diversity", i),
                              size = window,
                              method = index,
                              alpha = alpha)
        }

        # # region
        # rgrass::execGRASS(cmd = "g.region", flags = "a", vector = paste0("grid_temp", i))
        #
        # # calc
        # rgrass::execGRASS(cmd = "r.mapcalc",
        #                   flags = c("overwrite", "quiet"),
        #                   expression = paste0(input, output, "_diversity", i, "_", index, "_size_", window, "=", input, output, "_diversity", i, "_", index, "_size_", window))

    }

    # region
    rgrass::execGRASS(cmd = "g.region", flags = "a", raster = input, res = as.character(res))

    # patch
    files <- paste0(input, output, "_diversity", v$cat, "_", index, "_size_", window)

    if(length(files) == 1){
        rgrass::execGRASS(cmd = "g.remane", raster = paste0(input, output, "_diversity_", index, "_pct_buf", buffer_radius))

    } else{
        rgrass::execGRASS(cmd = "r.patch", flags = c("overwrite", "quiet"), input = paste0(files, collapse = ","), output = paste0(input, output, "_diversity_", index, "_pct_buf", buffer_radius), nprocs = nprocs)
    }

    # color
    rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")
    rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_diversity_", index, "_pct_buf", buffer_radius), color = "viridis")

    # clean
    rgrass::execGRASS(cmd = "g.message", message = "Cleaning vectors and rasters")
    # rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(files, collapse = ","))
    # rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "vector", name = paste0(paste0("grid_temp", v$cat), collapse = ","))
    # rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "vector", name = paste0(paste0("grid_temp_buf", v$cat), collapse = ","))

    if(grid_delete == TRUE){
        rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "vector", name = "grid")
    }

}