#' Calculate percentage using parallel computing
#'
#' Calculate focal ("moving window") values for each cell using the mean
#' from [r.neighbors] module and multiplies by 100.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param output `[character=""]` \cr Habitat map output name inside GRASS Data Base.
#' @param zero_as_na `[logical=""]` \cr
#' @param buffer_radius `[numeric()]` \cr Integer indicating window size.
#' @param buffer_cirular `[logical=""]` \cr
#' @param grid_size `[numeric()]` \cr Integer indicating window size.
#' @param nprocs `[numeric()]` \cr
#' @param memory `[numeric()]` \cr
#'
#' @example examples/lsm_percentage_parallel_example.R
#'
#' @name lsm_percentage_parallel
#' @export
lsm_percentage_parallel <- function(input,
                                    output = NULL,
                                    zero_as_na = FALSE,
                                    buffer_radius,
                                    buffer_circular = FALSE,
                                    grid_size,
                                    grid_delete = TRUE,
                                    nprocs = 1,
                                    memory = 300){

    # window
    res <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern=TRUE), value = TRUE)))

    if(buffer_radius/res >= 1){
        window <- 2 * round(buffer_radius/res, 0) + 1
    }else{
        stop("Buffer radius is smaller than map resolution. Choose a higher value for the buffer radius.")
    }

    # binary
    if(zero_as_na == TRUE){

        rgrass::execGRASS(cmd = "g.message", message = "Converting null as zero")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_percentage_binary = if(isnull(", input, "), 0, 1)"))
    } else{


        rgrass::execGRASS(cmd = "g.message", message = "Calculating proportion")
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = c("overwrite", "quiet"),
                          expression = paste0(input, output, "_percentage_binary = ", input))
    }

    # grid
    rgrass::execGRASS(cmd = "g.region", flags = "a", raster = paste0(input, output, "_percentage_binary"), res = as.character(grid_size))
    rgrass::execGRASS(cmd = "v.mkgrid", flags = c("overwrite", "quiet"), map = "grid")
    rgrass::execGRASS(cmd = "g.region", flags = "a", raster = paste0(input, output, "_percentage_binary"), res = as.character(res))

    # select grid
    rgrass::execGRASS(cmd = "v.rast.stats", flags = c("c", "quiet"), map = "grid", raster = input, type = "area", column_prefix = "land", method = "number")
    rgrass::execGRASS(cmd = "v.extract", flags = c("overwrite", "quiet"), input = "grid", output = "grid_sel", where = "land_number > '0'")

    # import grid
    v <- rgrass::read_VECT(vname = "grid_sel", flags = "quiet")
    v$cat2 <- 1:nrow(v)

    # calculate percentage
    for(i in v$cat){

        # information
        print(paste0(v[v$cat == i, ]$cat2, " of ", max(v$cat2)))

        # selection
        rgrass::execGRASS(cmd = "v.extract",
                          flags = c("overwrite", "quiet"),
                          input = "grid_sel",
                          output = paste0("grid_temp", i),
                          where = paste0("cat = '", i, "'"))

        # buffer
        rgrass::execGRASS(cmd = "v.buffer",
                          flags = c("s", "overwrite", "quiet"),
                          input = paste0("grid_temp", i),
                          output = paste0("grid_temp_buf", i),
                          distance = buffer_radius * 3)

        # region
        rgrass::execGRASS(cmd = "g.region", flags = "a", vector = paste0("grid_temp_buf", i))

        # percentage
        lsmetrics::lsm_percentage(input = paste0(input, output, "_percentage_binary"),
                                  output = i,
                                  buffer_radius = buffer_radius,
                                  buffer_circular = buffer_circular,
                                  nprocs = nprocs,
                                  memory = memory)

        # region
        rgrass::execGRASS(cmd = "g.region", flags = "a", vector = paste0("grid_temp", i))

        # calc
        rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite", "quiet"), expression = paste0(input, output, "_percentage_binary", i, "_pct_buf", buffer_radius, "=", input, output, "_percentage_binary", i, "_pct_buf", buffer_radius))

    }

    # region
    rgrass::execGRASS(cmd = "g.region", flags = "a", raster = paste0(input, output, "_percentage_binary"), res = as.character(res))

    # patch
    files <- paste0(paste0(input, output, "_percentage_binary", v$cat, "_pct_buf", buffer_radius), collapse = ",")
    rgrass::execGRASS(cmd = "r.patch", flags = c("overwrite", "quiet"), input = files, output = paste0(input, output, "_pct_buf", buffer_radius), nprocs = nprocs)

    # color
    rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")
    rgrass::execGRASS(cmd = "r.colors", flags = "quiet", map = paste0(input, output, "_pct_buf", buffer_radius), color = "forest_cover")

    # clean
    rgrass::execGRASS(cmd = "g.message", message = "Cleaning rasters")
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(input, output, "_percentage_binary"))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = files)
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "vector", name = paste0(paste0(input, output, "_grid_temp", v$cat), collapse = ","))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "vector", name = paste0(paste0(input, output, "_grid_temp_buf", v$cat), collapse = ","))

    if(grid_delete == TRUE){
        rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "vector", name = "grid")
    }

}
