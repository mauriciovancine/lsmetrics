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
                                   grid_delete = FALSE,
                                   nprocs = 1,
                                   memory = 300){

    # window
    res <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", rgrass::stringexecGRASS("g.region -p", intern = TRUE), value = TRUE)))

    if(buffer_radius/res >= 1){
        window <- 2 * round(buffer_radius/res, 0) + 1
    }else{
        stop("Buffer radius is smaller than map resolution. Choose a higher value for the buffer radius.")
    }

    # grid
    rgrass::execGRASS(cmd = "g.region", flags = "a", raster = input, res = as.character(grid_size))
    rgrass::execGRASS(cmd = "v.mkgrid", flags = c("overwrite", "quiet"), map = "grid")
    rgrass::execGRASS(cmd = "g.region", flags = "a", raster = input, res = as.character(res), align = input)

    # select grid
    rgrass::execGRASS(cmd = "v.rast.stats", flags = c("c", "quiet"), map = "grid", raster = input, type = "area", column_prefix = "land", method = "number")
    rgrass::execGRASS(cmd = "v.extract", flags = c("overwrite", "quiet"), input = "grid", output = "grid_sel", where = "land_number > '0'")

    # import grid
    v <- rgrass::read_VECT(vname = "grid_sel", flags = "quiet")
    v$cat2 <- 1:nrow(v)

    # calculate diversity
    for(i in v$cat){

        # information
        print(paste0(v[v$cat == i, ]$cat2, " of ", max(v$cat2)))

        # selection
        rgrass::execGRASS(cmd = "v.extract",
                          flags = c("overwrite", "quiet"),
                          input = "grid_sel",
                          output = paste0("grid_temp", i),
                          where = paste0("cat = '", i, "'"))

        # region
        rgrass::execGRASS(cmd = "g.region", flags = "a", vector = paste0("grid_temp", i), res = as.character(res), align = input)

        # diversity
        if(Sys.info()["sysname"] == "Windows"){
            grass_config_dirname <- "GRASS8"
            grass_config_dir <- file.path(Sys.getenv("APPDATA"), grass_config_dirname, fsep = "\\")
            r_li_dir <- file.path(grass_config_dir, "r.li", fsep = "\\")
            if(!dir.exists(r_li_dir)){
                dir.create(r_li_dir)
            }
            name <- paste0("conf_diversity_", as.character(buffer_radius))
            con_file_name <- file.path(r_li_dir, name, fsep = "\\")
            output_line <- "SAMPLINGFRAME 0|0|1|1"
        } else{
            grass_config_dirname <- ".grass8"
            grass_config_dir <- file.path(Sys.getenv("HOME"), grass_config_dirname)
            r_li_dir <- file.path(grass_config_dir, "r.li")
            if(!dir.exists(r_li_dir)){
                dir.create(r_li_dir)
            }
            name <- paste0("conf_diversity_", as.character(buffer_radius))
            con_file_name <- file.path(r_li_dir, name)
            output_line <- "SAMPLINGFRAME 0|0|1|1"
        }

        rgrass::execGRASS(cmd = "v.in.region", flags = c("overwrite", "quiet"), output = paste0(input, "_region"))
        rgrass::execGRASS(cmd = "v.buffer", flags = c("overwrite", "quiet"), input = paste0(input, "_region"), output = paste0(input, "_region_buffer"), distance = buffer_radius)

        rgrass::execGRASS(cmd = "g.region", flags = "a", vector = paste0(input, "_region_buffer"), res = as.character(res), align = input)
        v_info <- rgrass::execGRASS(cmd = "v.info", flags  = "g", map = paste0(input, "_region_buffer"), intern = TRUE)
        r_info <- rgrass::execGRASS(cmd = "r.info", flags  = "g", map = input, intern = TRUE)

        north <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("north", v_info, value = TRUE)))
        south <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("south", v_info, value = TRUE)))
        west <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("west", v_info, value = TRUE)))
        east <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("east", v_info, value = TRUE)))
        nsres <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("nsres", r_info, value = TRUE)))
        ewres <- as.numeric(gsub(".*?([0-9]+).*", "\\1", grep("ewres", r_info, value = TRUE)))

        rows <- (north - south)/nsres
        columns <- (east - west)/ewres
        rv <- window/rows
        cv <- window/columns

        output_line <- paste0(output_line, "\n", paste0("SAMPLEAREA -1|-1|", rv, "|", cv))
        output_line <- paste0(output_line, "\n", "MOVINGWINDOW")

        f <- file(con_file_name, open = "wb")
        writeLines(output_line, f)
        close(f)

        if(index == "renyi"){
            rgrass::execGRASS(cmd = "r.li.renyi",
                              flags = c("overwrite"),
                              input = input,
                              output = paste0(input, i, "_diversity_", index, "_alpha", alpha, "_buffer", buffer_radius),
                              config = con_file_name,
                              alpha = alpha)
        } else{
            rgrass::execGRASS(cmd = paste0("r.li.", index),
                              flags = "overwrite",
                              input = input,
                              output = paste0(input, i, "_diversity_", index, "_buffer", buffer_radius),
                              config = con_file_name)
        }

        unlink(con_file_name)

        rgrass::execGRASS(cmd = "g.region", flags = "a", raster = input, res = as.character(res), align = input)

        rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "vector", name = paste0(input, "_region"))
        rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "vector", name = paste0(input, "_region_buffer"))

    }

    # region
    rgrass::execGRASS(cmd = "g.region", flags = "a", raster = input, res = as.character(res), align = input)

    # patch
    files <- paste0(input, v$cat, "_diversity_", index, "_buffer", buffer_radius)

    if(length(files) == 1){
            rgrass::execGRASS(cmd = "g.remane", raster = paste0(input, output, "_diversity_", index, "_buffer", buffer_radius))
    } else if(length(files) <= 4){
        rgrass::execGRASS(cmd = "r.patch", flags = c("overwrite", "quiet"), input = paste0(files, collapse = ","), output = paste0(input, output, "_diversity_", index, "_buffer", buffer_radius), nprocs = nprocs)
    } else{
        files_list <- parallel::splitIndices(length(files), ifelse(length(files) <= 4, 1, 4))
        for(i in 1:length(files_list)){
            rgrass::execGRASS(cmd = "r.patch", flags = c("overwrite", "quiet"), input = paste0(files[files_list[[i]]], collapse = ","), output = paste0(input, "_diversity_", index, "_buffer", buffer_radius, "_list", i), nprocs = nprocs)
        }
        files_list <- paste0(input, "_diversity_", index, "_buffer", buffer_radius, "_list", 1:length(files_list))
        rgrass::execGRASS(cmd = "r.patch", flags = c("overwrite", "quiet"), input = paste0(files_list, collapse = ","), output = paste0(input, output, "_diversity_", index, "_buffer", buffer_radius), nprocs = nprocs)
    }

    # clean
    rgrass::execGRASS(cmd = "g.message", message = "Cleaning vectors and rasters")
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(files, collapse = ","))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = paste0(files_list, collapse = ","))
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "vector", name = input)
    rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "vector", name = paste0("grid_temp", v$cat, collapse = ","))

    if(grid_delete == TRUE){
        rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "vector", name = "grid")
        rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "vector", name = "grid_sel")
    }

}
