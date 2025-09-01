#' Calculate fragment percentage
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
#' @param nprocs `[numeric()]` \cr
#' @param memory `[numeric()]` \cr
#'
#' @example examples/lsm_percentage_example.R
#'
#' @name lsm_percentage
#' @export
lsm_percentage <- function(input,
                           output = NULL,
                           zero_as_na = FALSE,
                           buffer_radius,
                           buffer_circular = FALSE,
                           nprocs = 1,
                           memory = 300){

    # region ----
    if(region_input){
        rgrass::execGRASS("g.region", flags = "a", raster = input)
    }

    # window ----
    ## proj units ----
    proj_info <- rgrass::execGRASS("g.proj", flags = "g", intern = TRUE)
    proj_unit <- tolower(sub("units=", "", proj_info[grepl("^units=", proj_info)]))

    ## buffer ----
    if(proj_unit == "meters"){

        res <- rgrass::stringexecGRASS("g.region -p", intern = TRUE) %>%
            stringr::str_subset("nsres") %>%
            stringr::str_extract("\\d+") %>%
            as.numeric()

        if(buffer_radius/res >= 1){
            window <- 2 * round(buffer_radius/res, 0) + 1
        }else{
            stop("Buffer radius is smaller than map resolution. Please, choose a higher value for the buffer_radius.")
        }

    } else if (proj_unit == "degrees") {

        res <- rgrass::stringexecGRASS("g.region -p", intern = TRUE) %>%
            stringr::str_subset("nsres") %>%
            stringr::str_extract_all("\\d+") %>%
            unlist() %>%
            as.numeric() %>%
            {\(x) (x[1] + x[2]/60 + as.numeric(paste0(x[3], ".", x[4]))/3600) * 111320}()

        if(buffer_radius/res >= 1){
            window <- 2 * round(buffer_radius/res, 0) + 1
        }else{
            stop("Buffer radius is smaller than map resolution. Please, choose a higher value for the buffer radius.")
        }

    } else {
        warning(paste("Units:", projunits, "not currently supported"))
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

    # proportion
    if(buffer_circular == FALSE){

        rgrass::execGRASS(cmd = "g.message", message = "Calculating proportion")
        rgrass::execGRASS(cmd = "r.neighbors",
                          flags = c("overwrite", "quiet"),
                          input = paste0(input, output, "_percentage_binary"),
                          selection = paste0(input, output, "_percentage_binary"),
                          output = paste0(input, output, "_pct_buf", buffer_radius),
                          size = window,
                          nprocs = nprocs,
                          memory = memory)

    }else{

        rgrass::execGRASS(cmd = "g.message", message = "Calculating proportion")
        rgrass::execGRASS(cmd = "r.neighbors",
                          flags = c("c", "overwrite", "quiet"),
                          input = paste0(input, output, "_percentage_binary"),
                          selection = paste0(input, output, "_percentage_binary"),
                          output = paste0(input, output, "_pct_buf", buffer_radius),
                          size = window,
                          nprocs = nprocs,
                          memory = memory)

    }


    # percentage
    rgrass::execGRASS(cmd = "g.message", message = "Calculating percentage")
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite", "quiet"),
                      expression = paste0(input, output, "_pct_buf", buffer_radius, "= round(",
                                          input, output, "_pct_buf", buffer_radius, "*100)"))

    # color
    rgrass::execGRASS(cmd = "g.message", message = "Changing the raster color")
    rgrass::execGRASS(cmd = "r.colors",
                      flags = "quiet",
                      map = paste0(input, output, "_pct_buf", buffer_radius),
                      color = "forest_cover")

    # clean
    rgrass::execGRASS(cmd = "g.message", message = "Cleaning data")

    suppressWarnings(
        rgrass::execGRASS(cmd = "g.remove",
                          flags = c("b", "f", "quiet"),
                          type = "raster",
                          name = paste0(input, output, "_percentage_binary"))
    )
}
