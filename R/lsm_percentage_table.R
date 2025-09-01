#' Calculate percentage table
#'
#' Identifies percentage table.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat) or multi-class map inside GRASS Data Base.
#' @param output `[character=""]` \cr Map name output inside GRASS Data Base.
#' @param multi_class `[logical(1)=FALSE]` \cr
#' @param class_number_habitat `[numeric]` \cr
#'
#' @example examples/lsm_percentage_table_example.R
#'
#' @name lsm_percentage_table
#' @export
lsm_percentage_table <- function(input,
                              output = NULL,
                              multi_class = TRUE,
                              binary_habitat_classes){

    # region ----
    if(region_input){
        rgrass::execGRASS("g.region", flags = "a", raster = input)
    }

    # habitat class ----
    if(multi_class){

        rgrass::execGRASS(
            cmd = "r.mapcalc",
            flags = c("overwrite", "quiet"),
            expression = paste0(input, output, "_habitat_amount_classes = ", input))

        }else {

            rgrass::execGRASS(cmd = "g.message", message = "Converting to  map")
            if(is.empty(class_number_habitat)){
                stop("Define numbers to 'class_number_habitat'.")
            }else{
            rgrass::execGRASS(
                cmd = "r.mapcalc",
                flags = c("overwrite", "quiet"),
                expression = paste0(input, output, "_habitat_amount_classes = if(",
                                    paste(input, "==", binary_habitat_classes, collapse = "||"),
                                    ", 1, 0)"))
            }
        }

    # percentage ----
    rgrass::execGRASS(cmd = "g.message", message = "Calculationg habitat amount")
    rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "i", "p", "overwrite", "quiet"),
                  separator = ",",
                  input = paste0(input, output, "_habitat_amount_classes"),
                  output = paste0(input, output, "_percentage.csv"))

   # adjust table ----
    readr::read_csv(paste0(input, output, "_percentage.csv"),
                    col_names = c("class", "area_ha", "ncell", "perc"),
                    show_col_types = FALSE) %>%
        readr::write_csv(paste0(input, output, "_percentage.csv"))

}
