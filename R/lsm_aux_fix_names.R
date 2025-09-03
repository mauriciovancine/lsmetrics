#' Fix names
#'
#' Fix names.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification (e.g. values 1,0 or 1,NA for habitat,non-habitat) inside GRASS Data Base.
#'
#' @example examples/lsm_aux_fix_names_example.R
#'
#' @name lsm_aux_fix_names
#' @export
lsm_aux_fix_names <- function(input){

    input_fixed_name <- input %>%
        iconv(from = "", to = "ASCII//TRANSLIT") %>%
        stringr::str_replace_all("-", "_") %>%
        stringr::str_replace_all("\\.", "_")

    suppressWarnings(
        rgrass::execGRASS(cmd = "g.rename",
                          flags = "quiet",
                          raster = paste0(input, ",", input_fixed_name),
                          ignore.stderr = TRUE)
    )
    return(input_fixed_name)
}
