#' Calculate area
#'
#' Calculate area.
#'
#' @param input_null `[character=""]` Habitat map (binary classification: e.g., 1/0 or 1/NA) in GRASS.
#' @param input_id `[character=""]` Habitat map (binary classification: e.g., 1/0 or 1/NA) in GRASS.
#' @param area_round_digit `[integer]` Decimal digits for area rounding.
#' @param area_unit `[character=""]` Area unit: `"ha"`, `"m2"`, or `"km2"`.
#' @param map_ncell `[logical]` Calculate number of cells.
#' @param table_export `[logical]` Calculate number of cells.
#'
#' @example examples/lsm_aux_area_example.R
#'
#' @name lsm_aux_area
#' @export
lsm_aux_area <- function(input_null,
                         input_id,
                         area_round_digit = 0,
                         area_unit = "ha",
                         map_ncell = FALSE,
                         table_export = FALSE) {

    # options
    options(scipen = 1000)

    # count number of cells ----
    if(map_ncell){
        rgrass::execGRASS("g.message", message = "Cell counting")
        rgrass::execGRASS(cmd = "r.stats.zonal",
                          flags = c("overwrite"),
                          base = input_id,
                          cover = input_null,
                          method = "count",
                          output = paste0(sub("_null", "", input_null), "_ncell"))
    }

    # create mask ----
    rgrass::execGRASS("g.message", message = "Mask creating")
    suppressWarnings(
        rgrass::execGRASS("r.mask", flags = c("overwrite", "quiet"), raster = input_null)
    )

    # units ----
    if (area_unit == "m2") {
        exp <- paste0("area()")
    } else if (area_unit == "ha") {
        exp <- paste0("area()/10.^4")
    } else if (area_unit == "km2") {
        exp <- paste0("area()/10.^6")
    } else {
        stop("Choose a valid area_unit: 'm2', 'ha', or 'km2'")
    }

    # calculate area ----
    rgrass::execGRASS("g.message", message = "Area calculating")

    rgrass::execGRASS("r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input_null, "_area_cell = ", exp))

    rgrass::execGRASS(cmd = "r.stats.zonal",
                      flags = c("overwrite"),
                      base = input_id,
                      cover = paste0(input_null, "_area_cell"),
                      method = "sum",
                      output = paste0(sub("_null", "", input_null), "_area"))

    # round ----
    if(area_round_digit == 0){

        rgrass::execGRASS("r.mapcalc",
                          flags = "overwrite",
                          expression = paste0(sub("_null", "", input_null), "_area = int(",
                                              sub("_null", "", input_null), "_area)"))

    } else{

        rgrass::execGRASS("r.mapcalc",
                          flags = "overwrite",
                          expression = paste0(sub("_null", "", input_null), "_area = round(",
                                              sub("_null", "", input_null), "_area,",
                                              10^-area_round_digit, ")"))
    }


    # assign color ----
    rgrass::execGRASS("g.message", message = "Color assigning")
    rgrass::execGRASS(cmd = "r.colors",
                      flags = c("g", "quiet"),
                      map = paste0(sub("_null", "", input_null), "_area"),
                      color = "ryg")

    # remove mask ----
    suppressWarnings(
        rgrass::execGRASS("r.mask", flags = c("r", "quiet"))
    )

    # table ----
    rgrass::execGRASS("g.message", message = "Table exporting")
    if(table_export){
        rgrass::execGRASS(cmd = "r.stats",
                          flags = c("A", "c", "n", "quiet"),
                          input = paste0(input_id, ",",
                                         sub("_null", "", input_null), "_area"),
                          separator = ",",
                          nsteps = 1e9,
                          intern = TRUE) %>%
            tibble::as_tibble() %>%
            tidyr::separate(col = value, into = c("id", "area", "ncell"), sep = ",") %>%
            dplyr::mutate(area = round(as.numeric(area), area_round_digit)) %>%
            vroom::vroom_write(paste0(sub("_null", "", input_null), "_table_area.csv"), delim = ",")
    }

    # clean ----
    suppressWarnings(
        rgrass::execGRASS(cmd = "g.remove",
                          flags = c("f", "quiet"),
                          type = "raster",
                          name = paste0(input_null, "_area_cell"))
    )
}
