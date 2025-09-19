#' Calculate landscape diversity
#'
#' Calculate landscape diversity using [r.mapcalc] and [r.neighbors] modules.
#'
#' @param input `[character=""]` \cr Habitat map, following a binary classification
#' (e.g. values 1,0 or 1,NA for habitat,non-habitat).
#' @param output `[character=""]` \cr Habitat map output name inside GRASS Data Base.
#' @param buffer_radius `[numeric()]` \cr Integer indicating window size.
#' @param diversity_index `[character=""]` \cr Integer indicating window size.
#' @param alpha `[character=""]` \cr Integer indicating window size.
#' @param buffer_cirular `[logical=""]` \cr
#' @param nprocs `[numeric()]` \cr
#' @param memory `[numeric()]` \cr

#' @example examples/lsm_diversity_example.R
#'
#' @name lsm_diversity
#' @export
lsm_diversity <- function(input,
                          output = NULL,
                          region_input = FALSE,
                          buffer_radius,
                          buffer_circular = FALSE,
                          diversity_index,
                          alpha = NULL,
                          nprocs = 1,
                          memory = 300) {

    # region ----
    if(region_input){
        rgrass::execGRASS("g.region", flags = "a", raster = input)
    }

    # window ----
    ## proj units ----
    proj_info <- rgrass::execGRASS("g.proj", flags = c("g", "quiet"), intern = TRUE)
    proj_unit <- tolower(sub("units=", "", proj_info[grepl("^units=", proj_info)]))

    ## buffer ----
    if(proj_unit == "meters"){

        res <- rgrass::stringexecGRASS("g.region -p -quiet", intern = TRUE) %>%
            stringr::str_subset("nsres") %>%
            stringr::str_extract("\\d+") %>%
            as.numeric()

        if(buffer_radius/res >= 1){
            window <- 2 * round(buffer_radius/res, 0) + 1
        }else{
            stop("Buffer radius is smaller than map resolution. Please, choose a higher value for the buffer_radius.")
        }

    } else if (proj_unit == "degrees") {

        res <- rgrass::stringexecGRASS("g.region -p --quiet", intern = TRUE) %>%
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

    # integer ----
    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0(input, "=", "int(", input, ")"))

    # classes ----
    classes <- rgrass::execGRASS(cmd = "r.stats",
                                 flags = "n",
                                 input = input,
                                 intern = TRUE)

    # binary and proportion ----
    bin_maps <- character()
    prop_maps <- character()
    for(cl in classes) {

        bin_map <- paste0(input, "_class_", cl)
        prop_map <- paste0(input, "_prop_", cl)

        # binary
        rgrass::execGRASS(cmd = "r.mapcalc",
                          flags = "overwrite",
                          expression = paste0(bin_map, " = float(if(", input, " == ", cl, ", 1, 0))"))

        # proportion
        if(buffer_circular){
            flags_prop <- c("d", "overwrite")
        } else{
            flags_prop <- "overwrite"
        }

        rgrass::execGRASS(cmd = "r.neighbors",
                          flags = flags_prop,
                          input = bin_map,
                          output = prop_map,
                          size = window,
                          method = "average",
                          nprocs = nprocs,
                          memory = memory)

        bin_maps <- c(bin_maps, bin_map)
        prop_maps <- c(prop_maps, prop_map)
    }

    # diversity ----
    if(diversity_index %in% "shannon"){

        terms_shannon <- paste0("if(", prop_maps, ">0, ", prop_maps, "*log(", prop_maps, "), 0)")
        expr_shannon <- paste0(input, "_diversity_shannon_buffer", buffer_radius, " = -(",
                               paste(terms_shannon, collapse = " + "), ")")

        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = expr_shannon)
    }

    if(diversity_index %in% "simpson"){

        terms_simpson <- paste0("(", prop_maps, ")^2")
        expr_simpson <- paste0(input, "_diversity_simpson_buffer", buffer_radius,
                               " = 1 - (", paste(terms, collapse = " + "), ")")

        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = expr_simpson)
    }

    if(diversity_index %in% "pielou"){

        terms_shannon <- paste0("if(", prop_maps, ">0, ", prop_maps, "*log(", prop_maps, "), 0)")
        s <- length(prop_maps)
        expr_pielou <- paste0(input, "_diversity_pielou_buffer", buffer_radius,
                              " = -(", paste(terms_shannon, collapse = " + "), ")/log(", s, ")")

        rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = expr_pielou)
    }

    if(diversity_index %in% "renyi"){

        for(a in alpha){
            if(a == 1){
                message("If alpha = 1 Renyi index is not defined. (Ricotta et al., 2003, Environ. Model. Softw.)")
            } else if(a < 0){
                message("Alpha must be > 0 otherwise Renyi index is not defined. (Ricotta et al., 2003, Environ. Model. Softw.)")
            } else{
                terms_renyi <- paste0("double((", prop_maps, ")^", a, ")")
                expr_renyi <- paste0(input, "_diversity_renyi_alpha", a, "_buffer", buffer_radius,
                                     " = abs(1.0 / (1.0 - ", a, ")) * log(double(", paste(terms_renyi, collapse = " + "), "))")
                rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = expr_renyi)

            }
        }
    }

    # clean ----
    rgrass::execGRASS(cmd = "g.message", message = "Cleaning data")
    suppressWarnings(
        rgrass::execGRASS(cmd = "g.remove",
                          flags = c("b", "f", "quiet"),
                          type = "raster",
                          name = c(bin_maps, prop_maps))
    )

}
