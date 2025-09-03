#!/usr/bin/env Rscript

# -------------------------------------------------------------------------
# MODULE:    r.grow (via rgrass)
# AUTHOR(S): adaptação para R (baseado em Glynn Clements)
# PURPOSE:   Fast replacement for r.grow using r.grow.distance
# -------------------------------------------------------------------------

# Função principal
r_grow <- function(input,
                   output,
                   radius = 1.01,
                   metric = "euclidean",
                   old = NULL,
                   new = NULL,
                   mapunits = FALSE) {

    library(rgrass)

    tmp <- Sys.getpid()
    temp_dist <- sprintf("r.grow.tmp.%s.dist", tmp)
    temp_val  <- NULL

    shrink <- FALSE
    if (radius < 0) {
        shrink <- TRUE
        radius <- abs(radius)
    }

    if (is.null(new) && !shrink) {
        temp_val <- sprintf("r.grow.tmp.%s.val", tmp)
        new <- temp_val
    }

    if (is.null(old)) {
        old <- input
    }

    # Ajustar unidade do raio
    if (!mapunits) {
        kv <- execGRASS("g.region", flags = "g", intern = TRUE)
        kv <- as.list(strsplit(kv, "="))
        kv <- setNames(sapply(kv, `[`, 2), sapply(kv, `[`, 1))
        scale <- sqrt(as.numeric(kv["nsres"]) * as.numeric(kv["ewres"]))
        radius <- radius * scale
    }

    if (metric == "euclidean") {
        metric <- "squared"
        radius <- radius^2
    }

    # Checar existência do input
    input_check <- execGRASS("g.findfile", element = "cell", file = input, intern = TRUE)
    if (!any(grepl("file=", input_check))) {
        stop(sprintf("Raster map <%s> not found", input))
    }

    # Crescer ou encolher
    if (!shrink) {
        execGRASS("r.grow.distance",
                  input = input,
                  metric = metric,
                  distance = temp_dist,
                  value = temp_val)

        mapcalc_expr <- sprintf(
            '%s = if(!isnull(%s), %s, if(%s < %f, %s, null()))',
            output, input, old, temp_dist, radius, new
        )
        execGRASS("r.mapcalc", expression = mapcalc_expr)

    } else {
        execGRASS("r.grow.distance",
                  input = input,
                  metric = metric,
                  distance = temp_dist,
                  value = temp_val,
                  flags = "n")

        mapcalc_expr <- sprintf(
            '%s = if(isnull(%s), %s, if(%s < %f, null(), %s))',
            output, temp_dist, old, temp_dist, radius, old
        )
        execGRASS("r.mapcalc", expression = mapcalc_expr)
    }

    # Cores e histórico
    execGRASS("r.colors", map = output, raster = input)
    execGRASS("r.support", map = output, history = "r.grow wrapper via rgrass")

    # Limpeza
    cleanup(c(temp_dist, temp_val))

    return(invisible(TRUE))
}

# Exemplo de uso:
# initGRASS(gisBase = "/usr/lib/grass", location = "mylocation", mapset = "PERMANENT")
# r_grow("input_raster", "output_raster", radius = 2, metric = "euclidean")
