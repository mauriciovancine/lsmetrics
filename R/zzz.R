.onAttach <- function(libname, pkgname) {
    packageStartupMessage("This is ", pkgname, " ", packageVersion(pkgname),
                          "\nMake sure you have installed GRASS GIS (https://grass.osgeo.org/).")
}
