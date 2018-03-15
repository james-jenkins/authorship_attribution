#' Set the package specific options for gutenbergr
#'
#' /code{set_gutenbergr_options} sets up the default options for the gutenbergr
#' package
#'
#' This function is called when the package is loaded to define the options
#' that are used throughout the package. The options are initialised with
#' default values. The available options and their defaults are as follows:
#'
#' 
#' @param pkgname the name of the package being loaded
#' @param gutenbergr_cache the location to cache gutenbergr files such as metadata
set_gutenbergr_options <- function(pkgname,
                                   gutenbergr_cache = "~/gutenbergr",
                                   gutenbergr_download_loc = tempdir()) {
    options(gutenbergr_cache = gutenbergr_cache)
    options(gutenbergr_download_loc = gutenbergr_download_loc)
    
}
