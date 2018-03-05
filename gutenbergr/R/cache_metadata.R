#' Acquire and cache metadata
#'
#' /code{cache_metadata} calls /code{acquire_metadata}
#' to download and temporarily store the Project Gutenberg
#' metadata. It then formats and caches the metadata so
#' that it can be used again. Downloading and extracting
#' takes a long time, so it is useful to cache the data
#'
#' @param lib The location to store the cached metadata
#'
#' @return
#' @export
#'
#' @examples
cache_metadata <- function(lib = "~/gutenbergr") {
    if (is.null(lib)) {
        lib <- tempdir()
    }
    acquire_metadata()
    parse_metadata(paste(tempdir(), "cache", "epub", sep = "/"))
}
