metadata_url <- "http://www.gutenberg.org/cache/epub/feeds/rdf-files.tar.bz2"

#' Download and extract metadata
#'
#' @return
#' @export
#'
#' @examples
acquire_metadata <- function() {
    f_zip <- metadata_url
    dest <- normalizePath(paste(tempdir(), "tmp.tar.bz2", 
        sep = "/"), mustWork = FALSE)
    download.file(f_zip, destfile = dest, method = "curl")
    untar(dest, tempdir())
    invisible()
}
