metadata_url <- "http://www.gutenberg.org/cache/epub/feeds/rdf-files.tar.bz2"

#' Download and extract metadata
#'
#' /code{acquire_metadata} downloads (using curl as a default)
#' to download the metadata from Project Gutenberg. The compressed
#' file is stored in /code{tempdir()} and extracted there using
#' /code{untar}.
#'
#' @param method method passed to /code{download.file}
acquire_metadata <- function(method = "curl") {
    f_zip <- metadata_url
    dest <- normalizePath(paste(tempdir(), "tmp.tar.bz2", 
        sep = "/"), mustWork = FALSE)
    download.file(f_zip, destfile = dest, method = method)
    untar(dest, tempdir())
    invisible()
}
