metadata_url <- "http://www.gutenberg.org/cache/epub/feeds/rdf-files.tar.bz2"

#' Download and extract metadata
#'
#' /code{acquire_gutenberg_metadata} downloads (using curl as a default)
#' to download the metadata from Project Gutenberg. The compressed
#' file is stored in /code{tempdir()} and extracted there using
#' /code{untar}.
#'
#' @param method method passed to /code{download.file}
acquire_gutenberg_metadata <- function(method = "curl", path = tempdir()) {
    f_zip <- metadata_url
    dest <- normalizePath(file.path(path, "tmp.tar.bz2"), mustWork = FALSE)
    fl <- download.file(f_zip, destfile = dest, method = method)
    untar(dest, exdir = path)
    invisible()
}

#' Acquire and cache metadata
#'
#' /code{cache_gutenberg_metadata} calls /code{acquire_metadata}
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
cache_gutenberg_metadata <- function(lib = "~/gutenbergr") {
  if (is.null(lib)) {
    lib <- tempdir()
  }
  acquire_gutenberg_metadata()
  metadata <- parse_metadata_path(normalizePath(paste(tempdir(), "cache", "epub", sep = "/")))
  dir.create(lib, showWarnings = FALSE)
  saveRDS(metadata$work, file = file.path(lib, "work.rds"))
  saveRDS(metadata$author, file = file.path(lib, "author.rds"))
  invisible()
}

load_cached_authors <- function(lib = "~/gutenbergr") {
  readRDS(file.path(lib, "author.rds"))
}

load_cached_works <- function(lib = "~/gutenbergr") {
  readRDS(file.path(lib, "work.rds"))
}
