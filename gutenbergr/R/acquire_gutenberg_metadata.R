metadata_url <- "http://www.gutenberg.org/cache/epub/feeds/rdf-files.tar.bz2"

#' Download and extract metadata
#'
#' /code{acquire_gutenberg_metadata} downloads (using curl as a default)
#' to download the metadata from Project Gutenberg. The compressed
#' file is stored in /code{tempdir()} and extracted there using
#' /code{untar}.
#'
#' @param method method passed to /code{download.file}
#' @param path location to store downloaded files
acquire_gutenberg_metadata <- function(method = "curl",
                                       path = getOption("gutenbergr_download_loc")) {
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
#' @param create_dir Should the directory be created if it doesn't exist?
#' 
#' @export
#'
#' @examples
#' cache_gutenberg_metadata()
#' 
#' cache_gutenberg_metadata("~/gutenbergr")
#'
#' options(gutenbergr_cache = "~/gutenbergr")
#' cache_gutenberg_metadata()
cache_gutenberg_metadata <- function(lib = getOption("gutenbergr_cache"),
                                     create_dir = TRUE) {
  if (is.null(lib)) {
    lib <- tempdir()
  }
  acquire_gutenberg_metadata()
  metadata <- parse_metadata_path(normalizePath(paste(tempdir(), "cache", "epub", sep = "/")))
  if (create_dir)
      dir.create(lib, showWarnings = FALSE)
  saveRDS(metadata$work, file = file.path(lib, "work.rds"))
  saveRDS(metadata$author, file = file.path(lib, "author.rds"))
  invisible()
}


#' Load cached author metadata into memory
#'
#' @param lib the directory containing cached metadata. Generated with \code{cache_gutenberg_metadata}
#' @param ret should the metadata data.frame be returned
#'
#' @export
#'
#' @examples
#' load_cached_authors()
#'
#' load_cached_authors("~/gutenbergr")
#'
#' options(gutenbergr_cache = "~/gutenbergr")
#' load_cached_authors()
#'
#' gutenberg_authors <- load_cached_authors(ret = TRUE)
load_cached_authors <- function(lib = getOption("gutenbergr_cache"),
                                ret = FALSE) {
    fp <- file.path(lib, "author.rds")

    if (! file.exists(fp))
        stop("Cannot find 'author.rds'. Was the directory created with cache_gutneberg_metadata?")
    authors <- readRDS(fp)
    assign(
        "gutenberg_authors",
        authors,
        envir = as.environment("package:gutenbergr")
    )
    if (! ret)
        return(invisible())
    authors
}

#' Load cached work metadata into memory
#'
#' @param lib the directory containing cached metadata. Generated with \code{cache_gutenberg_metadata}
#' @param ret should the metadata data.frame be returned
#'
#' @export
#'
#' @examples 
#' load_cached_works()
#'
#' load_cached_works("~/gutenbergr")
#'
#' options(gutenbergr_cache = "~/gutenbergr")
#' load_cached_works()
#'
#' gutenberg_works <- load_cached_works(ret = TRUE)
load_cached_works <- function(lib = getOption("gutenbergr_cache"),
                              ret = FALSE) {
    fp <- file.path(lib, "work.rds")

    if (! file.exists(fp))
        stop("Cannot find 'work.rds'. Was the directory created with cache_gutneberg_metadata?")
    works <- readRDS(fp)
  assign(
      "gutenberg_works",
      works,
      envir = as.environment("package:gutenbergr")
  )
    if (! ret)
        return(invisible())
  works
}
