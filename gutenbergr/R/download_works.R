#' Combine metadata for works and authors
#'
#' \code{get_combined_metadata} checks that the metadata files
#' for author and work are loaded; if not, their cache files
#' are loaded from \code{cache_path}. The two metadata tables
#' are joined to get complete metadata about each work.
#'
#' @param cache_path location of cached metadata.
#' 
get_combined_metadata <- function(cache_path = getOption("gutenbergr_cache")) {
    if (! exists("gutenberg_works")) {
        if (! file.exists(file.path(cache_path, "work.rds"))) {
            stop(paste("Unable to locate cached work.rds in",
                       cache_path))
        }
        load_cached_works(cache_path)
    }

    if (! exists("gutenberg_authors")) {
        if (! file.exists(file.path(cache_path, "author.rds"))) {
            stop(paste("Unable to locate cached author.rds in ",
                       cache_path))
        }
        load_cached_authors(cache_path)
    }

    dplyr::inner_join(gutenberg_works, gutenberg_authors, by = "author_id")
}

#' Get download path to gutenbergr work
#'
#' \code{get_gutenberg_paths} uses the loaded or cached metadata
#' about the formats of Gutenberg works to determine the url of
#' the file corresponding to a work and file format.
#' 
#' @param id a numeric vector of Project Gutenberg work ids
#' @param file_format file format to download
#' @param zip download zipped format instead of raw if available
#' @param cache_path location of cached metadata
#'
#' @export
#'
#' @examples
#'
#' get_gutenberg_paths(76)
#' get_gutenberg_paths(76, zip = FALSE)
#'
#' get_gutenberg_paths(1:10, "text/html", zip = FALSE)
get_gutenberg_paths <- function(id, file_format = "text/plain", zip = TRUE,
                             cache_path = getOption("gutenbergr_cache")) {
    if (! exists("gutenberg_formats")) {
        if (! file.exists(file.path(cache_path, "format.rds"))) {
            stop(paste("Unable to locate cached format.rds in ",
                       cache_path))
        }
        load_cached_formats(cache_path)
    }

    format_files <- dplyr::filter(gutenberg_formats,
                                  gutenberg_id %in% id,
                                  grepl(file_format, format))
    if (zip & any(grepl("zip", format_files$format)))
        format_files <- dplyr::filter(format_files, grepl("zip", format))
    if (!zip)
        format_files <- dplyr::filter(format_files, !grepl("zip", format))
    
    dplyr::pull(format_files, file)
}

#' Download gutenberg work by id, title pattern, author, or title
#'
#' @param gutenberg_id numeric vector of Project Gutenberg work ids
#' @param pattern named character vector of search patterns to download
#' @param authors character vector of authors to download
#' @param titles character vector of titles to download
#' @param cache_path location of cached metadata
#' @param ... other parameters passed to \code{download_by_id}
#'
#' @export
#' 
#' @examples
#' 
download_gutenberg <- function(gutenberg_id,
                               pattern,
                               authors,
                               titles,
                               cache_path = getOption("gutenbergr_cache"),
                               ...) {

    if (!missing(gutenberg_id))
        return(download_by_id(gutenberg_id, ...))

    meta <- get_combined_metadata(cache_path)

    if (!missing(pattern) & "title" %in% names(pattern)) {
        meta <- dplyr::filter(meta, grepl(pattern["title"], title))
    } else if (!missing(titles)) {
        meta <- dplyr::filter(meta, grepl(paste(titles, collapse = "|"), title))
    }

    if (! missing(pattern) & "author" %in% names(pattern)) {
        meta <- dplyr::filter(meta, grepl(pattern["author"], author_name))
    } else if (! missing(authors)) {
        meta <-  dplyr::filter(meta, grepl(paste(authors, collapse = "|"), author_name))
    }

    download_by_id(meta$gutenberg_id, ...)
}

#' Download gutenberg work by id and format
#'
#' \code{download_by_id} performs the actual downloading  of a work's text
#' from Project Gutenberg. Given an id and file format, the file is downloaded
#' and either stored in an output directory or returned to the console.
#'
#' @param id a numeric vector of Project Gutenberg work ids
#' @param output.dir 
#' @param sleep_time time to sleep between file downloads, in seconds
#' @param file_format format of file to download
#' @param zip download a zip file instead of raw if available
download_by_id <- function(id, output.dir = NULL,
                           sleep_time = 5, file_format = "text/plain",
                           zip = FALSE) {
    file_paths <- get_gutenberg_paths(id, file_format)
    ret <- lapply(file_paths, function(x) {
        Sys.sleep(sleep_time)
        readLines(x, skipNul = TRUE)
    })
    ret <- lapply(ret, paste, sep = "\n")
    names(ret) <- gutenberg_id
    ret        
}
