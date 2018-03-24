get_combined_metadata <- function(cache_path = getOption("gutenbergr_cache")) {
    if (! exists(gutenberg_works)) {
        if (! file.exists(file.path(cache_path, "work.rds"))) {
            stop(paste("Unable to locate cached work.rds in",
                       cache_path))
        }
        load_cached_works(cache_path)
    }

    if (! exists(gutenberg_authors)) {
        if (! file.exists(file.path(cache_path, "author.rds"))) {
            stop(paste("Unable to locate cached author.rds in ",
                       cache_path))
        }
        load_cached_authors(cache_path)
    }

    inner_join(gutenberg_works, gutenberg_authors, by = "author__id")
}

download_gutenberg <- function(gutenberg_id,
                               pattern,
                               authors,
                               titles,
                               cache_path = getOption("gutenbergr_cache"),
                               output.dir,
                               sleep_time = 5) {
    if (! (type %in% c("id", "author", "title")))
        stop("type must be one of: id, author, title.")

    if (!missing(gutenberg_id))
        return(download_by_id(gutenberg_id, output.dir, sleep_time))

    meta <- get_combined_metadata(cache_path)

    if (! missing(pattern) & "title" %in% names(pattern)) {
        meta <- filter(meta, grepl(pattern["title"], title))
    } else if (! missing(titles)) {
        meta <- filter(meta, grepl(paste(titles, collapse = "|"), title))
    }

    if (! missing(pattern) & "author" %in% names(pattern)) {
        meta <- filter(meta, grepl(pattern["author"], author_name))
    } else if (! missing(authors)) {
        meta <-  filter(meta, grepl(paste(authors, collapse = "|"), author_name))
    }

    download_by_id(meta$gutenberg_id, output.dir, sleep_time)
}

download_by_id <- function(gutenberg_id, output.dir = NULL, sleep_time = 5) {
    root <-  "http://www.gutenberg.org/cache/epub"
    paths <- file.path(root, gutenberg_id, paste0("pg", gutenberg_id, ".txt"))
    ret <- lapply(paths, function(x) {
        Sys.sleep(sleep_time)
        readLines(x, skipNul = TRUE)
    })
    ret <- lapply(ret, cat, sep = "\n")
    names(ret) <- gutenberg_id
    ret        
}
