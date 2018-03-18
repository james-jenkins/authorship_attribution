download_gutenberg <- function(gutenberg_id,
                               cache_path = getOption("gutenbergr_cache"),
                               type = "id",
                               output.dir = NULL,
                               sleep_time = 5) {
    if (! (type %in% c("id", "author", "title")))
        stop("type must be one of: id, author, title.")

    if (type == "id")
        return(download_by_id(gutenberg_id, output.dir))

    if (! exists("gutenberg_works")) {
        if (! file.exists(file.path(cache_path, "work.rds")))
            stop(paste("Cannot find works metadata in memory or cached at",
                       cache_path,
                       "please run cache_gutenberg_metadata() or specify a",
                       "different cache directory."))
        load_cached_works()
    }
  
    if (type == "title")
        meta <- dplyr::filter(gutenberg_authors, title %in% gutenberg_id)
    
    if (type == "author") {
        if (! exists("gutenberg_authors")) {
            if (! file.exists(file.path(cache_path, "author.rds")))
                stop(paste("Cannot find authors metadata in memory or cached at",
                           cache_path,
                           "please run cache_gutenberg_metadata() or specify a",
                           "different cache directory."))
            load_cached_authors()
        }
        meta <- dplyr::inner_join(gutenberg_works, gutenberg_authors,
                                  by = "author_id")
        id_string <- paste(gutenberg_id, collapse = "|")
        meta <- dplyr::filter(meta, grepl(id_string, author_name))
        meta <- head(meta)
    }
    ret <- download_by_id(meta$gutenberg_id, sleep_time = sleep_time)
    ret
}

download_by_id <- function(gutenberg_id, output.dir = NULL, sleep_time = 5) {
    root <-  "http://www.gutenberg.org/cache/epub"
    paths <- file.path(root, gutenberg_id, paste0("pg", gutenberg_id, ".txt"))
    ret <- lapply(paths, function(x) {
        Sys.sleep(sleep_time)
        readLines(x)
    })
    ret <- lapply(ret, paste, sep = "\\n", skipNul = TRUE)
    names(ret) <- gutenberg_id
    ret        
}
