parse_metadata_path <- function(path, pattern = "*.rdf", parser = "parse_gutenberg_metadata") {
    file_list <- sample(list.files(path, pattern = "*.rdf", recursive = TRUE, full.names = TRUE), 100, FALSE)
    metadata <- lapply(file_list, get(parser))
    dplyr::bind_rows(metadata)
}
