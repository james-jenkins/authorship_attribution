parse_metadata <- function(path) {
  file_list <- list.files(path, pattern = "*.rdf", recursive = TRUE,
                          full.names = TRUE)
  lapply(file_list, parse_rdf_file)
}
