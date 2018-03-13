parse_rdf_metadata <- function(filepath) {
  file_xml <- xml2::read_xml(filepath)

  # Make sure that the xml file provided contains exactly one ebook node
  check_metadata_file(file_xml)

  # Parse the xml file to obtain different types of metadata
  work <- parse_rdf_work(file_xml)
  formats <- parse_rdf_formats(file_xml)
  author <- parse_rdf_author(file_xml)
  id <- get_rdf_id(file_xml)

  list(work = data.frame(id, work, stringsAsFactors = FALSE),
       author = data.frame(author, stringsAsFactors = FALSE))
}

check_metadata_file <- function(xml_file, root_ref = "pgterms:ebook") {
  # Count the number of ebook nodes
  ebook_nodes <- length(xml2::xml_find_all(xml_file, root_ref))

  if (ebook_nodes > 1)
    warning(paste("In", filepath, ": expected only one node of type '",
                  root_ref, "', found", ebook_nodes, ". Using the first node only."))

  if (ebook_nodes == 0)
    stop(paste("In", filepath, ": No node '", root_ref, "'. Is this an RDF/XML metadata file?"))
  invisible()
}

get_rdf_id <- function(xml_file, node_ref = "pgterms:ebook", id_attr = "about") {
  node <- xml2::xml_find_first(xml_file, node_ref)
  as.numeric(gsub(".*/", "", xml2::xml_attr(node, id_attr)))
}

get_author_id <- function(author_node) {
  author_nodes_count <- length(author_node)
  if (author_nodes_count > 1)
    warning(paste("Expected 1 author node: dcterms:creator got", author_nodes_count))

  if (author_nodes_count == 0)
    return(NA)

  author_id <- xml2::xml_find_first(author_node, "pgterms:agent")
  author_id <- xml2::xml_attr(author_id, "about")
  as.numeric(gsub(".*/.*/", "", author_id))
}

parse_rdf_author <- function(xml_file, root_ref = "pgterms:ebook") {
  # Find all nodes that match the author_ref under the root_ref We expect
  # only one and warn otherwise.
  author_node <- xml2::xml_find_all(xml_file, paste(root_ref, "dcterms:creator",
                                                    sep = "/"))

  author_id <- get_author_id(author_node)

  author <- list(
    author_name = xml2::xml_find_first(author_node, "pgterms:agent/pgterms:name"),
    author_alias = xml2::xml_find_all(author_node, "pgterms:agent/pgterms:alias"),
    author_birthdate = xml2::xml_find_first(author_node, "pgterms:agent/pgterms:birthdate"),
    author_deathdate = xml2::xml_find_first(author_node, "pgterms:agent/pgterms:deathdate")
  )
  author <- lapply(author, xml2::xml_text)
  author <- lapply(author, paste, collapse = " | ")

  data.frame(author_id, author, stringsAsFactors = FALSE)
}

parse_rdf_work <- function(xml_file, root_ref = "pgterms:ebook") {
  node <- xml2::xml_find_first(xml_file, root_ref)

  author_node <- xml2::xml_find_all(xml_file, paste(root_ref, "dcterms:creator",
                                                    sep = "/"))

  author_id <- get_author_id(author_node)

  work <- list(
    title = xml2::xml_find_all(node, "dcterms:title"),
    language = xml2::xml_find_all(node,"dcterms:language"),
    bookshelf = xml2::xml_find_all(node, "pgterms:bookshelf"),
    rights = xml2::xml_find_all(node, "dcterms:rights")
  )

  work <- lapply(work, xml2::xml_text)
  work <- lapply(work, paste, collapse = " | ")
  data.frame(work, author_id, stringsAsFactors = FALSE)
}

parse_rdf_formats <- function(xml_file) {

}

parse_metadata_path <- function(path, pattern = "*.rdf", parser = parse_rdf_metadata) {
  file_list <- list.files(path, pattern = pattern, recursive = TRUE,
                          full.names = TRUE)
  file_list <- file_list[basename(file_list) != "pg0.rdf"]
  file_list <- file_list
  metadata <- lapply(file_list, parser)
  lapply(purrr::transpose(metadata), dplyr::bind_rows, .id = NULL)
}

