parse_rdf_file <- function(path) {
    file_xml <- xml2::read_xml(path)

    root_nodes <- xml2::xml_find_all(file_xml, "pgterms:ebook")
    node_details <- lapply(root_nodes, parse_rdf_ebook)
    id <- gsub(".*/", "", xml2::xml_attr(root_nodes, "about"))
    names(node_details) <- id
    if (length(id) > 1)
      stop("Each file should only have one ebook node.")

    author_nodes <- xml2::xml_find_all(file_xml, "/rdf:RDF/rdf:Description")
    author_names <- basename(xml2::xml_attr(author_nodes, "about"))

    df <- dplyr::bind_rows(node_details, .id = "id")
    df$author <- paste(author_names, collapse = "/")
    df
}
