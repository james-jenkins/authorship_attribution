parse_gutenberg_metadata <- function(path) {
    file_xml <- xml2::read_xml(path)

    ebook_nodes <- length(xml2::xml_find_all(file_xml, "pgterms:ebook"))
    if (ebook_nodes > 1)
      warning(paste(
        "In",
        path,
        ": expected only one node of type 'pgterms:ebook', found",
        ebook_nodes,
        ". Using the first node only."
        ))

    ebook_node <- xml2::xml_find_first(file_xml, "pgterms:ebook")
    node_details <- parse_rdf_ebook(ebook_node)
    node_details$id <- gsub(".*/", "", xml2::xml_attr(ebook_node, "about"))
    node_details
}
