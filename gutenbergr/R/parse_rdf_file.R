parse_rdf_file <- function(path) {
    file_xml <- xml2::read_xml(path)
    root_nodes <- xml2::xml_find_all(file_xml, "pgterms:ebook")
    lapply(root_nodes, parse_rdf_ebook)
}
