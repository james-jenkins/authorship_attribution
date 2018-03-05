parse_rdf_ebook <- function(node) {
  children <- xml2::xml_children(node)

  formats <- xml2::xml_find_all(children, "//dcterms:hasFormat")

  other_names <- xml2::xml_name(children, ns = xml_ns(children))
  other_names <- other_names[other_names != "dcterms:hasFormat"]
  other_nodes <- xml2::xml_find_all(children, paste0("//", paste(other_names, collapse = " | //")))
  setNames(lapply(other_nodes, xml2::xml_text), other_names)
}
