parse_rdf_ebook <- function(node) {

  formats <- xml2::xml_find_all(node, "dcterms:hasFormat")

  ns <- xml2::xml_ns(node)

  other_names <- xml2::xml_name(xml2::xml_children(node), ns)
  other_names <- other_names[other_names != "dcterms:hasFormat"]
  print_names <- gsub(".*[:]", "", other_names)

  other_nodes <- xml2::xml_find_all(node, paste(other_names, collapse = " | "), ns = ns)


  setNames(lapply(other_nodes, xml2::xml_text), print_names)
}
