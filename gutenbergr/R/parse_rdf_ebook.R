parse_rdf_ebook <- function(node,
                            attrs = c("title", "language", "bookshelf", "rights")) {
    formats <- xml2::xml_find_all(node, "dcterms:hasFormat")

    ns <- xml2::xml_ns(node)

    other_names <- xml2::xml_name(xml2::xml_children(node), ns)
    other_names <- other_names[other_names != "dcterms:hasFormat"]
    print_names <- gsub(".*[:]", "", other_names)

    other_nodes <- xml2::xml_find_all(node, paste(other_names, collapse = " | "),
        ns = ns)
    node_text <- setNames(xml2::xml_text(other_nodes), print_names)
    node_text <- sapply(attrs, function(x) paste(node_text[print_names == x], collapse = "/"))
    as.data.frame(t(node_text))
}
