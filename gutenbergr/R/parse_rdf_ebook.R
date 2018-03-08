parse_rdf_ebook <- function(node,
                            attrs = c("title", "language", "bookshelf", "rights"),
                            author = TRUE) {

  formats <- xml2::xml_find_all(node, "dcterms:hasFormat")
  author_node  <- xml2::xml_find_all(node, "dcterms:creator")

    ns <- xml2::xml_ns(node)

    other_names <- xml2::xml_name(xml2::xml_children(node), ns)
    other_names <- other_names[other_names != "dcterms:hasFormat"]
    other_names <- other_names[other_names != "dcterms:creator"]
    print_names <- gsub(".*[:]", "", other_names)
    other_nodes <- xml2::xml_find_all(node, paste(other_names, collapse = " | "),
        ns = ns)
    node_text <- setNames(xml2::xml_text(other_nodes), print_names)
    node_text <- sapply(attrs, function(x) paste(node_text[print_names == x], collapse = "/"))
    df <- data.frame(t(node_text))

    author_id <- xml2::xml_find_first(author_node, "pgterms:agent")
    author_id <- xml2::xml_attr(author_id, "about")
    author_id <- as.numeric(gsub(".*/.*/", "", author_id))
    if(is.null(author_id))
      author_id <- NA
    author_name   <- xml2::xml_find_first(author_node, "pgterms:agent/pgterms:name")
    author_name   <- xml2::xml_text(author_name)
    if(is.null(author_name))
      author_name <- NA
    author_df <- data.frame(author_id, author_name)


    if (author)
      df <- dplyr::bind_cols(df, author_df)
    df
}
