#' Graph term pairs
#'
#' @param wos: the dataframe of references
#' @param min_n the minimum number of pairs to include in the graph
#' @param field the Web of Science field to use - TI, AB or DE (title, abstract or keyword)
#' @keywords terms
#' @export
#' @import stringr
#' @import widyr
#' @import tidyr
#' @import ggplot2
#' @import ggraph
#' @import igraph
#' @examples
#' term_graph()

term_graph  <-  function(wos, min_n = 20, field='TI') {

    data(stop_words)
    if (field == 'AB') {
        term_list  <-  wos %>% select(AB,DI) %>%
                unnest_tokens(word, AB) %>%
                anti_join(stop_words)
    }

    if (field == 'TI') {
        term_list  <-  wos %>% select(TI ,DI) %>%
                unnest_tokens(word, TI) %>%
                anti_join(stop_words)
    }

    if (field == 'DE') {
        term_list  <-  wos %>% select(DE ,DI) %>%
                unnest_tokens(word, DE) %>%
                anti_join(stop_words)
    }

    term_pairs <- term_list %>% 
      pairwise_count(word, DI, sort = TRUE, upper = FALSE)


    set.seed(1234)
    term_pairs %>%
      filter(n >= min_n) %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
      geom_node_point(color = "darkslategray4", size = 5) +
      geom_node_text(aes(label = name), repel = TRUE) +
      theme_void()

}
