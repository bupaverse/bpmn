# ================================= PACKAGES ===================================

# Delete this section when create_xml will be included in a package with imports?
library("DiagrammeR")
library("DiagrammeRsvg")
library("rvest")
library("tidyverse")

# ============================== MAIN FUNCTION =================================

# ???
compute_coordinates <- function(sequenceFlows) {
    # get edges from bpmn object (we assume that all nodes are connected to at least one edge)
    sequenceFlows -> edges
    
    # transform edges to long format
    # give each unique id a number from 1 to n_edges (using as.numeric_factor()) (this node_id is needed for diagrammer)
    edges %>% gather(key, original_id, sourceRef, targetRef) %>%
        mutate(node_id = as.numeric(factor(original_id))) -> edge_list_long
    
    # create key table that maps original id to node_id
    edge_list_long %>%
        select(original_id, node_id) %>%
        unique() -> node_keys
    
    # remove old id and recreate wide format of edges with new id
    edge_list_long %>%
        select(-original_id) %>%
        spread(key, node_id) -> edges
    
    # use sourceRef and targetRef (which are now simple ids from 1 till n) to build edge dataframe
    create_edge_df(from = edges$sourceRef, to = edges$targetRef) -> edf
    # create node dataframe with correct number of nodes (= number of rows in node_keys)
    create_node_df(nrow(node_keys)) -> ndf
    
    # create graph, set appropriate layout options, render graph, and save svg/dot notation
    create_graph(ndf, edf) %>%
        add_global_graph_attrs(attr = "rankdir",
                               value = "LR",
                               attr_type = "graph") %>%
        add_global_graph_attrs(attr = "layout",
                               value = "dot",
                               attr_type = "graph") %>%
        render_graph() %>%
        export_svg() -> dot
    
    # read dot notation and select all "g" elements
    read_html(dot) %>%
        html_nodes("g") -> g
    
    # subset nodes from g. These are elements which have a node attribute
    nodes <- g[map_lgl(g, ~ ("node" %in% html_attrs(.x)))]
    
    # create table with the numerical node id and coordinates from the node
    tibble(
        node_id = as.numeric(nodes %>% html_node("title") %>%  html_text()),
        x = as.numeric(nodes %>% html_node("ellipse") %>% html_attr("cx")),
        y = as.numeric(nodes %>% html_node("ellipse") %>% html_attr("cy"))
    ) -> coordinates
    
    # join coordinates with original ids
    node_keys %>%
        inner_join(coordinates) %>%
        # remove node_id and rename original id to id
        select(id = original_id, x, y) %>%
        # rescale x and y (probably to be further optimized / corrected for height/width of elements)
        mutate(y = y * -5,
               x = 2 * x) -> output
}
