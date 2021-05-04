# ================================= PACKAGES ===================================

# Delete this section when create_xml will be included in a package with imports?
library("DiagrammeR")
library("DiagrammeRsvg")
library("dplyr")
library("rvest")
library("tidyr")
library("uuid")
library("xml2")

# ============================== MAIN FUNCTION =================================

#' Create XML document from BPMN object.
#'
#' This creates an XML document based on a BPMN object.
#'
#' @param bpmn A BPMN object as a list of data.frames.
#' @param ... Additional arguments passed to methods.
#'
#' @return An XML document.
#'
#' @author Alessio Nigro
#'
#' @import DiagrammeR
#' @import DiagrammeRsvg
#' @import dplyr
#' @import rvest
#' @import tidyverse
#' @import uuid
#' @import xml2
#'
#' @export
#'
#' @examples
#' bpmn_instance <-
#'   create_bpmn(
#'     data.frame(
#'       "id" = "id_tsk_1",
#'       "name" = "task_1",
#'       stringsAsFactors = FALSE
#'     ),
#'     data.frame(
#'       "id" = c("id_sf_1", "id_sf_2"),
#'       "name" = c("", ""),
#'       "sourceRef" = c("id_se_1", "id_tsk_1"),
#'       "targetRef" = c("id_tsk_1", "id_ee_1"),
#'       stringsAsFactors = FALSE
#'     ),
#'     data.frame(),
#'     data.frame(
#'       "id" = "id_se_1",
#'       "name" = "start_event_1",
#'       stringsAsFactors = FALSE
#'     ),
#'     data.frame(
#'       "id" = "id_ee_1",
#'       "name" = "end_event_1",
#'       stringsAsFactors = FALSE
#'     )
#'   )
#'
#' bpmn_instance_xml <- create_xml(bpmn_instance)
#' print(bpmn_instance_xml)
create_xml <- function(bpmn, ...) {
  # Defines every data structure that can be changed
  singular_of_bpmn_elements <- list(
    tasks = "task",
    sequenceFlows = "sequenceFlow",
    gateways = "gateway",
    startEvent = "startEvent",
    endEvent = "endEvent"
  )
  plural_of_bpmn_elements <- list(
    task = "tasks",
    sequenceFlow = "sequenceFlows",
    gateway = "gateways",
    startEvent = "startEvent",
    endEvent = "endEvent"
  )
  bpmn_shape_dimensions <- list(
    task = list(height = "80.0", width = "100.0"),
    gateway = list(height = "50.0", width = "50.0"),
    startEvent = list(height = "36.0", width = "36.0"),
    endEvent = list(height = "36.0", width = "36.0")
  )
  elements_empty_allowed <- c("gateways")
  attributes_to_factors <- c("gatewayType", "gatewayDirection")
  xml_attributes <-
    c("id", "name", "sourceRef", "targetRef", "gatewayDirection")
  type_attributes <- c("gatewayType")
  
  # Converts certain attributes from a factor back to character type
  for (element in names(bpmn)) {
    for (attribute in names(bpmn[[element]])) {
      if (attribute %in% attributes_to_factors) {
        bpmn[[element]][, attribute] <-
          as.character(bpmn[[element]][, attribute])
      }
    }
  }
  
  # Creates "defitions" node
  bpmn_xml <- .xml.create.definitions.node()
  
  # Creates "process" node as a child from "definitions" node
  process_node <-
    .xml.create.process.node(
      bpmn_xml,
      bpmn,
      xml_attributes,
      type_attributes,
      singular_of_bpmn_elements,
      plural_of_bpmn_elements
    )
  
  # Creates "BPMNDiagram" node as a child from "definitions" node
  BPMNDiagram_node <- .xml.create.BPMNDiagram.node(bpmn_xml,
                                                   bpmn,
                                                   process_node,
                                                   plural_of_bpmn_elements,
                                                   bpmn_shape_dimensions)
  
  return(bpmn_xml)
}

# ============================= HELPER FUNCTIONS ===============================

# Creates "defitions" node
.xml.create.definitions.node <- function() {
  # Creates new XML document and assigns root node "definitions" in one step
  bpmn_xml <- xml_new_root("definitions")
  
  # Sets namespaces and other "definitions" attributes
  xml_set_attrs(
    bpmn_xml,
    c(
      "id" = paste0("sid-", UUIDgenerate()),
      "xmlns:bpmn" = "http://www.omg.org/spec/BPMN/20100524/MODEL",
      "xmlns:bpmndi" = "http://www.omg.org/spec/BPMN/20100524/DI",
      "xmlns:dc" = "http://www.omg.org/spec/DD/20100524/DC",
      "xmlns:di" = "http://www.omg.org/spec/DD/20100524/DI",
      "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
      "targetNamespace" = "http://bupar.net",
      "exporter" = "bupaR-suite bpmnR, https://github.com/bupaverse/bpmnR",
      "exporterVersion" = "0.0.1"
    )
  )
  
  # Adds "bpmn" prefix to root node (which could not be done before namespace "bpmn" was declared)
  xml_name(bpmn_xml) <- "bpmn:definitions"
  
  return(bpmn_xml)
}

# Adds child to XML node and returns this child
.xml.add.and.return.child <-
  function(parent, child_to_add) {
    xml_add_child(parent, child_to_add)
    xml_child(parent, search = length(xml_children(parent)))
  }

# Sets attributes to, changes name of and adds possible children to the BPMN element node
.xml.modifies.bpmn.element.node <-
  function(bpmn_element_node,
           individual_bpmn_element,
           xml_attributes,
           type_attributes) {
    for (attributes in individual_bpmn_element) {
      attribute_names <- names(attributes)
      for (j in seq_along(attributes)) {
        if (attribute_names[[j]] %in% xml_attributes) {
          # Sets attribute to the BPMN element node
          xml_set_attr(bpmn_element_node,
                       attribute_names[[j]],
                       attributes[[j]])
        } else if (attribute_names[[j]] %in% type_attributes) {
          # Changes name of the BPMN element node
          xml_name(bpmn_element_node) <-
            attributes[[j]]
        } else {
          # Adds child to the BPMN element node
          xml_add_child(bpmn_element_node,
                        paste("bpmn", attribute_names[[j]], sep = ":"),
                        attributes[[j]])
        }
      }
    }
  }

# Finds incoming and outgoing sequence flows for every BPMN element
.find.incoming.outgoing.sequenceFlows <-
  function(bpmn, bpmn_element) {
    bpmn[[bpmn_element]] %>%
      left_join(bpmn[["sequenceFlows"]], by = c("id" = "targetRef")) %>%
      select(id, id.y) %>%
      rename(incoming = id.y) %>%
      left_join(bpmn[["sequenceFlows"]], by = c("id" = "sourceRef")) %>%
      select(id, incoming, id.y) %>%
      rename(outgoing = id.y)
  }

# Retrieves incoming and outgoing elements for every BPMN element node
.retrieve.incoming.outgoing.elements <-
  function(bpmn, plural_of_bpmn_elements) {
    # Finds every non-empty BPMN element
    retrieve_empty_data_frames <- as_mapper(~ nrow(.x) == 0)
    bpmn_elements_empty <- bpmn %>%
      map_lgl(retrieve_empty_data_frames)
    bpmn_elements <- names(bpmn)
    bpmn_elements <- bpmn_elements[!bpmn_elements_empty]
    
    # Retrieves plural of every non-empty BPMN element
    plural_of_bpmn_elements_non_empty <-
      plural_of_bpmn_elements[!bpmn_elements_empty]
    
    # Finds incoming and outgoing sequence flows for every non-empty BPMN element
    incoming_outgoing_elements <- bpmn_elements %>%
      map(~ .find.incoming.outgoing.sequenceFlows(bpmn, .x))
    names(incoming_outgoing_elements) <-
      names(plural_of_bpmn_elements_non_empty)
    
    # Retrieves incoming and outgoing BPMN elements for every sequence flow
    incoming_outgoing_elements[["sequenceFlow"]][["incoming"]] <-
      bpmn[["sequenceFlows"]][["sourceRef"]]
    incoming_outgoing_elements[["sequenceFlow"]][["outgoing"]] <-
      bpmn[["sequenceFlows"]][["targetRef"]]
    
    # Adds name of BPMN element to every row of the data.frame
    for (element in names(incoming_outgoing_elements)) {
      element_list <- list()
      for (i in 1:nrow(incoming_outgoing_elements[[element]])) {
        element_list[[length(element_list) + 1]] <- element
      }
      incoming_outgoing_elements[[element]][["element"]] <-
        element_list
    }
    
    return(incoming_outgoing_elements)
  }

# Creates incoming and outgoing sequence flows for every BPMN element node
.xml.create.incoming.outgoing.sequenceFlows <-
  function(bpmn,
           process_node,
           plural_of_bpmn_elements) {
    # Retrieves incoming and outgoing elements for every BPMN element node
    incoming_outgoing_elements <-
      .retrieve.incoming.outgoing.elements(bpmn, plural_of_bpmn_elements)
    
    # Adds incoming and outgoing sequence flows for every BPMN element node
    bpmn_element_nodes <- xml_children(process_node)
    for (bpmn_element_node in bpmn_element_nodes) {
      element <- xml_name(bpmn_element_node)
      if (grepl("Gateway", element, fixed = TRUE)) {
        element <- "gateway"
      }
      
      # Splits all incoming and outgoing BPMN elements into two groups
      incoming_outgoing_elements_per_individual_bpmn_element <-
        incoming_outgoing_elements[[element]][which(incoming_outgoing_elements[[element]] == xml_attr(bpmn_element_node, "id")),]
      incoming_elements <-
        unique(incoming_outgoing_elements_per_individual_bpmn_element[["incoming"]])
      outgoing_elements <-
        unique(incoming_outgoing_elements_per_individual_bpmn_element[["outgoing"]])
      
      # Adds incoming and outgoing sequence flows for the BPMN element node
      if (element != "sequenceFlow") {
        for (incoming_element in incoming_elements) {
          if (!is.na(incoming_element)) {
            xml_add_child(bpmn_element_node,
                          "bpmn:incoming",
                          incoming_element)
          }
        }
        for (outgoing_element in outgoing_elements) {
          if (!is.na(outgoing_element)) {
            xml_add_child(bpmn_element_node,
                          "bpmn:outgoing",
                          outgoing_element)
          }
        }
      }
    }
  }

# Creates BPMN element nodes as children from "process" node
.xml.create.bpmn.element.nodes <- function(bpmn,
                                           process_node,
                                           xml_attributes,
                                           type_attributes,
                                           singular_of_bpmn_elements,
                                           plural_of_bpmn_elements) {
  # Adds BPMN element nodes as children from "process" node
  for (bpmn_element in names(bpmn)) {
    transposed_bpmn_element <- transpose(bpmn[[bpmn_element]])
    for (i in seq_along(transposed_bpmn_element)) {
      individual_bpmn_element <- list(transposed_bpmn_element[[i]])
      names(individual_bpmn_element) <-
        paste("bpmn", singular_of_bpmn_elements[[bpmn_element]], sep = ":")
      
      # Adds BPMN element node as a child from "process" node
      bpmn_element_node <-
        .xml.add.and.return.child(process_node, names(individual_bpmn_element))
      
      # Sets attributes to, changes name of and adds possible children to the BPMN element node
      .xml.modifies.bpmn.element.node(bpmn_element_node,
                                      individual_bpmn_element,
                                      xml_attributes,
                                      type_attributes)
    }
  }
  
  # Creates incoming and outgoing sequence flows for every BPMN element node
  .xml.create.incoming.outgoing.sequenceFlows(bpmn, process_node, plural_of_bpmn_elements)
}

# Creates "process" node as a child from "definitions" node
.xml.create.process.node <-
  function(bpmn_xml,
           bpmn,
           xml_attributes,
           type_attributes,
           singular_of_bpmn_elements,
           plural_of_bpmn_elements) {
    # Adds "process" node as a child from "definitions" node
    process_node <-
      .xml.add.and.return.child(bpmn_xml, "bpmn:process")
    xml_set_attr(process_node, "id", paste0("sid-", UUIDgenerate()))
    
    # Creates BPMN element nodes as children from "process" node
    .xml.create.bpmn.element.nodes(
      bpmn,
      process_node,
      xml_attributes,
      type_attributes,
      singular_of_bpmn_elements,
      plural_of_bpmn_elements
    )
    
    return(process_node)
  }

# Creates data.frame of incoming and outgoing elements for every BPMN element node
.create.incoming.outgoing.elements.df <-
  function(bpmn, plural_of_bpmn_elements) {
    # Retrieves incoming and outgoing elements for every BPMN element node
    incoming_outgoing_elements <-
      .retrieve.incoming.outgoing.elements(bpmn, plural_of_bpmn_elements)
    
    # Binds the data.frames of every BPMN element into one data.frame
    incoming_outgoing_elements_df <-
      bind_rows(incoming_outgoing_elements)
  }

# Computes x and y coordinates for every BPMN element except sequence flows
.compute.bpmn.element.coordinates <- function(bpmn) {
  # Gets edges from BPMN object (we assume that all nodes are connected to at least one edge)
  edges <- bpmn$sequenceFlows
  
  # Transforms edges to long format and gives each unique id a number from 1 to n_edges, using as.numeric(factor())
  # (This "node_id" is needed for DiagrammeR.)
  edge_list_long <- edges %>%
    gather(key, original_id, sourceRef, targetRef) %>%
    mutate(node_id = as.numeric(factor(original_id)))
  
  # Creates key table that maps "original_id" to "node_id"
  node_keys <- edge_list_long %>%
    select(original_id, node_id) %>%
    unique()
  
  # Removes old id ("original_id") and recreates wide format of edges with new id ("node_id")
  edges <- edge_list_long %>%
    select(-original_id) %>%
    spread(key, node_id)
  
  # Uses "sourceRef" and "targetRef" (which are now simple ids from 1 till n) to build edge data.frame
  edge_df <-
    create_edge_df(from = edges$sourceRef, to = edges$targetRef)
  
  # Creates node data.frame with correct number of nodes (which is the number of rows in "node_keys")
  node_df <- create_node_df(nrow(node_keys))
  
  # Creates graph, sets appropriate layout options, renders graph and saves SVG/dot notation
  dot <- create_graph(node_df, edge_df) %>%
    add_global_graph_attrs(attr = "rankdir",
                           value = "LR",
                           attr_type = "graph") %>%
    add_global_graph_attrs(attr = "layout",
                           value = "dot",
                           attr_type = "graph") %>%
    render_graph() %>%
    export_svg()
  
  # Reads dot notation and selects all SVG <g> elements
  g_elements <- read_html(dot) %>%
    html_nodes("g")
  
  # Subsets nodes from "g_elements" (which are elements that have a node attribute)
  nodes <-
    g_elements[map_lgl(g_elements, ~ ("node" %in% html_attrs(.x)))]
  
  # Creates table with the numerical node id and coordinates from the node
  coordinates <- tibble(
    node_id = as.numeric(nodes %>% html_node("title") %>% html_text()),
    x = as.numeric(nodes %>% html_node("ellipse") %>% html_attr("cx")),
    y = as.numeric(nodes %>% html_node("ellipse") %>% html_attr("cy"))
  )
  
  # Joins "coordinates" with original ids
  output <- node_keys %>%
    inner_join(coordinates, by = "node_id") %>%
    # Removes "node_id" and renames "original_id" to "id"
    select(id = original_id, x, y) %>%
    # Rescales x and y (probably to be further optimized / corrected for height/width of elements)
    mutate(y = y * -5,
           x = 2 * x)
}

# Creates "Bounds" node as a child from "BPMNPShape" node
.xml.create.bounds.node <-
  function(child_BPMNPlane_node,
           bpmn_element_node,
           element,
           x_y_coordinates,
           bpmn_shape_dimensions) {
    # Adds "Bounds" node as a child from "BPMNPlane" node
    bounds_node <-
      .xml.add.and.return.child(child_BPMNPlane_node, "dc:Bounds")
    
    # Adds "BPMNLabel" node as a child from "BPMNPlane" node (which is not required)
    BPMNLabel_node <-
      .xml.add.and.return.child(child_BPMNPlane_node, "bpmndi:BPMNLabel")
    
    # Sets height, width, x and y attribute to the "Bounds" node
    xml_set_attr(bounds_node,
                 "height",
                 bpmn_shape_dimensions[[element]][["height"]])
    xml_set_attr(bounds_node,
                 "width",
                 bpmn_shape_dimensions[[element]][["width"]])
    xml_set_attr(bounds_node,
                 "x",
                 x_y_coordinates[["x"]][which(x_y_coordinates[["id"]] == xml_attr(bpmn_element_node, "id"))] - as.numeric(bpmn_shape_dimensions[[element]][["width"]]) / 2)
    xml_set_attr(bounds_node,
                 "y",
                 x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == xml_attr(bpmn_element_node, "id"))] - as.numeric(bpmn_shape_dimensions[[element]][["height"]]) / 2)
  }

# Sets coordinates of first "waypoint" node
.xml.set.coordinates.first.waypoint.node <-
  function(first_waypoint_node,
           x_y_coordinates,
           id_incoming,
           id_outgoing,
           element_incoming,
           bpmn_shape_dimensions) {
    if (element_incoming == "gateway" &&
        x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)] > x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_incoming)]) {
      # Attaches starting point of sequence flow to the top of the gateway to eventually make a 90-degree angle to the right
      x_extra <- 0
      y_extra <-
        as.numeric(bpmn_shape_dimensions[[element_incoming]][["height"]]) / 2
    } else if (element_incoming == "gateway" &&
               x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)] < x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_incoming)]) {
      # Attaches starting point of sequence flow to the bottom of the gateway to eventually make a 90-degree angle to the right
      x_extra <- 0
      y_extra <-
        -as.numeric(bpmn_shape_dimensions[[element_incoming]][["height"]]) / 2
    } else {
      # Attaches starting point of sequence flow to the right side of the element
      x_extra <-
        as.numeric(bpmn_shape_dimensions[[element_incoming]][["width"]]) / 2
      y_extra <- 0
    }
    
    # Sets coordinates of first "waypoint" node
    xml_set_attr(first_waypoint_node,
                 "x",
                 x_y_coordinates[["x"]][which(x_y_coordinates[["id"]] == id_incoming)] + x_extra)
    xml_set_attr(first_waypoint_node,
                 "y",
                 x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_incoming)] + y_extra)
  }

# Sets coordinates of second "waypoint" node
.xml.set.coordinates.second.waypoint.node <-
  function(second_waypoint_node,
           child_BPMNPlane_node,
           x_y_coordinates,
           id_outgoing,
           id_incoming,
           element_incoming,
           element_outgoing,
           bpmn_shape_dimensions) {
    if (x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)] == x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_incoming)]) {
      # Sets coordinates of second "waypoint" node by attaching end of sequence flow to the left side of the next element
      xml_set_attr(
        second_waypoint_node,
        "x",
        x_y_coordinates[["x"]][which(x_y_coordinates[["id"]] == id_outgoing)] - as.numeric(bpmn_shape_dimensions[[element_outgoing]][["width"]]) / 2
      )
      xml_set_attr(second_waypoint_node,
                   "y",
                   x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)])
      
      # Sets logical variable to FALSE (because a 90-degree angle was not needed to attach sequence flow horizontally)
      third_waypoint_node_needed <- FALSE
    } else {
      if (element_incoming == "gateway") {
        # Sets coordinates of second "waypoint" node to x coordinate of the element and y coordinate of the next element
        xml_set_attr(second_waypoint_node,
                     "x",
                     x_y_coordinates[["x"]][which(x_y_coordinates[["id"]] == id_incoming)])
        xml_set_attr(second_waypoint_node,
                     "y",
                     x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)])
      } else {
        # Sets coordinates of second "waypoint" node to x coordinate of the next element and y coordinate of the element
        xml_set_attr(second_waypoint_node,
                     "x",
                     x_y_coordinates[["x"]][which(x_y_coordinates[["id"]] == id_outgoing)])
        xml_set_attr(second_waypoint_node,
                     "y",
                     x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_incoming)])
      }
      
      # Sets logical variable to TRUE (because a 90-degree angle was needed to attach sequence flow horizontally)
      third_waypoint_node_needed <- TRUE
    }
    
    return(third_waypoint_node_needed)
  }

# Sets coordinates of third "waypoint" node
xml.set.coordinates.third.waypoint.node <-
  function(third_waypoint_node,
           child_BPMNPlane_node,
           x_y_coordinates,
           id_incoming,
           id_outgoing,
           element_outgoing,
           bpmn_shape_dimensions) {
    if (element_outgoing == "gateway") {
      # Attaches end point of sequence flow to the bottom of the gateway
      if (x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)] > x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_incoming)]) {
        x_extra <- 0
        y_extra <-
          -as.numeric(bpmn_shape_dimensions[[element_outgoing]][["height"]]) / 2
      } else {
        # Attaches end point of sequence flow to the top of the gateway
        x_extra <- 0
        y_extra <-
          as.numeric(bpmn_shape_dimensions[[element_outgoing]][["height"]]) / 2
      }
    } else {
      # Attaches end point of sequence flow to the left side of the next element
      x_extra <-
        -as.numeric(bpmn_shape_dimensions[[element_outgoing]][["width"]]) / 2
      y_extra <- 0
    }
    
    # Sets coordinates of third "waypoint" node
    xml_set_attr(third_waypoint_node,
                 "x",
                 x_y_coordinates[["x"]][which(x_y_coordinates[["id"]] == id_outgoing)] + x_extra)
    xml_set_attr(third_waypoint_node,
                 "y",
                 x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)] + y_extra)
  }

# Creates "waypoint" nodes as children from "BPMNPEdge" node
.xml.create.waypoint.nodes <-
  function(incoming_outgoing_elements_df,
           child_BPMNPlane_node,
           x_y_coordinates,
           bpmn_shape_dimensions) {
    # Retrieves necessary ids and elements to create the "waypoint" nodes
    id_sequenceFlow <-
      incoming_outgoing_elements_df[["id"]][which(incoming_outgoing_elements_df[["id"]] == xml_attr(child_BPMNPlane_node, "bpmnElement"))]
    id_incoming <-
      incoming_outgoing_elements_df[["incoming"]][which(incoming_outgoing_elements_df[["id"]] == id_sequenceFlow)]
    id_outgoing <-
      incoming_outgoing_elements_df[["outgoing"]][which(incoming_outgoing_elements_df[["id"]] == id_sequenceFlow)]
    element_incoming <-
      unique(incoming_outgoing_elements_df[["element"]][which(incoming_outgoing_elements_df[["id"]] == id_incoming)])[[1]]
    element_outgoing <-
      unique(incoming_outgoing_elements_df[["element"]][which(incoming_outgoing_elements_df[["id"]] == id_outgoing)])[[1]]
    
    # Adds two "waypoint" nodes as children from "BPMNEdge" node (because there will always be one starting point and one end point of the sequence flow)
    first_waypoint_node <-
      .xml.add.and.return.child(child_BPMNPlane_node, "di:waypoint")
    second_waypoint_node <-
      .xml.add.and.return.child(child_BPMNPlane_node, "di:waypoint")
    
    # Sets coordinates of first "waypoint" node
    .xml.set.coordinates.first.waypoint.node(
      first_waypoint_node,
      x_y_coordinates,
      id_incoming,
      id_outgoing,
      element_incoming,
      bpmn_shape_dimensions
    )
    
    # Sets coordinates of second "waypoint" node
    third_waypoint_node_needed <-
      .xml.set.coordinates.second.waypoint.node(
        second_waypoint_node,
        child_BPMNPlane_node,
        x_y_coordinates,
        id_outgoing,
        id_incoming,
        element_incoming,
        element_outgoing,
        bpmn_shape_dimensions
      )
    
    # Creates third "waypoint" node as a child from "BPMNEdge" node if needed
    if (third_waypoint_node_needed) {
      # Adds "waypoint" node as a child from "BPMNEdge" node
      third_waypoint_node <-
        .xml.add.and.return.child(child_BPMNPlane_node, "di:waypoint")
      
      # Sets coordinates of third "waypoint" node
      xml.set.coordinates.third.waypoint.node(
        third_waypoint_node,
        child_BPMNPlane_node,
        x_y_coordinates,
        id_incoming,
        id_outgoing,
        element_outgoing,
        bpmn_shape_dimensions
      )
    }
  }

# Creates "BPMNShape" and "BPMNEdge" nodes as children from "BPMNPlane" node
.xml.create.BPMNPlane.node.children <-
  function(bpmn,
           process_node,
           BPMNPlane_node,
           bpmn_shape_dimensions,
           incoming_outgoing_elements_df) {
    # Computes x and y coordinates for every BPMN element except sequence flows
    x_y_coordinates <- .compute.bpmn.element.coordinates(bpmn)
    
    # Adds "BPMNShape" or "BPMNEdge" nodes as children from "BPMNPlane" node
    bpmn_element_nodes <- xml_children(process_node)
    for (bpmn_element_node in bpmn_element_nodes) {
      element <- xml_name(bpmn_element_node)
      if (element == "sequenceFlow") {
        bpmndi_element <- "BPMNEdge"
      } else {
        bpmndi_element <- "BPMNShape"
      }
      
      # Adds "BPMNShape" or "BPMNEdge" node as a child from "BPMNPlane" node
      child_BPMNPlane_node <-
        .xml.add.and.return.child(BPMNPlane_node,
                                  paste("bpmndi", bpmndi_element, sep = ":"))
      xml_set_attr(child_BPMNPlane_node,
                   "bpmnElement",
                   xml_attr(bpmn_element_node, "id"))
      
      if (element != "sequenceFlow") {
        # Sets "isMarkerVisible" attribute to the "BPMNShape" node
        if (grepl("Gateway", element, fixed = TRUE)) {
          xml_set_attr(child_BPMNPlane_node,
                       "isMarkerVisible",
                       "true")
          # Changes "element" to "gateway" if "Gateway" is in "element"
          element <- "gateway"
        }
        
        # Creates "Bounds" node as a child from "BPMNPShape" node
        .xml.create.bounds.node(
          child_BPMNPlane_node,
          bpmn_element_node,
          element,
          x_y_coordinates,
          bpmn_shape_dimensions
        )
      } else if (element == "sequenceFlow") {
        # Creates "waypoint" nodes as children from "BPMNPEdge" node
        .xml.create.waypoint.nodes(
          incoming_outgoing_elements_df,
          child_BPMNPlane_node,
          x_y_coordinates,
          bpmn_shape_dimensions
        )
      }
    }
  }

# Creates "BPMNPlane" node as a child from "BPMNDiagram" node
.xml.create.BPMNPlane.node <-
  function(bpmn,
           process_node,
           BPMNDiagram_node,
           plural_of_bpmn_elements,
           bpmn_shape_dimensions) {
    # Adds "BPMNPlane" node as a child from "BPMNDiagram" node (which is the "BPMNDiagram" container of "BPMNShape" and "BPMNEdge")
    BPMNPlane_node <-
      .xml.add.and.return.child(BPMNDiagram_node, "bpmndi:BPMNPlane")
    xml_set_attr(BPMNPlane_node,
                 "bpmnElement",
                 xml_attr(process_node, "id"))
    xml_set_attr(BPMNPlane_node, "id", paste0("sid-", UUIDgenerate()))
    
    # Creates data.frame of incoming and outgoing elements for every BPMN element node
    incoming_outgoing_elements_df <-
      .create.incoming.outgoing.elements.df(bpmn, plural_of_bpmn_elements)
    
    # Creates "BPMNShape" and "BPMNEdge" nodes as children from "BPMNPlane" node
    .xml.create.BPMNPlane.node.children(
      bpmn,
      process_node,
      BPMNPlane_node,
      bpmn_shape_dimensions,
      incoming_outgoing_elements_df
    )
    
    return(BPMNPlane_node)
  }

# Creates "BPMNDiagram" node as a child from "definitions" node
.xml.create.BPMNDiagram.node <-
  function(bpmn_xml,
           bpmn,
           process_node,
           plural_of_bpmn_elements,
           bpmn_shape_dimensions) {
    # Adds "BPMNDiagram" node as a child from "definitions" node
    BPMNDiagram_node <-
      .xml.add.and.return.child(bpmn_xml, "bpmndi:BPMNDiagram")
    xml_set_attr(BPMNDiagram_node, "id", paste0("sid-", UUIDgenerate()))
    
    # Creates "BPMNPlane" node as a child from "BPMNDiagram" node
    BPMNPlane_node <- .xml.create.BPMNPlane.node(
      bpmn,
      process_node,
      BPMNDiagram_node,
      plural_of_bpmn_elements,
      bpmn_shape_dimensions
    )
    
    return(BPMNDiagram_node)
  }

# ================================= EXAMPLES ===================================

# Delete this section when create_xml will be included in a package?
golf_club_subscription <-
  create_bpmn(
    data.frame(
      "id" = c(
        "sid-E34BD051-438B-4804-9597-73410072D3A6",
        "sid-5D5B1DDF-B775-427D-9426-2870D4B03071",
        "sid-F3631DA4-BC36-4CC3-A471-760EB1ED1B31",
        "sid-7E6BDDB4-802C-4DF2-A5CC-74486A3A837A",
        "sid-E0EB68AD-A40C-4273-BBA4-8122245B772A",
        "sid-661F8D79-1358-4DB0-9F61-7D74B2D12347",
        "sid-8FDF7771-CDA5-47B4-AAF3-1EC6009B808B",
        "sid-F9B1515D-D5D7-4A56-92B2-800AEC4684F3",
        "sid-0A0DAE5D-EBF2-4C56-B810-3C64F64BC033"
      ),
      "name" = c(
        "Register Membership Details",
        "Send Outfit Arrival Confirmation",
        "Member&#10;Arrived",
        "Check Outfit",
        "Receive Signature",
        "Create Purchase Order",
        "Send Purchase Order",
        "Receive Parcel",
        "Check Parcel"
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = c(
        "sid-C75AC69E-AFD7-48D3-8DA8-00785F5C4D3C",
        "sid-20F3BC1F-DAF3-4D2A-B4E8-DA7A9DE9AE55",
        "sid-509F6F8B-F865-45C9-A33C-563B6D91D317",
        "sid-DB46E492-044D-4E6D-BAF4-A6982FF237FF",
        "sid-F3E2BB4E-80FB-4BF3-9DF3-2A7F11412313",
        "sid-6A2150CE-2EE9-4BA0-93D8-9629A3D9788E",
        "sid-306EE856-1A42-48AE-B053-DCAB96D1676F",
        "sid-708FEB02-67F0-4B39-981D-B1064F733131",
        "sid-C1A07F71-0415-4443-8258-7E38D6DC92D3",
        "sid-76C72008-D96B-42BD-B470-529BE4A8D368",
        "sid-506A82F3-CC53-4CAA-977F-126B3CAA76BF",
        "sid-EF45F19A-F757-4372-B0E7-91F236553C63"
      ),
      "name" = c("", "", "", "Yes", "No", "", "", "", "", "", "", ""),
      "sourceRef" = c(
        "sid-C3AC1DA1-41B7-493E-A1F7-A880E4CCA512",
        "sid-E34BD051-438B-4804-9597-73410072D3A6",
        "sid-661F8D79-1358-4DB0-9F61-7D74B2D12347",
        "sid-6E824B39-960C-4754-AE27-DBFB04D77430",
        "sid-6E824B39-960C-4754-AE27-DBFB04D77430",
        "sid-8FDF7771-CDA5-47B4-AAF3-1EC6009B808B",
        "sid-F9B1515D-D5D7-4A56-92B2-800AEC4684F3",
        "sid-0A0DAE5D-EBF2-4C56-B810-3C64F64BC033",
        "sid-5D5B1DDF-B775-427D-9426-2870D4B03071",
        "sid-F3631DA4-BC36-4CC3-A471-760EB1ED1B31",
        "sid-7E6BDDB4-802C-4DF2-A5CC-74486A3A837A",
        "sid-E0EB68AD-A40C-4273-BBA4-8122245B772A"
      ),
      "targetRef" = c(
        "sid-E34BD051-438B-4804-9597-73410072D3A6",
        "sid-6E824B39-960C-4754-AE27-DBFB04D77430",
        "sid-8FDF7771-CDA5-47B4-AAF3-1EC6009B808B",
        "sid-661F8D79-1358-4DB0-9F61-7D74B2D12347",
        "sid-36C8000A-339B-4F00-A0FA-14E7DE6AF9DD",
        "sid-F9B1515D-D5D7-4A56-92B2-800AEC4684F3",
        "sid-0A0DAE5D-EBF2-4C56-B810-3C64F64BC033",
        "sid-5D5B1DDF-B775-427D-9426-2870D4B03071",
        "sid-F3631DA4-BC36-4CC3-A471-760EB1ED1B31",
        "sid-7E6BDDB4-802C-4DF2-A5CC-74486A3A837A",
        "sid-E0EB68AD-A40C-4273-BBA4-8122245B772A",
        "sid-D40117CA-03F5-4B09-9402-23104DC9C483"
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = c("sid-6E824B39-960C-4754-AE27-DBFB04D77430"),
      "name" = c("Outfit needed?"),
      "gatewayType" = c("exclusiveGateway"),
      "gatewayDirection" = c("Diverging"),
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = "sid-C3AC1DA1-41B7-493E-A1F7-A880E4CCA512",
      "name" = "Membership Application Received",
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = c(
        "sid-36C8000A-339B-4F00-A0FA-14E7DE6AF9DD",
        "sid-D40117CA-03F5-4B09-9402-23104DC9C483"
      ),
      "name" = c(
        "Subscription Finalized",
        "Membership Completed & Outfit Delivered"
      ),
      stringsAsFactors = FALSE
    )
  )

golf_club_subscription_xml <- create_xml(golf_club_subscription)
print(golf_club_subscription_xml)

handling_of_incoming_job_applications <-
  create_bpmn(
    data.frame(
      "id" = c(
        "sid-4F0E0157-E720-4389-A0DA-FFD482BBECC4",
        "sid-E2BAC2CC-2174-44DB-B3B0-432F9F39AE01",
        "sid-B93CB368-C546-4328-A092-221C707A90AA",
        "sid-DE5E2224-C6AD-4038-95DF-111666175C3F"
      ),
      "name" = c(
        "Validate Documents",
        "Discuss Conditions",
        "Approve Job Application",
        "Interview Applicant"
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = c(
        "sid-E2EC2A5A-56D8-4375-AAB3-001A44A4C4E0",
        "sid-FE99FEE1-9E05-4050-9AF8-0D306CD7FAB7",
        "sid-F8EA8548-1319-472D-9FCE-D0D7BA9A4966",
        "sid-C3A881E9-815B-401F-8ED2-A145D6F8D5CC",
        "sid-4293D3A0-F35F-40F1-92B1-9AB539D3124A",
        "sid-E41C859E-E42A-4B7E-BE69-2C1B04322A89",
        "sid-D1010ED3-FCC3-42E2-841F-D74F11303A58",
        "sid-7AC73D76-BBBE-4115-989D-832B9C008894",
        "sid-5A9A940D-6955-48F0-8055-6C2E5526F0F7",
        "sid-E68E0EF8-CECA-4079-AE38-E527C6302392",
        "sid-C959299E-341C-4498-A877-906F872EFF57",
        "sid-A4F998F5-946F-477A-A0C9-E0C2A629C17E",
        "sid-094B2688-476E-4534-95DE-32E26F3DA13E"
      ),
      "name" = c("",
                 "",
                 "",
                 "",
                 "No",
                 "",
                 "Yes",
                 "No",
                 "No",
                 "Yes",
                 "Yes",
                 "No",
                 "Yes"),
      "sourceRef" = c(
        "sid-019B9091-517F-4DB0-BE89-F35D54C42F55",
        "sid-4F0E0157-E720-4389-A0DA-FFD482BBECC4",
        "sid-DE5E2224-C6AD-4038-95DF-111666175C3F",
        "sid-B93CB368-C546-4328-A092-221C707A90AA",
        "sid-2C382092-F364-4ACA-B542-B6F87E2522D3",
        "sid-E2BAC2CC-2174-44DB-B3B0-432F9F39AE01",
        "sid-EC2C25DE-D625-4EA2-9A65-1E085B09E181",
        "sid-EC2C25DE-D625-4EA2-9A65-1E085B09E181",
        "sid-CB862DAF-EC4B-4D81-8EC6-2E0BCFACEC92",
        "sid-CB862DAF-EC4B-4D81-8EC6-2E0BCFACEC92",
        "sid-068C0CAD-BFE0-4119-BDA7-B54A9E635170",
        "sid-068C0CAD-BFE0-4119-BDA7-B54A9E635170",
        "sid-2C382092-F364-4ACA-B542-B6F87E2522D3"
      ),
      "targetRef" = c(
        "sid-4F0E0157-E720-4389-A0DA-FFD482BBECC4",
        "sid-068C0CAD-BFE0-4119-BDA7-B54A9E635170",
        "sid-CB862DAF-EC4B-4D81-8EC6-2E0BCFACEC92",
        "sid-2C382092-F364-4ACA-B542-B6F87E2522D3",
        "sid-DDE2D417-4D2A-4A7D-A279-E503B9A88E62",
        "sid-EC2C25DE-D625-4EA2-9A65-1E085B09E181",
        "sid-B93CB368-C546-4328-A092-221C707A90AA",
        "sid-DDE2D417-4D2A-4A7D-A279-E503B9A88E62",
        "sid-DDE2D417-4D2A-4A7D-A279-E503B9A88E62",
        "sid-E2BAC2CC-2174-44DB-B3B0-432F9F39AE01",
        "sid-DE5E2224-C6AD-4038-95DF-111666175C3F",
        "sid-DDE2D417-4D2A-4A7D-A279-E503B9A88E62",
        "sid-507A3982-7898-468C-9D32-F42E46416438"
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = c(
        "sid-EC2C25DE-D625-4EA2-9A65-1E085B09E181",
        "sid-068C0CAD-BFE0-4119-BDA7-B54A9E635170",
        "sid-2C382092-F364-4ACA-B542-B6F87E2522D3",
        "sid-CB862DAF-EC4B-4D81-8EC6-2E0BCFACEC92"
      ),
      "name" = c(
        "Agreement&#10;Reached?",
        "Valid&#10;Documents?",
        "Approved?",
        "Positive &#10;Feedback?"
      ),
      "gatewayType" = c(
        "exclusiveGateway",
        "exclusiveGateway",
        "exclusiveGateway",
        "exclusiveGateway"
      ),
      "gatewayDirection" = c("Diverging",
                             "Diverging",
                             "Diverging",
                             "Diverging"),
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = "sid-019B9091-517F-4DB0-BE89-F35D54C42F55",
      "name" = "Job Application&#10;Received",
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = c(
        "sid-DDE2D417-4D2A-4A7D-A279-E503B9A88E62",
        "sid-507A3982-7898-468C-9D32-F42E46416438"
      ),
      "name" = c("Application&#10;Refused",
                 "Job Offer Sent"),
      stringsAsFactors = FALSE
    )
  )

handling_of_incoming_job_applications_xml <-
  create_xml(handling_of_incoming_job_applications)
print(handling_of_incoming_job_applications_xml)

training_and_certification_propose_training <-
  create_bpmn(
    data.frame(
      "id" = c(
        "sid-CA4655A9-1103-4C90-B7A3-FDC05B21A4DF",
        "sid-63F04EDD-4B9A-4F5D-B039-9E7DB15124EA",
        "sid-519D3C08-C80F-41D5-86BF-534454059264",
        "sid-A4486FE4-898A-4ACE-8E28-4EDB25D9CA80"
      ),
      "name" = c(
        "Provide Training Details",
        "Review Proposed Training",
        "Complete Training Details",
        "Analyse Training"
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = c(
        "sid-40604AA1-4A4D-4971-8E30-60C5C370F88F",
        "sid-4E0795AA-34EF-4B3C-BFB7-A730A139D751",
        "sid-905EE1D5-43C2-4A7F-8BF9-EDF9BCDA6937",
        "sid-56FFCF34-DA5E-424D-9C6A-FB1E84ACB51C",
        "sid-D844A582-FB49-40A3-86D8-87A7E464ADA6",
        "sid-D9783CE0-6739-40B6-83AA-C4FD78B2D31F",
        "sid-68052AB2-FCBB-4E6A-98DC-A2C247A299CA",
        "sid-97590648-C5B4-4A5B-B22A-FCFEF041E022",
        "sid-67A02DD8-8C8D-4966-89A5-E3724CBD6FC4"
      ),
      "name" = c("",
                 "",
                 "No",
                 "Yes",
                 "",
                 "",
                 "No",
                 "Yes",
                 ""),
      "sourceRef" = c(
        "sid-5138A3E5-D13E-4096-808F-A733CB53A25E",
        "sid-63F04EDD-4B9A-4F5D-B039-9E7DB15124EA",
        "sid-256C86CE-024E-4FA9-8F9C-CB79CF9B83B9",
        "sid-256C86CE-024E-4FA9-8F9C-CB79CF9B83B9",
        "sid-519D3C08-C80F-41D5-86BF-534454059264",
        "sid-A4486FE4-898A-4ACE-8E28-4EDB25D9CA80",
        "sid-193B76B3-69B8-4651-93B4-9A232F583DF8",
        "sid-193B76B3-69B8-4651-93B4-9A232F583DF8",
        "sid-CA4655A9-1103-4C90-B7A3-FDC05B21A4DF"
      ),
      "targetRef" = c(
        "sid-CA4655A9-1103-4C90-B7A3-FDC05B21A4DF",
        "sid-256C86CE-024E-4FA9-8F9C-CB79CF9B83B9",
        "sid-5C45E10B-0B75-4773-B3C5-F4229C335B2C",
        "sid-519D3C08-C80F-41D5-86BF-534454059264",
        "sid-A4486FE4-898A-4ACE-8E28-4EDB25D9CA80",
        "sid-193B76B3-69B8-4651-93B4-9A232F583DF8",
        "sid-2CB5E4EF-7650-45BA-B1EA-074F233E126A",
        "sid-6866567E-B23A-4D65-B06A-BC449EABBF78",
        "sid-63F04EDD-4B9A-4F5D-B039-9E7DB15124EA"
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = c(
        "sid-256C86CE-024E-4FA9-8F9C-CB79CF9B83B9",
        "sid-193B76B3-69B8-4651-93B4-9A232F583DF8"
      ),
      "name" = c("Training Approved?",
                 "Acceptance criteria&#10;met?"),
      "gatewayType" = c("exclusiveGateway",
                        "exclusiveGateway"),
      "gatewayDirection" = c("Diverging",
                             "Diverging"),
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = "sid-5138A3E5-D13E-4096-808F-A733CB53A25E",
      "name" = "New Training &#10;Identified",
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = c(
        "sid-5C45E10B-0B75-4773-B3C5-F4229C335B2C",
        "sid-2CB5E4EF-7650-45BA-B1EA-074F233E126A",
        "sid-6866567E-B23A-4D65-B06A-BC449EABBF78"
      ),
      "name" = c(
        "Training Rejected",
        "Training Fails &#10;Acceptance Criteria",
        "Training Accepted"
      ),
      stringsAsFactors = FALSE
    )
  )

training_and_certification_propose_training_xml <-
  create_xml(training_and_certification_propose_training)
print(training_and_certification_propose_training_xml)

test_diagram <-
  create_bpmn(
    data.frame(
      "id" = c(
        "Activity_0zcfpea",
        "Activity_0sinj4a",
        "Activity_0s2tmps",
        "Activity_0zr0ind",
        "Activity_0rw02ds",
        "Activity_0dd9jla",
        "Activity_0nopzsl"
      ),
      "name" = c("",
                 "",
                 "",
                 "",
                 "",
                 "",
                 ""),
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = c(
        "Flow_11r4tsn",
        "Flow_04tnsga",
        "Flow_0jsb0pf",
        "Flow_1sg05qw",
        "Flow_12vyrtr",
        "Flow_14kdvwk",
        "Flow_14cjlcr",
        "Flow_0t5go0n",
        "Flow_1lfvbnk",
        "Flow_1ywheut",
        "Flow_0d9wza8",
        "Flow_19h7v98",
        "Flow_1jzkx4u",
        "Flow_11mm5w3",
        "Flow_0xfld7v"
      ),
      "name" = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),
      "sourceRef" = c(
        "StartEvent_03kjntk",
        "Activity_0zcfpea",
        "Gateway_18e2vrb",
        "Gateway_18e2vrb",
        "Activity_0sinj4a",
        "Activity_0s2tmps",
        "Gateway_0cttwi8",
        "Activity_0zr0ind",
        "Gateway_0vw9hbn",
        "Gateway_0vw9hbn",
        "Gateway_0vw9hbn",
        "Activity_0rw02ds",
        "Activity_0dd9jla",
        "Activity_0nopzsl",
        "Gateway_0zfor4i"
      ),
      "targetRef" = c(
        "Activity_0zcfpea",
        "Gateway_18e2vrb",
        "Activity_0sinj4a",
        "Activity_0s2tmps",
        "Gateway_0cttwi8",
        "Gateway_0cttwi8",
        "Activity_0zr0ind",
        "Gateway_0vw9hbn",
        "Activity_0rw02ds",
        "Activity_0dd9jla",
        "Activity_0nopzsl",
        "Gateway_0zfor4i",
        "Gateway_0zfor4i",
        "Gateway_0zfor4i",
        "Event_0g4tnux"
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = c(
        "Gateway_18e2vrb",
        "Gateway_0cttwi8",
        "Gateway_0vw9hbn",
        "Gateway_0zfor4i"
      ),
      "name" = c("", "", "", ""),
      "gatewayType" = c(
        "exclusiveGateway",
        "exclusiveGateway",
        "exclusiveGateway",
        "exclusiveGateway"
      ),
      "gatewayDirection" = c("diverging", "diverging", "diverging", "diverging"),
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = "StartEvent_03kjntk",
      "name" = "",
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = "Event_0g4tnux",
      "name" = "",
      stringsAsFactors = FALSE
    )
  )

test_diagram_xml <- create_xml(test_diagram)
print(test_diagram_xml)

# ==============================================================================

write_bpmn(
  golf_club_subscription_xml,
  file.path(
    getwd(),
    "test_outputs",
    "test_create_xml",
    "golf_club_subscription_xml.bpmn"
  ),
  options = c("format", "as_xml")
)

write_bpmn(
  handling_of_incoming_job_applications_xml,
  file.path(
    getwd(),
    "test_outputs",
    "test_create_xml",
    "handling_of_incoming_job_applications_xml.bpmn"
  ),
  options = c("format", "as_xml")
)

write_bpmn(
  training_and_certification_propose_training_xml,
  file.path(
    getwd(),
    "test_outputs",
    "test_create_xml",
    "training_and_certification_propose_training_xml.bpmn"
  ),
  options = c("format", "as_xml")
)

write_bpmn(
  test_diagram_xml,
  file.path(
    getwd(),
    "test_outputs",
    "test_create_xml",
    "test_diagram_xml.bpmn"
  ),
  options = c("format", "as_xml")
)
