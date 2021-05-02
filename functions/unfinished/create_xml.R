# ================================= PACKAGES ===================================

# Delete this section when create_xml will be included in a package with imports?
library("DiagrammeR")
library("DiagrammeRsvg")
library("dplyr")
library("rvest")
library("tidyr")
library("uuid")
library("xml2")

# ============================= HELPER FUNCTIONS ===============================

# ???
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
      "exporter" = "bupaR-suite bpmn, https://github.com/bupaverse/bpmn",
      "exporterVersion" = "0.0.1"
    )
  )
  
  # Adds "bpmn" prefix to root node (which could not be done before namespace "bpmn" was declared)
  xml_name(bpmn_xml) <- "bpmn:definitions"
  
  return(bpmn_xml)
}

# ???
.xml.create.process.node <-
  function(bpmn_xml,
           bpmn,
           singular_of_bpmn_elements,
           plural_of_bpmn_elements,
           xml_attributes,
           type_attributes) {
    # Adds "process" node as a child from "definitions" node
    process_node <-
      .xml.add.and.return.child(bpmn_xml, "bpmn:process")
    xml_set_attr(process_node, "id", paste0("sid-", UUIDgenerate()))
    
    # ???
    .xml.create.bpmn.elements(
      process_node,
      bpmn,
      singular_of_bpmn_elements,
      plural_of_bpmn_elements,
      xml_attributes,
      type_attributes
    )
    
    return(process_node)
  }

# ???
.xml.create.bpmn.elements <- function(process_node,
                                      bpmn,
                                      singular_of_bpmn_elements,
                                      plural_of_bpmn_elements,
                                      xml_attributes,
                                      type_attributes) {
  # Adds BPMN elements as children from "process" node
  for (bpmn_element in names(bpmn)) {
    transposed_bpmn_element <- transpose(bpmn[[bpmn_element]])
    for (i in seq_along(transposed_bpmn_element)) {
      individual_bpmn_elements <- list(transposed_bpmn_element[[i]])
      names(individual_bpmn_elements) <-
        paste("bpmn", singular_of_bpmn_elements[[bpmn_element]], sep = ":")
      
      # Adds BPMN element node as a child from "process" node
      bpmn_element_child_process_node <-
        .xml.add.and.return.child(process_node, names(individual_bpmn_elements))
      
      # ???
      .xml.create.bpmn.element.children(
        bpmn_element_child_process_node,
        individual_bpmn_elements,
        xml_attributes,
        type_attributes
      )
    }
  }
  
  # ???
  .xml.create.incoming.outgoing.sequenceFlows(process_node, bpmn, plural_of_bpmn_elements)
}

# ???
.xml.create.bpmn.element.children <-
  function(bpmn_element_child_process_node,
           individual_bpmn_elements,
           xml_attributes,
           type_attributes) {
    # Adds attributes and ??? to the BPMN element node ???
    for (individual_bpmn_element in individual_bpmn_elements) {
      attribute_names <- names(individual_bpmn_element)
      for (j in seq_along(individual_bpmn_element)) {
        if (attribute_names[[j]] %in% xml_attributes) {
          # ???
          xml_set_attr(
            bpmn_element_child_process_node,
            attribute_names[[j]],
            individual_bpmn_element[[j]]
          )
        } else if (attribute_names[[j]] %in% type_attributes) {
          # ???
          xml_name(bpmn_element_child_process_node) <-
            individual_bpmn_element[[j]]
        } else {
          # ???
          xml_add_child(
            bpmn_element_child_process_node,
            paste("bpmn", attribute_names[[j]], sep = ":"),
            individual_bpmn_element[[j]]
          )
        }
      }
    }
  }

# ???
.find.incoming.outgoing.sequenceFlows <- function(x, bpmn) {
  bpmn[[x]] %>%
    left_join(bpmn[["sequenceFlows"]], by = c("id" = "targetRef")) %>%
    select(id, id.y) %>%
    rename(incoming = id.y) %>%
    left_join(bpmn[["sequenceFlows"]], by = c("id" = "sourceRef")) %>%
    select(id, incoming, id.y) %>%
    rename(outgoing = id.y)
}

.retrieve.incoming.outgoing.elements <-
  function(bpmn, plural_of_bpmn_elements) {
    # Checks for empty data.frames
    retrieve_empty_data_frames <- as_mapper(~ nrow(.x) == 0)
    bpmn_elements_empty <- bpmn %>%
      map_lgl(retrieve_empty_data_frames)
    
    # ???
    bpmn_elements <- names(bpmn)
    bpmn_elements <- bpmn_elements[!bpmn_elements_empty]
    
    # ???
    plural_of_bpmn_elements_without_empty <-
      plural_of_bpmn_elements[!bpmn_elements_empty]
    
    # ???
    incoming_outgoing_elements <- bpmn_elements %>%
      map(~ .find.incoming.outgoing.sequenceFlows(.x, bpmn))
    names(incoming_outgoing_elements) <-
      names(plural_of_bpmn_elements_without_empty)
    
    # ???
    incoming_outgoing_elements[["sequenceFlow"]][["incoming"]] <-
      bpmn[["sequenceFlows"]][["sourceRef"]]
    incoming_outgoing_elements[["sequenceFlow"]][["outgoing"]] <-
      bpmn[["sequenceFlows"]][["targetRef"]]
    
    # ???
    for (element in names(incoming_outgoing_elements)) {
      element_list <- list()
      my_range <- 1:nrow(incoming_outgoing_elements[[element]])
      for (i in my_range) {
        element_list[[length(element_list) + 1]] <- element
      }
      incoming_outgoing_elements[[element]][["element"]] <-
        element_list
    }
    
    return(incoming_outgoing_elements)
  }

# ???
.xml.create.incoming.outgoing.sequenceFlows <-
  function(process_node,
           bpmn,
           plural_of_bpmn_elements) {
    # ???
    incoming_outgoing_elements <-
      .retrieve.incoming.outgoing.elements(bpmn, plural_of_bpmn_elements)
    
    # ???
    children_process_node <- xml_children(process_node)
    for (child_process_node in children_process_node) {
      element <- xml_name(child_process_node)
      if (grepl("Gateway", element, fixed = TRUE)) {
        element <- "gateway"
      }
      # ???
      incoming_outgoing_elements_element <-
        incoming_outgoing_elements[[element]][which(incoming_outgoing_elements[[element]] == xml_attr(child_process_node, "id")),]
      incoming_flows_element <-
        unique(incoming_outgoing_elements_element[["incoming"]])
      outgoing_flows_element <-
        unique(incoming_outgoing_elements_element[["outgoing"]])
      
      # ???
      if (element != "sequenceFlow") {
        for (incoming_flow in incoming_flows_element) {
          if (!is.na(incoming_flow)) {
            xml_add_child(child_process_node,
                          "bpmn:incoming",
                          incoming_flow)
          }
        }
        for (outgoing_flow in outgoing_flows_element) {
          if (!is.na(outgoing_flow)) {
            xml_add_child(child_process_node,
                          "bpmn:outgoing",
                          outgoing_flow)
          }
        }
      }
    }
  }

# ???
.create.incoming.outgoing.elements.df <-
  function(bpmn, plural_of_bpmn_elements) {
    # ???
    incoming_outgoing_elements <-
      .retrieve.incoming.outgoing.elements(bpmn, plural_of_bpmn_elements)
    
    # ???
    incoming_outgoing_elements_df <-
      bind_rows(incoming_outgoing_elements)
  }

# ???
.xml.create.BPMNDiagram.node <-
  function(bpmn_xml,
           process_node,
           bpmn,
           plural_of_bpmn_elements,
           bpmn_shape_dimensions,
           x_y_coordinates,
           incoming_outgoing_elements_df) {
    # Adds "BPMNDiagram" node as a child from "definitions" node
    BPMNDiagram_node <-
      .xml.add.and.return.child(bpmn_xml, "bpmndi:BPMNDiagram")
    xml_set_attr(BPMNDiagram_node, "id", paste0("sid-", UUIDgenerate()))
    
    # ???
    BPMNPlane_node <-
      .xml.create.BPMNPlane.node(process_node, BPMNDiagram_node)
    
    # ???
    incoming_outgoing_elements_df <-
      .create.incoming.outgoing.elements.df(bpmn, plural_of_bpmn_elements)
    
    # ???
    .xml.create.BPMNPlane.children(
      process_node,
      BPMNPlane_node,
      bpmn,
      bpmn_shape_dimensions,
      x_y_coordinates,
      incoming_outgoing_elements_df
    )
    
    return(BPMNDiagram_node)
  }

# ???
.xml.create.BPMNPlane.node <-
  function(process_node, BPMNDiagram_node) {
    # Adds "BPMNPlane" node as a child from "BPMNDiagram" node, which is the BPMNDiagram container of BPMNShape and BPMNEdge
    BPMNPlane_node <-
      .xml.add.and.return.child(BPMNDiagram_node, "bpmndi:BPMNPlane")
    xml_set_attr(BPMNPlane_node,
                 "bpmnElement",
                 xml_attr(process_node, "id"))
    xml_set_attr(BPMNPlane_node, "id", paste0("sid-", UUIDgenerate()))
    
    return(BPMNPlane_node)
  }

# ???
.xml.create.BPMNPlane.children <-
  function(process_node,
           BPMNPlane_node,
           bpmn,
           bpmn_shape_dimensions,
           x_y_coordinates,
           incoming_outgoing_elements_df) {
    # ???
    x_y_coordinates <- .compute.coordinates(bpmn$sequenceFlows)
    
    # ???
    children_process_node <- xml_children(process_node)
    for (child_process_node in children_process_node) {
      # ???
      element <- xml_name(child_process_node)
      if (element == "sequenceFlow") {
        bpmndi_element <- "BPMNEdge"
      } else {
        bpmndi_element <- "BPMNShape"
      }
      
      # ???
      child_BPMNPlane_node <-
        .xml.add.and.return.child(BPMNPlane_node,
                                  paste("bpmndi", bpmndi_element, sep = ":"))
      xml_set_attr(child_BPMNPlane_node,
                   "bpmnElement",
                   xml_attr(child_process_node, "id"))
      
      # ???
      if (element != "sequenceFlow") {
        if (grepl("Gateway", element, fixed = TRUE)) {
          element <- "gateway"
          
          # ???
          xml_set_attr(child_BPMNPlane_node,
                       "isMarkerVisible",
                       "true")
        }
        
        # Adds ???
        dc_node <-
          .xml.add.and.return.child(child_BPMNPlane_node, "dc:Bounds")
        
        # Adds ???
        BPMNLabel_node <-
          .xml.add.and.return.child(child_BPMNPlane_node, "bpmndi:BPMNLabel")
        
        # ???
        xml_set_attr(dc_node,
                     "height",
                     bpmn_shape_dimensions[[element]][["height"]])
        xml_set_attr(dc_node,
                     "width",
                     bpmn_shape_dimensions[[element]][["width"]])
        xml_set_attr(dc_node,
                     "x",
                     x_y_coordinates[["x"]][which(x_y_coordinates[["id"]] == xml_attr(child_process_node, "id"))] - as.numeric(bpmn_shape_dimensions[[element]][["width"]]) / 2)
        xml_set_attr(dc_node,
                     "y",
                     x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == xml_attr(child_process_node, "id"))] - as.numeric(bpmn_shape_dimensions[[element]][["height"]]) / 2)
      } else if (element == "sequenceFlow") {
        id_sequenceFlow <-
          incoming_outgoing_elements_df[["id"]][which(
            incoming_outgoing_elements_df[["id"]] == xml_attr(child_BPMNPlane_node, "bpmnElement")
          )]
        id_incoming <-
          incoming_outgoing_elements_df[["incoming"]][which(incoming_outgoing_elements_df[["id"]] == id_sequenceFlow)]
        id_outgoing <-
          incoming_outgoing_elements_df[["outgoing"]][which(incoming_outgoing_elements_df[["id"]] == id_sequenceFlow)]
        element_incoming <-
          unique(incoming_outgoing_elements_df[["element"]][which(incoming_outgoing_elements_df[["id"]] == id_incoming)])[[1]]
        element_outgoing <-
          unique(incoming_outgoing_elements_df[["element"]][which(incoming_outgoing_elements_df[["id"]] == id_outgoing)])[[1]]
        
        xml_add_child(child_BPMNPlane_node, "di:waypoint")
        xml_add_child(child_BPMNPlane_node, "di:waypoint")
        
        children_BPMNEdge_node <-
          xml_children(child_BPMNPlane_node)
        
        count <- 0
        for (child_BPMNEdge_node in children_BPMNEdge_node) {
          if (count == 0) {
            if (element_incoming == "gateway" &&
                x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)] > x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_incoming)]) {
              x_extra <- 0
              y_extra <-
                as.numeric(bpmn_shape_dimensions[[element_incoming]][["height"]]) / 2
            } else if (element_incoming == "gateway" &&
                       x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)] < x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_incoming)]) {
              x_extra <- 0
              y_extra <-
                -as.numeric(bpmn_shape_dimensions[[element_incoming]][["height"]]) / 2
            } else {
              x_extra <-
                as.numeric(bpmn_shape_dimensions[[element_incoming]][["width"]]) / 2
              y_extra <- 0
            }
            xml_set_attr(child_BPMNEdge_node,
                         "x",
                         x_y_coordinates[["x"]][which(x_y_coordinates[["id"]] == id_incoming)] + x_extra)
            xml_set_attr(child_BPMNEdge_node,
                         "y",
                         x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_incoming)] + y_extra)
            count <- count + 1
          } else {
            if (x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)] == x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_incoming)]) {
              xml_set_attr(
                child_BPMNEdge_node,
                "x",
                x_y_coordinates[["x"]][which(x_y_coordinates[["id"]] == id_outgoing)] - as.numeric(bpmn_shape_dimensions[[element_outgoing]][["width"]]) / 2
              )
              xml_set_attr(child_BPMNEdge_node,
                           "y",
                           x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)])
            } else {
              if (element_incoming == "gateway") {
                xml_set_attr(child_BPMNEdge_node,
                             "x",
                             x_y_coordinates[["x"]][which(x_y_coordinates[["id"]] == id_incoming)])
                xml_set_attr(child_BPMNEdge_node,
                             "y",
                             x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)])
              } else {
                xml_set_attr(child_BPMNEdge_node,
                             "x",
                             x_y_coordinates[["x"]][which(x_y_coordinates[["id"]] == id_outgoing)])
                xml_set_attr(child_BPMNEdge_node,
                             "y",
                             x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_incoming)])
              }
              
              xml_add_child(child_BPMNPlane_node, "di:waypoint")
              
              children_BPMNEdge_node <-
                xml_children(child_BPMNPlane_node)
              
              if (element_outgoing == "gateway") {
                if (x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)] > x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_incoming)]) {
                  x_extra <- 0
                  y_extra <-
                    -as.numeric(bpmn_shape_dimensions[[element_outgoing]][["height"]]) / 2
                } else {
                  x_extra <- 0
                  y_extra <-
                    as.numeric(bpmn_shape_dimensions[[element_outgoing]][["height"]]) / 2
                }
              } else {
                x_extra <-
                  -as.numeric(bpmn_shape_dimensions[[element_outgoing]][["width"]]) / 2
                y_extra <- 0
              }
              
              xml_set_attr(children_BPMNEdge_node[[3]],
                           "x",
                           x_y_coordinates[["x"]][which(x_y_coordinates[["id"]] == id_outgoing)] + x_extra)
              xml_set_attr(children_BPMNEdge_node[[3]],
                           "y",
                           x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)] + y_extra)
            }
          }
        }
      }
    }
  }

# Adds child to XML node and returns this child
.xml.add.and.return.child <-
  function(parent, child_to_add) {
    xml_add_child(parent, child_to_add)
    xml_child(parent, search = length(xml_children(parent)))
  }

# ???
.compute.coordinates <- function(sequenceFlows) {
  # get edges from bpmn object (we assume that all nodes are connected to at least one edge)
  edges <- sequenceFlows
  
  # transform edges to long format
  # give each unique id a number from 1 to n_edges (using as.numeric_factor()) (this node_id is needed for diagrammer)
  edge_list_long <- edges %>%
    gather(key, original_id, sourceRef, targetRef) %>%
    mutate(node_id = as.numeric(factor(original_id)))
  
  # create key table that maps original id to node_id
  node_keys <- edge_list_long %>%
    select(original_id, node_id) %>%
    unique()
  
  # remove old id and recreate wide format of edges with new id
  edges <- edge_list_long %>%
    select(-original_id) %>%
    spread(key, node_id)
  
  # use sourceRef and targetRef (which are now simple ids from 1 till n) to build edge dataframe
  edf <-
    create_edge_df(from = edges$sourceRef, to = edges$targetRef)
  # create node dataframe with correct number of nodes (= number of rows in node_keys)
  ndf <- create_node_df(nrow(node_keys))
  
  # create graph, set appropriate layout options, render graph, and save svg/dot notation
  dot <- create_graph(ndf, edf) %>%
    add_global_graph_attrs(attr = "rankdir",
                           value = "LR",
                           attr_type = "graph") %>%
    add_global_graph_attrs(attr = "layout",
                           value = "dot",
                           attr_type = "graph") %>%
    render_graph() %>%
    export_svg()
  
  # read dot notation and select all "g" elements
  g <- read_html(dot) %>%
    html_nodes("g")
  
  # subset nodes from g. These are elements which have a node attribute
  nodes <- g[map_lgl(g, ~ ("node" %in% html_attrs(.x)))]
  
  # create table with the numerical node id and coordinates from the node
  coordinates <- tibble(
    node_id = as.numeric(nodes %>% html_node("title") %>% html_text()),
    x = as.numeric(nodes %>% html_node("ellipse") %>% html_attr("cx")),
    y = as.numeric(nodes %>% html_node("ellipse") %>% html_attr("cy"))
  )
  
  # join coordinates with original ids
  output <- node_keys %>%
    inner_join(coordinates, by = "node_id") %>%
    # remove node_id and rename original id to id
    select(id = original_id, x, y) %>%
    # rescale x and y (probably to be further optimized / corrected for height/width of elements)
    mutate(y = y * -5,
           x = 2 * x)
}

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
  
  # ??? (bpmn_xml vervangen door definitions_node?)
  bpmn_xml <- .xml.create.definitions.node()
  
  # ???
  process_node <-
    .xml.create.process.node(
      bpmn_xml,
      bpmn,
      singular_of_bpmn_elements,
      plural_of_bpmn_elements,
      xml_attributes,
      type_attributes
    )
  
  # ???
  BPMNDiagram_node <- .xml.create.BPMNDiagram.node(
    bpmn_xml,
    process_node,
    bpmn,
    plural_of_bpmn_elements,
    bpmn_shape_dimensions,
    x_y_coordinates,
    incoming_outgoing_elements_df
  )
  
  return(bpmn_xml)
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
