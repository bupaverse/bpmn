# ================================= PACKAGES ===================================

library("htmlwidgets")
library("xml2")

# ============================== MAIN FUNCTION =================================

# ???
render_bpmn <-
  function(bpmn_model,
           viewer.suppress = FALSE,
           width = NULL,
           height = NULL,
           elementId = NULL,
           xml_version_number = "1.0",
           xml_encoding_declaration = "UTF-8") {
    # ???
    xml_declaration <-
      paste0(
        '<?xml version="',
        xml_version_number,
        '" encoding="',
        xml_encoding_declaration,
        '"?>'
      )
    
    # ???
    if (inherits(bpmn_model, "xml_document")) {
      bpmn_model <- as.character(bpmn_model)
    } else if (inherits(bpmn_model, "character") &&
               substring(bpmn_model, 1, 38) != xml_declaration) {
      # this must be a file name
      bpmn_model <- as.character(read_xml(bpmn_model))
    }
    
    # forward options using x
    x <- list(bpmn_model = bpmn_model)
    
    # create widget
    htmlwidgets::createWidget(
      name = "render_bpmn",
      x,
      width = width,
      height = height,
      package = "bpmn",
      elementId = elementId,
      sizingPolicy(
        viewer.fill = TRUE,
        viewer.suppress = viewer.suppress,
        browser.fill = TRUE
      )
    )
  }

#' Shiny bindings for render_bpmn
#'
#' Output and render functions for using render_bpmn within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a render_bpmn
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name render_bpmn-shiny
#'
#' @export
render_bpmnOutput <-
  function(outputId,
           width = '100%',
           height = '400px') {
    htmlwidgets::shinyWidgetOutput(outputId, 'render_bpmn', width, height, package = 'bpmn')
  }

#' @rdname render_bpmn-shiny
#' @export
renderRender_bpmn <-
  function(expr,
           env = parent.frame(),
           quoted = FALSE) {
    if (!quoted) {
      expr <- substitute(expr)
    } # force quoted
    htmlwidgets::shinyRenderWidget(expr, render_bpmnOutput, env, quoted = TRUE)
  }

# ================================= EXAMPLES ===================================

# Delete this section when render_bpmn will be included in a package?
bpmn_file_path <-
  file.path(getwd(), "example_models", "Golf Club Subscription.bpmn")
render_bpmn(bpmn_file_path)

golf_club_subscription_bpmn_file <-
  read_xml(
    file.path(
      getwd(),
      "test_outputs",
      "test_create_xml",
      "golf_club_subscription_xml.bpmn"
    )
  )
render_bpmn(golf_club_subscription_bpmn_file)

handling_of_incoming_job_applications_bpmn_file <-
  read_xml(
    file.path(
      getwd(),
      "test_outputs",
      "test_create_xml",
      "handling_of_incoming_job_applications_xml.bpmn"
    )
  )
render_bpmn(handling_of_incoming_job_applications_bpmn_file)

training_and_certification_propose_training_bpmn_file <-
  read_xml(
    file.path(
      getwd(),
      "test_outputs",
      "test_create_xml",
      "training_and_certification_propose_training_xml.bpmn"
    )
  )
render_bpmn(training_and_certification_propose_training_bpmn_file)

test_diagram_bpmn_file <-
  read_xml(file.path(
    getwd(),
    "test_outputs",
    "test_create_xml",
    "test_diagram_xml.bpmn"
  ))
render_bpmn(test_diagram_bpmn_file, viewer.suppress = TRUE)
