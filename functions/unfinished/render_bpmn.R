# ================================= PACKAGES ===================================

library("htmlwidgets")
library("xml2")

# ============================== MAIN FUNCTION =================================

#' Render BPMN diagram.
#'
#' This renders a BPMN diagram based on an XML document.
#'
#' @param bpmn An XML document, a string, a connection, or a raw vector. ???
#'
#'   A string can be either a path, a url or literal xml. Urls will be converted
#'   into connections either using \code{base::url} or, if installed,
#'   \code{curl::curl}. Local paths ending in \code{.gz}, \code{.bz2},
#'   \code{.xz}, \code{.zip} will be automatically uncompressed.
#'
#'   If a connection, the complete connection is read into a raw vector before
#'   being parsed.
#' @param viewer.suppress Never display the widget within the RStudio Viewer
#'   (useful for widgets that require a large amount of space for rendering).
#'   Defaults to FALSE.
#' @param width Fixed width for widget (in css units). The default is NULL,
#'   which results in intelligent automatic sizing based on the widget's
#'   container.
#' @param height Fixed height for widget (in css units). The default is NULL,
#'   which results in intelligent automatic sizing based on the widget's
#'   container.
#' @param elementId Use an explicit element ID for the widget (rather than an
#'   automatically generated one). Useful if you have other JavaScript that
#'   needs to explicitly discover and interact with a specific widget instance.
#' @param xml_version_number The version of the XML standard used.
#' @param xml_encoding_declaration The character encoding used in the XML
#'   declaration. \sQuote{UTF-8} is the default encoding used.
#' @param ... Additional arguments passed to methods.
#'
#' @author Alessio Nigro
#'
#' @import htmlwidgets
#' @import xml2
#'
#' @export
render_bpmn <- function(bpmn,
                        viewer.suppress = FALSE,
                        width = NULL,
                        height = NULL,
                        elementId = NULL,
                        xml_version_number = "1.0",
                        xml_encoding_declaration = "UTF-8",
                        ...) {
  UseMethod("render_bpmn")
}

#' @rdname render_bpmn
#' @export
render_bpmn.bpmn <-
  function(bpmn,
           viewer.suppress = FALSE,
           width = NULL,
           height = NULL,
           elementId = NULL,
           xml_version_number = "1.0",
           xml_encoding_declaration = "UTF-8",
           ...) {
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
    if (inherits(bpmn, "bpmn")) {
      bpmn_model <- as.character(bpmn[["xml"]])
    } else if (inherits(bpmn, "character") &&
               substring(bpmn, 1, 38) != xml_declaration) {
      # this must be a file name
      bpmn_model <- as.character(read_xml(bpmn))
    } else {
      bpmn_model <- as.character(bpmn)
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
           width = "100%",
           height = "400px") {
    htmlwidgets::shinyWidgetOutput(outputId, "render_bpmn", width, height, package = "bpmn")
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