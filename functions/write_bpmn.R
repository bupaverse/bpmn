# ================================= PACKAGES ===================================

# Delete this section when write_bpmn will be included in a package with imports?
library("xml2")

# ============================== MAIN FUNCTION =================================

#' Write XML or HTML to disk.
#'
#' This writes out both XML and normalised HTML. The default behavior will
#' output the same format which was read. If you want to force output pass
#' `option = "as_xml"` or `option = "as_html"` respectively.
#'
#' @param x A document or node to write to disk. It's not possible to
#'   save nodesets containing more than one node.
#' @param file Path to file or connection to write to.
#' @param ... Additional arguments passed to methods.
#' @param options default: \sQuote{format}. Zero or more of
#' \Sexpr[results=rd]{xml2:::describe_options(xml2:::xml_save_options())}
#' @param encoding The character encoding to use in the document. The default
#' encoding is \sQuote{UTF-8}. Available encodings are specified at
#' <http://xmlsoft.org/html/libxml-encoding.html#xmlCharEncoding>.
#'
#' @author Alessio Nigro
#'
#' @import xml2
#'
#' @export
#'
#' @examples
#' h <- read_html("<p>Hi!</p>")
#'
#' tmp <- tempfile(fileext = ".bpmn")
#' write_bpmn(h, tmp, options = c("format", "as_xml"))
#' readLines(tmp)
#'
#' # write formatted HTML output
#' write_bpmn(h, tmp, options = c("format", "as_html"))
#' readLines(tmp)
write_bpmn <-
  function(x,
           file,
           ...,
           options = "format",
           encoding = "UTF-8") {
    write_xml(x, file, ..., options = options, encoding = encoding)
    return(message(paste(
      "Successfully written file to '", file, "'.", sep = ""
    )))
  }

# ================================= EXAMPLES ===================================

# Delete this section when write_bpmn will be included in a package?
xml_file <-
  read_xml(file.path(getwd(), "example_models", "Golf Club Subscription.bpmn"))

write_bpmn(
  xml_file,
  file.path(
    getwd(),
    "test_outputs",
    "test_write_bpmn",
    "Golf Club Subscription (write_bpmn version).bpmn"
  ),
  options = c("format", "as_xml")
)
