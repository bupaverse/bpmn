# ================================= PACKAGES ===================================

library("bpmn")
library("xml2")

# ============================== MAIN FUNCTION =================================

# ???
render_bpmn <- function(bpmn_file) {
  bpmn(bpmn_file)
}

# ================================= EXAMPLES ===================================

# Delete this section when render_bpmn will be included in a package?
bpmn_file <-
  read_xml(file.path(getwd(), "example_models", "Golf Club Subscription.bpmn"))
render_bpmn(bpmn_file)

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
