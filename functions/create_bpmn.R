# ================================= PACKAGES ===================================

# Delete this section when create_bpmn will be included in a package with imports?
library("assertive")
library("knitr")
library("purrr")

# ============================= HELPER FUNCTIONS ===============================

# Gives warning message without stopping execution
.give.warning <- compose(message, paste)

# Stops execution and gives a simple error message
.stop.script <- compose(stop, simpleError, paste)

# Prints output based on elements that are allowed to be empty or not
.print.output <-
  function(bpmn_element,
           message_string,
           elements_empty_allowed) {
    if (bpmn_element %in% elements_empty_allowed) {
      .give.warning(bpmn_element, message_string)
    } else {
      .stop.script(bpmn_element, message_string)
    }
  }

# Prints message about the BPMN elements
.print.message <-
  function(bpmn,
           message_string,
           elements_empty_allowed) {
    bpmn %>%
      bpmn[.] %>%
      names() %>%
      map(~ .print.output(.x, message_string, elements_empty_allowed))
  }

# Compares attributes available in the data with a minimal subset of attributes
.compare.attributes <-
  function(bpmn_attributes,
           bpmn_element,
           minimal_subset_attributes_list,
           singular_of_bpmn_elements) {
    minimal_subset_attributes <-
      minimal_subset_attributes_list[[bpmn_element]]
    list_of_logicals <-
      minimal_subset_attributes %in% bpmn_attributes
    if (!(all(list_of_logicals))) {
      errors_attributes <-
        combine_words(minimal_subset_attributes[which(!list_of_logicals)])
      if (sum(!list_of_logicals) == 1) {
        error_message <- " is needed as an attribute of the BPMN element "
      } else {
        error_message <- " are needed as attributes of the BPMN element "
      }
      .stop.script(errors_attributes,
                   error_message,
                   singular_of_bpmn_elements[[bpmn_element]],
                   ".",
                   sep = "")
    }
  }

# Checks for empty data.frames
.check.for.empty.data.frames <-
  function(bpmn, elements_empty_allowed) {
    retrieve_empty_data_frames <- as_mapper(~ nrow(.x) == 0)
    bpmn %>%
      map_lgl(retrieve_empty_data_frames) %>%
      .print.message("is an empty data.frame.", elements_empty_allowed = elements_empty_allowed)
  }

# Checks per BPMN element if required attributes are present
.check.for.minimal.subset.attributes <-
  function(bpmn,
           minimal_subset_attributes_list,
           singular_of_bpmn_elements) {
    bpmn %>%
      map(~ names(.x)) %>%
      keep(is_non_empty) %>%
      imap(
        ~ .compare.attributes(
          .x,
          .y,
          minimal_subset_attributes_list,
          singular_of_bpmn_elements
        )
      )
  }

# ============================== MAIN FUNCTION =================================

#' Create BPMN object.
#'
#' This creates a BPMN object by specifying a set of tasks, sequence flows, gateways, and a start and end event.
#'
#' @param tasks A data.frame of all tasks and their attributes.
#' @param sequenceFlows A data.frame of all sequence flows and their attributes.
#' @param gateways A data.frame of all gateways and their attributes.
#' @param startEvent A data.frame containing the start event and its attributes.
#' @param endEvent A data.frame containing the end event and its attributes.
#' @param ... Additional arguments passed to methods.
#'
#' @return A BPMN object as a list of data.frames.
#'
#' @author Alessio Nigro
#'
#' @import assertive
#' @import dplyr
#' @import knitr
#' @import purrr
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
#' print(bpmn_instance)
create_bpmn <-
  function(tasks,
           sequenceFlows,
           gateways,
           startEvent,
           endEvent,
           ...) {
    # Stores arguments of create_bpmn as a list of data.frames
    bpmn <- as.list(environment())
    
    # Checks if arguments are data.frames
    assert_is_data.frame(tasks)
    assert_is_data.frame(sequenceFlows)
    assert_is_data.frame(gateways)
    assert_is_data.frame(startEvent)
    assert_is_data.frame(endEvent)
    
    # Defines every data structure that can be changed
    singular_of_bpmn_elements <- list(
      tasks = "task",
      sequenceFlows = "sequenceFlow",
      gateways = "gateway",
      startEvent = "startEvent",
      endEvent = "endEvent"
    )
    minimal_subset_attributes_list <-
      list(
        tasks = c("id", "name"),
        sequenceFlows = c("id", "name", "sourceRef", "targetRef"),
        gateways = c("id", "name", "gatewayType", "gatewayDirection"),
        startEvent = c("id", "name"),
        endEvent = c("id", "name")
      )
    elements_empty_allowed <- c("gateways")
    attributes_to_factors <- c("gatewayType", "gatewayDirection")
    
    # Checks for empty data.frames
    bpmn %>%
      .check.for.empty.data.frames(elements_empty_allowed = elements_empty_allowed)
    
    # Checks per BPMN element if required attributes are present
    bpmn %>%
      .check.for.minimal.subset.attributes(
        minimal_subset_attributes_list = minimal_subset_attributes_list,
        singular_of_bpmn_elements = singular_of_bpmn_elements
      )
    
    # Converts missing values in data.frames to empty string
    for (element in names(bpmn)) {
      bpmn[[element]][is.na(bpmn[[element]])] <- ""
    }
    
    # Converts all values to character type
    for (element in names(bpmn)) {
      bpmn[[element]][] <- sapply(bpmn[[element]], as.character)
    }
    
    # Converts certain attributes to a factor
    for (element in names(bpmn)) {
      for (attribute in names(bpmn[[element]])) {
        if (attribute %in% attributes_to_factors) {
          bpmn[[element]][, attribute] <-
            as.factor(bpmn[[element]][, attribute])
        }
      }
    }
    
    class(bpmn) <- "bpmn"
    
    return(bpmn)
  }

# ================================= EXAMPLES ===================================

# Delete this section when create_bpmn will be included in a package?
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

print(golf_club_subscription)

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

print(handling_of_incoming_job_applications)

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

print(training_and_certification_propose_training)
