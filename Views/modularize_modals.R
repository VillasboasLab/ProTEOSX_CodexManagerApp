## UI Component to popup form for adding a new Project Record
newProjectModal <- function(ns) {
  modalDialog(
    title = "Create a new Project",
    fluidRow(
      box(
        width = 6,
        background = "blue",
        height = 100,
        textInput(ns("project_name_new"), label = "Project Name"),
      ),
      box(
        width = 6,
        background = "blue",
        height = 100,
        textAreaInput(ns("project_description"), "Project Summary", "", width = "600px"),
      )
    ),
    fluidRow(
      box(
        width = 12,
        background = "blue",
        height = 90,
        textInput(ns("project_primary_contact"), label = "Primary Collaborator(s)"),
      )
    ),
    footer = tagList(
      actionButton(ns("Save_newProject"), label = "Save"),
      modalButton('Close')
    )
  )
}

## UI Component to popup list to select project record and remove from other UI elements
closeProjectModal <- function(ns) {
  list <- get_OpenProjectNames()$id
  names(list) <- get_OpenProjectNames()$name
  modalDialog(
    title = "Close Project",
    "Closing will serialize all the records under this project and prevent further editing",
    br(),
    fluidRow(
      box(
        width = 6,
        background = "blue",
        height = 100,
        selectInput(ns("new_close_project"), label = "Project Name:", choices = list)
      ),
      box(
        width = 6,
        background = "blue",
        height = 100,
        textAreaInput(ns("tmpPX123"), "Project Summary", "", width = "600px"),
      )
    ),
    footer = tagList(
      actionButton(ns("Update_closeProject"), label = "Update"),
      modalButton('Close')
    )
  )
}

## UI Component to popup
newBarcodeModal <- function(ns) {
  modalDialog(
    title = "Add Another Barcoded Reporter",
    fluidRow(
      box(
        width = 6,
        background = "aqua",
        textInput(ns("new_barcode_name"), label = "Barcode:")
      )
    ),
    fluidRow(
      box(
        width = 6,
        background = "aqua",
        numericInput(ns("new_barcode_channel"), 3, label = "Channel:", min=2, max=4)
      ),
      box(
        width = 6,
        background = "aqua",
        textInput(ns("new_barcode_reporter"), label = "Reporter Fluorphore:")
      )
    ),
    fluidRow(
      dataTableOutput("barcodeTbl")
    ),
    footer = tagList(
      actionButton(ns("save_new_barcode"), label = "Save"),
      modalButton('Close')
    )
  )
}



## UI Component to popup
newTissueModal <- function(ns) {
  modalDialog(
    title = "Generate New Tissue Type",
    fluidRow(
      box(
        width = 11,
        background = "blue",
        height = 100,
        textAreaInput(ns("tmpTissueX456"), "Tissue", "", width = "1000px"),
      )
    ),
    footer = tagList(
      actionButton(ns("update_tissue_dropdown"), label = "Generate"),
      modalButton('Close')
    )
  )
}


## UI Component to popup
newDiagnosisModal <- function(ns) {
  modalDialog(
    title = "Generate New Diagnosis",
    fluidRow(
      box(
        width = 11,
        background = "blue",
        height = 100,
        textAreaInput(ns("tmpDiagnosisX789"), "Diagnosis", "", width = "1000px"),
      )
    ),
    footer = tagList(
      actionButton(ns("update_diagnosis_dropdown"), label = "Generate"),
      modalButton('Close')
    )
  )
}



## UI Component to popup
newReviewer1Modal <- function(ns) {
  modalDialog(
    title = "Add a review of this Marker",
    box(
      width = 12,
      background = "purple",
      fluidRow(
        box(
          width = 6,
          background = "fuchsia",
          selectInput(ns("new_review_qc_grade"), label = "QC Grade:", choices = c("Excellent","Good","Fair","Poor","Failed") )
        )
      ),
      fluidRow(
        box(
          width = 12,
          background = "fuchsia",
          height = 120,
          textAreaInput(ns("new_review_qc_comment"), label = "Notes/Comments", "", width = "600px")
        )
      )
    ),
    footer = tagList(
      actionButton(ns("add_new_review"), label = "Add"),
      modalButton('Close')
    )
  )
}
