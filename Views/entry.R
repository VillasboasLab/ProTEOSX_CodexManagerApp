entrypage <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    h3("Database Operations:"),
    actionButton("newProjectModal", label = "Create New Project"),
    br(),
    actionButton("closeOutProjectModal", label = "Close A Project"),
    br(),
    actionButton("newBarcodeModal", label = "Add New Barcode"),
    br(),
    actionButton("newTissueModal", label = "Add New Tissue Type"),
    br(),
    actionButton("newDiagnosisModal", label = "Add New Diagnosis")
  ),
  dashboardBody(
    tabsetPanel(id="tabs",
                tabPanel("New Run", value='tab1',
                         fluidPage(
                           h2("Add New Acquisition to Database"),
                           box(
                             width = 12,
                             background = "light-blue",
                             fluidRow(
                               box(
                                 width = 4,
                                 background = "light-blue",
                                 uiOutput("ProjectPicker")
                               ),
                               box(
                                 width = 5,
                                 background = "light-blue",
                                 textInput(inputId = "new_entry_acquisition_name", label = "Acquisition Name:", placeholder = "Required..."),
                               ),
                               box(
                                 width = 3,
                                 background = "light-blue",
                                 radioButtons(inputId = "new_entry_h2o2_bleach", label = "H2O2 bleached:",  inline = TRUE,
                                              choices = c("N" = 0, "Y" = 1), selected = 0)
                               )
                             ),
                             fluidRow(
                               box(
                                 width = 3,
                                 background = "light-blue",
                                 dateInput(inputId = "new_entry_cut_date", label = "Date Cut:", format = "m-d-yyyy")
                               ),
                               box(
                                 width = 3,
                                 background = "light-blue",
                                 dateInput(inputId = "new_entry_stain_date", label = "Date Stained:", format = "m-d-yyyy")
                               ),
                               box(
                                 width = 3,
                                 background = "light-blue",
                                 dateInput(inputId = "new_entry_render_date", label = "Date Rendered:", format = "m-d-yyyy")
                               ),
                               box(
                                 width = 3,
                                 background = "light-blue",
                                 dateInput(inputId = "new_entry_process_date", label = "Date Processed:", format = "m-d-yyyy")
                               )
                             ),
                             fluidRow(
                               box(
                                 width = 2,
                                 background = "light-blue",
                                 numericInput(inputId = "new_entry_thickness", 1, label = "Thickness (µM):", min=0, max=100)
                               ),
                               box(
                                 width = 2,
                                 background = "light-blue",
                                 numericInput(inputId = "new_entry_section_to_image", 0.0, label = "Section-to-Image Interval:", min=0, max=1000)
                               ),
                               box(
                                 width = 2,
                                 background = "light-blue",
                                 numericInput(inputId = "new_entry_zstack", 9, label = "Z stacks:", min=0, max=1000)
                               ),
                               box(
                                 width = 3,
                                 background = "light-blue",
                                 selectInput("new_entry_antigen_retrival", label = "Antigen Retrival:", choices = c("None","EDTA Tris pH 9","Citrate pH 6.0") )
                               ),
                               box(
                                 width = 3,
                                 background = "light-blue",
                                 selectInput("new_entry_machine_name", label = "Machine:", choices = c("Wolverine") )
                               )
                             ),
                             fluidRow(
                               box(
                                 width = 6,
                                 background = "light-blue",
                                 textAreaInput("new_entry_sectioning_comment", "Sectioning Comment(s)", "", width = "1200px"),
                               )
                             )
                           ),
                           hr(),
                           h3("Add Rounds of Experimental Design"),
                           box(
                             width = 12,
                             background = "olive",
                             fluidRow(
                               box(
                                 width = 1,
                                 background = "olive",
                                 numericInput(inputId = "new_muliplex_cycle", 1, label = "Cycle", min=1, max=99)
                               ),
                               box(
                                 width = 2,
                                 background = "olive",
                                 textInput(inputId = "new_muliplex_antibody", label = "Antibody:")
                               ),
                               box(
                                 width = 2,
                                 background = "olive",
                                 textInput(inputId = "new_muliplex_vendor", label = "Vendor:")
                               ),
                               box(
                                 width = 2,
                                 background = "olive",
                                 textInput(inputId = "new_muliplex_ab_clone", label = "AB Clone:")
                               ),
                               box(
                                 width = 2,
                                 background = "olive",
                                 numericInput(inputId = "new_muliplex_ab_conc", 0.25, label = "Conc. (µL):", min=0, max=1000)
                               ), 
                               box(
                                 width = 2,
                                 background = "olive",
                                 uiOutput("BarcodePicker")
                               ),
                               box(
                                 width = 1,
                                 background = "olive",
                                 numericInput(inputId = "new_muliplex_exposure", 0, label = "Exposure:")
                               )
                             ),
                             actionButton(inputId = "add_multiplex_row", label = "Add Cycle"),
                           ),
                           DTOutput('recentMultiplexTable'),
                           hr(),
                           h3("Regions Of Interest (ROIs)"),
                           box(
                             width = 12,
                             background = "aqua",
                             fluidRow(
                               box(
                                 width = 2,
                                 background = "aqua",
                                 textInput(inputId = "new_roi_name", label = "ROI:", value = "Reg001")
                               ),
                               box(
                                 width = 4,
                                 background = "aqua",
                                 textInput(inputId = "new_roi_specimen_id", label = "Specimen ID/Block #:")
                               ),
                               box(
                                 width = 4,
                                 background = "aqua",
                                 textInput(inputId = "new_roi_study_id", label = "Clinic Number/Study ID:")
                               ),
                               box(
                                 width = 2,
                                 background = "aqua",
                                 radioButtons(inputId = "new_roi_tma_source", label = "From TMA:",  inline = TRUE,
                                              choices = c("N" = 0, "Y" = 1), selected = 0)
                               )
                             ),
                             fluidRow(
                               box(
                                 width = 3,
                                 background = "aqua",
                                 textInput(inputId = "new_roi_lab_source", label = "Specimen Source:")
                               ),
                               box(
                                 width = 3,
                                 background = "aqua",
                                 uiOutput("TissuePicker")
                               ),
                               box(
                                 width = 3,
                                 background = "aqua",
                                 uiOutput("DiagnosisPicker")
                               ),
                               box(
                                 width = 3,
                                 background = "aqua",
                                 textInput(inputId = "new_roi_tile_size", label = "Tile Size:", placeholder = "W x H")
                               )
                             ),
                             actionButton(inputId = "add_roi_row", label = "Add ROI")
                           ),
                           DTOutput('recentRoiTable'),
                           hr(),
                           div(style="display:inline-block",actionButton(inputId = "save_entry_form_to_database", label = "Save to Database"), style="float:right"),
                           br()
                         )
                ),
                

                tabPanel("Upload PPT", value='tab2',
                         h4("Step 1: Select ROI from database."),
                         fluidPage(
                           fluidRow(
                             box(
                               width = 4,
                               uiOutput("ProjectPickerSyncPlex")
                             ),
                             box(
                               width = 4,
                               uiOutput("AquisitionPickerSyncPlex")
                             ),
                             box(
                               width = 4,
                               uiOutput("ROIPickerSyncPlex")
                             )
                           )
                         ),
                         h4("Step 2: Select corresponding Codex summery PPT."),
                         h6("  Large File sizes (~100Mb) take some time, please be patient."),
                         fileInput("pptCodexProcessorFile", "Choose PPT File",
                                   multiple = FALSE,
                                   accept = c("application/vnd.ms-powerpoint",".ppt", 
                                              "application/vnd.openxmlformats-officedocument.presentationml.presentation",".pptx")),
                         
                         DTOutput('selectedMultiplexMetricsTable'),
                         
                         
                ),
                
                
                
                tabPanel("Quality Review Comments", value='tab3',
                         h4("Step 1: Select ROI from database."),
                         h5("Only Open Projects with uploaded PPT summaries will appear here."),
                         fluidPage(
                           fluidRow(
                             box(
                               width = 4,
                               uiOutput("ProjectPickerReviewer")
                             ),
                             box(
                               width = 4,
                               uiOutput("AquisitionPickerReviewer")
                             ),
                             box(
                               width = 4,
                               uiOutput("ROIPickerReviewer")
                             )
                           ),
                           fluidRow(
                             box(
                               width = 5,
                               DTOutput("ReviewableParameters")
                             ),
                             box(
                               width = 7,
                               DTOutput("ReviewableMetrics")
                             ),
                           )
                         ),
                         h4("Step 2: Select a round to retrieve & review."),
                         h4("Step 3: Add review comment."),
                         fluidPage(
                           fluidRow(
                             box(
                               width = 5,
                               uiOutput("MultiplexRoundPickerReviewer"),
                               actionButton("fetchMultiplexImage", label = "Retrieve from Database")
                             ),
                             box(
                               width = 3,
                               offset = 3,
                               actionButton("openModelForMultiplexComments", label = "Add Review Comment")
                             )
                           ),
                           fluidRow(
                             box(
                               width = 3,
                               htmlOutput("ReviewHistogram")
                             ),
                             box(
                               width = 9,
                               htmlOutput("ReviewBigRaw")
                             ),
                           )
                         ),
                         
                )
    )
    )
  )
  