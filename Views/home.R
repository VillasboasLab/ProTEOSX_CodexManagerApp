homepage <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar( 
    h3("Welcome!", style="padding-left:8px;"),
    h4(textOutput("menu1"), style="padding-left:10px;"),
    h4(textOutput("menu2"), style="padding-left:10px;"),
    hr(),
    conditionalPanel(condition="input.tabs == 'tab2'",
                     h4("Stratify"),
                     uiOutput("field_picker"),
                     uiOutput("metric_picker")
    ),
    conditionalPanel(condition="input.tabs.startsWith(\"static\")",
                     h4("Filters"),
                     uiOutput("tissue_picker"),
                     uiOutput("channel_picker"),
                     dateRangeInput("home_date_stain_range", "Stained Date Range:",start = "2019-05-01" ),
                     uiOutput("static_metric_picker")
    )
  ),
  dashboardBody(
    tabsetPanel(id="tabs",
                tabPanel("Database", value='tab1',
                         fluidRow(
                           box(
                             width = 12,
                             title = "What's inside",
                             solidHeader = TRUE, collapsible = TRUE,
                             fluidRow(
                               infoBox("Projects:", h3(textOutput("project_count")), width = 3, color = "olive",
                                       fill = TRUE, icon = icon("book")),
                               infoBox("Acquisitions/ROIs:", h3(textOutput("acque_count")), width = 3, color = "yellow",
                                       fill = TRUE, icon = icon("address-card")),
                               infoBox("Images:", h3(textOutput("slide_count")), width = 3, color = "blue",
                                       fill = TRUE, icon = icon("microscope")),
                               infoBox("QC Comments:", h3(textOutput("qcflag_count")), width = 3, color = "red",
                                       fill = TRUE, icon = icon("flag")),
                             )
                           )
                         ),
                         fluidRow(
                           column(6,plotOutput("missingMetricPieChart")),
                           column(6,plotOutput("missingFieldsChart"))
                         ),
                ),
                tabPanel("Metrics #1", value='tab2',
                         h6("Query #1: What is the baseline background auto-fluorescence on each of the 3 channels per protein?"),
                         fluidRow(
                           column(12, plotOutput("field_strata_channels_plot", height = "900px") )
                         )
                ),
                tabPanel("Parameters #2", value='config1',
                         fluidRow(
                           column(6, uiOutput("parameter_picker")),
                           column(6, uiOutput("expmetadata_picker"))
                         ),
                         fluidRow(
                           column(6, plotOutput("pptParamsPiePlot")),
                           column(6, plotOutput("pptExpMetaPiePlot"))
                         )
                ),
                tabPanel("Bleach #4", value='static3',
                         h6("What is the bleaching distribution?"),
                         fluidRow(
                           column(4, plotOutput("bleachPiePlot")),
                           column(8, plotOutput("bleachBarPlot"))
                         ),
                         hr(),
                         # fluidRow(
                         #   column(12,plotOutput("bleachMetricsPlot"))
                         # )
                ),
                tabPanel("Time Series #5", value='static4',
                         fluidRow(
                           column(12, plotOutput("cutdataPlot"))
                         ),
                         fluidRow(
                           column(12, plotOutput("cutdataBarPlot"))
                         )
                ),
                tabPanel("Image Review #6", value='static4',
                         fluidRow(
                           box(
                             width = 4,
                             uiOutput("TissuePickerReviewer")
                           ),
                           box(
                             width = 4,
                             uiOutput("AquisitionPickerReviewer2")
                           ),
                           box(
                             width = 4,
                             uiOutput("ROIPickerReviewer2")
                           )
                         ),
                         fluidRow(
                           box(
                             width = 5,
                             DTOutput("ReviewableParameters2")
                           ),
                           box(
                             width = 7,
                             DTOutput("ReviewableMetrics2")
                           ),
                         )
                )
    )
  )
  
)
