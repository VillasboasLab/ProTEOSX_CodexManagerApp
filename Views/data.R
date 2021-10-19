datapage <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar( ),
  dashboardBody(
    tabsetPanel(id="tabs",
                tabPanel("By Project", value='tab1',
                         uiOutput("ProjectPickerForData")
                         
                         
                ),
                tabPanel("By Date Range", value='tab1',
                         h4("Under construction")
                )
    )
    #DTOutput('FullDataTable')
  )
)