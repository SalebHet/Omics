#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import InteractiveComplexHeatmap
#' @noRd

library(shinydashboard)
app_ui <- function(request) {
  golem_add_external_resources()
  dashboardPage(
  dashboardHeader(title = "Omics Graphs"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("VolcanoPlot", tabName = "dashboard", icon = icon("paintbrush"),startExpanded = FALSE,
               selectizeInput("log2FC",label = "log2FC",
                              choices = c(Choose = "", NULL),
                              options = list(placeholder = 'Please select a column name below')),
               selectizeInput("pval",label = "pval",
                              choices = c(Choose = "", NULL),
                              options = list(placeholder = 'Please select a column name below')),
               selectizeInput("GenesName",label = "Genes Name",
                              choices = c(Choose = "", NULL),
                              options = list(placeholder = 'Please select a column name below'))
               ),

      menuItem("HeatMap", tabName = "HeatMap", icon = icon("paintbrush"),startExpanded = FALSE,
               checkboxInput(inputId = "colCluster", label = "Clustering Column",TRUE),
               checkboxInput(inputId = "rowCluster", label = "Clustering Row",TRUE),
               radioButtons(inputId = "rowMetaData","Select dataset for rows legend",
                            choices = c(NONE = "NONE",meta1 = "Metadata1",meta2 = "Metadata2"),
                            selected = "NONE"),
               selectizeInput("rowMetaDataVec",label = "Select column for row class",
                                                              choices = c(Choose = "", NULL),
                                                              options = list(placeholder = 'Please select a column name below')),
               radioButtons(inputId = "colMetaData",
                            "Select dataset for columns legend",
                            choices = c(NONE = "NONE",meta1 = "Metadata1",meta2 = "Metadata2"),
                            selected = "NONE"),
               selectizeInput("colMetaDataVec",label = "Select column for column class",
                              choices = c(Choose = "", NULL),
                              options = list(placeholder = 'Please select a column name below'))),
      actionBttn(inputId = "DrawPlot", label = "Draw Plot")
  )),
  dashboardBody(tabsetPanel(id = "MainTabs",
    tabPanel("Dataset",  tagList(
      DT::dataTableOutput("dataSet")
    )),
    tabPanel("Metadata1",  tagList(
      DT::dataTableOutput("meta1")
    )),
    tabPanel("Metadata2",  tagList(
      DT::dataTableOutput("meta2")
    )),
    tabPanel("Plot",  tagList(
      plotOutput("plotOut"),
      InteractiveComplexHeatmapOutput(heatmap_id = "heatmapOutput")
    )),
  )))

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  # add_resource_path(
  #   "www",
  #   app_sys("app/www")
  # )

  tags$head(
    favicon(),
    # bundle_resources(
    #   path = app_sys("app/www"),
    #   app_title = "Omics"
    # )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
