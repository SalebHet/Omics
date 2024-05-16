#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import InteractiveComplexHeatmap
#' @import shinyjs
#' @import colourpicker
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
               selectizeInput("rowNameCol",label = "Select column for row names",
                              choices = c(Choose = "", NULL),
                              options = list(placeholder = 'Please select a column name below')),

               menuItem("HeatmapColor", tabName = "HeatmapColor",startExpanded = FALSE,
               colourpicker::colourInput(inputId = "colorLow",label = "Color for lowest value",showColour = "background",value = "Blue",returnName = TRUE),
               colourpicker::colourInput(inputId = "colorMid",label = "Color for mid value",showColour = "background",value = "White",returnName = TRUE),
               colourpicker::colourInput(inputId = "colorHigh",label = "Color for highest value",showColour = "background",value = "Red",returnName = TRUE)
               ),

               menuItem("HeatmapConfig", tabName = "HeatmapConfig",startExpanded = FALSE,
                 checkboxInput(inputId = "colCluster", label = "Clustering Column",TRUE),
                 checkboxInput(inputId = "rowCluster", label = "Clustering Row",TRUE),
                 checkboxInput(inputId = "colDendogram", label = "Show Column Deondogram",TRUE),
                 checkboxInput(inputId = "rowDendogram", label = "Show Row Dendogram",TRUE),
                 checkboxInput(inputId = "colNames", label = "Show Column Names",TRUE),
                 checkboxInput(inputId = "rowNames", label = "Show Row Names",TRUE)
               ),

              menuItem("RowMetadata", tabName = "RowMetadata",startExpanded = FALSE,
               radioButtons(inputId = "rowMetaData","Select dataset for rows legend",
                            choices = c(NONE = "NONE",meta1 = "Metadata1",meta2 = "Metadata2"),
                            selected = "NONE"),
               selectizeInput("rowMetaDataVec",label = "Select column for row class",
                                                              choices = c(Choose = "", NULL),
                                                              options = list(placeholder = 'Please select a column name below')),
               uiOutput("rowLegendColors")
               ),

              menuItem("ColumnMetadata", tabName = "ColumnMetadata",startExpanded = FALSE,
               radioButtons(inputId = "colMetaData",
                            "Select dataset for columns legend",
                            choices = c(NONE = "NONE",meta1 = "Metadata1",meta2 = "Metadata2"),
                            selected = "NONE"),
               selectizeInput("colMetaDataVec",label = "Select column for column class",
                              choices = c(Choose = "", NULL),
                              options = list(placeholder = 'Please select a column name below'))
               )
              ),

      menuItem("Umap", tabName = "Umap", icon = icon("paintbrush"),startExpanded = FALSE,
               sliderInput(inputId = "NVoisin", label = "NVoisin",min = 2,max = 100, value = 15),
               sliderInput(inputId = "distMin", label = "distMin", min = 0 , max = 1 , value = 0.01),
               sliderInput(inputId = "n_composant", label = "n_composant", min = 2, max = 100, value = 2),
               selectizeInput("colLabel",label = "Select column for Labels",
                              choices = c(Choose = "", NULL),
                              options = list(placeholder = 'Please select a column name below'))
               ),
      actionBttn(inputId = "DrawPlot", label = "Draw Plot")
  )),

  dashboardBody(tabsetPanel(id = "MainTabs",
    shinyjs::useShinyjs(),
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
  ))
  )

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
