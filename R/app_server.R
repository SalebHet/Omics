#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import Rlabkey
#' @import shinydashboard
#' @import shinyjs
#' @noRd
app_server <- function(input, output, session) {
  library(shinydashboard)
  # Your application server logic
  key <- NULL
  subF <- NULL
  assay <- NULL
  set <- NULL
  dataDF <- NULL
  meta1 <- NULL
  meta2 <- NULL
  metaData1 <- NULL
  metaData2 <- NULL
  library(Rlabkey)
  shinyjs::hide(id = "plotOut")
  shinyjs::hide(id = "heatmapOutput")
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['key']])) {

      #updateSliderInput(session, "bins", value = query[['bins']])
      key <<- query[['key']]
      subF <<- query[['sub']]
      assay <<- query[['file']]
      set <<- paste0("apikey|",key)
      type <- query[['type']]
      meta1 <<- query[['meta1']]
      meta2 <<- query[['meta2']]
      cat("meta1: ",meta1)
      cat("meta2: ",meta2)

      Rlabkey::labkey.setDefaults(apiKey=key)#"apikey|73ea3ff0973f38d52f5b1bbd8980f62c")
      Rlabkey::labkey.setDefaults(baseUrl = "https://labk.bph.u-bordeaux.fr/")#(baseUrl="https://labkey.bph.u-bordeaux.fr:8443/")
      if(type=="assay"){
        labkey.data <- labkey.selectRows(
          baseUrl="https://labk.bph.u-bordeaux.fr",
          #folderPath="/EBOVAC/assays/EBL2001/ICS",
          folderPath=subF,  #"/VASI/VICI/SISTM",
          schemaName=paste0("assay.General.",assay),#"assay.General.Vici_Sistm",
          queryName="Data",
          viewName="",
          colSort="",
          #colFilter=makeFilter(c("Run/RowId", "EQUAL", "140"),c("Antigen", "NOT_EQUAL_OR_MISSING", "Negative control")),
          containerFilter=NULL
        )
        if(length(meta1) > 0){
          metaData1 <<- labkey.selectRows(
            baseUrl="https://labk.bph.u-bordeaux.fr",
            #folderPath="/EBOVAC/assays/EBL2001/ICS",
            folderPath=subF,  #"/VASI/VICI/SISTM",
            schemaName=paste0("assay.General.",meta1),#"assay.General.Vici_Sistm",
            queryName="Data",
            viewName="",
            colSort="",
            #colFilter=makeFilter(c("Run/RowId", "EQUAL", "140"),c("Antigen", "NOT_EQUAL_OR_MISSING", "Negative control")),
            containerFilter=NULL
          )
        }
        if(length(meta2) > 0){
          metaData2 <<- labkey.selectRows(
            baseUrl="https://labk.bph.u-bordeaux.fr",
            #folderPath="/EBOVAC/assays/EBL2001/ICS",
            folderPath=subF,  #"/VASI/VICI/SISTM",
            schemaName=paste0("assay.General.",meta2),#"assay.General.Vici_Sistm",
            queryName="Data",
            viewName="",
            colSort="",
            #colFilter=makeFilter(c("Run/RowId", "EQUAL", "140"),c("Antigen", "NOT_EQUAL_OR_MISSING", "Negative control")),
            containerFilter=NULL
          )
        }
      }
      if(type=="dataset"){
        labkey.data <- labkey.selectRows(
          baseUrl="https://labk.bph.u-bordeaux.fr",
          #folderPath="/EBOVAC/assays/EBL2001/ICS",
          folderPath=subF,  #"/VASI/VICI/SISTM",
          schemaName="study",#paste0("assay.General.",assay),#"assay.General.Vici_Sistm",
          queryName=assay,
          viewName="",
          colSort="",
          #colFilter=makeFilter(c("Run/RowId", "EQUAL", "140"),c("Antigen", "NOT_EQUAL_OR_MISSING", "Negative control")),
          containerFilter=NULL
        )
        if(length(meta1) > 0){
          metaData1 <<- labkey.selectRows(
            baseUrl="https://labk.bph.u-bordeaux.fr",
            #folderPath="/EBOVAC/assays/EBL2001/ICS",
            folderPath=subF,  #"/VASI/VICI/SISTM",
            schemaName="study",#paste0("assay.General.",assay),#"assay.General.Vici_Sistm",
            queryName=meta1,
            viewName="",
            colSort="",
            #colFilter=makeFilter(c("Run/RowId", "EQUAL", "140"),c("Antigen", "NOT_EQUAL_OR_MISSING", "Negative control")),
            containerFilter=NULL
          )
        }
        if(length(meta2) > 0){
          metaData2 <<- labkey.selectRows(
            baseUrl="https://labk.bph.u-bordeaux.fr",
            #folderPath="/EBOVAC/assays/EBL2001/ICS",
            folderPath=subF,  #"/VASI/VICI/SISTM",
            schemaName="study",#paste0("assay.General.",assay),#"assay.General.Vici_Sistm",
            queryName=meta2,
            viewName="",
            colSort="",
            #colFilter=makeFilter(c("Run/RowId", "EQUAL", "140"),c("Antigen", "NOT_EQUAL_OR_MISSING", "Negative control")),
            containerFilter=NULL
          )
        }
      }
      #cat("Result request => ")
      #cat(as.character(labkey.data),"\n")
      if(!is.null(metaData1)){
        metaData1 <<- metaData1[,colSums(is.na(dataDF))<nrow(dataDF)]
        output$meta1 <- DT::renderDT(metaData1)
      }
      if(!is.null(metaData2)){
        metaData2 <<- metaData2[,colSums(is.na(dataDF))<nrow(dataDF)]
        output$meta2 <- DT::renderDT(metaData2)
      }
      dataDF <<- labkey.data
      dataDF <<- dataDF[,colSums(is.na(dataDF))<nrow(dataDF)]
      output$dataSet <- DT::renderDT(dataDF)
    }
  })
  observeEvent(input$sidebarItemExpanded,{
    plotType <- input$sidebarItemExpanded
    if(plotType == "VolcanoPlot"){
      updateSelectizeInput(session, inputId = "log2FC",
                           selected = '',
                           choices = c('',colnames(dataDF)),
                           options = list(placeholder = 'Please select a variable below'))
      updateSelectizeInput(session, inputId = "pval",
                           selected = '',
                           choices = c('',colnames(dataDF)),
                           options = list(placeholder = 'Please select a variable below'))
      updateSelectizeInput(session, inputId = "GenesName",
                           selected = '',
                           choices = c('',c(colnames(dataDF),"NONE")),
                           options = list(placeholder = 'Please select a variable below'))
    }
    if(plotType == "HeatMap"){

    }
  })
  observeEvent(input$DrawPlot,{
    plotType <- input$sidebarItemExpanded
    if(plotType == "VolcanoPlot"){
      shinyjs::hide(id = "heatmapOutput")
      shinyjs::show(id = "plotOut")
      if(input$log2FC == "" | input$pval == ""){
        cat("Must show alert")
        # shinyalert("Please Select Column",type = "warning",showConfirmButton = TRUE,
        #            closeOnClickOutside = TRUE)
        showModal(modalDialog(
          title = "Information",
          "Please select columns for log2FC and PValue"
        ))
      }else{
        log2FC <- input$log2FC
        pval <- input$pval
        geneName <- input$GenesName
        cat("geneName: ",geneName)
        cat("Shouldn't show alert")
        if(geneName == "NONE" | geneName==""){
          plotRes <<- volcanoPlot(log2fc = log2FC,pValue = pval,data = dataDF)
        }
        else{
          plotRes <<- volcanoPlot(log2fc = log2FC,pValue = pval,data = dataDF,geneNames = geneName)
        }
        #browser()
        output$plotOut <- renderPlot(plotRes)
      }
    }
    if(plotType == "HeatMap"){
      shinyjs::show(id = "heatmapOutput")
      shinyjs::hide(id = "plotOut")
      dataDF <- as.matrix(dataDF)
      if(input$colCluster == FALSE & input$rowCluster == FALSE){
        cat("No clustering at all")
        plotRes <<- ComplexHeatmap::Heatmap(matrix = dataDF,cluster_rows = FALSE,cluster_columns = FALSE)
      }
      else if(input$colCluster == FALSE){
        cat("No column clustering")
        plotRes <<- ComplexHeatmap::Heatmap(matrix = dataDF,cluster_columns = FALSE)
      }
      else if(input$rowCluster == FALSE){
        cat("No Row clustering")
        plotRes <<- ComplexHeatmap::Heatmap(matrix = dataDF,cluster_rows = FALSE)
      }else{
        cat("Full Clustering")
        plotRes <<- ComplexHeatmap::Heatmap(dataDF)
      }
      makeInteractiveComplexHeatmap(input = input,output = output,session = session,ht_list = plotRes,heatmap_id = "heatmapOutput")
    }
    updateTabsetPanel(inputId = "MainTabs",selected = "Plot")
  })
}
