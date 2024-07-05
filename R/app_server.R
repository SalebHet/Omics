#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import Rlabkey
#' @import shinydashboard
#' @import shinyjs
#' @import InteractiveComplexHeatmap
#' @import ComplexHeatmap
#' @import colourpicker
#' @import circlize
#' @import uwot
#'
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
  listRowClass <- NULL
  listColClass <- NULL
  graphType <- NULL
  rowMetaDataVec <- NULL
  colMetaDataVec <- NULL
  library(Rlabkey)
  shinyjs::hide(id = "plotOut")
  shinyjs::hide(id = "heatmapOutput")
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['key']])) {
      #browser()
      #updateSliderInput(session, "bins", value = query[['bins']])
      key <<- query[['key']]
      subF <<- query[['sub']]
      assay <<- query[['file']]
      set <<- paste0("apikey|",key)
      type <- query[['type']]
      meta1 <<- query[['meta1']]
      meta2 <<- query[['meta2']]
      expressionAssayType <<- query[['expressionAssayType']]
      metaAssayType <<- query[['metaAssayType']]
      meta2AssayType <<- query[['meta2AssayType']]
      cat("meta1: ",meta1)
      cat("meta2: ",meta2)
      Rlabkey::labkey.setDefaults(apiKey=key)#"apikey|73ea3ff0973f38d52f5b1bbd8980f62c")
      Rlabkey::labkey.setDefaults(baseUrl = "https://labk.bph.u-bordeaux.fr/")#(baseUrl="https://labkey.bph.u-bordeaux.fr:8443/")
      #browser()
      if(type=="Assay"){
        #browser()
        labkey.data <- labkey.selectRows(
          baseUrl="https://labk.bph.u-bordeaux.fr",
          #folderPath="/EBOVAC/assays/EBL2001/ICS",
          folderPath=subF,  #"/VASI/VICI/SISTM",
          #schemaName=paste0("assay.General.",assay),#"assay.General.Vici_Sistm",
          schemaName=paste0("assay.",expressionAssayType,".",assay),
          queryName="Data",
          viewName="",
          colSort="",
          #colFilter=makeFilter(c("Run/RowId", "EQUAL", "140"),c("Antigen", "NOT_EQUAL_OR_MISSING", "Negative control")),
          containerFilter=NULL
        )
        #browser()
        if(nchar(meta1) > 1){
          metaData1 <<- labkey.selectRows(
            baseUrl="https://labk.bph.u-bordeaux.fr",
            #folderPath="/EBOVAC/assays/EBL2001/ICS",
            folderPath=subF,  #"/VASI/VICI/SISTM",
            #schemaName=paste0("assay.General.",meta1),#"assay.General.Vici_Sistm",
            schemaName=paste0("assay.",metaAssayType,".",meta1),
            queryName="Data",
            viewName="",
            colSort="",
            #colFilter=makeFilter(c("Run/RowId", "EQUAL", "140"),c("Antigen", "NOT_EQUAL_OR_MISSING", "Negative control")),
            containerFilter=NULL
          )
        }
        if(nchar(meta2) > 1){
          metaData2 <<- labkey.selectRows(
            baseUrl="https://labk.bph.u-bordeaux.fr",
            #folderPath="/EBOVAC/assays/EBL2001/ICS",
            folderPath=subF,  #"/VASI/VICI/SISTM",
            #schemaName=paste0("assay.General.",meta2),#"assay.General.Vici_Sistm",
            schemaName=paste0("assay.",meta2AssayType,".",meta2),
            queryName="Data",
            viewName="",
            colSort="",
            #colFilter=makeFilter(c("Run/RowId", "EQUAL", "140"),c("Antigen", "NOT_EQUAL_OR_MISSING", "Negative control")),
            containerFilter=NULL
          )
        }
      }
      if(type=="DataSet"){
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
        if(nchar(meta1) > 1){
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
        if(nchar(meta2) > 1){
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
        #browser()
        metaData1 <<- metaData1[,colSums(is.na(metaData1))<nrow(metaData1)]
        output$meta1 <- DT::renderDT(metaData1)
      }
      if(!is.null(metaData2)){
        metaData2 <<- metaData2[,colSums(is.na(metaData1))<nrow(metaData1)]
        output$meta2 <- DT::renderDT(metaData2)
      }
      #browser()
      dataDF <<- labkey.data
      dataDF <<- dataDF[,colSums(is.na(dataDF))<nrow(dataDF)]
      output$dataSet <- DT::renderDT(dataDF)
    }
  })
  #browser()
  observeEvent(input$sidebarItemExpanded,{
    plotType <- input$sidebarItemExpanded
    if(plotType == "VolcanoPlot"){
      graphType <<- "VolcanoPlot"
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
      graphType <<- "HeatMap"
      shinyjs::hide(id = "colMetaDataVec")
      shinyjs::hide(id = "rowMetaDataVec")
      updateSelectizeInput(inputId = "rowNameCol",
                           choices = c('',c(colnames(dataDF),"NONE"))
                           )
      rowNameCol <- ""
      observeEvent(input$rowNameCol,{
        #browser()
        if(input$rowNameCol != ""){
          rowNameCol <<- input$rowNameCol
          cat("update rowNameCol as: ",rowNameCol)
        }
        if(input$rowNameCol == "" && rowNameCol != ""){
          cat("reset selection with: ",rowNameCol)
          updateSelectizeInput(inputId = "rowNameCol",
                               choices = c('',c(colnames(dataDF),"NONE")),
                               selected = rowNameCol
          )
        }
      })
      observeEvent(input$colMetaData,{
        cat("\n colMetaData:",input$colMetaData)
        if(input$colMetaData != "NONE"){
          shinyjs::show(id = "colMetaDataVec")
          if(input$colMetaData == "Metadata1"){
            updateSelectizeInput(inputId = "colMetaDataVec",
                                 choices = colnames(meta1),
                                 options = list(placeholder = 'Please select a variable below'))
          }
          if(input$colMetaData == "Metadata2"){
            updateSelectizeInput(inputId = "colMetaDataVec",
                                 choices = colnames(meta2),
                                 options = list(placeholder = 'Please select a variable below'))
          }

        }else{
          shinyjs::hide(id = "colMetaDataVec")
        }
      })
      observeEvent(input$rowMetaData,{
        cat("\n rowMetaData:",input$rowMetaData)
        if(input$rowMetaData != "NONE"){
          shinyjs::show(id = "rowMetaDataVec")
          if(input$rowMetaData == "Metadata1"){
            #browser()
            updateSelectizeInput(inputId = "rowMetaDataVec",
                                 choices = colnames(metaData1),
                                 options = list(placeholder = 'Please select a variable below'))
          }
          if(input$rowMetaData == "Metadata2"){
            updateSelectizeInput(inputId = "rowMetaDataVec",
                                 choices = colnames(metaData2),
                                 options = list(placeholder = 'Please select a variable below'))
          }
        }else{
          shinyjs::hide(id = "rowMetaDataVec")
        }
      })

      observeEvent(input$rowMetaDataVec,{
        #browser()
          if(is.null(input$sidebarItemExpanded) == TRUE){#input$sideBarItemExpanded != "HeatMap"){#
            #browser()
            if(input$rowMetaData == "Metadata1"){
              rowMetaDataVec <<- input$rowMetaDataVec
              listRowClass <<- unique(metaData1[,input$rowMetaDataVec])

              output$rowLegendColors <- renderUI({
                #map(listRowClass(),)
                rowLegendColors <- lapply(listRowClass, function(i){
                  #browser()
                  colourpicker::colourInput(inputId = paste0("color",i),label = paste("Select color for",i),showColour = "background",
                                            value = "Blue",returnName = TRUE)
                })

                do.call(tagList,rowLegendColors)
              })
            }
            if(input$rowMetaData == "Metadata2"){
              rowMetaDataVec <<- input$rowMetaDataVec
              listRowClass <<- unique(metaData2[,input$rowMetaDataVec])

              output$rowLegendColors <- renderUI({
                #map(listRowClass(),)
                rowLegendColors <- lapply(listRowClass, function(i){
                  #browser()
                  colourpicker::colourInput(inputId = paste0("color",i),label = paste("Select color for",i),showColour = "background",
                                            value = "Blue",returnName = TRUE)
                })

                do.call(tagList,rowLegendColors)
              })
            }
          }
      }

        # for( i in i:length(listRowClass)){
        #
        # }
      )
      observeEvent(input$colMetaDataVec,{
        #browser()
        if(is.null(input$sidebarItemExpanded) == TRUE){#input$sideBarItemExpanded != "HeatMap"){#
          #browser()
          if(input$colMetaData == "Metadata1"){
            colMetaDataVec <<- input$colMetaDataVec
            listcolClass <<- unique(metaData1[,input$colMetaDataVec])

            output$colLegendColors <- renderUI({
              #map(listRowClass(),)
              colLegendColors <- lapply(listColClass, function(i){
                #browser()
                colourpicker::colourInput(inputId = paste0("color",i),label = paste("Select color for",i),showColour = "background",
                                          value = "Blue",returnName = TRUE)
              })

              do.call(tagList,colLegendColors)
            })
          }
          if(input$colMetaData == "Metadata2"){
            colMetaDataVec <<- input$colMetaDataVec
            listColClass <<- unique(metaData2[,input$colMetaDataVec])

            output$colLegendColors <- renderUI({
              #map(listRowClass(),)
              colLegendColors <- lapply(listColClass, function(i){
                #browser()
                colourpicker::colourInput(inputId = paste0("color",i),label = paste("Select color for",i),showColour = "background",
                                          value = "Blue",returnName = TRUE)
              })

              do.call(tagList,colLegendColors)
            })
          }
        }
      })
    }
    if(plotType == "Umap"){
      graphType <<- "Umap"
      updateSelectizeInput(session, inputId = "colLabel",
                           selected = '',
                           choices = c('',colnames(dataDF)),
                           options = list(placeholder = 'Please select a variable below'))
    }
  })

  observeEvent(input$DrawPlot,{
    #browser()
    #plotType <- input$sidebarItemExpanded
    if(graphType == "VolcanoPlot"){
      shinyjs::hide(id = "heatmapOutput_heatmap_group")
      shinyjs::hide(id = "heatmapOutput_sub_heatmap_group")
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
    if(graphType == "HeatMap"){
      shinyjs::show(id = "heatmapOutput_heatmap_group")
      shinyjs::show(id = "heatmapOutput_sub_heatmap_group")
      shinyjs::hide(id = "plotOut")
      topAnno = NULL
      leftAnno = NULL
      #browser()
      if(input$rowNameCol != ""){
        if(input$rowNameCol %in% colnames(dataDF)){
          rowNameCol <- input$rowNameCol
          rownames(dataDF) <<- dataDF[,rowNameCol]
          dataDF <<- dataDF[,!names(dataDF) %in% c(rowNameCol)]
        }

      }
      #browser()
      if(!is.null(rowMetaDataVec)){
        if(rowMetaDataVec != ""){
          #browser()
          colVec <- list(LeftClass = c())
          listCol <- c()
          for (x in 1:length(listRowClass)) {
            #browser()
            cat(paste("\n color of ",listRowClass[x]))
            cat(input[[paste0("color",listRowClass[x])]])
            #newColor <- list(input[[paste0("color",listRowClass[x])]])
            #browser()
            listCol <- c(listCol,input[[paste0("color",listRowClass[x])]])
          }
          colVec$LeftClass <- listCol
          #browser()
          names(colVec$LeftClass) <- listRowClass
          if(input$rowMetaData == "Metadata1"){
            cat("Create LeftMetaData1")
            leftAnno <- rowAnnotation(LeftClass = sample(as.character(metaData1[,rowMetaDataVec])),col = colVec)
          }
          if(input$colMetaData == "Metadata2"){
            cat("Create LeftMetaData2")
            leftAnno <- rowAnnotation(LeftClass =sample(as.character(metaData2[,rowMetaDataVec])),col = colVec)
          }
        }
      }
      if(input$colMetaDataVec != ""){
        colVec <- list(TopClass = c())
        listCol <- c()
        for (x in 1:length(listColClass)) {

          listCol <- c(listCol,input[[paste0("color",listColClass[x])]])
        }
        colVec$TopClass <- listCol
        #browser()
        names(colVec$LeftClass) <- listRowClass
        if(input$colMetaData == "Metadata1"){
          cat("Create TopMetaData1")
          topAnno <- HeatmapAnnotation(TopClass = sample(as.character(metaData1[,input$colMetaDataVec])),col = colVec)
        }
        if(input$colMetaData == "Metadata2"){
          cat("Create TopMetaData2")
          topAnno <- HeatmapAnnotation(TopClass = sample(as.character(metaData2[,input$colBMetaDataVec])),col = colVec)
        }
      }

      #build color list
      #browser()
      dataDF <- as.matrix(dataDF)
      #browser()
      minMat <- unname(quantile(dataDF,0.025))#min(dataDF)
      maxMat <- unname(quantile(dataDF,0.975))#max(dataDF)
      moyMat <- (minMat+maxMat)/2
      #browser()
      colMin <- input$colorLow
      colMid <- input$colorMid
      colMax <- input$colorHigh
      col_name <- circlize::colorRamp2(c(minMat, moyMat, maxMat), c(colMin,colMid,colMax))
      #browser()

        if(input$colCluster == FALSE & input$rowCluster == FALSE){
          cat("No clustering at all")
          plotRes <<- ComplexHeatmap::Heatmap(matrix = dataDF,cluster_rows = FALSE,cluster_columns = FALSE,
                                              left_annotation = leftAnno, top_annotation = topAnno,show_row_dend = input$rowDendogram,
                                              show_column_dend = input$colDendogram,show_column_names = input$colNames,
                                              show_row_names = input$rowNames,col = col_name)
        }
        else if(input$colCluster == FALSE){
          cat("No column clustering")
          plotRes <<- ComplexHeatmap::Heatmap(matrix = dataDF,cluster_columns = FALSE,
                                              left_annotation = leftAnno, top_annotation = topAnno,show_row_dend = input$rowDendogram,
                                              show_column_dend = input$colDendogram,show_column_names = input$colNames,
                                              show_row_names = input$rowNames,col = col_name)
        }
        else if(input$rowCluster == FALSE){
          cat("No Row clustering")
          plotRes <<- ComplexHeatmap::Heatmap(matrix = dataDF,cluster_rows = FALSE,
                                              left_annotation = leftAnno, top_annotation = topAnno,show_row_dend = input$rowDendogram,
                                              show_column_dend = input$colDendogram,show_column_names = input$colNames,
                                              show_row_names = input$rowNames,col = col_name)
        }else{
          cat("Full Clustering")
          #browser()
          plotRes <<- ComplexHeatmap::Heatmap(dataDF,left_annotation = leftAnno, top_annotation = topAnno,show_row_dend = input$rowDendogram,
                                              show_column_dend = input$colDendogram,show_column_names = input$colNames,
                                              show_row_names = input$rowNames,col = col_name)
        }
      makeInteractiveComplexHeatmap(input = input,output = output,session = session,ht_list = plotRes,heatmap_id = "heatmapOutput")
    }
    if(graphType == "Umap"){
      #browser()
      shinyjs::hide(id = "heatmapOutput_heatmap_group")
      shinyjs::hide(id = "heatmapOutput_sub_heatmap_group")
      shinyjs::show(id = "plotOut")
      nNeigh <- input$NVoisin
      distMin <- input$distMin
      nComp <- input$n_composant
      dataDF[,input$colLabel] <- as.factor(dataDF[,input$colLabel])
      plotumap <- umap(dataDF, n_neighbors = nNeigh, min_dist = distMin, n_components = nComp,verbose = FALSE)
      #browser()
      output$plotOut <- renderPlot(plot(  plotumap,
                                          cex = 0.1,
                                          col = grDevices::rainbow(n = length(levels(dataDF[,input$colLabel])))[as.integer(dataDF[,input$colLabel])] |>
                                            grDevices::adjustcolor(alpha.f = input$alphaF),
                                          main = paste(assay, "Umap"),
                                          xlab = "",
                                          ylab = ""))
    }
    updateTabsetPanel(inputId = "MainTabs",selected = "Plot")
  })
}
