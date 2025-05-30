##############################################################################-

###############           MAIN CODE SHINY APP          #######################-

##############################################################################-


# Julia G Currás - 30/05/2025

setwd("C:/Users/julia/Documents/GitHub/metaMarkersT2D")
rm(list=ls())
graphics.off()
library(dplyr)
library(ggplot2)
library(plotly)


# Loading data ####
basal <- readRDS(file = "app/basalDatabase.rds")
data <- readRDS(file = "app/completeDatabase.rds")

# Input ####
protein <- "P01011"
inData <- protein %in% data$ProteinID

if (inData){
  # Selecting protein data ####
  df <- data %>% 
    dplyr::filter(ProteinID == protein) 
  ids <- df$ID
  
  ## Protein information ####
  df <- df %>%
    dplyr::select(Reference, ProteinName:pvalAdj) %>%
    dplyr::arrange(Reference) %>%
    rename(
      `Protein name or gene name` = ProteinName, 
      `Protein description` = ProteinDescription, 
      `P-value` = pval, 
      `Adjusted p-value` = pvalAdj)
  
  ## Display ####
  DT::datatable(df, extensions = "Buttons", rownames = F, escape = F,
                options = list(ordering = F, dom = "Brft", scrollY = '500px',
                              scrollX = T, pageLength = nrow(df),
                              # columnDefs = list(list(className = 'dt-center', targets = '_all')),
                              full_width = TRUE, rowCallback = DT::JS(js),
                              buttons = list(list(extend = "copy"),
                                            list(extend = "csv"), 
                                            list(extend = "excel")))) %>% 
  DT::formatStyle(1, font = 'bold') %>%
    DT::formatRound(4:ncol(df), digits = 3)
  
  # Basal info tables  ####
  dfBasal <- basal %>% 
    filter(ID %in% ids) %>%
    dplyr::arrange(Reference) %>%
    dplyr::select(Reference, Title:DOI, DataAdquisition, DIAMethod,
                  ComercialHouse:IntrumentModel,
                  ReferenceLibrary:SoftwareA, SampleSizeDiabetics:TypeOfSample, 
                  TypeOfSample_2, 
                  SoftwareDownstreamAnalysis:StatisticalTest, 
                  AdjustedMethodDic, AdjustedMethod:Tool_FuncitonalAnalysis, 
                  infoPvalue:RawData
                  )
  
  ### Table 1  ####
  dfBasalImportant <- dfBasal %>% 
    dplyr::select(Reference, SampleSizeControls, SampleSizeDiabetics, 
                  TypeOfSample, TypeOfSample_2, InfoFC, infoPvalue, 
                  infoAdjustedPvalue) %>%
    dplyr::rename(
       `Sample size control group` = SampleSizeControls,
       `Sample size T2DM group` = SampleSizeDiabetics,
       `Type of sample` = TypeOfSample, 
       `Type of sample (grouped)` = TypeOfSample_2, 
       `Fold change data?` = InfoFC,
       `P-value data?` = infoPvalue,
       `Adjusted p-value data?` = infoAdjustedPvalue
       )
  
  DT::datatable(dfBasalImportant, extensions = "Buttons", rownames = F, escape = F,
                options = list(ordering = F, dom = "Brft", scrollY = '500px',
                               scrollX = T, pageLength = nrow(dfBasalImportant),
                               # columnDefs = list(list(className = 'dt-center', targets = '_all')),
                               full_width = TRUE, rowCallback = DT::JS(js),
                               buttons = list(list(extend = "copy"),
                                              list(extend = "csv"), 
                                              list(extend = "excel")))) %>% 
    DT::formatStyle(1, font = 'bold') 
  
  ### Table 2 ####
  dfBasalPaper <- dfBasal %>% 
    dplyr::select(Reference, Country:DOI, Title:Authors)
  DT::datatable(dfBasalPaper, extensions = "Buttons", rownames = F, escape = F,
                options = list(ordering = F, dom = "Brft", scrollY = '500px',
                               scrollX = T, pageLength = nrow(dfBasalPaper),
                               full_width = T, rowCallback = DT::JS(js),
                               buttons = list(list(extend = "copy"),
                                              list(extend = "csv"), 
                                              list(extend = "excel")))) %>% 
    DT::formatStyle(1, font = 'bold') %>% 
    DT::formatStyle(
      columns = colnames(dfBasalPaper),  # Aplica el estilo a todas las columnas
      target = 'row',
      whiteSpace = 'nowrap'  # Evita que el contenido se ajuste dentro de las celdas
    )
  
  
  ### Table 3 ####
  dfBasalProteomics <- dfBasal %>% 
    dplyr::select(Reference, DataAdquisition:SoftwareA, 
                  Normalization:Tool_FuncitonalAnalysis)
  colnames(dfBasalProteomics) <- c(
    "Reference", "Data acquisition mode", "DIA method", "Comercial house", 
    "Type of MS", "Instrument model", "Reference library", 
    "Identification & Quantification software", "Normalization", "Statistical test", 
    "Adjusted p-values?", "Method of adjustment", "Data imputation?", 
    "Type of imputation", "Functional analysis?", "Tool for functional analysis"
  )
  DT::datatable(dfBasalProteomics, extensions = "Buttons", rownames = F, escape = F,
                options = list(ordering = F, dom = "Brft", scrollY = '500px',
                               scrollX = T, pageLength = nrow(dfBasalProteomics),
                               full_width = TRUE, rowCallback = DT::JS(js),
                               buttons = list(list(extend = "copy"),
                                              list(extend = "csv"), 
                                              list(extend = "excel")))) %>% 
    DT::formatStyle(1, font = 'bold') 
  
  
  
  # Displaying results ####
  ## Barplot ####
  dfPlot <- df
  ### Removing proteins without fold change info ####
  dfPlot <- dfPlot[!(is.na(dfPlot$logFC) & is.na(dfPlot$FC)), ]
  dfPlot$logFC <- as.numeric(dfPlot$logFC)
  ### Finding significance ####
  dfPlot$pval <- ifelse(is.na(dfPlot$`P-value`), NA,
                              ifelse(dfPlot$`P-value` < 0.05, "Significant (p-val)", "Non significant (p-val)"))
  dfPlot$pvalAdj <- ifelse(is.na(dfPlot$`Adjusted p-value`), NA,
                              ifelse(dfPlot$`Adjusted p-value` < 0.05, "Significant (adjusted)", "Non significant (adjusted)"))
  dfPlot$Significance <- ifelse(is.na(dfPlot$pval) & is.na(dfPlot$pvalAdj), "Without p-value info", 
                               ifelse(is.na(dfPlot$pvalAdj), dfPlot$pval, dfPlot$pvalAdj))
  dfPlot$pval <- NULL
  dfPlot$pvalAdj <- NULL
  dfPlot$Significance <- factor(dfPlot$Significance, 
                                levels = c("Without p-value info", 
                                           "Significant (p-val)",
                                           "Significant (adjusted)",
                                           "Non significant (p-val)", 
                                           "Non significant (adjusted)"))
  ylimInfo <- max(abs(min(dfPlot$logFC)), abs(max(dfPlot$logFC)))*1.1
  
  ### Displaying ####
  plogFC <- ggplot(dfPlot, aes(x = Reference, y = logFC, fill = Significance)) +
    geom_bar(stat = "identity") +
    labs(x = "", y = "logFC", fill = "", title = protein) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text = element_text(size = 18),
      title = element_text(size = 35),
      axis.title = element_text(size = 22),
      legend.text = element_text(size = 18),
      legend.position = "bottom", 
      axis.line = ggplot2::element_line(linewidth = 0.5, 
                                        colour = "black"), 
      axis.ticks = ggplot2::element_line(linewidth = 0.5, 
                                         colour = "black")) +
    scale_fill_manual(values = c("Without p-value info" = "darkgrey",
                                 "Significant (p-val)" = "darkgreen",
                                 "Significant (adjusted)" = "#4DAF4A",
                                 "Non significant (p-val)" = "darkred", 
                                 "Non significant (adjusted)" = "pink" 
                                 )) +
    ylim(c(-ylimInfo, ylimInfo))
  
  plotlogFC <- suppressWarnings(plotly::ggplotly(plogFC, 
                                               tooltip = "text")
                             ) %>% 
    plotly::config(
      modeBarButtonsToRemove = c("autoScale2d", "lasso2d", "select2d", "pan2d"), 
      displaylogo = FALSE
    ) %>% 
    plotly::layout(
      yaxis = list(range = c(-ylimInfo, ylimInfo)), 
      legend = list(orientation = "h",   # horizontal
                    x = 0.5,             # centrado horizontalmente
                    xanchor = "center",
                    y = -0.7)
    )
  
  
} else {
  # no protein info
  outMessage <- paste0("Protein ", protein, " was not matched any entry from the 
                       studies considered in this metaanalysis. Review the 
                       protein ID and make sure that is a valid entry from UniProtKB.")
  
}































