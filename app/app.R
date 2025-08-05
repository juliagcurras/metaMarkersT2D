#
# This app is used to find proteins in the results of the metaanalysis
#

# Julia García Currás
# 05/2025

source(file = "global.R", encoding = 'UTF-8')


# UI ####

ui <- bslib::page_fluid(
  ## General ####
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  fillable = TRUE,
  fillable_mobile = TRUE,
  
  ## Title ####
  shiny::titlePanel(windowTitle = "metaMarkersT2D",
                    title =
                      tags$div(
                        style = "
    position: relative;
    background-image: url('fondo5.jpeg');
    background-size: cover;
    background-position: center;
    padding: 40px 20px;
    border-radius: 10px;
    overflow: hidden;
  ",
                        tags$div(
                          style = "
      position: absolute;
      top: 0; left: 0; right: 0; bottom: 0;
      background-color: rgba(0,0,0,0.55);  /* oscurece la imagen */
    "
                        ),
                        tags$div(
                          style = "
      position: relative;
      text-align: center;
      color: white;
    ",
                        tags$h1("metaMarkersT2D"),
                        tags$h5("This repository contains the results of a 
                                meta-analysis of proteomics studies focused on 
                                identifying potential biomarkers for type 2 
                                diabetes (T2D). Using mass spectrometry (MS) 
                                data from multiple independent studies, this 
                                project aims to uncover consistent protein-level 
                                changes associated with T2D.",
                            style = "margin-top: 10px; color: #E2E2E2;")
                        ))),
  # Favicon
  tags$head(tags$link(rel="shortcut icon",
                      href="logo_biostatech.png")
  ),
  

    ## Sidebar ####
  layout_sidebar(
    div(textOutput(outputId = "textNoInfo"),
        style = "color: darkred; font-weight: bold; font-size: 25px; text-align: center; margin-top: 10px; margin-bottom: 10px"),
    sidebar = sidebar(
      title = h3("Selection panel"), accept = ".txt",
      width = 350,
      textInput(inputId = "idProt", 
                label = "Protein ID from UniProt", 
                value = "P04004"), 
      uiOutput("summaryText"),
      # fileInput(inputId = "txtFile",
      #           label = "Text file with the list of protein IDs from UniProt: "),
      h4("Filters"),
      selectInput(
        "typeOfSample",
        "Select type of sample",
        choices = list("All" = "All",
                       "Eye" = "Eye",
                       "Extracelular vesicles (blood)" = "Extracelular vesicles (blood)",
                       "Liver tissue" = "Liver tissue",
                       "Muscle biopsy" = "Muscle biopsy",
                       "Pancreas biopsy (pancreatic island cells)" = "Pancreas biopsy (pancreatic island cells)", 
                       "Plasma" = "Plasma",
                       "Saliva" = "Saliva",
                       "Serum" = "Serum",
                       "Sperm" = "Sperm",
                       "Urine" = "Urine",
                       "Visceral Adipose Tissue" = "Visceral Adipose Tissue"
                       ),
        selected = 1, multiple = F
      ),
      selectInput(
        "acqMode",
        "Select type of data acquision mode",
        choices = list("All" = "All",
                       "DDA" = "DDA",
                       "DIA" = "DIA"
                       ),
        selected = 1, multiple = F
      ),
      selectInput(
        "typeTest",
        "Select type of test",
        choices = list("All" = "All",
                       "T-test" = "ttest",
                       "limma" = "limma",
                       "ANOVA" = "ANOVA"
                       ),
        selected = 1, multiple = F
      ),
      selectInput(
        "countryID",
        "Select studies by country",
        choices = list(
          "All" = "All",
          "Bahrain" = "Bahrain",
          "Brazil" = "Brazil",
          "China" = "China",
          "Denmark" = "Denmark",
          "Germany" = "Germany",
          "Indonesia" = "Indonesia",
          "Jordan" = "Jordan",
          "Norway" = "Norway",
          "Poland" = "Poland",
          "Portugal" = "Portugal",
          "Republic of Korea" = "Republic of Korea",
          "USA" = "USA"
                       ),
        selected = 1, multiple = F
      ),
      actionButton(inputId = "goSearch", label = "Start search!"),
      helpText("Use ", 
               a("UniProtKB database", href = "https://www.uniprot.org/", 
                 target="_blank"),
               " to check your protein identifier or to find it.")
      ),
  
  
    ## Right panel ####
  navset_card_underline(
    ## Box 1 - Tables ####
    height = '800px',
    title = h2("Final studies"),
    # height = '1500px',
    ### Relevant table ####
    nav_panel(h4("Relevant information"), 
              shinycustomloader::withLoader(DT::dataTableOutput(outputId = "relevantTab"), 
                                            type = 'html', loader = 'dnaspin'),
    ),
    ### Article table ####
    nav_panel(h4("Article information"),
              # p("..."),
              shinycustomloader::withLoader(DT::dataTableOutput(outputId = "articleTab"), 
                                            type = 'html', loader = 'dnaspin'),
    ),
    ### Proteomic table ####
    nav_panel(h4("Proteomics information"),
              # p("..."),
              shinycustomloader::withLoader(DT::dataTableOutput(outputId = "proteomicTab"), 
                                            type = 'html', loader = 'dnaspin'),
    )
    )
  ),
  
  card(    
    height = '1700px',
    card_header(h2("Protein search results")), 
    card_body(
      p("For each study, the corresponding log2 fold change (logFC) is shown in 
        the bar plot below. Bars are grouped according to statistical significance
        and the type of p-value (raw or adjusted). A positive logFC indicates 
        higher protein abundance in type 2 diabetic patients compared to 
        normoglycemic controls, whereas a negative logFC indicates higher 
        abundance in normoglycemic individuals."),
      p("The “Adjusted p-value (meta-analysis)” column in the output table refers 
        to the adjusted values used in the meta-analysis performed with the 
        Amanida R package. Adjusted p-values originally reported by each study 
        were retained. However, when only raw p-values were available, adjusted 
        p-values were computed using the Benjamini–Hochberg correction based on 
        the total number of comparisons (i.e., proteins) in the corresponding study. "),
      p("For some articles, raw data had to be reanalyzed to obtain differential
        abundance results for the target groups or to address issues such as 
        missing fold-change or p-values for the full list of proteins. This may 
        result in minor discrepancies between the original article data and the 
        information provided in this app. Details about the reanalysis are available
        in the panel above, under the Relevant Proteins table"),
      # helpText(textOutput("summaryText")),
      fillable = T, fill = T, 
      shinycustomloader::withLoader(uiOutput(outputId = "logFCPlot"), 
                                    type = 'html', loader = 'dnaspin'),
      shinycustomloader::withLoader(DT::dataTableOutput(outputId = "tabProtein"),
                                    type = 'html', loader = 'dnaspin')
      )
  ),
  

  ## Footer ####
  tags$div(class = "footer",
           includeHTML("footer.html"))
)











# SERVER ####
server <- function(input, output) {

  ## Input information ####
  protein <- eventReactive(input$goSearch, {
    if(is.null(input$idProt)){
      return(NULL)
    }
    protein <- input$idProt
    return(protein)
  })
  
  dfFilter <- eventReactive(input$goSearch, {
    protein <- req(protein())
    if(is.null(protein)){
      return(NULL)
    }
    inData <- protein %in% data$ProteinID
    if (inData){
      df <- searchProtein(protein, 
                          sample = input$typeOfSample, 
                          acquisition = input$acqMode, 
                          test = input$typeTest, 
                          country = input$countryID)
      ## Filter
      df <- df %>%
        dplyr::select(ID, Reference, ProteinName:pvalAdjFinal) %>%
        dplyr::arrange(Reference) %>%
        rename(
          `Protein name or gene name` = ProteinName, 
          `Protein description` = ProteinDescription, 
          `P-value` = pval, 
          `Adjusted p-value` = pvalAdj,
          `Adjusted p-value (meta-analysis)` = pvalAdjFinal)
      return(df)  
    } else {
      return(NULL)
    }
  })
  
  
  dfBasal <- eventReactive(input$goSearch, {
    df <- req(dfFilter())
    protein <- req(protein())
    if(is.null(df) | nrow(df) == 0){
      return(NULL)
    }
    
    dfBasal <- searchDataBasal(df)
    return(dfBasal)
  })
  
  output$textNoInfo <- renderText({
    df <- req(dfFilter())
    protein <- req(protein())
    if(nrow(df) == 0){
      return("No articles match the selected options.")
    } else {
      return("")
    }
    
  })
  
  
  ## No protein ID found message ####
  output$summaryText <- renderUI({ 
    protein <- req(protein())
    if(is.null(protein)){
      return(NULL)
    }
    inData <- protein %in% data$ProteinID
    if(!inData){
      return(HTML(paste0(
        # "<span style='color: darkred; font-weight: bold;'>",
        "<div class='alert alert-danger' role='alert'>",
        "Protein ", protein, " was not matched any entry from the 
        studies considered in this metaanalysis. Review the 
        protein ID and make sure that is a valid entry from 
        UniProtKB.",
        "</div>"
        # "</span>"
        )))
      
    }
  })
  
  
  
  ## Box 1 - Display relevant table ####
  output$relevantTab <- DT::renderDataTable({
    dfBasal <- req(dfBasal())
    if(is.null(dfBasal)){
      return(NULL)
    }
    
    ## Filter
    dfBasalImportant <- dfBasal %>% 
      dplyr::select(Reference, SampleSizeControls, SampleSizeDiabetics, 
                    TypeOfSample, TypeOfSample_2, InfoFC, infoPvalue, 
                    infoAdjustedPvalue, RawData, Reanalysis) %>%
      dplyr::rename(
        `Sample size control group` = SampleSizeControls,
        `Sample size T2DM group` = SampleSizeDiabetics,
        `Type of sample` = TypeOfSample, 
        `Type of sample (grouped)` = TypeOfSample_2, 
        `Fold change data?` = InfoFC,
        `P-value data?` = infoPvalue,
        `Adjusted p-value data?` = infoAdjustedPvalue, 
        `Raw data?` = RawData,
        `Reanalysis?` = Reanalysis
      )
    
    ## Display 
    tab <- DT::datatable(dfBasalImportant, extensions = "Buttons",  
                         rownames = F, escape = F, filter = "top",
                  options = list(ordering = T, dom = "Brft", scrollY = '500px',
                                 scrollX = T, pageLength = nrow(dfBasalImportant),
                                 full_width = TRUE, rowCallback = DT::JS(js),
                                 buttons = list(list(extend = "copy"),
                                                list(extend = "csv"), 
                                                list(extend = "excel")))) %>% 
      DT::formatStyle(1, font = 'bold') 
    return(tab)
  })
  
  ## Box 2 - Display article table ####
  output$articleTab <- DT::renderDataTable({
    dfBasal <- req(dfBasal())
    if(is.null(dfBasal)){
      return(NULL)
    }
    
    ## Filter
    dfBasalPaper <- dfBasal %>% 
      dplyr::select(Reference, Country:DOI, Title:Authors)
    
    ## Display 
    tab <- DT::datatable(dfBasalPaper, extensions = "Buttons", 
                         rownames = F, escape = F, filter = "top", 
                             options = list(ordering = T, dom = "Brft", scrollY = '500px',
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
    return(tab)
  })
  
  
  ## Box 3 - Display proteomic table ####
  output$proteomicTab <- DT::renderDataTable({
    dfBasal <- req(dfBasal())
    if(is.null(dfBasal)){
      return(NULL)
    }
    
    ## Filter
    dfBasalProteomics <- dfBasal %>% 
      dplyr::select(Reference, DataAdquisition:SoftwareA, 
                    Normalization:Tool_FuncitonalAnalysis)
    colnames(dfBasalProteomics) <- c(
      "Reference", "Data acquisition mode", "DIA method", "Comercial house", 
      "Instrument model", "Reference library", 
      "Identification & Quantification software", "Normalization", "Statistical test", 
      "Adjusted p-values?", "Method of adjustment", "Data imputation?", 
      "Type of imputation", "Functional analysis?", "Tool for functional analysis"
    )
    
    ## Display 
    tab <- 
      DT::datatable(dfBasalProteomics, extensions = "Buttons",
                    rownames = F, escape = F, filter = "top", 
                    options = list(ordering = T, dom = "Brft", scrollY = '500px',
                                   scrollX = T, pageLength = nrow(dfBasalProteomics),
                                   full_width = TRUE, rowCallback = DT::JS(js),
                                   buttons = list(list(extend = "copy"),
                                                  list(extend = "csv"), 
                                                  list(extend = "excel")))) %>%
      DT::formatStyle(1, font = 'bold') %>% 
      DT::formatStyle(
        columns = colnames(dfBasalProteomics),  # Aplica el estilo a todas las columnas
        target = 'row',
        whiteSpace = 'nowrap'  # Evita que el contenido se ajuste dentro de las celdas
      )
    return(tab)
  })
  
  
  ## Bottom box - Figure ####
  output$logFCPlot <- renderUI({
    df <- req(dfFilter())
    if(is.null(df)){
      return(NULL)
    }
    protein <- req(protein())
    
    dfPlot <- df %>% dplyr::select(-ID)
    
    ### Removing proteins without fold change info ####
    dfPlot <- dfPlot[!(is.na(dfPlot$logFC) & is.na(dfPlot$FC)), ]
    dfPlot$logFC <- as.numeric(dfPlot$logFC)
    ### Finding significance ####
    dfPlot$pval <- ifelse(is.na(dfPlot$`P-value`), NA,
                          ifelse(dfPlot$`P-value` < 0.05, "Significant (p-val)", 
                                 "Non significant (p-val)"))
    dfPlot$pvalAdj <- ifelse(is.na(dfPlot$`Adjusted p-value`), NA,
                             ifelse(dfPlot$`Adjusted p-value` < 0.05, 
                                    "Significant (adjusted)", 
                                    "Non significant (adjusted)"))
    dfPlot$Significance <- ifelse(is.na(dfPlot$pval) & is.na(dfPlot$pvalAdj), 
                                  "Without p-value info", 
                                  ifelse(is.na(dfPlot$pvalAdj), 
                                         dfPlot$pval, 
                                         dfPlot$pvalAdj))
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
    plogFC <- ggplot(dfPlot, aes(x = Reference, 
                                 y = logFC,
                                 fill = Significance, 
                                 text = paste0("Article: ", Reference, "\n",
                                               "logFC: ", round(logFC, 2), "\n", 
                                               "Interpretation: ", Significance))) +
      geom_bar(stat = "identity", width = 0.5) +
      labs(x = "", y = "logFC", fill = "", title = protein) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 14),
        title = element_text(size = 22),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.position = "bottom", 
        axis.line = ggplot2::element_line(linewidth = 0.5, 
                                          colour = "black"), 
        axis.ticks = ggplot2::element_line(linewidth = 0.5, 
                                           colour = "black")) +
      scale_fill_manual(values = c("Without p-value info" = "pink",
                                   # "Without p-value info" = "darkgrey",
                                   # "Significant (p-val)" = "darkgreen",
                                   "Significant (p-val)" = "lightskyblue",
                                   # "Significant (adjusted)" = "#4DAF4A",
                                   "Significant (adjusted)" = "royalblue4",
                                   # "Non significant (p-val)" = "darkred", 
                                   # "Non significant (adjusted)" = "pink" 
                                   "Non significant (p-val)" = "lightgrey", 
                                   "Non significant (adjusted)" = "#595959" 
      )) +
      ylim(c(-ylimInfo, ylimInfo))
    
    plotlogFC <- suppressWarnings(plotly::ggplotly(plogFC, 
                                                   tooltip = "text", 
                                                   height = 700)
    ) %>% 
      plotly::config(
        modeBarButtonsToRemove = c("autoScale2d", "lasso2d", "select2d", "pan2d"), 
        displaylogo = FALSE
      ) %>% 
      plotly::layout(
        yaxis = list(range = c(-ylimInfo, ylimInfo))) 
        # legend = list(orientation = "h",   # horizontal
        #               x = 0.5,             # centrado horizontalmente
        #               xanchor = "center",
        #               y = -0.7))
    return(plotlogFC)
  })
  
  
  ## Bottom box - Results table ####
  output$tabProtein <- DT::renderDataTable({
    df <- req(dfFilter())
    if(is.null(df)){
      return(NULL)
    }
    
    ## Filter
    df <- df %>% dplyr::select(-ID)
    
    ## Display 
    tab <- DT::datatable(df, extensions = "Buttons", rownames = F, escape = F,
                  options = list(ordering = T, dom = "Brft", scrollY = '500px',
                                 scrollX = T, pageLength = nrow(df),
                                 full_width = TRUE, rowCallback = DT::JS(js),
                                 buttons = list(list(extend = "copy"),
                                                list(extend = "csv"), 
                                                list(extend = "excel")))) %>% 
      DT::formatStyle(1, font = 'bold') %>%
      DT::formatRound(4:ncol(df), digits = 3) 

    return(tab)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

































