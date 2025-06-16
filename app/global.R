### Global file

# Libraries
library(dplyr)
library(shiny)
library(shinycustomloader)
library(bslib)
library(DT)
library(plotly)
library(Hmisc)
library(ggplot2)


# Loading data ####
basal <- readRDS(file = "basalDatabase.rds")
basal$DOI <- paste0("https://doi.org/", basal$DOI)
# basal$DOI <- paste0(
#   "<a href='https://doi.org/", basal$DOI, "' target='_blank'>Ver en NCBI</a>"
# )
data <- readRDS(file = "completeDatabase.rds")
data$ID <- gsub(x = data$ID, pattern = "_Pro", replacement = "")

# Global variables ####
js <- c(
  "function(row, data, num, index){",
  "  var $row = $(row);",
  "  if($row.hasClass('even')){",
  "    $row.css('background-color', 'white');",
  "    $row.hover(function(){",
  "      $(this).css('background-color', 'lightgrey');",
  "     }, function(){",
  "      $(this).css('background-color', 'white');",
  "     }",
  "    );",  
  "  }else{",
  "    $row.css('background-color', '#EDFDFA');",
  "    $row.hover(function(){",
  "      $(this).css('background-color', 'lightgrey');",
  "     }, function(){",
  "      $(this).css('background-color', '#EDFDFA');",
  "     }",
  "    );",  
  "  }",
  "}"  
)

# Functions ####
searchProtein <- function(protein, sample = "All", test = "All",
                          acquisition = "All", country = "All"){
  df <- data %>% 
    dplyr::filter(ProteinID == protein) 
  # type os sample filter
  if (!(sample == "All")){
    ids <- basal %>% 
      dplyr::filter(TypeOfSample_2 == sample) %>%
      pull(ID)
    df <- df %>% dplyr::filter(ID %in% ids)
  }
  # Type of test filter
  if (!(test == "All")){
    ids <- basal %>% 
      dplyr::filter(StatisticalTest == test) %>%
      pull(ID)
    df <- df %>% dplyr::filter(ID %in% ids)
  }
  # Type of acquisition mode
  if (!(acquisition == "All")){
    ids <- basal %>% 
      dplyr::filter(DataAdquisition == acquisition) %>%
      pull(ID)
    df <- df %>% dplyr::filter(ID %in% ids)
  }
  # country
  if (!(country == "All")){
    ids <- basal %>% 
      dplyr::filter(Country == country) %>%
      pull(ID)
    df <- df %>% dplyr::filter(ID %in% ids)
  }
  return(df)
}


searchDataBasal <- function(df){
  ids <- df$ID
  if ("1027_15_Pla" %in% ids){
    ids <- gsub(x = ids, pattern = "_Pla", replacement = "")
    typeSample <- "Plasma"
  } else if ("1027_15_Sal" %in% ids){
    ids <- gsub(x = ids, pattern = "_Sal", replacement = "")
    typeSample <- "Saliva"
  }
  
  dfBasal <- basal %>% 
    filter(ID %in% ids) %>%
    dplyr::arrange(Reference) %>%
    dplyr::select(ID, Reference, Title:DOI, DataAdquisition, DIAMethod,
                  ComercialHouse, IntrumentModel, 
                  ReferenceLibrary:SoftwareA, SampleSizeDiabetics:TypeOfSample, 
                  TypeOfSample_2, 
                  SoftwareDownstreamAnalysis:StatisticalTest, 
                  AdjustedMethodDic, AdjustedMethod:Tool_FuncitonalAnalysis, 
                  infoPvalue:RawData
    )
  dfBasal$DOI <- paste0(
    "<a href='", dfBasal$DOI, "' target='_blank'>", dfBasal$DOI,"</a>"
  )
  if ("1027_15" %in% dfBasal$ID){
    dfBasal[which(dfBasal$ID == "1027_15"), "TypeOfSample"] <- typeSample
  }
  return(dfBasal)
}
















