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
data <- readRDS(file = "completeDatabase.rds")

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
  return(dfBasal)
}
















