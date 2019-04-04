library(shiny)
library(dplyr)
library(readr)
load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_4850/datasets/movies.Rdata"))

# UI
ui <- fluidPage(
  sidebarLayout(
    
    # Input(s)
    sidebarPanel(
      
      # Select filetype
      radioButtons(inputId = "filetype",
                   label = "Select filetype:",
                   choices = c("csv", "tsv"),
                   selected = "csv"),
      
      # Select variables to download
      checkboxGroupInput(inputId = "selected_var",
                         label = "Select variables:",
                         choices = names(movies),
                         selected = c("title"))
      
    ),
    
    # Output(s)
    mainPanel(
      uiOutput(outputId = "n"),
      DT::dataTableOutput(outputId = "moviestable"),
      downloadButton("download_data", "Download data")
      
    )
  )
)

# Server
server <- function(input, output) {
  
  output$n <- renderUI({
    HTML(paste0("<h1> Movie select data table </h1>"))
  })
  
  # Create reactive data frame
  movies_selected <- reactive({
    req(input$selected_var)
    movies %>% select(input$selected_var)
  })
  
  # Create data table
  output$moviestable <- DT::renderDataTable({
    DT::datatable(data = movies_selected() %>% select(input$selected_var), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  # Download file
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("movies.", input$filetype)
    },
    content = function(file) { 
      if(input$filetype == "csv"){ 
        write_csv(movies_selected() %>% select(input$selected_var), file) 
      }
      if(input$filetype == "tsv"){ 
        write_tsv(movies_selected() %>% select(input$selected_var), file) 
      }
    }
  )
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)