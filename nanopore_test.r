library(shiny)
library(dplyr)
library(ggplot2)
library(ggtree)
library(collapsibleTree)
library(readr)
library(tools)
nanopore <- read.csv(file="C:/Users/t862537/nanopore_tree.csv", header=TRUE, sep = ',')

#UI
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
           #select variable for y-axis
            selectInput(
                inputId = "y", 
                label = "Y-axis:",
                choices = names(nanopore)[0:(length(nanopore)-2)],
                selected = "Genus"
            ),
            selectInput(
                inputId = "x", 
                label = "X-axis:",
                choices = names(nanopore),
                selected = "Genus"
            ),
            radioButtons(
                inputId = "filetype",
                label = "select filetype:",
                choices = c("csv", "tsv"),
                selected = "csv"
            ),
            checkboxGroupInput(
                inputId = "selected_var",
                label = "select variables: ",
                choices = names(nanopore),
                selected = c("Genus")
            ),
            HTML("Select filetype and Taxa Rank, then hit 'Download' </br>"),
            downloadButton("download_data", "Download data")
        ),
        #Outputs
        mainPanel(
            plotOutput(outputId = "scatterplot"),
            uiOutput(outputId = "n"),
            DT::dataTableOutput(outputId = "treetable"),
            collapsibleTreeOutput(outputId = "tree")
        ) 
    )
)
#Server
server <- function(input, output) {
    #create scatterplot
    output$scatterplot <- renderPlot({
        ggplot(data = nanopore, aes_string(x = input$x, y = input$y)) + geom_point()
    })
    #Download
    output$download_data <- downloadHandler(
        filename = function(){
            paste0("TreeTable.", input$filetype)
        },
        content = function(file){
            if(input$filetype == "csv"){
                write_csv(nanopore %>% select(input$selected_var), file)
            }
            if(input$filetype == "tsv"){
                write_tsv(nanopore %>% select(input$selected_var, file))
            }
        }
    )
    taxa_selected <- reactive({
        req(input$selected_var)
        nanopore %>% select(input$selected_var)
    })
    output$treetable <- DT::renderDataTable({
        DT::datatable(data = taxa_selected() %>% select(input$selected_var),
            options = list(pageLength = 10),
            rownames = FALSE)
    })
    output$tree <- renderCollapsibleTree({
        collapsibleTree(nanopore, hierarchy = names(nanopore))
    })

}

shinyApp(ui = ui, server = server)