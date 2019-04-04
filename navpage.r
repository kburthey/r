library(shiny)
library(dplyr)
library(ggplot2)
library(treeio)
library(ggtree)
library(collapsibleTree)
library(readr)
library(tools)
library(shinythemes)
nanopore <- read.csv(file="C:/Users/t862537/nanopore_tree.csv", header = TRUE, sep = ",")
tree <- read.tree(file="C:/Users/t862537/DEV/tree5.txt")
nanopore2 <- read.csv(file="C:/Users/t862537/nanopore_tree2.csv", header = TRUE, sep = ",")
#UI

ui <- shinyUI(
    
    navbarPage("My Application", 
      navbarMenu("Tree",
        tabPanel("Basic",
          shinythemes::themeSelector(),
          tags$head(
            tags$style(
              HTML("
                
                h2, p, h3, h4 {
                  text-align: center;
                }
                hr {
                  color: lightgreen;
                  background-color: lightgreen;
                  height: 2px; 
                }
                .info {
                  color: grey;
                  font-size: 10px;
                }
                .selectize-input {
                  max-height: 80px;
                  overflow-y: scroll;
                }
                
              ")
            )
          ), 
          titlePanel("Collapsible Tree"),
          tags$div(
            HTML("<p> View Collapsible Tree plot below. Select variable to display specific taxa </p><hr>")
          ),
          sidebarPanel(
            HTML("<h4>Tree Filters: </h4>"),
            checkboxGroupInput(
              inputId = "selected_var",
              label = "Select variables: ",
              choices = names(nanopore),
              selected = c("Superkingdom", "Phylum", "Class", "Order", "Family", "Genus", "Cumulative.Read.Count", "Avg.Accuracy")
            )
          ),
          mainPanel(
            HTML('<h3>Collapsible Tree</h3>'),
            collapsibleTreeOutput(outputId = "tree", height = "600px")
          ),
          HTML("<hr><h3>Data Table</h3><p class='info'>Select variables to view in table</p>"),
          DT::dataTableOutput(outputId = "treetable2")
        ),
        tabPanel("All",
          titlePanel("Collapsible Tree: Summary"),
          tags$div(
            HTML("<p> View Collapsible Tree Summary plot below. Select variable to display specific taxa </p>
            <p> Use the sliders to filter numeric data and the text box to filter by Genus </p>
            <hr>")
          ),
          sidebarPanel(
            HTML("<h4>Tree Filters: </h4>"),
            selectInput(
              inputId = "name",
              selectize = TRUE,
              multiple = TRUE,
              label = "Select Genus:",
              choices = nanopore2$Genus,
              selected = nanopore2$Genus
            ),
            checkboxGroupInput(
              inputId = "selected_var2",
              label = "Select variables: ",
              choices = names(nanopore),
              selected = c("Superkingdom", "Phylum", "Class", "Order", "Family", "Genus", "Cumulative.Read.Count", "Avg.Accuracy")
            ),
            sliderInput(
              inputId = "slide2",
              label = "Filter by Read Count: ",
              min = 0.0, max = 20000.0,
              value = 100
            ),
              sliderInput(
                inputId = "slide",
                label = "Filter by Average Accuracy: ",
                min = 75.0, max = 90.0,
                value = 75.0
            ),
              radioButtons(
              inputId = "filetype",
              label = "Select filetype:",
              choices = c("csv", "tsv"),
              selected = "csv"
            ),
            downloadButton("download_data", "Download Table")
          ),
          mainPanel(
            HTML("<h3>Collapsible Tree Summary: Interactive </h3><p class='info'>Select Cumulative Read Count and Avg Accuracy variables to see graph</p>"), 
            collapsibleTreeOutput(outputId = "tree4", height = "750px")
          ),
          HTML("<hr><h3>Data Table</h3><p class='info'>Required: Genus, Cumulative Read Count, Avg Accuracy</p>"),
          DT::dataTableOutput(outputId = "treetable")
        )
      ),
      navbarMenu("Bar",
        tabPanel("Order", 
          
          titlePanel("Bar Chart: 'Order'"),
          tags$div(
            HTML("<p> View Bar Chart below that filters by Order for taxa Class </p><hr>")
          ),
          #HTML('<h3>Bar Chart: "Order"</h3>'), 
          plotOutput(outputId = "bar", height = "700px")
        ),
        tabPanel("Class",
          titlePanel("Bar Chart: 'Class'"),
          tags$div(
            HTML("<p> View Bar Chart below that filters by Family for taxa Order </p><hr>")
          ),
          #HTML('<h3>Bar Chart: "Order"</h3>'), 
          plotOutput(outputId = "bar2", height = "700px")        
      
        )
      ),
      tabPanel("Plot",
        sidebarPanel(
          HTML("<h4>ScatterPlot Controls: </h4>"),
          selectInput(
            inputId = "y", 
            label = "Y-axis:",
            choices = names(nanopore),
            selected = "Genus"
          ),
          
          selectInput(
            inputId = "x",
            label = "X-axis:",
            choices = c("Superkingdom", "Phylum", "Class", "Order"),
            selected = "Class"
          ),
          radioButtons(
            inputId = "color_filter",
            label = "Color scatterplot by: ",
            choices = c("Cumulative.Read.Count", "Avg.Accuracy"),
            selected = "Cumulative.Read.Count"
          )
        ),
        mainPanel(
          HTML("<h3>Scatterplot</h3>"), 
          plotOutput(outputId = "scatterplot", height = "700px")
        )

      )
      
    )
)

server <- function(input, output){
  output$details <- renderText({
    paste0("Avg Accuracy filter: ",input$slide,". Cumulative Read Count filter: ", input$slide2)
  })
  output$scatterplot <- renderPlot({
    ggplot(data = nanopore, aes_string(x = input$x, y = input$y, color = input$color_filter)) + geom_point()
  })
  
  output$download_data <- downloadHandler(
    filename = function(){
      paste0("TreeTable.", input$filetype)
    },
    content = function(file){
      if(input$filetype == "csv"){
        write_csv(combined() %>% select(input$selected_var2), file)
      }
      if(input$filetype == "tsv"){
        write_tsv(combined() %>% select(input$selected_var2), file)
      }
    }
  )
  
  var_selected <- reactive({
    req(input$selected_var)
    nanopore %>% select(input$selected_var)
  })
  var_selected2 <- reactive({
    req(input$selected_var2)
    nanopore %>% select(input$selected_var2)
  })
  output$treetable <- DT::renderDataTable({
    req(var_selected2()$Genus)
    DT::datatable(data = combined() %>% select(input$selected_var2),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  output$treetable2 <- DT::renderDataTable({
    req(var_selected()$Genus)
    DT::datatable(data = var_selected() %>% select(input$selected_var),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  }) 
  taxa_selected <- reactive({
    req(input$select_taxa)
    nanopore %>% select(input$select_taxa)
  })
  acc_selected <- reactive({
    req(input$slide)
    req(var_selected()$Avg.Accuracy)
    nanopore %>% filter(var_selected2()$Avg.Accuracy >= input$slide)
  })
  read_slide <- reactive({
    req(input$slide2)
    req(var_selected()$Cumulative.Read.Count)
    nanopore %>% filter(var_selected2()$Cumulative.Read.Count >= input$slide2)
  })
  combined <- reactive({
    req(input$slide)
    req(input$slide2)
    req(var_selected2()$Cumulative.Read.Count)
    req(var_selected2()$Avg.Accuracy)
    req(input$name)
    nanopore %>% filter(var_selected2()$Cumulative.Read.Count >= input$slide2 & var_selected()$Avg.Accuracy >= input$slide & var_selected2()$Genus %in% input$name)
  })
  output$tree <- renderCollapsibleTree({
    collapsibleTree(nanopore, hierarchy = names(var_selected()), collapsed = FALSE, tooltip = TRUE, zoomable = TRUE)
  })
  
  output$ggtree <- renderPlot({
    ggtree(tree) + geom_tiplab()
  })

  output$tree2 <- renderCollapsibleTree({
    collapsibleTreeSummary(acc_selected(), root="nanopore", hierarchy = names(var_selected()), collapsed = FALSE, tooltip = TRUE, zoomable = TRUE)
  })
  output$tree3 <- renderCollapsibleTree({
    collapsibleTreeSummary(read_slide(), root="nanopore", hierarchy = names(var_selected()), collapsed = FALSE, tooltip = TRUE, zoomable = TRUE)
  })
  output$tree4 <-renderCollapsibleTree({
    collapsibleTreeSummary(combined(), root="nanopore", hierarchy = names(var_selected()), collapsed = FALSE, tooltip = TRUE, zoomable = TRUE)
  })
  output$bar <- renderPlot({
    ggplot(nanopore, aes(Order, fill = Family)) + geom_bar()
  })
  output$bar2 <- renderPlot({
    ggplot(nanopore, aes(Class, fill = Order)) + geom_bar()
  })
}
shinyApp(ui = ui, server = server)