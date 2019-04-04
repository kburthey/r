library(shiny)
library(ggplot2)
library(collapsibleTree)
library(shinythemes)
library(ggtree)
load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_4850/datasets/movies.Rdata"))

# Define UI for application that plots features of movies
ui <- fluidPage(theme = shinytheme("cosmo"),
  
  #style html elements with css            
  tags$head(
    tags$style(HTML("
           h2, p {
            text-align: center;
           }
           "))
  ),
  #Title for page
  titlePanel(
    "Test", windowTitle = "3 Plot test"
    
  ),
  
    tags$div(
    HTML("<p> Choose different tabs to browse the plots </p>
          <hr/>")
    ),
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(inputId = "y",
                  label = "Y-axis:",
                  choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
                  selected = "audience_score"),
      
      # Select variable for x-axis
      selectInput(inputId = "x",
                  label = "X-axis:",
                  choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
                  selected = "critics_score")
    ),
    
    # Outputs
    mainPanel(
      tabsetPanel(
        tabPanel("scatterplot", plotOutput(outputId = "scatterplot")),
        tabPanel("densityplot", plotOutput(outputId = "densityplot", height = 400)),
        tabPanel("tree", collapsibleTreeOutput(outputId = "tree")),
        tabPanel("ggtree", plotOutput(outputId = "ggtree"))
      )
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create scatterplot
  output$scatterplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
  
  # Create densityplot
  output$densityplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x)) +
      geom_density()
  })
  output$tree <- renderCollapsibleTree({
    org <- data.frame(
         Manager = c(
            NA, "Ana", "Ana", "Bill", "Bill", "Bill", "Claudette", "Claudette", "Danny",
             "Fred", "Fred", "Grace", "Larry", "Larry", "Nicholas", "Nicholas"
           ),
         Employee = c(
            "Ana", "Bill", "Larry", "Claudette", "Danny", "Erika", "Fred", "Grace",
             "Henri", "Ida", "Joaquin", "Kate", "Mindy", "Nicholas", "Odette", "Peter"
           ),
         Title = c(
             "President", "VP Operations", "VP Finance", "Director", "Director", "Scientist",
             "Manager", "Manager", "Jr Scientist", "Operator", "Operator", "Associate",
             "Analyst", "Director", "Accountant", "Accountant"
         ),
         Distance = c(
           ".22", ".88", ".3", ".5", ".4", ".4",
           ".22", ".88", ".3", ".5", ".4", ".4",
           ".9", ".1", ".9", ".9"
         )
       )
    org$Color <- org$Title
    levels(org$Color) <- colorspace::rainbow_hcl(11)
    collapsibleTreeNetwork(org, c("Manager", "Employee", "Title"), attribute = "Distance", tooltip = TRUE, fill = "Color", zoomable = FALSE)
  })
  output$ggtree <- renderPlot({
    ggtree(data = movies) + ggtitle("test")
  })
}

# Create the Shiny app object
shinyApp(ui = ui, server = server)