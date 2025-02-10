#Shiny app

library(shiny)
library(ggplot2)

# Define UI and Server in one file
ui <- fluidPage(
  titlePanel("Iris Dataset: Histograms with Density Lines"),
  
  # Enable LaTeX rendering
  withMathJax(),
  
  # Displaying the density function formula
  p("The density function formula for a normal distribution is given by:"),
  p("$$ f(x) = \\frac{1}{\\sqrt{2\\pi\\sigma^2}} e^{-\\frac{(x - \\mu)^2}{2\\sigma^2}} $$"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu to select the variable
      selectInput("variable", 
                  "Choose a variable to display:", 
                  choices = colnames(iris)[1:4], 
                  selected = "Sepal.Length"),
      
      # Checkbox group to select species
      checkboxGroupInput("species", "Select species:", 
                         choices = unique(iris$Species), 
                         selected = unique(iris$Species)),
      
      # Slider for the number of bins in the histogram
      sliderInput("bins", "Number of bins:", 
                  min = 5, 
                  max = 50, 
                  value = 20),
      
      # Slider for bandwidth (density smoothing) adjustment
      sliderInput("bandwidth", "Adjust Density Smoothing:", 
                  min = 0.1, 
                  max = 1.5, 
                  value = 0.5, 
                  step = 0.1)
    ),
    
    
    # Main panel to display the plot
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram with Density", plotOutput("histDensityPlot")),
        tabPanel("Data Preview", tableOutput("dataTable"))
      )
    )
  )
)

# Server logic for the app
server <- function(input, output) {
  
  # Render the histogram with density line
  output$histDensityPlot <- renderPlot({
    # Filter the dataset based on selected species
    filtered_data <- iris[iris$Species %in% input$species, ]
    
    # Plot the histogram with a density line
    ggplot(filtered_data, aes_string(x = input$variable, fill = "Species", color = "Species")) +
      geom_histogram(aes(y = ..density..), bins = input$bins, alpha = 0.5, position = "identity") +
      geom_density(alpha = 0.3, adjust = input$bandwidth) +
      labs(title = paste("Histogram and Density of", input$variable), 
           x = input$variable, 
           y = "Density") +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  output$dataTable <- renderTable({
    # Filter dataset based on selected species
    head(iris[iris$Species %in% input$species, ])
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
