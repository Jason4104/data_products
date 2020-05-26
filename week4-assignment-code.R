library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Statistical Distribution Simulator"),
  
  sidebarLayout( sidebarPanel(
    selectInput(inputId = "u",
                label = "Mean",
                choices = c("-10", "-5", "0", "5", "10"),
                selected = "0"),
    
    selectInput(inputId = "std",
                label = "Standard Deviation",
                choices = c("1", "3", "5"),
                selected = "1"),
    
    radioButtons(inputId = "NumSamp",
                 label = "Number of Samples",
                 choices = c("100", "500", "1000"),
                 selected = "500"),
    
    sliderInput(inputId = "NumBins",
                label = "Number of Bins in Histogram",
                min = 1,
                max = 50,
                value = 30),
    
    actionButton(inputId = "showStats",
                 label = "Print Stats")),
    
    mainPanel(
      plotOutput("distPlot"),
      textOutput("meanValue"),
      textOutput("stdValue")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #Create a reactive object `sample_data` which can be used to other render functions
  sample_data <- reactive({
    Sample_size <- as.numeric(input$NumSamp)
    Avg <- as.numeric(input$u)
    Std_dev <- as.numeric(input$std)
    rnorm(n = Sample_size, mean = Avg, sd = Std_dev)
  })
  
  output$distPlot <- renderPlot({
    Num_bins <- input$NumBins
    samples_df <- as.data.frame(sample_data())
    ggplot(samples_df, aes(x = samples_df$sample_data)) + geom_histogram(aes(y = ..density..), 
                                                                         stat = "bin", bins = Num_bins) +  
      geom_density() + coord_cartesian(xlim = c(-20, 20)) + xlab("Data Values")
  })
  
  
  observeEvent(input$showStats, {
    output$meanValue <- renderText(isolate({paste("Mean =", toString(mean(sample_data())), sep = " ")}))
    output$stdValue <- renderText(isolate({paste("Standard Deviation =", toString(sd(sample_data())), sep = " ")}))
  })
}

shinyApp(ui = ui, server = server)
  