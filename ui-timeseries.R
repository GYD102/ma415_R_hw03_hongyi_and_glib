# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Aprehension in US-Mexico border from 2000 to 2017"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      sliderInput("max", 
                  label = "Range of year:",
                  min = 2000, max = 2017, value = 2017),
      hr(),
      helpText("Please select the range of year.")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("graph")  
    )
    
  ))