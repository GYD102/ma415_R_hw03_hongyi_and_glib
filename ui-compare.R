rowname <- function(x){
  rownames(x) <- x[,1]
  x<-subset(x,select=-Sector)
  x
}

#load the data and clean them
year2010 <- read.csv("BP Apprehensions 2010.csv")
year2017 <- read.csv("PB Apprehensions 2017.csv")
year2010 <- rowname(year2010)
year2017 <- rowname(year2017)
sector <- rownames(year2010)
month <- colnames(year2010)

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Give the page a title
  titlePanel("Aprehension in US-Mexico border between 2010 and 2017"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Sector panel
    sidebarPanel(
      selectInput("region", "Sector:", 
                  choices=sector),
      hr(),
      helpText("Please select the sector you want, compare them in different months between 2010 and 2017")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("graph")  
    )
    
  )))
