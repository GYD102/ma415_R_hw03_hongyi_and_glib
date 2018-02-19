#convert the sector column into rowname
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

# Rearranging Columns into calendar order
year2010 <- cbind(year2010[4:12], year2010[1:3])
year2017 <- cbind(year2017[4:12], year2017[1:3])

sector <- rownames(year2010)
month <- colnames(year2010)

#convert the input string into correct index
w<-c(1:12)
names(w)<-month
v<-c(1:9)
names(v)<-sector

#function that shows side by side graph for each sector
sbsgraph_sec <- function(i){ 
  year_comb <- rbind(year2010[i,],year2017[i,])
  barplot(as.matrix(year_comb), beside = TRUE, col = c("red", "blue"), bty="n",las=2 )
  legend("topright", c(paste(sector[i] ,"2010"),paste(sector[i],"2017")), pch=15,  col=c("red","blue"),  bty="n")
}

#function that shows side by side graph for each year
sbsgraph_year <- function(i){
  year2010<-t(year2010)
  year2017<-t(year2017)
  year_comb <- rbind(year2010[i,],year2017[i,])
  barplot(as.matrix(year_comb), beside = TRUE, col = c("green", "yellow"), bty="n" ,las=2)
  legend("topleft", c(paste(month[i] ,"2010"),paste(month[i],"2017")), pch=15,  col=c("green", "yellow"),  bty="n")
}
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Give the page a title
  titlePanel("Apprehension on the US-Mexico border between 2010 and 2017"),
  
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
    
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$graph <- renderPlot({
    
    sbsgraph_sec(v[input$region])
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

