ts<-read.csv("PB monthly summaries.csv")
rownames(ts) <- ts[,1]
#rearrange the dataframe, sort it with respect to year from 2000 to 2017
ts <- ts[ order(ts$year) , ]
#delete the year column
ts <- subset( ts , select=-year )
#rearrange the column so that it is ranked from Jan to Dec.
ts<-ts[,c("January",
          "February",
          "March",
          "April",
          "May",
          "June",
          "July",
          "August",
          "September",
          "October",
          "November",
          "December")]
ts_df <- ts
ts <- as.vector(t(ts))
ts_avg <- apply(ts_df,1,mean)

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  
  # Give the page a title
  titlePanel("Aprehension in US-Mexico border"),
  
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
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # Fill in the spot we created for a plot
  output$graph <- renderPlot({
    ts <- ts(ts , start = 2000 , end=c(input$max,12) , frequency=12)
    ts_avg <- ts(ts_avg , start = 2000 , end=c(input$max) , frequency=1)
    
    # Render a barplot
    ts.plot(ts, 
            col = 3, 
            xlab="year", 
            ylab="Apprehensions", 
            lty=c(1:3), 
            main =paste("Apprehension from 2000 to",input$max))
    points(ts_avg,col = 4,pch=20)
    lines(ts_avg,col = 4,pch=20,lty=2)
    
    max_index <- time(ts)[ts==max(ts)]
    min_index <- time(ts)[ts==min(ts)]
    max_avg <- time(ts_avg)[ts_avg==max(ts_avg)]
    min_avg <- time(ts_avg)[ts_avg==min(ts_avg)]
    points(max_index,max(ts),pch=19,col=2)
    points(min_index,min(ts),pch=19,col=7)
    points(max_avg,max(ts_avg),pch=19,col=3)
    points(min_avg,min(ts_avg),pch=19,col=5)
    legend("topright", 
           c(paste("maximum apprehension:",max(ts)," year:",floor(max_index)),
             paste("minimum apprehension:",min(ts)," year:",floor(min_index)),
             paste("maximum annual average apprehension:",floor(max(ts_avg))," ",floor(max_avg)),
             paste("minimum annual average apprehension:",floor(min(ts_avg))," ",floor(min_avg)),
             "annual average apprehension "),
           col = c(2,7,3,5,4),
           pch =c(19,19,19,19,20),
           lty=c(0,0,0,0,2),
           bty = "n",
           bg = 'gray90')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

