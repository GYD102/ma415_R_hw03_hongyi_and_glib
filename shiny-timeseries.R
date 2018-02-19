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
ts_avg_tmp2 <- apply(ts_df,1,mean)

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  
  # Give the page a title
  titlePanel("Apprehensions on the US-Mexico Border"),

  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      sliderInput("max", 
                  label = "Range of year:",
                  min = 2000, max = 2017, value = c(2000,2017),
                  sep = ""),
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
    ts_tmp <- ts[(input$max[1]-2000)*12 + 1:length(ts)]
    ts_tmp2 <- ts(ts_tmp , start = c(input$max[1],1) , end=c(input$max[2],12) , frequency=12)
    
    ts_avg_tmp <- ts_avg[(input$max[1]-2000)+1:length(ts_avg)]
    ts_avg_tmp2 <- ts(ts_avg_tmp , start = c(input$max[1]) , end=c(input$max[2]) , frequency=1)
    
    # Render a lineplot
    ts.plot(ts_tmp, 
            col = 3, 
            xlab="year", 
            ylab="Apprehensions", 
            lty=c(1:3), 
            main =paste("Apprehension from ", input$max[1], " to ",input$max[2]))
    points(ts_avg_tmp2,col = 4,pch=20)
    lines(ts_avg_tmp2,col = 4,pch=20,lty=2)
    
    max_index <- time(ts_tmp2)[ts_tmp2==max(ts_tmp2)]
    min_index <- time(ts_tmp2)[ts_tmp2==min(ts_tmp2)]
    max_avg <- time(ts_avg_tmp2)[ts_avg_tmp2==max(ts_avg_tmp2)]
    min_avg <- time(ts_avg_tmp2)[ts_avg_tmp2==min(ts_avg_tmp2)]
    points(max_index,max(ts_tmp2),pch=19,col=2)
    points(min_index,min(ts_tmp2),pch=19,col=7)
    points(max_avg,max(ts_avg_tmp2),pch=19,col=3)
    points(min_avg,min(ts_avg_tmp2),pch=19,col=5)
    legend("topright", 
           c(paste("maximum apprehension:",max(ts_tmp2)," year:",floor(max_index)),
             paste("minimum apprehension:",min(ts_tmp2)," year:",floor(min_index)),
             paste("maximum annual average apprehension:",floor(max(ts_avg_tmp2))," ",floor(max_avg)),
             paste("minimum annual average apprehension:",floor(min(ts_avg_tmp2))," ",floor(min_avg)),
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

