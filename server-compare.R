#function that turn the sector col to row name
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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$graph <- renderPlot({
    
    sbsgraph_sec(v[input$region])
  })
  
})
