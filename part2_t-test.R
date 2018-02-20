#load the data
year2010 <- read.csv("BP Apprehensions 2010.csv")
year2017 <- read.csv("PB Apprehensions 2017.csv")

#function that turn the sector col to row name
operation_row <- function(x){
  rownames(x) <- x[,1]
  x<-subset(x,select=-Sector)
  x <- rbind(x, colSums(x))
  -length(rownames(x))  
  rownames(x) <- c(rownames(x)[-length(rownames(x))], "Total")
  x <- cbind(x,rowSums(x))
  colnames(x) <- c(colnames(x)[-length(colnames(x))], "Total")
  x
}

#clean the data
year2010 <- operation_row(year2010)
year2017 <- operation_row(year2017)

#select the sector with the most apprehensions in 2010 and 2017
year2010_rowtotal<-year2010[-nrow(year2010),]
year2017_rowtotal<-year2017[-nrow(year2017),]
most_sec_2010<-(year2010_rowtotal[year2010_rowtotal$Total==max(year2010_rowtotal$Total),])
most_sec_2017<-(year2017_rowtotal[year2017_rowtotal$Total==max(year2017_rowtotal$Total),])
most_sec_2010<-most_sec_2010[,-ncol(most_sec_2010)]
most_sec_2017<-most_sec_2017[,-ncol(most_sec_2017)]

#do the t-test, compare the sector with the most apprehensions in two years
most_sec_2010<-t(most_sec_2010)
most_sec_2017<-t(most_sec_2017)
print(paste(colnames(most_sec_2010),"is sector with most apprehension in 2010")) 
print(paste(colnames(most_sec_2017),"is sector with most apprehension in 2017")) 
t.test(most_sec_2010,most_sec_2017,paired=T)

#select the 3 month periods with the most apprehensions in 2010 and 2017
year2010_coltotal <- year2010[,-ncol(year2010)]
year2017_coltotal <- year2017[,-ncol(year2010)]

#sort the data in 2010,2017 with respect to total apprehension by month
year2010_coltotal <- year2010_coltotal[,order(year2010_coltotal["Total",],decreasing = T)]
year2017_coltotal <- year2017_coltotal[,order(year2017_coltotal["Total",],decreasing = T)]

#select the months with top three total apprehension
most_month_2010 <-  year2010_coltotal[,1:3]
most_month_2017 <-  year2017_coltotal[,1:3]
most_month_2010 <- most_month_2010[-nrow(most_month_2010),]
most_month_2017 <- most_month_2017[-nrow(most_month_2017),]


a <- cbind(most_month_2010[,1],most_month_2010[,2],most_month_2010[,3])
b <- cbind(most_month_2017[,1],most_month_2017[,2],most_month_2017[,3])
#do the t-test, compare the sector with the most apprehensions in two years
print(paste(colnames(most_month_2010),
            "is one of the 3 month periods with the most apprehensions in 2010")) 
print(paste(colnames(most_month_2017),
            "is one of the 3 month periods with the most apprehensions in 2017")) 
t.test(a,b,paired=T)

