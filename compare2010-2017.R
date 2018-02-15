#function that turn the sector col to row name


#load the data and clean them
year2010 <- read.csv("BP Apprehensions 2010.csv")
year2017 <- read.csv("PB Apprehensions 2017.csv")
year2010 <- rowname(year2010)
year2017 <- rowname(year2017)
sector <- rownames(year2010)
month <- colnames(year2010)

#function that shows side by side graph for each sector


#function that shows side by side graph for each year


sbsgraph_sec(6)
sbsgraph_year(11)
sbsgraph_sec(1)
