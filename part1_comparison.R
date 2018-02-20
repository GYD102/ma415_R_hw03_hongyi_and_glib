library(ggplot2)
library(tidyverse)

#function that turn the sector col to row name
rowname <- function(x){
  rownames(x) <- x[,1]
  x<-subset(x,select=-Sector)
  x
}

itemize_df <- function(df,yr){
  tibble("Year" = rep(yr,
                      times = nrow(df)*ncol(df)),
         "Sector" = rep(rownames(df),
                        each = ncol(df)),
         "Month" = rep(colnames(df),
                       times = nrow(df)),
         "Apprehensions" = c(t(df)))
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

x <- itemize_df(year2010,"2010")
y <- itemize_df(year2017,"2017")
z <- rbind(x,y)

# This function creates the desired plot given a data input
ourplot <- function(dset){
  monthOrder <- rep(1:12, times = length(dset$Month)/12)
  ggplot(data = dset, aes(reorder(Month,monthOrder),
                       Apprehensions,
                       color = Sector,
                       group = interaction(Sector,Month),
                       shape = Year)) +
    geom_point(size = 2,
               position = position_dodge(width = 0.8)) +
    geom_line(position = position_dodge(width=0.8)) +
    geom_line(aes(group = interaction(Year, Sector)),
              position = position_dodge(width = 0.8),
              linetype = "dotted") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
ourplot(z)

# Since it is difficult to visualize smaller data, we take a subset excluding
# the sectors that have the most extreme apprehension tallies. This effectively
# "zooms the graph in."
z2 = subset(z, !(Sector %in% c("Tucson","Rio Grande Valley")))
ourplot(z2)

# Again we "zoom in" by excluding the sectors with the most apprehensions per
# month.
z3 = subset(z2, !(Sector %in% c("San Diego", "El Centro", "El Paso")))
ourplot(z3)
