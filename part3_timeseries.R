#load, clean and rerrange the data
ts<-read.csv("PB monthly summaries.csv")
rownames(ts) <- ts[,1]
ts <- ts[order(ts$year),]
ts<-subset(ts,select=-year)

#turn it into timeseries format
ts_df<-ts
ts <- as.vector(t(ts))
ts<-ts(ts,start = c(2000,1), frequency=12)

#plot the timeseries data
ts.plot(ts, 
        col = 3, 
        xlab="year", 
        ylab="Apprehensions", 
        lty=c(1:3), 
        main ="Apprehension from 2000 to 2017")

#draw the avg
ts_avg<-apply(ts_df,1,mean)
ts_avg<-ts(ts_avg,start = c(2000), frequency=1)
points(ts_avg,col = 4,pch=20)
lines(ts_avg,col = 4,pch=20,lty=2)

#get the max/min month index
pos_max<-which(ts_df == max(ts_df), arr.ind=T)
pos_min<-which(ts_df == min(ts_df), arr.ind=T)

#get the month vector
month<-c("October","November","December","January","February","March","April","May","June","July","August","September")

#get the exact time point of the maximum/minimum element in timeseries data
max_index <- time(ts)[ts==max(ts)]
min_index <- time(ts)[ts==min(ts)]
max_avg <- time(ts_avg)[ts_avg==max(ts_avg)]
min_avg <- time(ts_avg)[ts_avg==min(ts_avg)]

#label the maximum/minimum apprehension in the graph
text(max_index,max(ts),
     paste("num:",max(ts),"\n","date:",floor(max_index),month[as.vector(pos_max)[2]]),
     cex=0.4,pos=4)
text(min_index,min(ts),
     paste("num:",min(ts),"\n","date:",floor(min_index),month[as.vector(pos_min)[2]]),
     cex=0.4,pos=2)
text(max_avg,
     max(ts_avg),
     paste("avg max:",floor(max(ts_avg)),"\n","year:",floor(max_avg)),
     cex=0.4,
     pos=4)
text(min_avg,
     min(ts_avg),
     paste("avg min:",floor(min(ts_avg)),"\n","year:",floor(min_avg)),
     cex=0.4,
     pos=3)
text(2005,
     ts_avg[time(ts_avg)==2005],
     paste("avg:",floor(ts_avg[time(ts_avg)==2005]),"\n","year:",2005),
     cex=0.4,
     pos=3)

#pinpoint the maximum/minimum apprehension in the graph
points(max_index,max(ts),pch=19,col=2)
points(min_index,min(ts),pch=19,col=7)

#add a legend which denotes the meaning of the points
legend("topright", 
       c("maximum apprehension",
         "minimum apprehension",
         "annual average apprehension"),
       col = c(2,7,4),
       pch =c(19,19,20),
       lty=c(0,0,2),
       bty = "n",
       bg = 'gray90')