# Author: Sharan Kunar Ravindran
# Walmart Store Sales Forecasting Model
# This only contain the basic version of the forecasting model used.
# The pre-processing code and the actual model used in the contest is lost

for (i in 1:45) {
  for (j in 1:99) {
    
    #h=paste("C:/Users/Flutura Business/Desktop/Walmart/Data/", paste(paste(i,j,sep="-"),".csv",sep=""), sep="")
    sql <- sprintf("select Weekly_Sales from trainData where Store=%s AND Dept=%s", i, j)
    score<-sqldf(sql);
    if(nrow(score)>104){
      
      ts=ts(score,start=c(2010,2),frequency=52)
      h_model=HoltWinters(ts,gamma=FALSE)
      require("forecast")#Loading Forecast Package
      hforecast=forecast.HoltWinters(h_model,h=39)
      predscore <- hforecast$mean
      a <- paste(i,j,sep="_")
      prediction <- cbind(a,predscore)
      h=paste("C:/Users/Flutura Business/Desktop/Walmart/Data/Predictions/Predictions",".csv", sep="")
      write.table(prediction, file=h, sep=",", append=TRUE,col.names=FALSE)
    }
    else{rows=nrow(score);tc=paste(a,rows,"");print(tc);}
    
  }
}


