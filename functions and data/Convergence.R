###################### 2. ASSESING CONVERGENCE ###################### 
#HISTORY PLOTS
history.plot <- function(x, col1, col2, title){ #Function for constructing history plot (2 chains)
  #x = The sample from the parameter posterior distribution 
  #col1 = collumn number chain 1 of the variable to be plotted
  #col2 = collumn number chain 2 of the variable to be plotted
  #title = title to be attributed to plot
  
  ggplot(data=x)+ #Plotting
    geom_line(aes(y=x[,col1], x=c(1:nrow(x)), colour="red"))+ #Addings lines chain 1
    geom_line(aes(y=x[,col2], x=c(1:nrow(x)), colour="blue"))+ #Addings lines chain 2
    theme(legend.position = "none")+ #Remove legend
    ylab("")+ #Name y
    xlab("Iteration")+ #Name x
    ggtitle("",title) #Title
}





#AUTOCORRELATION PLOTS
autocorr.plot <- function(x, col1, col2, title, max.lag=40){ #Function for constructing autocorr plot
  #x = The sample from the parameter posterior distribution 
  #col1 = collumn number chain 1 of the variable to be plotted
  #col2 = collumn number chain 2 of the variable to be plotted
  #title = title to be attributed to plot
  #max.lag = the maximum amount of laggs for which to calculate and plot the autocorrelation
  
  out1 <- rep(1,max.lag) #Object for storing autocorr values
  index <- 1 #Indexing variable for lags
  for(i in 1:(max.lag-1)){ #Calculate autocorrelation for all laggs
    out1[i+1] <- cor(x[1:(nrow(x)-index),col1], x[(index+1):nrow(x),col1]) #Calculate autocorr
    index <- index+1 #Attributing +1 to lag index
  }
  Lag <- 0:(max.lag-1) #Lag indication variavle
  foo <- as.data.frame(cbind(out1,Lag)) #Combining autocorr values and lagg indicator
  
  index <- 1 #Resetting index at 1
  foo[1,3] <- 1 #Creating a collumn for storing second chain and setting autocorr lag 0 to 1
  for(i in 1:(max.lag-1)){ #Calculate autocorrelation for all laggs
    foo[i+1,3] <- cor(x[1:(nrow(x)-index),col2], x[(index+1):nrow(x),col2]) #Calculate autocorr
    index <- index+1 #Attributing +1 to lag index
  }
  
  ggplot(data=foo)+ #Plotting
    geom_bar(aes(y=out1, x=Lag), stat="identity", colour = "black", fill="red",alpha = 0.4)+ #Plotting chain 1 
    geom_bar(aes(y=V3, x=Lag), stat="identity", colour = "black", fill="blue",alpha = 0.4)+ #Plotting chain 2
    ylab('')+ #Name Y axis
    ggtitle("",title) #Name title
}
