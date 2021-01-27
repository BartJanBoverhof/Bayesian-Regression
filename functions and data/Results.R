posterior.hist <- function(x, variable, title){ #Function for plotting posterior sample per param
  #Prereqs
  quant1 <- c(as.vector(quantile(x[,variable], probs = 0.01))) #1% Quant
  meann <- mean(x[,variable]) #mean
  quant2 <- c(as.vector(quantile(x[,variable], probs = 0.99))) #99% Quant

  ggplot(data=x, aes(x=x[,variable]))+
    geom_histogram(colour = "#474642",size=0.7, fill="grey")+ #Histogram
    geom_vline(xintercept = round(meann,3), colour="red")+ #Add vertical line for mean
    geom_vline(xintercept = round(quant1,3), colour = "#82a0c2",size=0.7)+ #Add vertical line for 1 quant
    geom_vline(xintercept = round(quant2,3), colour = "#82a0c2",size=0.7)+ #Add vertical line for 99 quant
    scale_x_continuous(breaks = c(round(quant1,3) ,round(meann,3), round(quant2,3)))+ #Display mean and quants on x
    ggtitle("",title)+ #Title
    xlab("")+ 
    ylab("")
}
