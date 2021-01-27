###################### 5. DIC ###################### 

#Dic function for model WITHOUT interaction
DIC <- function(x, data=data){ #Function for determining DIC
  #x = The sample from the parameter posterior distribution 
  #data = data used
  
  loglikelihood.2 <- NULL #Defining object for storage later 
  
  #DHAT
  residuals <- data[,dv] - (mean(x[,1]) + mean(x[,2])*data[,iv[1]] + mean(x[,3])*data[,iv[2]] + mean(x[,4])*data[,iv[3]] + mean(x[,5])*data[,iv[4]]) #Calculate residuals conditional on posterior mean
  part1 <- -(N/2) * log(2*pi) #First term of loglikelihood
  part2 <- N*log(sqrt(mean(x[,6]))) #Second term of loglikelihood
  part3 <-   (1/(2*mean(x[,6]))) * sum(residuals^2) #Third term of loglikelihood
  DHAT <-  -2* (part1 - part2 - part3) #Dhat (-2*loglikelihod)
  
  #DBAR
  for (i in 1:nrow(x)){ #Iterate over samples of the posterior
    residuals2 <- data[,dv] - (x[i,1] + x[i,2]*data[,iv[1]] + x[i,3]*data[,iv[2]] + x[i,4]*data[,iv[3]] + x[i,5]*data[,iv[4]]) #For iteration i, calculate residuals
    part1.2 <- -(N/2) * log(2*pi) #For iteration i, first term of the loglikelihood
    part2.2 <- N*log(sqrt(x[i,6])) #For iteration i, second term of the loglikelihood
    part3.2 <-   (1/(2*x[i,6])) * sum(residuals2^2) #For iteration i, third term of the loglikelihood
    loglikelihood.2[i] <-  -2* (part1.2 - part2.2 - part3.2) #For iteration i, loglikelihood *-2 
  }
  DBAR <- 1/nrow(x) * sum(loglikelihood.2) #Determining DBAR
  PD <- 2*(DBAR - DHAT )#Determining the penalty PD
  
  return.object <- round(rbind("Mean deviance" = DHAT, "Pentaly" = PD, "Penalized deviance" = (DHAT+PD)),3) #Format an summary to return
  colnames(return.object) <- "Value" #Attributing collumn name
  return(return.object)  
}