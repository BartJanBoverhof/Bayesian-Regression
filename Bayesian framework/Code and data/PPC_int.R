###################### 3. MODEL ASSUMPTION BY PPC ###################### 
ppc2 <- function(x, t, data){
  #x = The sample from the parameter posterior distribution 
  #t = the amount of samples to draw (can't exceed the amount of samples from posteriors of the paramaters!)
  #data = data used
  
  #Prerequisite objects
  observed.data.stats <- vector(length = t) #Vector for storing test stats observed data
  observed.data.stats.2 <- vector(length = t)
  simulated.data.stats <- vector(length=t) #Vector for storing test stats simulated data
  simulated.data.stats.2 <- vector(length=t)
  sim <- vector(length=nrow(data)) #Vector for storing simulated Y-values
  sim.2 <- vector(length=nrow(data)) 
  data <- data 
  
  #Order the data based on the fitted values (low to high)
  data <- data %>% mutate(order1 = sort(mean(x[,1]) + mean(x[,2])*data[,iv[1]] + mean(x[,3])*data[,iv[2]] + mean(x[,4])*data[,iv[3]] + mean(x[,5])*data[,iv[4]]) + mean(x[,6])*data[,iv[1]]*data[,iv[2]]) %>% 
    mutate(order2 = sort(mean(x[,8]) + mean(x[,9])*data[,iv[1]] + mean(x[,10])*data[,iv[2]] + mean(x[,11])*data[,iv[3]] + mean(x[,12])*data[,iv[4]]) + mean(x[,13])*data[,iv[1]]*data[,iv[2]]) #Create ordering for both chain 1 and 2
  
  data.chain1 <- data %>% arrange(order1)
  data.chain2 <- data %>% arrange(order2)
  
  for (i in 1:t){ #Loop over the amount of replications t
    #CHAIN 1
    #TEST STAT OBSERVED DATA
    resid.obs <- data.chain1[,dv] - x[i,1] + x[i,2]*data.chain1[,iv[1]] + x[i,3]*data.chain1[,iv[2]] + x[i,4]*data.chain1[,iv[3]] + x[i,5]*data.chain1[,iv[4]] + x[i,6]*data.chain1[,iv[1]]*data.chain1[,iv[2]] #Calculating residuals
    resid.obs1 <- mean(resid.obs[1:(N/2)]) #Mean of the residuals in the first half
    resid.obs2 <- mean(resid.obs[((N/2)+1):length(resid.obs)]) #Mean of the residuals in the second half
    observed.data.stats[i] <-  abs(resid.obs2-resid.obs1) #Difference in residuals
    
    #TEST STAT SIMULATED DATA
    mu <- x[i,1] + x[i,2]*data.chain1[,iv[1]] + x[i,3]*data.chain1[,iv[2]] + x[i,4]*data.chain1[,iv[3]] + x[i,5]*data.chain1[,iv[4]] + x[i,6]*data.chain1[,iv[1]]*data.chain1[,iv[2]]
    sim <- rnorm(nrow(data),mu,sqrt(x[i,7]))
    
    resid.sim <- sim - x[i,1] + x[i,2]*data.chain1[,iv[1]] + x[i,3]*data.chain1[,iv[2]] + x[i,4]*data.chain1[,iv[3]] + x[i,5]*data.chain1[,iv[4]] + x[i,6]*data.chain1[,iv[1]]*data.chain1[,iv[2]] #Calculating residuals
    resid.sim1 <- mean(resid.sim[1:(N/2)]) #Mean of the residuals in the first half
    resid.sim2 <- mean(resid.sim[((N/2)+1):length(resid.sim)]) #Mean of the residuals in the second half
    simulated.data.stats[i] <-  abs(resid.sim2 - resid.sim1) #Difference in residuals
    
    #CHAIN 2
    #TEST STAT OBSERVED DATA
    resid.obs.2 <- data[,dv] - x[i,8] + x[i,9]*data.chain2[,iv[1]] + x[i,10]*data.chain2[,iv[2]] + x[i,11]*data.chain2[,iv[3]] + x[i,12]*data.chain2[,iv[4]]+ x[i,13]*data.chain2[,iv[1]]*data.chain2[,iv[2]]#Calculating residuals
    resid.obs1.2 <- mean(resid.obs.2[1:(N/2)]) #Mean of the residuals in the first half
    resid.obs2.2 <- mean(resid.obs.2[((N/2)+1):length(resid.obs.2)]) #Mean of the residuals in the second half
    observed.data.stats.2[i] <-  abs(resid.obs2.2-resid.obs1.2) #Difference in residuals
    
    #TEST STAT SIMULATED DATA
    mu.2 <- x[i,8] + x[i,9]*data.chain2[,iv[1]] + x[i,10]*data.chain2[,iv[2]] + x[i,11]*data.chain2[,iv[3]] + x[i,12]*data.chain2[,iv[4]] + x[i,13]*data.chain2[,iv[1]]*data.chain2[,iv[2]]
    sim.2 <- rnorm(nrow(data),mu.2,sqrt(x[i,14]))
    
    resid.sim.2 <- sim.2 - x[i,8] + x[i,9]*data.chain2[,iv[1]] + x[i,10]*data.chain2[,iv[2]] + x[i,11]*data.chain2[,iv[3]] + x[i,12]*data.chain2[,iv[4]] + x[i,13]*data.chain2[,iv[1]]*data.chain2[,iv[2]] #Calculating residuals
    resid.sim1.2 <- mean(resid.sim.2[1:(N/2)]) #Mean of the residuals in the first half
    resid.sim2.2 <- mean(resid.sim.2[((N/2)+1):length(resid.sim.2)]) #Mean of the residuals in the second half
    simulated.data.stats.2[i] <-  abs(resid.sim2.2 - resid.sim1.2) #Difference in residuals
  }
  combined.obs <- c(observed.data.stats, observed.data.stats.2)
  combined.sim <- c(simulated.data.stats, simulated.data.stats.2)
  mean(combined.sim > combined.obs) #Bayesian p-value
}
