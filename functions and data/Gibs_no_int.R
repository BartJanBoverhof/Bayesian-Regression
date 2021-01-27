###################### 1. GIBBS SAMPLER & METROPOLIS HASTINGS WITH 4 VARIABLE ###################### 
gibtropolis <- function(data, k=10000, dv, iv, inits, n.chains=2, burn = 1000){
  #data = dataset to use
  #k = the amount of iterations per chain
  #dv = name of the dependent variable in the data object
  #iv = names of the independent variables in the data object
  #inits = initial values 
  
  #Prerequisite objects
  N <- nrow(data) #Number of cases
  sample <- matrix(nrow = k+1, ncol = n.chains*(length(iv)+2)) #Matrix for storing sampled values
  sample[1,] <- inits #Attributing initials to matrix
  index <- seq(from = 0, to = (n.chains*(length(iv)+2)-1), by = length(iv)+2) #Creating an index variable used for allocation of the value to the correct collumn
  
  #Prerequisites to MH step
  model.ml <- lm(formula = data[,dv]~ data[,iv[1]] + data[,iv[2]] + data[,iv[3]] + data[,iv[4]] ) #ML estimate
  coeffs <- as.vector(model.ml$coefficients) #Saving coefficients for constructing proposal density
  stdev <- as.vector(sqrt(diag(vcov(model.ml)))) #Saving standard errors for constructing proposal density
  
  #Defining a function for the conditional posterior of B1
  conditional.B1 <- function(B1){
    term1.1 <- -(B1^2) * (sum(data[,iv[1]]^2) / (2*sample[j,index[chain]+6])) #First part of the equation
    term1.2 <- B1* (sum(data[,iv[1]] * (data[,dv]) - sample[j+1,index[chain]+1] - sample[j,index[chain]+3]*data[,iv[2]] - sample[j,index[chain]+4]*data[,iv[3]] - sample[j,index[chain]+5]*data[,iv[4]]) / sample[j,index[chain]+6]) #Second part of the equation
    term1 <- term1.1+term1.2 #Combining both parts
    term2 <- log((1+ ((B1 - 0)^2 / (1*0.001))) ^ (-(1+1)/2)) #Log of the second part of the equation
    total <- term1 + term2 #Combining both terms to the Log of the equation we require 
    return(total)
  }
  
  for (chain in 1:n.chains){ #Loop over the amount of chains
    for (j in 1:k){ #Loop over iterations
      
      #GIBS FOR SAMPLING B0
      sum.b0 <- sum(data[,dv] - sample[j,index[chain]+2]*data[,iv[1]] - sample[j,index[chain]+3]*data[,iv[2]] - sample[j,index[chain]+4]*data[,iv[3]] - sample[j,index[chain]+5]*data[,iv[4]])
      mu0 <- ((sum.b0 / sample[j,index[chain]+6]) +  (0 / 1000))   /   ((N / sample[j,index[chain]+6]) + (1/1000))
      tau0 <- 1 / ((N / sample[j,index[chain]+6]) + (1/1000))
      sample[j+1,index[chain]+1] <- rnorm(1,mu0,tau0)
      
      #METROPOLIS HASTINGS STEP FOR SAMPLING B1
      #A: Sample from proposal and sample u
      proposal <- rnorm(1, coeffs[2],3*(stdev[2])) #Drawing a proposal 
      u <- runif(1) #Drawing u to evaluate r against
      
      #B: Calculate & evaluate acceptance ratio
      r <- conditional.B1(proposal) - conditional.B1(sample[j,index[chain]+2]) #Log of the acceptance ratio
      if(r>log(u)){ #If the acceptance ratio is larger than u...
        sample[j+1,index[chain]+2] <- proposal #Accept the proposal
      }else { #if not...
        sample[j+1,index[chain]+2] <- sample[j,index[chain]+2] #Retain previous value
      }
      
      #GIBBS FOR SAMPLING B2
      density.b2 <- data[,iv[2]]* (data[,dv] - sample[j+1,index[chain]+1] - sample[j+1,index[chain]+2]*data[,iv[1]] - sample[j,index[chain]+4]*data[,iv[3]] - sample[j,index[chain]+5]*data[,iv[4]])   
      mu2 <- ((sum(density.b2) / sample[j,6]) +( 0 / 1000))   /   ((sum(data[,iv[2]]^2) / sample[j,6]) + (1/1000))
      tau2 <- 1 / ((sum(data[,iv[2]]^2) / sample[j,6]) + (1/1000))
      sample[j+1,index[chain]+3] <- rnorm(1,mu2,tau2)
      
      #GIBBS FOR SAMPLING B3
      density.b3 <- data[,iv[3]]* (data[,dv] - sample[j+1,index[chain]+1] - sample[j+1,index[chain]+2]*data[,iv[1]] - sample[j+1,index[chain]+3]*data[,iv[2]] - sample[j,index[chain]+5]*data[,iv[4]])   
      mu3 <- ((sum(density.b3) / sample[j,6]) +( 0 / 1000))   /   ((sum(data[,iv[3]]^2) / sample[j,6]) + (1/1000))
      tau3 <- 1 / ((sum(data[,iv[3]]^2) / sample[j,6]) + (1/1000))
      sample[j+1,index[chain]+4] <- rnorm(1,mu3,tau3)
      
      #GIBBS FOR SAMPLING B4
      density.b4 <- data[,iv[4]]* (data[,dv] - sample[j+1,index[chain]+1] - sample[j+1,index[chain]+2]*data[,iv[1]] - sample[j+1,index[chain]+3]*data[,iv[2]] - sample[j+1,index[chain]+4]*data[,iv[3]])   
      mu4 <- ((sum(density.b4) / sample[j,6]) +( 0 / 1000))   /   ((sum(data[,iv[4]]^2) / sample[j,6]) + (1/1000))
      tau4 <- 1 / ((sum(data[,iv[4]]^2) / sample[j,6]) + (1/1000))
      sample[j+1,index[chain]+5] <- rnorm(1,mu4,tau4)
      
      #GIBBS FOR SAMPLING S2
      SS <- data[,dv] - (sample[j+1,index[chain]+1] + sample[j+1,index[chain]+2]*data[,iv[1]] + sample[j+1,index[chain]+3]*data[,iv[2]] + sample[j+1,index[chain]+4]*data[,iv[3]] + sample[j+1,index[chain]+5]*data[,iv[4]])
      SS <- sum(SS^2) #Determining sum of squares
      alpha <- (N/2) + 0.001 #Determining alpha input 
      beta <- (SS/2)+ 0.001 #Determining beta input 
      sample[j+1,index[chain]+6] <- 1/(rgamma(1, shape = alpha, rate = beta)) #Sampling a value
      
    }
  }
  return(as.data.frame(sample[burn:k,])) #Return sample excluding burn in
}
