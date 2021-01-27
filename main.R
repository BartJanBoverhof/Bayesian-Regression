################################################################# 
###################### 0. PREREQUISITES ######################### 
################################################################# 
#Loading required packages
library(tidyverse)
library(plyr)
library(gridExtra)
library(plyr)
library(MASS)

#SET YOUR WORKING DIRECTORY HERE
setwd("~....../Assignment Bart-Jan/Code and data")
data <- read.csv("movies.csv")

#SAMPLER PREREQUISITES
burn = 1000 #Burn in
k=10000 #Amount of iterations
N=nrow(data) #Total number of cases
n.chains=2 #Number of chains
dv = "gross" #Dependent variable
iv = c("budget","score","year","usa") #Independent variables

#Determining initial values for first run (withouth interaction)
init1<- c(1,-4,6,-3,3,100) #Initial values chain 1
init2<- c(3,5,-7,4,-1,10) #Initial values chain 2
inits <- c(init1,init2) #Combined initials values

#Determining initial values for second run (with interaction)
init11<- c(1,-4,6,-3,3,4,100) #Initial values chain 1
init22<- c(3,5,-7,4,-1,-1,10) #Initial values chain 2
inits2 <- c(init11,init22) #Combined initials values

#Centering predictors
data$budget <- data$budget - mean(data$budget) #Center predictor 1
data$score <- data$score - mean(data$score) #Center predictor 2
data$year <- data$year - mean(data$year) #Center predictor 3
data$usa <- data$usa - mean(data$usa)#Center predictor 4

#Creating a standardized dataset (this data set is used for a Gibbs/MH run, in order to obtain standardized coefficients for later use)
data_std <- data
sd.budget <- sd(data$budget)
sd.score <- sd(data$score)
sd.year <- sd(data$year)
sd.gross <- sd(data$gross)
sd.usa <- sd(data$usa)
data_std$gross <- data_std$gross - mean(data_std$gross) #Also centering the DV for the gibbs run with standardized data

for (j in 1:nrow(data_std)){ #For loop for dividing each value by it's standard deviation
  data_std[j,"budget"] <- data_std[j,"budget"] / sd.budget
  data_std[j,"score"] <- data_std[j,"score"] / sd.score
  data_std[j,"year"] <- data_std[j,"year"] / sd.year
  data_std[j,"gross"] <- data_std[j,"gross"] / sd.gross
  data_std[j,"usa"] <- data_std[j,"usa"] / sd.usa
}



################################################################# 
###################### I. SAMPLING ############################## 
################################################################# 
source("Gibs_no_int.R")
source("Gibs_int.R")

view(gibtropolis) #View source code Gibbs sampling WITHOUTH interaction
view(gibtropolis2.0) #View source code Gibbs sampling WITH interaction

#Running the gibbs samplers
set.seed(204)
x.wide <- gibtropolis(data, k=10000, dv=dv, iv=iv, inits=inits, n.chains = 2, burn = 1000) #Model without interaction
set.seed(204)
y.wide <- gibtropolis2.0(data, k=10000, dv=dv, iv=iv, inits=inits2, n.chains = 2, burn = 1000) #Model with interaction
set.seed(204)
y.std <- gibtropolis2.0(data_std, k=10000, dv=dv, iv=iv, inits=inits2, n.chains = 2, burn = 1000) #Model with standardized data (for std. coefficients)

################################################################# 
########### II. CREATING LONGFORMAT DATA OBJECTS ################ 
################################################################# 

#Longformat for model withouth interaction (model 1)
x.long <- matrix(nrow = (n.chains*(k-burn))+n.chains, ncol = ncol(x.wide) /2) #Creating a matrix new matrix for storing sample from posterior distributions
for (i in 1:6){ #Combining the chains for each parameter in a single collumn
  x.long[,i] <- c(x.wide[,i],x.wide[,6+i])
}
x.long <- as.data.frame(x.long) #Saving as dataframe


#Longformat for model with interaction (model 2)
y.long <- matrix(nrow = (n.chains*(k-burn))+n.chains, ncol = ncol(y.wide) /2) #Creating a matrix new matrix for storing sample from posterior distributions
for (i in 1:7){ #Combining the chains for each parameter in a single collumn
  y.long[,i] <- c(y.wide[,i],y.wide[,7+i])
}
y.long <- as.data.frame(y.long) #Saving as dataframe


#Longformat for model standardized model with interaction
y.std.long <- matrix(nrow = (n.chains*(k-burn))+n.chains, ncol = ncol(y.wide) /2) #Creating a matrix new matrix for storing sample from posterior distributions
for (i in 1:7){ #Combining the chains for each parameter in a single collumn
  y.std.long[,i] <- c(y.std[,i],y.std[,7+i])
}
y.std.long <- as.data.frame(y.std.long) #Saving as dataframe



################################################################# 
###################### III. CONVERGENCE #########################
################################################################# 
source("Convergence.R")

view(history.plot) #View source code history plot 
view(autocorr.plot) #View source autocorrelation plot

#Histrory plots. Model WITHOUTH interaction
grid.arrange(arrangeGrob(history.plot(x.wide,1,7,"Beta0 - Intercept"), #Display plots in grid
                         history.plot(x.wide,2,8, "Beta1 - Budget"),
                         history.plot(x.wide,3,9, "Beta2 - Score"),
                         history.plot(x.wide,4,10, "Beta3 - Year"),
                         history.plot(x.wide,5,11, "Beta4 - USA"),
                         history.plot(x.wide,6,12, "Residual Variance"),
                         ncol = 2))

#Histrory plots. Model WITH interaction
grid.arrange(arrangeGrob(history.plot(y.wide,1,8,"Beta0 - Intercept"), #Display plots in grid
                         history.plot(y.wide,2,9, "Beta1 - Budget"),
                         history.plot(y.wide,3,10, "Beta2 - Score"),
                         history.plot(y.wide,4,11, "Beta3 - Year"),
                         history.plot(y.wide,5,12, "Beta4 - USA"),
                         history.plot(y.wide,6,13, "Beta5 - Budget*Score"),
                         history.plot(y.wide,7,14, "Residual Variance"),
                         ncol = 2))

#Autocorrelation plots. Model WITHOUTH interaction
grid.arrange(arrangeGrob(autocorr.plot(x.wide,1,7,"Beta0 - Intercept"), #Plot autocorrelations
                         autocorr.plot(x.wide,2,8,"Beta1 - Budget"),
                         autocorr.plot(x.wide,3,9,"Beta2 - Rating"),
                         autocorr.plot(x.wide,4,10,"Beta3 - Year"),
                         autocorr.plot(x.wide,5,11,"Beta4 - USA"),
                         autocorr.plot(x.wide,6,12,"Residual Variance"),
                         ncol = 2))

#Autocorrelation plots. Model WITH interaction
grid.arrange(arrangeGrob(autocorr.plot(y.wide,1,8,"Beta0 - Intercept"), #Plot autocorrelations
                         autocorr.plot(y.wide,2,9,"Beta1 - Budget"),
                         autocorr.plot(y.wide,3,10,"Beta2 - Rating"),
                         autocorr.plot(y.wide,4,11,"Beta3 - Year"),
                         autocorr.plot(y.wide,5,12,"Beta4 - USA"),
                         autocorr.plot(y.wide,6,13,"Beta5 - Budget*Score"),
                         autocorr.plot(y.wide,7,14,"Residual Variance"),
                         ncol = 2))

#MARKOV CHAIN ERROR
#Model withouth interaction
(b1.1 <- sd(x.long[,1]) / sqrt((k*n.chains)-2*burn)) #B0
(b2.1 <- sd(x.long[,2])  / sqrt((k*n.chains)-2*burn))#B1
(b3.1 <- sd(x.long[,3])  / sqrt((k*n.chains)-2*burn)) #B2
(b4.1 <- sd(x.long[,4]) / sqrt((k*n.chains)-2*burn) )#B3
(b5.1 <- sd(x.long[,5])  / sqrt((k*n.chains)-2*burn)) #B4
(s2.1 <- sd(x.long[,6]) / sqrt((k*n.chains)-2*burn)) #Sigma-squared

#In percentage of standard deviation in sample
(b1.1 / sd(x.long[,1])) *100
(b2.1 / sd(x.long[,2])) *100
(b3.1 / sd(x.long[,3])) *100
(b4.1 / sd(x.long[,4])) *100 
(b5.1 / sd(x.long[,5])) *100 
(s2.1 / sd(x.long[,6])) *100

#Model with interaction
(b1.2 <- sd(y.long[,1]) / sqrt((k*n.chains)-2*burn)) #B0
(b2.2 <- sd(y.long[,2]) / sqrt((k*n.chains)-2*burn)) #B1
(b3.2 <- sd(y.long[,3]) / sqrt((k*n.chains)-2*burn)) #B2
(b4.2 <- sd(y.long[,4]) / sqrt((k*n.chains)-2*burn)) #B3
(b5.2 <- sd(y.long[,5]) / sqrt((k*n.chains)-2*burn)) #B4
(b6.2 <- sd(y.long[,6]) / sqrt((k*n.chains)-2*burn)) #B5
(s2.2 <- sd(y.long[,7]) / sqrt((k*n.chains)-2*burn)) #Sigma-squared

#In percentage of standard deviation in sample
(b1.2 / sd(y.long[,1])) *100 
(b2.2 / sd(y.long[,2])) *100
(b3.2 / sd(y.long[,3])) *100
(b4.2 / sd(y.long[,4])) *100
(b5.2 / sd(y.long[,5])) *100
(b6.2 / sd(y.long[,6])) *100 
(s2.2 / sd(y.long[,7])) *100

#ACCEPTANCE RATIO MH STEP (of both chains combined)
#Model withouth interaction
(length(unique(x.wide[,2])) / length(x.wide[,2]) + length(unique(x.wide[,8])) / length(x.wide[,8])) /n.chains

#Model with interaction
(length(unique(y.wide[,2])) / length(y.wide[,2]) + length(unique(y.wide[,9])) / length(y.wide[,9])) /n.chains



################################################################# 
############### IV. POSTERIOR PREDICTIVE CHECK ##################
#################################################################
source("PPC.R")
source("PPC_int.R")

view(ppc) #View source code for ppc model withouth interaction
view(ppc2) #View source code for ppc model with interaction

ppc(x.wide, k-burn, data)
ppc2(y.wide, k-burn, data)



################################################################# 
############################ V. DIC ############################
################################################################# 
source("DIC_no_int.R")
source("DIC_int.R")

view(DIC) #View source code for the DIC model withouth interaction
view(DIC2) #View source code for the DIC model with interaction 

DIC(x.long,data) #DIC model withouth interaction
DIC2(y.long,data) #DIC model with interaction 



################################################################# 
######################## VI. RESULTS ############################
################################################################# 
source("Results.R")

view(posterior.hist)

#Displaying posterior histograms
grid.arrange(arrangeGrob(posterior.hist(y.long, 1,"B0 - Intercept"), 
                         posterior.hist(y.long, 2,"B1 - Budget"),
                         posterior.hist(y.long, 3,"B2 - Score"),
                         posterior.hist(y.long, 4,"B3 - Year"),
                         posterior.hist(y.long, 5,"B4 - USA"),
                         posterior.hist(y.long, 6,"B5 - Budget * Score"),
                         posterior.hist(y.long, 7,"Residual Variance"),
                         ncol = 2))

#Calculating standardized coefficients from the standardized sampling run
mean(y.std.long[,2]) #Standardized coefficient budget
mean(y.std.long[,3]) #Standardized coefficient score
mean(y.std.long[,6]) #Standardized coefficient interaction



################################################################# 
###################### VII. BAYES FACTOR ########################
################################################################# 
source("Bayes_Factor.R")

view(BayesFactor)

set.seed(204)
BayesFactor(data)
