###################### 8. Bayes Factor ###################### 
BayesFactor <- function(data){
  
  #Prerequisites
  model <- lm(scale(gross, center = F)~ scale(budget, center = F) + scale(score, center = F) + scale(year, center = F) + scale(usa, center = F) + scale(budget*score, center = F), data = data)
  coefficients <- c(mean(y.std.long[,1]), mean(y.std.long[,2]) , mean(y.std.long[,3]), mean(y.std.long[,4]) ,mean(y.std.long[,5]),mean(y.std.long[,6])) #Saving standardized coefficients. 
  matrixx <- vcov(model)[c(1,2,3,4,5,6),c(1,2,3,4,5,6)] 
  b <- 5/N #Defining b
  n.hypo <- 4
  prior.model.prob <- 1/n.hypo
  
  posterior <- (mvrnorm(10000, mu = coefficients, Sigma = matrixx)) #Sample from multivar density posterior
  prior <- mvrnorm(10000, mu = c(0,0,0,0,0,0), Sigma = matrixx/b) #Sample from multivar density prior
  
  #Hypothesis 1: Both direct effects are positive
  f.1 <- mean(posterior[,2] > 0.1 & posterior[,3] > 0.1)
  c.1 <- mean(prior[,2] > 0.1 & prior[,3] > 0.1)
  bf1 <- f.1/c.1 #BF1: All direct effects are positive
  
  #Hypothesis 2: The effect op budget is more important as compared with the effect for score
  f.3 <- mean(posterior[,2] > posterior[,3] )
  c.3 <- mean(prior[,2] > prior[,3] )
  bf2 <- f.3/c.3 
  
  #Hypothesis 3: The interaction effect is positive
  f.5 <- mean(posterior[,6] > 0.1)
  c.5 <- mean(prior[,6] > 0.1)
  bf3 <- f.5/c.5
  
  #Hypothesis 4: Budget
  f.6 <- mean(posterior[,2] > posterior[,6] & posterior[,3] > posterior[,6])
  c.6 <- mean(prior[,2] > prior[,6] & prior[,3] > prior[,6])
  bf4 <- f.6/c.6
  
  cat("\n\tH1:",bf1,
      "\n\tH2:",bf2,
      "\n\tH3:",bf3,
      "\n\tH4:",bf4)
}
