# What determines movie succes? A Bayesian approach from scratch.
This repository contains several materials with which Bayesian Inference is done. Two multiple regression models, out of which one including an interaction term, are fitted specified and sampled within a Bayesian framework. Coefficients are sampled from the conditional posteriors by means of Gibbs sampling, and Metropolis Hasting sampling for a single parameter. Hereafter, convergence is assessed, and several inferences are done in a Bayesian way. For more information on the specifics of the data and the research question, please consult `report.pdf`. All code with which the coefficients are sampled and inferences are managed are programmed by Bart-Jan Boverhof.

## Content: 
- `report.pdf`: Written report anseweing a research problem by means of Bayesian inference.  
- `functions and data` folder: Contains all source code functions used throughout the assignment and the data file. 
     - `Gibs_no_int.R`: Gibbs sampler with Metropolis Hastings step for the no interaction model. 
     - `Gibs_int.R`: Gibbs sampler with Metropolis Hastings step for the interaction model.
     - `Convergence.R`: History plot & autocorrelation plot functions.
     - `DIC_no_int.R`: Model fit by means of deviance information criterion for no interaction model.
     - `DIC_int.R`: Model fit by means of deviance information criterion for interaction model
     - `PPC_no_int.R`: Posterior predictive check homoscedasticity regression assumption for no interaction model.
     - `PPC_int.R`: Posterior predictive check homoscedasticity regression assumption for no interaction model.
     - `Results.R`: Result plots.
     - `Bayes_factor.R`: Bayes factor calculation for informative hypothesis testing.
- `main.R`: Main file from which all inference is done.

## How to use
All code, and the prerequisites are ran in the `main.R` file. This file does not contain the actual functions. Right after each function is called, a command is specified to view this code. This will automatically open a new window with the function you specifically want to see. You this don't need to open any of the seperate scripts.

Before starting, please specify the working directory. This should be done at the start of the Master_script.R.
