
#######################################################
################       Structure      #################
#######################################################

In the current folder you find three things.

##### 1 - Report Bart-Jan.pdf: this is a PDF file of the report.
##### 2 - Code and data folder: This is a folder containing all the source code functions used throughout the assignment and the data file. 
##### 3 - Master_Script.R: This is the master file, in which all functions are called and ran.



#######################################################
###############       How to use      #################
#######################################################

All code, and the prerequisites, for this assignment are ran in the Master_script.R file. This file does not contain the actual functions! Thus, from this Master_script.R file, all functions are called whenever they are required. Right after each function is called, a command is specified to view this code. This will automatically open a new window with the function you specifically want to see. You this don't need to open any of the seperate R files: you can rather view and run all code from the Master_script.R file! 

The reason for why i did this (in opposite to what you specified in the assignment) is for the following reason: Organisation like the current, especially in big projects, implies a clear structuring and easy navigation. Putting all code in one single file makes everything very messy and difficult to navigate in, which I consider to be bad coding practices. The current approach doesn't have the drawback of having to open all kinds of different files, because everything can be accessed and modified from the master script. Consequently, I find the cunrrent structuring superior and chose for it. 

Before starting, you have to specify the working directory. This should be done at the start of the Master_script.R. (line 12). It is very important that you specify the folder "Code and data" as the working directory, otherwise it does not work. The last part of the path is already specified (for mac). 






#######################################################
#######       Navigation Master_Script.R.     #########
#######################################################

The following piece of this document is a summary of what can be found on each line of code in the Master_script.R. This may help with navigation through the Master_script.R document.




############################################
########### 0. prerequisites ##############
############################################

Line 5 - 9	Loading all required packages
Line 12 - 13	Settwing WD and Loading data
Line 16 - 21	Setting prerequisite objects for samplers
Line 24 - 31	Setting initial values for the two models
Line 34 - 37	Centering the predictors
Line 38 - 54	Creating standardized data for a third sampling run



############################################
############## I. Sampling #################
############################################

Line 61 - 65 	Calling and viewing the sampling functions
Line 68 - 73 	Running the samplers 3 times (One for Model 1, One for model 2 and One for the standardize data)



################################################################
########## II. CREATING LONGFORMAT DATA OBJECTS ################ 
################################################################

Line 79 - 83 	Longformat for model withouth interaction (model 1)
Line 86 - 90	Longformat for model with interaction (model 2)
Line 95 - 100	Longformat for model standardized model with interaction



################################################################# 
###################### III. CONVERGENCE #########################
################################################################# 

Line 107 - 110	Calling and viewing the history plot and autocorrelation plot functions
Line 113 - 119	Displaying history plots for model 1
Line 122 - 129	Displaying history plots for model 2
Line 132 - 138	Displaying autocorrelation plots for model 1
Line 141 - 148	Displaying autocorrelation plots for model 2
Line 152 - 157 	Calculating Markov chain error model 1
Line 160 - 165	Express Markov Chain error in terms of standard deviation model 1
Line 168 - 174 	Calculating Markov chain error model 2
Line 177 - 183	Express Markov Chain error in terms of standard deviation model 2
Line 187 - 190	Calculating Acceptance ratio MH steps both model 1 and 2



################################################################# 
############### IV. POSTERIOR PREDICTIVE CHECK ##################
#################################################################

Line 197 - 201	Calling and viewing PPC code for both models
Line 203 - 204 	Running ppc code for both models



################################################################# 
############################ V. DIC ############################
################################################################# 

Line 211 - 215	Calling and viewing DIC code for both models
Line 217 - 218	Running DIC for both models



################################################################# 
######################## VI. RESULTS ############################
################################################################# 

Line 225 - 227	Calling and viewing code for posterior histograms
Line 230 - 237 	Running code for displaying posterior histograms
Line 240 - 242	Calculting standardized coefficients from the standardized sampling run



################################################################# 
###################### VII. BAYES FACTOR ########################
################################################################# 

Line 249 - 251	Calling and viewing code for Bayes factor
Line 253 - 254	RObtaining Bayes factor