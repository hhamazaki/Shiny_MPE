## Custom Escapement Goal Range Analyses
Custom Escapement Goal Range Analyses are designed to explore probability of achieving yield or recruit target at given escapement goal range. 

The simulation model is conducted as follows:
* Select 200 Ss equally spaced between the lower and upper goal range. 
* At each S, calculates expected mean and annual recruit for each Bayesian MCMC posterior.  
* Summarize yiled and recruit. Calculate probability (proportion) of MCMC samples that are above the target yield or recruit. 


**How to run the custom escapent goal analyses**
* Enter lower and upper escapement goal range
* Enter yield and recruit target
* Click Run 

**Model Results**

**Profile Analyses**
Profile Analyses panel calculates MSY and Rmax Profile probability at escapement goal range selected. This is calculated without simulation 


**Expected Maen & Annual Yield-Recruit**
Mean and Annual Yield-Recruit are calculated after running the simulation.  
Mean and Annual Yield-Recruitment probability answer the questions:  

"If escapement are achieved within the set goal range for xxx years"

The Mean Yield-Recruit Analyses asks and answers 
* What are average yield-recruit over the xxx years?  
* what is the probability the average yield-recruit exceeds the minimum target? 

The Annual Yield-Recruit Analyses asks and answers 
* Of the xxx years, how many years will the yield-recruit exceeds the minimum target? 

Suppose the target yield is 500, and annual yields were 100,200, 400, and 1000 over 4 years.  Average yield is 425 that met the mean yield target, whereas the probability of annually achieving the target is 1/4 or 25%. 





