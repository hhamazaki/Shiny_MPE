### Running Bayesian missing passage model 
Bayesian  missing passage model is coded in **JAGS** (see Model Codes Tab). 

---
**Model Running Steps**    
* **Select Error structure :**  Normal, Poisson, Negative Binomial.  Default is normal.
* **Set error CV:**  Set CV that you would think **reasonable***. Default is 0.25 or 25%  
* **Set Simulation:** Use default numbers or increase numbers
* **Click Run** Wait until Trace plots and Summary show up.  After that, click other tabs. 

---
**Bayesian Model Setting**  
Bayesian analyses is based on numerical simulation and sampling. Bayesian statistical quantities (e.g., model parameters estimates, CI) are simple summary of the simulation samples. To obtain a good statistics, simulation samples should be stable.  When the simulation reaches stable state, trace plots should look like a flat band, and density plots should have a single defined peak.  **Note** Because Bayesian model is based on simulation, estimated parameter values will differ every time model is run.  


To achieve the stable simulation, every Bayesian models have following 4 controls:
* **Burn-in**  Initial simulation and highly unstable, so that all samples in this stage is thrown out. (Default: 5000)  
* **Simulation**  This is the stage when samples can be taken. Ideally this phase should be stable. (Default: 10000) 
* **Thinning**  Sample every xxth simulation. Length of the simulation and thinning will determine the number of samples from which parameter estimates are calculated: samples = simulation/ thinning.   Generally, this should be from 1000 to 10000 (Default 5)
* **Chains**  The number of simulation experiments with different starting points. JAGS selects starting points randomly from the priors. If model is correct, final simulation should reach identical mean-median regardless starting points. The number of chains are generally 1 - 5. (Default 3)  

Under the default settings, a total of (burn-in + simulation)xchains (5000+10000)x3 =45000) simulation is conducted.  Of those, 8000 samples are taken (simulation/thinning = 40000/5 = 8000).  Model parameters estimates are based on 8000 samples.

The default is set to produce **quick and reasonable estimates**. It is **recommended** to increase the length of burn-in and simulations and the number of chains to obtain **better and more stable estimates**.  Generally, **longer burn-in and simulation and more chains will produce better model parameter estimates.** However, this will also **increase** simulation time significantly. A rule of thumb is (1) run the model with default setting, (2) check trace and density plots, (3) if the plots do not look good, increase burn-in, simulation, and chains until getting good plots.   

When good parameter estimates are not achieved after long simulations, this is an indication that:  (2) **wrong model / parameter specifications** (model priors are too vague or wrong), and/or (2) **data are uninformative**.  
**Caution** Check data **first** and ask whether **the data are informative.**   It is very easy for Bayesian model to **make data speak**.   


## Bayeian Misssing Passge Estimation Methods 

Bayesian Missing Passage Estimation Method atempts to fit a run timing model to observed   

Two Run timing are available:**Log-normal** and **Gumbel** 

**Log-normal distribution model*** 
$$y_{d,y}= exp(a_{y})\cdot exp\left ( -\frac{1}{2}\left ( \frac{ln(d)-ln(m_{y})}{b^{_{y}}}  \right )^{2}\right )+e_{d}$$
where 
$$a_{y}$$ is a log of the peak passage for each year
$$m_{y}$$ is the peak passage day from the first day 
$$m_{y}$$ is standard deviation of the log-normal distribution. 
$$e_{d} \sim N(0,\sigma_{d} ^{2})$$


### Gumbel distribution model 


#### Model Parameters and Priors 
The model estimates following parameters

|Parameter |  | Prior  |  |    Note  |
|  ------- |---:| -------:|---:|-------:|
|_________|_|________________|_|________________________|
| $ln(\alpha)$  | | $ln(\alpha) \sim U(0,10)$ | | All models   | 
| $\beta$    |  |   $\beta \sim U(0,10)$  | | All models   | 
| $\sigma$    |   |   $\sigma \sim U(0,10)$  | | All models    | 
| $\phi$    |   | $\phi \sim U(-1,1)$   |  |**AR1 option**  | 
| $\varepsilon_{0}$ |   |  $\varepsilon_{0} \sim N(0,100)$   |  |**AR1 option**  | 
| $\sigma _{\omega}$|   |  $\sigma _{\omega} \sim U(0,10)$   |  |**TVA option**| 

---
All the priors are set to be flat, uninformative, reasonable range that are seen in salmon stock assessment. Model starting points are randomly selected from the above prior ragnes. 



