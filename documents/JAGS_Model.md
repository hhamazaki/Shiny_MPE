# Bayeian Misssing Passge Estimation Methods 

## Before uising this model 
Bayesian Missing Passage Estimation Method is **intended** to estimate missed passages of more than 2 consecutive days by fitting a log-normal distribution curve to the observed data.  The model assumes **single modal run timing** and **peak counts are observed.**  **DO NOT APPLY THE MODEL** when run timing is **multi-modal** or **Peak counts are missed**.  

Here are common sense 
* **The best estimate of missed 1 day passage is the averaging passages of 1 day before and after the missed day.** 
* **The best estimate of partial day counts is expansion of the partial day counts**



Two Run timing are available:**log-normal** and **Gumbel** 

**log-normal distribution model*** 
$$y_{d,y}= exp(a_{y})\cdot exp\left ( -\frac{1}{2}\left ( \frac{ln(d)-ln(m_{y})}{b^{_{y}}}  \right )^{2}\right )+e_{d}$$
where 
$$a_{y}$$ is a log of the peak passage for each year
$$m_{y}$$ is the peak passage day from the first day 
$$b_{y}$$ is standard deviation of the log-normal distribution. 
$$e_{d} \sim N(0,\sigma_{d} ^{2})$$

Under the Bayesian Hierachical Modeling structiure, it was assumed that all the parameters are derived from common 
$$a_{y} \sim N(a0,\sigma_{a0} ^{2})$$
$$b_{y} \sim N(b0,\sigma_{b0} ^{2})$$
$$m_{y} \sim N(m0,\sigma_{m0} ^{2})$$


### Gumbel distribution model 


#### Model Parameters and Priors 
The model estimates following parameters

|Parameter |  | Prior  |  |    Note  |
|  ------- |---:| -------:|---:|-------:|
|_________|_|________________|_|________________________|
| $a0$    | | $a0 \sim N(a0m,\sigma_{a0} ^{2})$ | | ave   | 
| $b0$    |  |   $\beta \sim U(0,10)$  | | All models   | 
| $m0$    |   |   $\sigma \sim U(0,10)$  | | All models    | 
| $\sigma_{a0}$    |   | $\sigma_{a0} \sim U(0,2)$   |  |  | 
| $\sigma_{b0}$ |   |  $\sigma_{b0} \sim N(0,0.5)$   |  |  | 
| $\sigma_{m0}$|   |  $\sigma_{m0} \sim U(0,5)$   |  || 

---
a0m: average passage date across all years 


All the priors are set to be flat, uninformative, reasonable range that are seen in salmon stock assessment. Model starting points are randomly selected from the above prior ragnes. 



