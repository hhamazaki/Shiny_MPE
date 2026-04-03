## Bayesian Missing Passage Estimation Method 

#### Modeling Principle 
The missing passage estimation method attempts to fit a model to all historical observed daily passage.  Missed passages are estimated from the model.  The modeling uses **Hierarchical Modeling** approach in which the model assumes that each year's run time model parameters are **not independent** but derived from a common distribution (**Hyper Parameter**). This means that: each year's peak passage date and its duration are similar among years (thus, normal, early, late timing...).  The hierarchical model draws strength from all years combined.  

**log-normal distribution curve** to daily passage data. 
Missing passage is a model estimated passage 

**Log-normal distribution model*** 
$$y_{d,y}= exp(a_{y})\cdot exp\left ( -\frac{1}{2}\left ( \frac{ln(d)-ln(m_{y})}{b^{_{y}}}  \right )^{2}\right )+e_{d}$$
where 
$${a_{y}} \sim N(a_{0},{\sigma _{a}}^{2})$$ is a log of the peak passage for each year $a_{0}$ and  ${\sigma _{a}}^{2}$ are hyper parameters. 
$${m_{y}} \sim N(m_{0},{\sigma _{m}}^{2})$$ is the peak passage day from the first day $m_{0}$ and  ${\sigma _{m}}^{2}$ are hyper parameters.
$${b_{y}} \sim N(b_{0},{\sigma _{b}}^{2})$$ is standard deviation of the log-normal distribution. $b_{0}$ and  ${\sigma _{b}}^{2}$ are hyper parameters.
$$e_{d} \sim N(0,\sigma_{d} ^{2})$$


#### Model Parameters and Priors 
The model estimates following parameters

|Parameter |  | Prior  |  |    Note  |
|  ------- |---:| -------:|---:|-------:|
|_________|_|________________|_|________________________|
| $a_{0}$  | | $a_{0} \sim N(a0,10)T[]$ | | a0 is defined by data  | 
| $m_{0}$    |  |   $m_{0} \sim N(m0,10)T[]$  | | m0 is defined by data    | 
| $b_{0}$    |   |   $b_{0} \sim N(b0,10)T[]$  | | b0 is defined by data    | 
| $\sigma _{a}$    |   | $\sigma_{a} \sim U(0,1)$   |  | | 
| $\sigma _{m}$ |   |  $\sigma_{m} \sim U(0,2)$   |  |  | 
| $\sigma _{b}$|   |  $\sigma _{b} \sim U(0,0.5)$   |  | | 
| $\sigma _{d}$|   |  $\sigma _{d} \sim U(0,5)$   |  | | 

---
a0 is a mean of the ln(peak passage) of all years 
m0 is a mean of the peak passage date of all years 




