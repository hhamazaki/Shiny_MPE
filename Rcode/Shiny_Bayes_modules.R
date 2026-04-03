#'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Shiny Bayes Modules  
#'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'===============================================================================
#  BayesModelUI----
#  Usage: UI section 
#  BayesModelUI("ns.name", "User data (.csv format)")
#  Usage: Server section
#  callModule(dataInput, "ns.name",stringsAsFactors = FALSE)
#'===============================================================================
BayesInputUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    fluidRow(
      p(strong("Start Bayesian Analyses")),
      actionButton(ns("RunBayes"),"Run"),
      checkboxInput(ns('config'), "Modify Model Running Setting", FALSE),
      conditionalPanel(condition="input.config == true",
     column(6,
        numericInput(ns('n.burnin'),'Burn-in x 1000',value=5,min=0,step = 1),
        numericInput(ns('n.thin'),'Thinning',value=5,min=0,step = 1)
                     ),  
      column(6,
             numericInput(ns('n.iter'),'Sim x 1000',value=10,min=0,step=1), 
             numericInput(ns('n.chain'),'Chains',value=1,min=1,step = 1)
             ),
       ns=NS(id) 
       ) # End conditional Panel    
      )  # End fluidRow 
    ) # End tabList
  }
#'-------------------------------------------------------------------------------
BayesInputServer <- function(id,Bayesdata,hyper,inits,Bayesmodel){
  moduleServer(id,
               function(input, output, session) {
   # The selected file, if any
  run.JAGS <- eventReactive(input$RunBayes,{
# Progress------------------------------------------------------------------------------  
    progress <- Progress$new(min=1,max=100)
    on.exit(progress$close())
    progress$set(message = paste('JAG Model in progress'),
                 detail = 'This will take a while. Be patient please....')
    for (i in 1:100) {
      progress$set(value = i)
    }
#'-------------------------------------------------------------------------------   
#----  Import model data ------------------------------------------------------- 
    datnew <- Bayesdata() 
    hyper <- hyper()
    niter <- 1000*input$n.iter      
    nburn <- 1000*input$n.burnin
    titer <- nburn+niter
    nthin <- input$n.thin
    nchain <- input$n.chain
    #  JAGS model selection 
    rjag.inits <- inits()
    jagmodel <- Bayesmodel()
    pars <- c('a','b','mu','y') 
    initss <- function(){rjag.inits}
    # Run JAGS 

    output <- jags(data=append(datnew,hyper),inits=initss,parameters.to.save=pars, model.file= jagmodel,
                            n.chains=nchain, n.iter=titer,n.burnin=nburn,n.thin=nthin,DIC=TRUE)
#    output <- jags.parallel(data=datnew,parameters.to.save=pars, model.file= jagmodel,
#                   n.chains=nchain, n.iter=titer,n.burnin=nburn,n.thin=nthin,
#                   jags.seed = seed)
    return(output)
  })
    } # End fundtion
  ) # End moduleServer
} # End BayesInputServer

#'===============================================================================
#  Bayesmodel  Model Functions ----  
#'===============================================================================
#-------------------------------------------------------------------------------
# Runtiming function  
#-------------------------------------------------------------------------------
lognormal <- function(a,b,m,day){
  pred <- exp(a)*exp(-0.5*(log(day/m)/b)^2)
  return(pred)
}
# Modified Gumbel distribution function 
gumbel <- function(a,b,m,day){
  pred <- exp(a-1)*exp(-((day-m)*b+exp((m-day)*b)))
  return(pred)
  }


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
# jag.model.n  Normal assumption 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   
jag.model.n <- function(){
  for(j in 1:nyrs) {
    for(i in 1:ndays){
#-----------------------------------------------------------------------
# Observation Error Assumptions 
#-----------------------------------------------------------------------
# Normal Error Assumption 
  y[j,i] ~ dnorm(theta[j,i],tausqd[i])
#-------------------------------------------------------------------------
# Rum Timing Model Assumptions 
#------------------------------------------------------------------------=
# Log-normal run timing
      theta[j,i] <- a[j]*pow(10,D[j])*exp(-0.5*pow(log(i/mu[j])/b[j],2))
#      theta[j,i] <- exp(a[j]-1)*exp(-((i-mu[j])*b[j]+exp((mu[j]-i)*b[j])))
    }
  }
  
# a[] indicates the maximum height (amplitude) of the function a>0
# mu[] indicates the function peaks when x = mu mu>0 : Peak timing
# b[] indicates peak width of the function b>0 standard deviation
#-------------------------------------------------------------------------------
# Priors 
#-------------------------------------------------------------------------------
  for(j in 1:nyrs) {
# Normal distribution Positive only 
  #  All parameters are hierarchical 
    a[j] ~ dnorm(a0,a0.prec)%_%T(mina,)
    b[j] ~ dnorm(b0,b0.prec)%_%T(minb,)   
    mu[j] ~ dnorm(mu0,mu0.prec)%_%T(minmu,)
  }  
  
# Rule of thumb prior
# a log of the highest daily passage 
# b 1/(log(total passage))
# m peak passage date. 
  a0 ~ dnorm(a0m,a0tau)
  b0 ~ dnorm(b0m,b0tau)
  mu0 ~ dnorm(mu0m,mu0tau)
  a0.prec <- 1/pow(a0.sigma,2)
  a0.sigma ~ dunif(0,2)  	
  b0.prec <- 2/pow(b0.sigma,2)
  b0.sigma ~ dunif(0,1)  
  mu0.prec <- 1/pow(mu0.sigma,2)
  mu0.sigma ~ dunif(0,5) 

## This assumes that variance of each day is independent.     
  for(i in 1:ndays) {    
    sigmad[i] ~ dunif(0,sigma0[i])  
    tausqd[i] <-pow(sigmad[i],-2)
  }		
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   
# jag.model.p: Poisson distribution assumption 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
jag.model.p <- function(){
  for(j in 1:nyrs) {
    for(i in 1:ndays){
      #-------------------------------------------------------------------------------
      # Obervation Error Assumptions 
      #-------------------------------------------------------------------------------
      # Normal Error Assumption 
      #y[j,i] ~ dnorm(theta[j,i],tausqd[i])
      
      # Log-normal Error Assumption 
      # y[j,i] ~ dlnorm(log.theta[j,i], tausqd[i])
      
      # Poisson Negative-bionomial Error Assumpton 
      y[j,i] ~ dpois(lambda[j,i])
      lambda[j,i] <- theta[j,i]      #  Poissson  
      #    lambda[j,i] <- theta[j,i]*v[j,i]      #  Negative-binomial as   
      #    v[j,i] ~ dgamma(kappa[j], kappa[j])    #  a gamma-Poisson mixture  
      
      #-------------------------------------------------------------------------------
      # Rum Timing Model Assumptionss 
      #-------------------------------------------------------------------------------
      # Log-normal Timing 
      theta[j,i] <- exp(a[j])*exp(-0.5*pow(log(i/mu[j])/b[j],2))
      
      # Extreme value distribution 
      #   theta[j,i] <- exp(a[j])*exp(-exp((mu[j]-i)/b[j])+(mu[j]-i)/b[j]+1)
    }
  }
  
# a[] indicates the maximum height (amplitude) of the function a>0
# mu[] indicates the function peaks when x = mu mu>0 : Peak timing
# b[] indicates peak width of the function b>0 standard deviation
  
#-------------------------------------------------------------------------------
# Priors 
#-------------------------------------------------------------------------------
  for(j in 1:nyrs) {
    # Normal distribution Positive only 
    #  a: is independent not hierarhical 
    a[j] ~ dnorm(a0,a0.prec)%_%T(1,)
    b[j] ~ dnorm(b0,b0.prec)%_%T(0.25,)   
    mu[j] ~ dnorm(mu0,mu0.prec)%_%T(1,)
    #      kappa[j] ~ dgamma(kappaa, kappab)
  }  
  # Rule of thumb prior
  # a log of the highest passage 
  # b 1/(log(total passage))
  # m peak passage date. 
  
  a0 ~ dnorm(a0m,a0tau)
  b0 ~ dnorm(b0m,b0tau)
  mu0 ~ dnorm(mu0m,mu0tau)
  
  a0.prec <-1/pow(a0.sigma,2)
  a0.sigma ~ dunif(0,2)  	
  b0.prec <-1/pow(b0.sigma,2)
  b0.sigma ~ dunif(0,2)  
  mu0.prec <-1/pow(mu0.sigma,2)
  mu0.sigma ~ dunif(0,10) 
#    kappaa<- kappamu * kappamu / kappasig2
#    kappab<- kappamu / kappasig2
#    kappamu ~ dexp(0.0001)
#    kappasig2 ~ dexp(0.0001)
  
## This assumes that variance of each year is independent.     
#    for(i in 1:ndays) {    
#      sigmad[i] ~ dunif(0,50)  
#      tausqd[i] <-pow(sigmad[i],-2)
#  }		
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   
# jag.model.nb: Negative Binomial distribution assumption 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
jag.model.nb <- function(){
  for(j in 1:nyrs) {
    for(i in 1:ndays){
#-------------------------------------------------------------------------------
# Obervation Error Assumptions 
# Normal Error Assumption 
#      y[j,i] ~ dnorm(theta[j,i],tausqd[i])
      
# Poisson Negative-bionomial Error Assumpton 
      y[j,i] ~ dpois(lambda[j,i])
#      lambda[j,i] <- theta[j,i]      #  Poisson 
      lambda[j,i] <- theta[j,i]*v[j,i]      #  Negative-binomial as   
      v[j,i] ~ dgamma(kappa[j], kappa[j])    #  a gamma-Poisson mixture  
      
#-------------------------------------------------------------------------------
# Rum Timing Model Assumptionss 
#-------------------------------------------------------------------------------
      # Log-normal
      theta[j,i] <- exp(a[j])*exp(-0.5*pow(log(i/mu[j])/b[j],2))
      # Extreme value distribution 
      #   theta[j,i] <- exp(a[j])*exp(-exp((mu[j]-i)/b[j])+(mu[j]-i)/b[j]+1)
      
    }# End ndays
  }# End nyrs
  
  # a[] indicates the maximum height (amplitude) of the function a>0
  # mu[] indicates the function peaks when x = mu mu>0 : Peak timing
  # b[] indicates peak width of the function b>0 standard deviation
  
  #-------------------------------------------------------------------------------
  # Priors 
  #-------------------------------------------------------------------------------
  for(j in 1:nyrs) {
    # Normal distribution Positive only 
    #  a: is independent not hierarhical 
    a[j] ~ dnorm(a0,a0.prec)%_%T(1,)
    b[j] ~ dnorm(b0,b0.prec)%_%T(0.2,)   
    mu[j] ~ dnorm(mu0,mu0.prec)%_%T(1,)
    kappa[j] ~ dgamma(kappaa, kappab)
  }  
  # Rule of thumb prior
  # a log of the highest passage 
  # b 1/(log(total passage))
  # m peak passage date. 
  
  a0 ~ dnorm(a0m,a0tau)%_%T(1,)
  b0 ~ dnorm(b0m,b0tau)%_%T(0.3,)
  mu0 ~ dnorm(mu0m,mu0tau)%_%T(15,)	
  
  a0.prec <-1/pow(a0.sigma,2)
  a0.sigma ~ dunif(0,2)  	
  b0.prec <-1/pow(b0.sigma,2)
  b0.sigma ~ dunif(0,2)  
  mu0.prec <-1/pow(mu0.sigma,2)
  mu0.sigma ~ dunif(0,10) 
  kappaa<- kappamu * kappamu / kappasig2
  kappab<- kappamu / kappasig2
  kappamu ~ dexp(0.0001)
  kappasig2 ~ dexp(0.0001)
  
  #    for(i in 1:ndays) {    
  #      sigmad[i] ~ dunif(0,50)  
  #      tausqd[i] <-pow(sigmad[i],-2)
  #    }		
}# End Jag.model



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#+#   JAG Model Written by Randy Petersen 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   
jag.model.Rp <- function(){
    for(j in 1:nyrs) {
    #sum of mean predictions
     ytot[j] <- sum( y[j,] )
     yobstot[j] <- sum( yobs[j,] )
     ypredtot[j] <- sum( ypred[j,] )
    #
    for(i in 1:ndays) {
    #likelihood
     y[j,i] ~ dnorm(theta[j,i],tau)
    #new predictions
     yobs[j,i] <- y[j,i]
     ypred[j,i] ~ dnorm(theta[j,i],tau)
    #deterministic run timing model
     theta[j,i] <- Ey[j]*((1/(Sy[j]*i*sqrt(2*3.141593)))*exp(-0.5*((log(i)-mu[j])/Sy[j])^2)) / sum(psi[j,])
    #normalizing constant
     psi[j,i] <- (1/(Sy[j]*i*sqrt(2*3.141593)))*exp(-0.5*((log(i)-mu[j])/Sy[j])^2)
    }
#-------------------------------------------------------------------------------
# Priors 
#-------------------------------------------------------------------------------
    for(j in 1:nyrs) {
    mu[j] ~ dnorm(mu0,mu0.prec)
    Sy[j] <- exp(logSy[j])
    logSy[j] ~ dnorm(S,tauSy)
    Ey[j] ~ dnorm(E,tauEy)
   }
   sigma ~ dunif(0,200)
   tau <- pow(sigma,-2)
  #hyperparameters / hyperpriors
   mu0 ~ dnorm(muM,0.0001)
   S <- exp(logS)
   logS ~ dnorm(muS,0.0001)
   E ~ dnorm(muE,tauE) #after struggling and struggling with node errors from 12/29 to 12/30, i think the cause was ultimately negative E values...
   mu0.prec <- pow(sigmaMy,-2)
   tauSy <- pow(sigmaSy,-2)
   tauEy <- pow(sigmaEy,-2)
   sigmaMy ~ dunif(0.001,10)
   sigmaSy ~ dunif(0.001,10)
   sigmaEy ~ dunif(0.001,10000)
}

}

