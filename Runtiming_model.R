#initialize
library(shiny)
library(shinythemes)
library(datasets)# used to read markdown file
library(rmarkdown)     # used to get rmarkdown file
library(markdown)     
library(coda)
library(R2jags)
library(openxlsx)
#=======================================================================    
#  UI:  
#=======================================================================
ui<-fluidPage(
  navbarPage(
    theme = shinytheme("cerulean"),  
    title = div(
      img(src="Picture2.png",height=40, width=40)
      ,  "Missing passage estimation"),
#-----------------------------------------------------------------------
#  Panel 1:  Data Input and Submit 
#-----------------------------------------------------------------------
  tabPanel("Data Input",
   sidebarPanel(width = 3,
#---------------------------------------------------    
#  File Inuput
#---------------------------------------------------
# Input: Select a file ----
  fileInput("file1", "Upload CSV File",
         multiple = TRUE,
          accept = c("text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")),
# Input: Checkbox if file has header ----
    checkboxInput("header", "Header", TRUE),

# Input: Select separator ----
    radioButtons("sep", "Separator",
      choices = c(Comma = ",", Tab = "\t"), selected = ","),
    # Horizontal line ----
    tags$hr(),
#-------------------------------------------------------------------------------    
# UI:  Limit data 
#-------------------------------------------------------------------------------
uiOutput('yrange'),
   ), # End SidePanel
  # output
  mainPanel(
    tabsetPanel(
      tabPanel("Table",    
               h4("Passage timing data: Date (day-month), counts by year"),
               tableOutput("table")),
      tabPanel("Help", 
               includeMarkdown("documents/Input_help.md")
      )# End tabPanel
        )  # End tabsetPanel
      )  # End mainPanel
    ), # End tabpanle
#-----------------------------------------------------------------------
#  Panel 2
#-----------------------------------------------------------------------
  tabPanel("Run Model",
    sidebarPanel(width = 3,
      # Horizontal line ----
      p(strong("Run Bayesian model")),
      p("Simulation Setting"),      
      fluidRow(
      column(6,
             numericInput('n.burnin','Burn-in',value=5000,min=0,step = 1000),
             numericInput('n.thin','Thinning',value=5,min=0,step = 1)
      ),  
      column(6,
             numericInput('n.iter','Simulation',value=10000,min=0,step=10000), 
             numericInput('n.chain','Chains',value=3,min=1,step = 1)
      )
      ),      
      selectInput('Er',"Select Error Structure",choice=list('Normal','Poisson','Negative Binomial'),selected ='Normal'),
      p("Set Error CV"),
      numericInput(inputId='n.cv','CV',value=0.25,min=0,step = 0.1),
      checkboxInput("more","More Control"),
      conditionalPanel(
        condition = "input.more == true",
        numericInput(inputId='mina','Minimum Peak Passage',value=10,min=0,step=100), 
        numericInput(inputId='minmu','Minimum Peak day',value=0,min=0,step = 1),        
        numericInput(inputId='minb','Minimum Peak Passage days',value=14,min=0,step = 50),
      ),
      actionButton("Run","Run"),
      
      conditionalPanel(condition="input.conditionedPanels == 'Trace Plots'", 
                                                    uiOutput('lpage')            
            )      
            ),  # End sidebarPanel
           mainPanel(tabsetPanel(
             tabPanel("Bayesian Model",
                      p(strong("Model summary")), 
                      verbatimTextOutput('summary')                      
                      ),
             tabPanel("model",
                      verbatimTextOutput('test')
             ),# End tabPanel
             tabPanel("Model output",
             ),
             tabPanel("Trace Plots",
                      plotOutput(height='500px','pfig')
             ),
             tabPanel("Help",
                withMathJax(               
                  includeMarkdown("documents/JAGS_help.md")
                )            
             ),# End tabPanel
             id = "conditionedPanels"
                         )#End tabsetPanel
             )#End mainPanel
           ),#End tabPanel
#-----------------------------------------------------------------------
#  Panel 3 
#-----------------------------------------------------------------------
# navbarMenu("Model Diganoses",
    tabPanel("Model Output",      
      sidebarPanel(width = 3,
      p(strong("Model Output")),
        numericInput("CI", "% Credible Interval", value=90,min=0,max=100,step=5),
          conditionalPanel(condition="input.conditionedPanel2 == 'Plot'",       
                        uiOutput('fpage')
                      ),    
          conditionalPanel(condition="input.conditionedPanel2 == 'Daily passage'", 
                        uiOutput('columns'),
                      ),
          downloadButton("downloadData", "Download")    
                ),
        mainPanel(
          tabsetPanel(
            tabPanel("Plot",
              plotOutput(height='500px','pfig2')
            ),
            tabPanel("Annual summary",
                      tableOutput("ptable")
              ),
            tabPanel("Daily passage",
                    tableOutput("ytable")
              ),
            tabPanel("Model Parameter", 
                    tableOutput("partable")
              ),
            id = "conditionedPanel2"
               )#End tabsetPanel
             )#End mainPanel
          )#End tabPanel
#      tabPanel("Panel 2",
#            sidebarPanel(width = 3,
#            p(strong("Extra")
#              )            
#                  ), 
#                mainPanel(
#                  tabsetPanel(
#                    tabPanel("Tab 1",
#                             p(strong("To be completed"))
#                             ),
#                  tabPanel("Tab 2",
#                           p(strong("To be completed"))
#                           )
#                          
#                         )#End tabsetPanel
#                      )#End mainPanel
#                   )#End tabPanel
#  )#End nabVarMenu
  ),#End nabVarPage
#------------------------------------------------------------------------
# Citation Discraimer  
#------------------------------------------------------------------------
# withMathJax(),
hr(),
h5("Disclaimer"),
print(strong("This App is developed by Toshihide Hamachan Hamazaki, Alaska Department of Fish and Game Division of Commercial Fisheries")),

h5("Contact about this applicaiton"), 
print(strong("Questions and improvement suggestions? Please contact",
a(href="mailto:toshihide.hamazaki@alaska.gov", "Hamachan"))),
h5("Suggested Citation"),
print(strong(paste("Hamazaki, T.",format(Sys.Date(), "%Y"),". Missing Passage Estimation Analyses(source: https://shiny.rstudio.com/). Available from https://hamachan.shinyapps.io/Missed_Run/")))
 )#End fluidPage


#=======================================================================    
#  Server:  
#=======================================================================
server<-shinyServer(function(input, output, session){
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 1: Data upload and output 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-----------------------------------------------------------------------
#  1.0 Upload File 
#-----------------------------------------------------------------------  
# Upload file 
data.0 <- reactive({
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, head of that data file by default,
  # or all rows if selected, will be shown.
  req(input$file1)
  df <- read.csv(input$file1$datapath,
                 header = input$header,
                 sep = input$sep)
  names(df)[1] <- 'Date'
  return(df)
  })  

# yrange --- UI output to determine data year range ----------------------------
output$yrange <- renderUI({
# Count the number of data per day 
  dat <- data.0()
  dat$nu <- rowSums(!is.na(dat[,-1]))
# Remove days with no data 
  dat <- dat[which(dat$nu>0),]
  dates <- as.Date(dat$Date,"%d-%b")
  
  fday <- min(dates)       # First brood year 
  lday <- max(dates)       # Last brood year
  #  Slider input UI 
  dateRangeInput("sdays", "Trim date range:",
                 start = fday,
                 end   = lday,
                 min    = fday,
                 max    = lday,
                 format = "dd-M",
                 separator = " - "
                 )
  })

#-------------------------------------------------------------------------------
#  1.1: Output data  
#-------------------------------------------------------------------------------
# Extract years 
data <- reactive({
  dat <- data.0()
  dates <- as.Date(dat$Date,"%d-%b")
  dat <- dat[which(dates>=input$sdays[1]&dates<=input$sdays[2]), ]
  return(dat)
  })
# output table -----------------------------------------------------------------
output$table <- renderTable({  data.0() })

#-------------------------------------------------------------------------------
#  1.2: Output page range determined by data 
#-------------------------------------------------------------------------------
# Extract years 
years <- reactive({
  dat <- data()
  year <- substr(names(dat[,-1]),2,5)
  return(year)
})

output$columns = renderUI({
  selectInput('columns2', 'Year', years())
  })

#Bayesian figure page
output$fpage = renderUI({
  mn <- length(years())%/%4
  numericInput("pf", "page", value=1,min=1,max=mn+1,step=1)
  })

#Bayesian trace-density plot figure page
output$lpage = renderUI({
  mn <- length(years())%/%2
  numericInput("p1", "page", value=1,min=1,max=mn+1,step=1)
})

#-----------------------------------------------------------------------
#  1.2: Create Bayes data 
#-----------------------------------------------------------------------
Bayesdata <- reactive({
#  Import data 
    x <- data()
# nyrs is the number of years (i.e. number of columns) 
    nyrs <- dim(x)[2]-1
# ndays is the number of days (i.e. number of rows)  
    ndays <- dim(x)[1]
# y is observed transposed escapement matrix   
    y <- matrix(0,nyrs,ndays)
    for (i in 1:ndays){
      for (j in 1:nyrs){
        # Add 0.01 so that the model will not fail. 
        y[j,i]<-  ifelse(x[i,j+1]<=0,0,x[i,j+1])
      }
    }
# dat is Bayesian data        
    dat <-list(nyrs=nyrs, ndays=ndays,y=y)
    return(dat)
  })

#-----------------------------------------------------------------------
#  1.3: Create Initial Parameters 
#-----------------------------------------------------------------------
inits <- reactive({
  x <- data()
  # nyrs is the number of years (i.e. number of columns) 
  nyrs <- dim(x)[2]-1
  # ndays is the number of days (i.e. number of rows)  
  ndays <- dim(x)[1]
  d <- seq(1,ndays)
  mu <- rep(0,nyrs) # Mean passage date  
  a <- rep(0,nyrs)  # Peak passage height
  b <- rep(0,nyrs)  # Spreadk 
  for (j in 1:nyrs){
    a[j]<-  log(max(x[,j+1],na.rm=TRUE))    
#    mu[j]<-  sum(d*x[,j+1],na.rm=TRUE)/sum(x[,j+1],na.rm=TRUE)
    mu[j]<-  which.max(x[,j+1])
    b[j]<-   sqrt(sum((d-mu[j])^2*(x[,j+1]),na.rm=TRUE)/sum((x[,j+1]),na.rm=TRUE))/mu[j]
  }
  # Initial starting data    
  inits<- list(
    a =a*rnorm(nyrs,1,0.1), 
    mu = mu*rnorm(nyrs,1,0.1),
    b = b*rnorm(nyrs,1,0.025)
  )
  return(inits)
})

#-----------------------------------------------------------------------
#  1.4: Create Initial Parameters 
#-----------------------------------------------------------------------
#---- UI Output-----------------------------------------------------------------
#-----------------------------------------------------------------------
#  1.3: Create Hyper Parameter priors  
#-----------------------------------------------------------------------
hyper <- reactive({
  x <- inits()
  y <- data()
  ym <- apply(y[,-1],1,FUN=median, na.rm=TRUE)
#  ym <- apply(y[,-1],1,FUN=sd, na.rm=TRUE)
  ym <- round(ym*input$n.cv)
  ym <- round(ym)
  ym[ym<1] <- 1
  hyper <- list(
  a0m = median(x$a),
  a0tau = 1/var(x$a),
  b0m = median(x$b),
  b0tau = 1/var(x$b),
  mu0m = median(x$mu),
  mu0tau = 1/var(x$mu),
  sigma0 = ym,
  mina = (0.9*min(x$a)),
  minb = (0.9*min(x$b)),
  minmu = (0.9*min(x$mu))
  ) 
  return(hyper)
})

  
#========================================================================
#  JAG Models 
#========================================================================
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  #   JAG Model Normal distribution assumption 
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
      theta[j,i] <- exp(a[j])*exp(-0.5*pow(log(i/mu[j])/b[j],2))
      
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
  #  All parameters are hierachical 
    a[j] ~ dnorm(a0,a0.prec)%_%T(mina,)
    b[j] ~ dnorm(b0,b0.prec)%_%T(minb,)   
    mu[j] ~ dnorm(mu0,mu0.prec)%_%T(minmu,)
    #    kappa[j] ~ dgamma(kappaa, kappab)
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
  b0.sigma ~ dunif(0,0.5)  
  mu0.prec <- 1/pow(mu0.sigma,2)
  mu0.sigma ~ dunif(0,2) 
  #  kappaa<- kappamu * kappamu / kappasig2
  #  kappab<- kappamu / kappasig2
  #  kappamu ~ dexp(0.0001)
  #  kappasig2 ~ dexp(0.0001)
  
## This assumes that variance of each day is independent.     
  for(i in 1:ndays) {    
#    sigmad[i] ~ dnorm(0,0.000001)%_%T(0,)  
    sigmad[i] ~ dunif(0,sigma0[i])  
    tausqd[i] <-pow(sigmad[i],-2)
  }		
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   
#   JAG Model Poisson distribution assumption 
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
#   JAG Model Negative Binomial distribution assumption 
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

#=======================================================================
#  End of JAG Model Code 
#=======================================================================
Bayesmodel <- reactive({
  #  JAGS model selection 
  if(input$Er=='Normal'){jagmodel <- jag.model.n}
  if(input$Er=='Poisson'){jagmodel <- jag.model.p}
  if(input$Er=='Negative Binomial'){jagmodel <- jag.model.nb}
  return(jagmodel)
  })
output$test <- renderPrint({
  Bayesmodel()
})


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Pane 2: Run JAG Model 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
run.JAGS <- eventReactive(input$Run,{
#-----------------------------------------------------------------------  
# The number of replicates
  boot.n <- (input$n.iter*input$n.chain)
#-----------------------------------------------------------------------  
  progress <- Progress$new(min=1,max=boot.n)
  on.exit(progress$close())
  progress$set(message = paste('JAG Model in progress'),
               detail = 'This will take a while. Be patient please....')
  for (i in 1:boot.n) {
    
    progress$set(value = i)
  }

#-----------------------------------------------------------------------   

# Import data 
datnew <-  Bayesdata()
hyper <- hyper()
# Import inits
rjag.inits <- inits()
niter <- input$n.iter
nburn <- input$n.burnin
nthin <- input$n.thin
nchain <- input$n.chain

parameters <- c('a','b','mu','y') 

initss <- function(){rjag.inits}

#  JAGS model selection 
if(input$Er=='Normal'){jagmodel <- jag.model.n}
if(input$Er=='Poisson'){jagmodel <- jag.model.p}
if(input$Er=='Negative Binomial'){jagmodel <- jag.model.nb}


output <- jags.parallel(data=append(datnew,hyper),inits=initss,parameters.to.save=parameters, model.file= jagmodel,
               n.chains=nchain, n.iter=niter,n.burnin=nburn,n.thin=nthin,DIC=TRUE)
return(output)
})

#-----------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Pane 3: Post Data processing 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-----------------------------------------------------------------------
#  1.3: Extract JAG results 
#-----------------------------------------------------------------------
sim <- reactive({
  x <- run.JAGS()
  return(x)
})


mcmc <- reactive({
  # Read mcmc data
  mcmc <- as.mcmc(sim())
  # post is mcmc data 
  post <- as.matrix(mcmc)  
  return(post)
})

post.summary <- reactive({
  sim_sum <- print(sim())
  post <- sim_sum$summary
  return(post)
})

output$summary <- renderPrint({
  sim_sum <- print(sim())
  print(sim_sum$summary)
})

#-----------------------------------------------------------------------
#  Create model parameter lists ordered by years
#-----------------------------------------------------------------------
par.cols <- reactive({
  # nyrs is the number of years (i.e. number of columns) 
  nyrs <- dim(data())[2]-1  
  col.a <-  character(nyrs)
  col.b <-  character(nyrs)
  col.m <-  character(nyrs)
  for(i in 1:nyrs){
    col.a[i] <- paste0('a[',i,']')
    col.b[i] <- paste0('b[',i,']')
    col.m[i] <- paste0('mu[',i,']')
  }
  out <- data.frame(a=col.a,b=col.b,mu=col.m)
  return(out)
})

#-------------------------------------------------------------------------------
#  5.4 Extract Annual model parameters CI
#-------------------------------------------------------------------------------
model.pars <- reactive({
  # CI interval probability   
  pci <- (100-input$CI)/200
  # Read mcmc data
  post <- as.matrix(mcmc())
  # import model parameter column names
  pars <- par.cols()
  # extract only mcmc model parameters
  model.pars <- post[,as.vector(as.matrix(pars))]
  # calculate mean, lci, uci
  ym <- apply(model.pars,2,mean)
  yl <- apply(model.pars,2,function(x) quantile(x, pci))
  yu <-  apply(model.pars,2,function(x) quantile(x, 1-pci))
  # extract model par names 
  parname <- names(ym)
  # create data.frame and output
  out <- data.frame(pars=parname,mean=ym, LCI = yl, UCI = yu) 
  return(out)
})


#-----------------------------------------------------------------------
#  1.3: Extract mcmc of missing passage by day, year 
#-----------------------------------------------------------------------
post.samp <- reactive({
# post is mcmc data 
  post <- as.matrix(mcmc())
# Get original data 
  x <- data()
# nyrs is the number of years (i.e. number of columns) 
  nyrs <- dim(x)[2] -1
# ndays is the number of days (i.e. number of rows)  
  ndays <- dim(x)[1]
# Create na.list matrix   
  na.list <- matrix(NA,nyrs,ndays)
# Find Row and column of NA and insert location y[,] name
  for (i in 1:ndays){
    for (j in 1:nyrs){
      na.list[j,i]<- ifelse(is.na(x[i,j+1]),paste0('y[',j,',',i,']'),NA) 
    }
  }
# Vectorize the matrix, and remove NA
# navector is a vector of the y matrix with NA   
  navector <- na.list[which(!is.na(na.list))]
# out is mcmc matrix that include only missing passage estimates   
  out <- post[,navector]
  return(out)  
})

#-----------------------------------------------------------------------
#  5.2 Calculate Daily median,  CI of missing dates 	  
#-----------------------------------------------------------------------
y.pred.yd <- reactive({
# mcme resutls   
  y.pred <- post.samp()
# CI interval probability   
  pci <- (100-input$CI)/200
# Median estimate 
ymed <- apply(y.pred,2,median)
# cut off to zero
ymed <- ifelse(ymed<0,0,ymed)
# Lower CI
ylow <- apply(y.pred,2, function(x) quantile(x, pci))
# cut off to zero
ylow <- ifelse(ylow<0,0,ylow)
# Upper CI
yup <- apply(y.pred,2, function(x) quantile(x, 1-pci))
out <- data.frame(y = names(ymed), ymed=ymed,ylow=ylow,yup=yup)
return(out)
})

#-------------------------------------------------------------------------------
#  5.4 Extract Annual 95% CI passage 
#-------------------------------------------------------------------------------
pred.y <- reactive({
# CI interval probability   
  pci <- (100-input$CI)/200
# Read mcmc data
  y.pred <- post.samp()
#extract names: this extracts only First part of bracket (year id)
tyear <- substr(colnames(y.pred),3,4)
# Remove comma 
tyear <- ifelse(substr(tyear,2,2)== ',',substr(tyear,1,1),tyear)
# Replace names to year id 
colnames(y.pred) <- tyear
# Combine columns based on year id 
y.pred.yr <- as.data.frame(sapply(unique(colnames(y.pred)), function(x) rowSums(y.pred[, colnames(y.pred) == x, drop = FALSE])))
# Calculate median, CI of missing dates  
# ym, ylow, and yup are median, CI of annual passage of missing dates
ym <- round(apply(y.pred.yr,2,median),0)
ym <- ifelse(ym<0,0,ym)
ylow <- round(apply(y.pred.yr,2,function(x) quantile(x, pci)),0)
ylow<- ifelse(ylow<0,0,ylow)
yup <- round(apply(y.pred.yr,2,function(x) quantile(x, 1-pci)),0)
year <- years()
nyrs <- length(year)
tname <- as.numeric(names(ym))
# Create vector that will include missing years
ym2 <- vector('numeric',nyrs)
yl2 <- vector('numeric',nyrs)
yu2 <- vector('numeric',nyrs)

for(i in tname){
  ym2[i] <- ym[as.numeric(names(ym))==i] 
  yu2[i] <- yup[as.numeric(names(yup))==i] 
  yl2[i] <- ylow[as.numeric(names(ylow))==i] 
}
obs <- data()
esc.ob <- colSums(obs[,-1],na.rm = TRUE)
p.est <- round(100*ym2/(ym2+esc.ob),1)
# Change NA to 0 
ym2[is.na(ym2)]<-0
yl2[is.na(yl2)]<-0
yu2[is.na(yu2)]<-0
est.t <- esc.ob+ym2
lci <- esc.ob+yl2
uci <- esc.ob+yu2
sum <- data.frame(year, esc.ob, ym2, est.t, lci,uci,p.est)
names(sum) <- c('year','observed','Median Est Missed','Total','LCI','UCI','% Est')
return(sum)
})

#-----------------------------------------------------------------------
#  1.1: Data output table 
#-----------------------------------------------------------------------
# Annual passage 
output$ptable <- renderTable({
  pred.y()
})

# Model parameters
output$partable <- renderTable({
  model.pars()
})

# Daily passage by year 
output$ytable <- renderTable({
  pred.yd()[[input$columns2]][,1:6]
})

#-------------------------------------------------------------------------------
#  5.4 Create  Missing passage by date, year   
#-------------------------------------------------------------------------------
pred.yd <- reactive({
  # Read predicted daily estimate 
  ypred <- y.pred.yd()
  # Get original data 
  x <- data()
  # nyrs is the number of years (i.e. number of columns) 
  nyrs <- dim(x)[2] -1
  # ndays is the number of days (i.e. number of rows)  
  ndays <- dim(x)[1]
  
#  Create Data Matrix of Meidan, L U CI 	  
  # y2m is a matrix of median passage 
  y2m <- matrix(NA,ndays,nyrs)
  # y2u is a matrix of upper CI passage    
  y2u <- matrix(NA,ndays,nyrs)
  # y2l is a matrix of lower CI passage   
  y2l <- matrix(NA,ndays,nyrs)
  
# Fill in predicted passage   
  for (j in 1:nyrs){
    for (i in 1:ndays){
     if(paste0('y[',j,',',i,']') %in% ypred$y){
      y2m[i,j] <- ypred[ypred$y==paste0('y[',j,',',i,']'),2]
      y2l[i,j] <- ypred[ypred$y==paste0('y[',j,',',i,']'),3]
      y2u[i,j] <- ypred[ypred$y==paste0('y[',j,',',i,']'),4]
      }
    }
  }
# Create a list file   
 yd <- list()  
for(i in 1:nyrs){
  # Create a data.frame with observed escapement, estimated, lower CI, and upper CI
  ob <- x[,1+i]
  ob[is.na(ob)] <- 0
  ym <- y2m[,i]
  yl <- y2l[,i]
  yu <- y2u[,i]
  ym[is.na(ym)] <- 0
  t <- ob+ym
  tl <- ob+yl
  tu <- ob+yu
  yd[[i]] <- data.frame(x$Date,t,x[,1+i],y2m[,i],tl,tu,y2l[,i],y2u[,i])
  # Rename the column name 
  names(yd[[i]]) <- c('Date','Total','observed','Est.Missing','LCI','UCI','yl','yu')
 }
 names(yd) <- years()
 return(yd)
})

#-----------------------------------------------------------------------
#  Results download
#-----------------------------------------------------------------------
# Downloadable csv of selected dataset ----
output$downloadData <- downloadHandler(
  filename = function() {
    "downloaddata.xlsx"
  },
  content = function(file) {
    foo <-pred.yd()
    foo$sum<- pred.y()
    write.xlsx(foo, file)
  }
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Pane 4: Model Diagnoses  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-----------------------------------------------------------------------
#  Trace and density plots 
#-----------------------------------------------------------------------
output$pfig <- renderPlot({
  pg <- input$p1
  mcmc <- as.mcmc(sim())
  pars <- par.cols()
  par(mfrow=c(2,6),mai=c(0.2,0.2,0.2,0.2))
  pg1 <- 2*(pg-1)+1
  pg2 <- min(2*(pg-1)+2,length(years()))
  plot(mcmc[, as.vector(t(as.matrix(pars[pg1:pg2,])))],auto.layout=FALSE)
})

#-----------------------------------------------------------------------
#  Passage figures  
#-----------------------------------------------------------------------
output$pfig2 <- renderPlot({
# page number
  pg <- input$pf
# Daily estimate 
  predyd <- pred.yd()
# Annual estimate 
  predy <- pred.y()
# Observed  data 
  x <- data()
# Date format 
  dates <- as.Date(x$Date,"%d-%b")
# Model parameter estimates 
  modelpar <- model.pars()  
# Model parameter names
  pars <- par.cols()  
# nyrs is the number of years (i.e. number of columns) 
  nyrs <- dim(x)[2] -1
# ndays is the number of days (i.e. number of rows)  
  ndays <- dim(x)[1]
  days <- seq(1,ndays)
# import model parameter column names  
  am <- modelpar[modelpar$pars %in% pars$a,2]
  bm <- modelpar[modelpar$pars %in% pars$b,2]
  mum <- modelpar[modelpar$pars %in% pars$mu,2]
  
# create estimated model passage   
  Modelesc <- matrix(0,ndays,nyrs)  
  for (i in 1:ndays){
    for (j in 1:nyrs){
      #     Expected log normal run timing   
      Modelesc[i,j]<- exp(am[j])*exp(-0.5*(log(days[i]/mum[j])/bm[j])^2)
      #     Expected Extreme value normal run timing   
      #      Modelesc[i,j] <-am[j]*exp(-exp(-(x[i]-mum[j])/bm[j])-(x[i]-mum[j])/bm[j]+1)
      #     Expected log logistic run timing
      #     Modelesc[i,j] <- (am[j]*(bm[j]/mum[j])*((x[i]/mum[j])^(bm[j]-1))/(1+(x[i]/mum[j])^bm[j])^2)-1   
    }
  }

# Create a data.frame with observed escapement, estimated, lower CI, and upper CI

# plot graph
par(mfrow=c(2,2),mai=c(.6,.5,.1,.1))
pg1 <-  4*(pg-1)+1
pg2 <-  min(4*(pg-1)+4,nyrs)

 for(i in pg1:pg2){
# get annual daily data 
  pred <- predyd[[i]]  
# get annual data 
  y <- predy[i,]
# Get maximum daily passage estimate
  maxdat<-max(pred[,-1],na.rm=TRUE)
# plot observed passage
  plot(dates,pred$observed,col='blue', ylim = c(0,maxdat), xlab= 'Date', ylab='escapement')
  tex <- c(years()[i],paste('Total',y$Total),paste('CI',y$LCI,'-',y$UCI))
  legend('topright',bty ='n', tex,cex=1.2)
    # plot modeled run timing
  lines(dates,Modelesc[,i],col='red')
    # plot 95% CI lines    
  arrows(dates,y0=pred$yl,y1=pred$yu,code=0)
    # plot median passage stimate    
  points(dates,pred$Est.Missing, pch=21, col='black',bg='white')
  }
})


})# End of Server 




# Create Shiny app ----
shinyApp(ui, server)
