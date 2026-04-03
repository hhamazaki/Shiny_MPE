#'==============================================================================
#'  Shiny Missing passage estimation model 
#'  File name: server.R
#'  Author:  Toshihide "Hamachan" Hamazaki
#'  Date:
#'  Description
#'  This Shiny model estimates daily missing passage
#'   by fitting a log-normal run timing model.
#'==============================================================================
#'==============================================================================    
#  Server:  
#'==============================================================================
server<-shinyServer(function(input, output, session){
#'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 1: Data upload and output ----
#'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'-----------------------------------------------------------------------
##  1.0 Upload File -----------------------
#'-----------------------------------------------------------------------  
### data1 Data file reading module ---- 
data.in <-  dataInputServer("datain")
# data.0  Original input data 
data.0 <- reactive({
  if(isTRUE(input$Sample)){
    out <- read.csv('Sample_data/Sample_data.csv',header=T)
    } 
  else {
   out <- data.in()
  }
  names(out)[1] <- 'Date'
  return(out)
})

#### yrange --- UI output to determine data year range ----------------------------
output$drange <- renderUI({
# Count the number of data per day 
  dat <- data.0()
  dat$nu <- rowSums(!is.na(dat[,-1]))
# Remove days with no data 
  dat <- dat[which(dat$nu>0),]
  dates <- as.Date(dat$Date,"%d-%b")
  
  fday <- min(dates)       # First dates
  lday <- max(dates)       # Last dates
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

#### Limit date range for analyses----- 
data <- reactive({
  dat <- data.0()
  dates <- as.Date(dat$Date,"%d-%b")
  dat <- dat[which(dates>=input$sdays[1]&dates<=input$sdays[2]), ]
  return(dat)
  })

#### output table -----------------------------------------------------------------
output$table <- renderTable({data.0()})

# Extract years 
year.0 <- reactive({
  dat <- data.0()
  year <- substr(names(dat[,-1]),2,5)
  return(year)
})

#Plot figure page
output$fpage.1 = renderUI({
  mn <- length(year.0())%/%6
  selectInput("pf0", "Figure Page",c(1:(mn+1)))
  })
#'-----------------------------------------------------------------------
#  Passage figures  
#'-----------------------------------------------------------------------
output$Plt_Pass <- renderPlot({
# page number
  pg <- as.integer(input$pf0)
# Observed  data 
  x <- data()
# Date format 
  dates <- as.Date(x$Date,"%d-%b")
# nyrs is the number of years (i.e. number of columns) 
  nyrs <- dim(x)[2] -1
# ndays is the number of days (i.e. number of rows)  
  ndays <- dim(x)[1]
  days <- seq(1,ndays)

# plot graph
par(mfrow=c(3,2),mar=c(2,2,2,2),bty='l',cex=1.0)
pg1 <-  6*(pg-1)+1
pg2 <-  min(6*(pg-1)+6,nyrs)

for(i in pg1:pg2){
# Get maximum daily passage estimate
  maxdat<-max(x[,i+1],na.rm=TRUE)
# plot observed passage
  plot(dates,ifelse(is.na(x[,i+1]),0,x[,i+1]),pch=19,col=ifelse(is.na(x[,i+1]),2,4), xlim=c (input$sdays),ylim = c(0,maxdat), xlab= 'Date', ylab='Passage')
  tex <- c(year.0()[i],paste('missed days',sum(is.na(x[,i+1]))))
  legend('topright',bty ='n', tex,cex=1.2)
  }
})



#'-------------------------------------------------------------------------------
#  1.2: Output page range determined by data ----
#'-------------------------------------------------------------------------------
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
  selectInput("pf", "page", c(1:(mn+1)))
  })

#Bayesian trace-density plot figure page
output$lpage = renderUI({
  mn <- length(years())%/%2
  selectInput("p1", "page", c(1:(mn+1)))
})

#'-----------------------------------------------------------------------
#  1.3: Create Bayes data ----
#'-----------------------------------------------------------------------
Bayesdata <- reactive({
#  Import data 
    x <- data()
# nyrs is the number of years (i.e. number of columns) 
    nyrs <- dim(x)[2]-1
# ndays is the number of days (i.e. number of rows)  
    ndays <- dim(x)[1]
# y is observed transposed escapement matrix   
    y <- matrix(NA,nyrs,ndays)
    for (i in 1:ndays){
      for (j in 1:nyrs){
        y[j,i]<-  ifelse(x[i,j+1]<0,0,x[i,j+1])
      }
    }
# dat is Bayesian data 
     D <- rep(0,nyrs)
    for (j in 1:nyrs){  
    D[j] <- as.integer(trunc(log10(max(x[,j+1],na.rm=TRUE)),0))
      }
    dat <-list(nyrs=nyrs, ndays=ndays,y=y,D=D)
    return(dat)
  })

#-----------------------------------------------------------------------
#  1.3: Create Initial Parameters 
#-----------------------------------------------------------------------
inits <- reactive({
  x <- data()
  y <- Bayesdata()$y
  # nyrs is the number of years (i.e. number of columns) 
  nyrs <- dim(x)[2]-1
  # ndays is the number of days (i.e. number of rows)  
  ndays <- dim(x)[1]
  d <- seq(1,ndays)
  mu <- rep(0,nyrs) # Mean passage date  
  a <- rep(0,nyrs)  # Peak passage height
  b <- rep(0,nyrs)  # Spread
  D <- Bayesdata()$D
  for (j in 1:nyrs){
    a[j]<-  max(y[j,],na.rm=TRUE)/(10^D[j])
    mu[j]<-  which.max(y[j,])
    b[j]<-   sqrt(sum((d-mu[j])^2*(y[j,]),na.rm=TRUE)/sum((y[j,]),na.rm=TRUE))/mu[j]
  }
  # Initial starting data    
  inits<- list(
    a = a*rnorm(nyrs,1,0.1), 
    mu = mu*rnorm(nyrs,1,0.1),
    b = b*rnorm(nyrs,1,0.025)
  )
  return(inits)
})

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
  ym[ym<1|is.na(ym)] <- 1
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
  inits()
})


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Pane 2: Run JAG Model 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-----------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Pane 3: Post Data processing 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-----------------------------------------------------------------------
#  1.3: Extract JAG results 
#-----------------------------------------------------------------------
# Run Bayesian Model 
sim <- BayesInputServer('Bayes', Bayesdata, hyper, inits, Bayesmodel)


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
  pg <- as.integer(input$p1)
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
  pg <- as.integer(input$pf)
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
  D <- Bayesdata()$D
# create estimated model passage   
  Modelesc <- matrix(0,ndays,nyrs)  
  for (i in 1:ndays){
    for (j in 1:nyrs){
      #     Expected log normal run timing   
      Modelesc[i,j]<- am[j]*(10^D[j])*exp(-0.5*(log(days[i]/mum[j])/bm[j])^2)
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



