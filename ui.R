#'==============================================================================
#'  Shiny Missed passage estimation model 
#'  File name: ui.R
#'  Author:  Toshihide "Hamachan" Hamazaki
#'  Date:
#'  Description
#'  This Shiny model estimates missed passage by fitting a log-normal run timing model. 
#'==============================================================================
#initialize
library(shiny)
library(shinythemes) # used to specify themes
library(datasets)# used to read markdown file
library(rmarkdown)     # used to get rmarkdown file
library(markdown)     # used to get markdown file
library(coda)         # used to read MCMC data 
library(R2jags)       # used to rUN JAGS 
library(openxlsx)     # used to save file in EXCEL 
options(scipen=999)   # Do not show Scientific notation
source("Rcode/Shiny_modules.R")   #  Module codes 
source("Rcode/Shiny_Bayes_modules.R")  #  Modules related to Bayesian model 
#'=======================================================================    
#  UI:  
#'=======================================================================
ui<-fluidPage(
  navbarPage(
    theme = shinytheme("cerulean"),  
    title = div(
      img(src="Picture2.png",height=40, width=40)
      ,  "Missed Passage Estimation"),
#'------------------------------------------------------------------------------
#  Panel 1:  Data Input and Submit ---- 
#'------------------------------------------------------------------------------
  tabPanel("Data Input",
   sidebarPanel(width = 3,
#'------------------------------------------------------------------------------
#'  File Inuput
#'------------------------------------------------------------------------------
# Input: Select a file ----

# File input UI: shows when sample data input is false
conditionalPanel(condition="input.Sample== false",
# File Input module 
  dataInputUI("datain", "User data (.csv format)")
 ),
hr(),
checkboxInput(inputId="Sample", "Inport Sample Data", FALSE), 
# Limit Data year range 
  uiOutput('drange'),
  uiOutput('fpage.1')
   ), # End SidebarPanel

#' Main Panel ---------------------
  mainPanel(
    tabsetPanel(
#' Data input table       
      tabPanel("Table",    
               tableOutput("table")
               ),
#' Data output figures       
      tabPanel("Plot",    
               plotOutput(height='500px',"Plt_Pass")
               ),
        )  # End tabsetPanel
      )  # End mainPanel
    ), # End tabpanle
#-------------------------------------------------------------------------------
#  Panel 2  Model run ----
#-------------------------------------------------------------------------------
  tabPanel("Run Model",
    sidebarPanel(width = 3,
      # Horizontal line ----
      p(strong("Run Bayesian model")),
      BayesInputUI('Bayes'),
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
  conditionalPanel(condition="input.conditionedPanels == 'Trace Plots'", 
                                                    uiOutput('lpage')            
            ) # End ConditionalPanel      
            ),  # End sidebarPanel
 mainPanel(tabsetPanel(
    tabPanel("Bayesian Model",
          p(strong("Model summary")), 
          verbatimTextOutput('summary')                      
              ),
    tabPanel("model",
          verbatimTextOutput('test')
             ),# End tabPanel
#    tabPanel("Model output",
#             ),
    tabPanel("Trace Plots",
            plotOutput(height='500px','pfig')
         ),
      id = "conditionedPanels"
                         )#End tabsetPanel
        )#End mainPanel
      ),#End tabPanel
#-----------------------------------------------------------------------
#  Panel 3 
#-----------------------------------------------------------------------
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
          ),#End tabPanel
#-----------------------------------------------------------------------
#  Panel 4 Help Page  
#-----------------------------------------------------------------------
 tabPanel("Help",
    navlistPanel(widths = c(3,9),
  "Data Input",
    tabPanel("Passage Data",
        includeMarkdown("documents/Input_help.md")
             ),
  "Model Setting",
    tabPanel("Bayesian Model",
      tabsetPanel(
        tabPanel("Mdel Seting",       
          withMathJax(includeMarkdown("documents/JAGS_help.md"))
            ),
        tabPanel("Model 1",       
#          withMathJax(includeMarkdown("documents/Escapement_Only/Percentile_Analyses.md"))
            ),
        tabPanel("Model",       
#          withMathJax(includeMarkdown("documents/Escapement_Only/Risk_Analyses.md"))
          )
        ) # End tabsetPanel
      ) # End Tab Panel
      ) # End navlistPanel
    ) # End Help Panel
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

