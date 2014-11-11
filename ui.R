library(shiny)
require(RSQLite)

source('/media/FD/BIOSCREEN/R/BIOPROD/PE_getConnection.R')
source('/media/FD/BIOSCREEN/R/BIOPROD/PE_Subpop_functions.R')

listOfKT <- c("DiseaseDuration", "AgetAtExam") #getKTFromDB(connection)
listOfFilters <- dbGetQuery(connection, 'SELECT name FROM filters_meta')[[1]]
listOfVariables <- getFiltersFromDB(connection, c("NUM", "DISC"))

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("the Contextualization Engine Web App", "CE Web App"), # NOT SHOWN FOR EMBEDDING PURPOSES
  
  sidebarPanel(
    selectInput( "s_Y", "Variable to be plotted:", choices=as.list(listOfVariables), selected='ActualEDSS' ),
    tags$hr(),
    selectInput( "s_KT", "Temporal alignement on:", choices=as.list(listOfKT) ),
    #     sliderInput( "r_KT", "Select the range of KT you want to have a look at:", min=output$filterMin, max=output$filterMax, value=output$sampleFilter)
    uiOutput("KTUI"),
    textOutput("KTError"),
    numericInput( "KT0", "Apply filters at year:", NULL),
    #     numericInput( "test", "TESTING BOX:", NULL),
    textOutput("KT0Error"),
    tags$hr(),
    selectizeInput( "Filter1", "First Filter:", choices=c(list("Choose a Filter..."=""), as.list(listOfFilters)), selected = "NONE"),
    uiOutput("FilterUI1"),
    tags$hr(),
    selectizeInput( "Filter2", "Second Filter:", choices=c(list("Choose a Filter..."=""), as.list(listOfFilters)), selected = "NONE"),
    uiOutput("FilterUI2"),
    tags$hr(),
    checkboxInput("b_Traj", "Show one example Trajectory", FALSE),
    checkboxInput("b_Enrich", "Get the enriched versions of the datasets", FALSE)
    ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Plot",
               h2("Plot of the contextualization"),
               plotOutput("plot")),
      tabPanel("Extrapolations",
               h2("Filter Extrapolations"),
               tableOutput("FiltersTable")),
      tabPanel("DEBUG",
               verbatimTextOutput("DEBUG"))
#                conditionalPanel(
#                  condition = 'output.IMTFiltersTable != NULL',
#                  h3('non-interpolated filters'),
#                  tableOutput("IMTFiltersTable")))
#                conditionalPanel(
#                  condition = 'output.NumDDVFiltersTable',
#                  h3('numerical interpolated filters'),
#                  tableOutput("IMTFiltersTable")),
#                conditionalPanel(
#                  condition = 'output.ProbaDDVFiltersTable',
#                  h3('probabilistic interpolated filters'),
#                  tableOutput("IMTFiltersTable")),
#                textOutput("message"))
      ))
))