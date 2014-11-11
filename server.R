library(shiny)

source('/media/FD/BIOSCREEN/R/BIOPROD/PE_getConnection.R')
source('/media/FD/BIOSCREEN/R/BIOPROD/PE_PatientVisualize.R')
source('/media/FD/BIOSCREEN/R/BIOPROD/PE_Subpop_functions.R')
source('/media/FD/BIOSCREEN/R/BIOPROD/WEB_BaseFunctions.R')

## DEBUG FUNCTIONS
debug_called <- function(x=NULL) cat(x, "called at", format(Sys.time()), '\n')
debug_print <- function(var) print(var)

## Define the plot
plotPCT <- createThemeTrajViz(annualRibbon = T, borderColor = 'black', baseSize=20, spacing = "normal")

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {
  
  
  #### UI CONTROL ------------------
  
  #Test
  output$title <- renderText({
    paste(input$Y, 'depending on:', input$TK)
  })
  
  # Test
  output$message <- renderPrint({
    cat('bla \n')
    cat('blou \n')
  })
    
  ## KT --
  
  DVs <- reactive({
    DV <- getKTDVFromDB(input$s_KT, connection)
    debug_print(input$s_KT)
    debug_print(DV)
    debug_print(dbListTables(connection))
    return(DV)
  })
  
  checkr_KT <- reactive({
    DV <- DVs()
    dKT <- diff(input$r_KT)
    if (is.null(input$r_KT))
      "IN"
    else if (dKT < DV[3])
      "LOW"
    else if (dKT > DV[4])
      "HIGH" 
    else
      "IN"
  })
  
  r_KT <- reactive({    
    DV <- DVs()
    if (is.null(input$r_KT))
      c(DV[1], DV[1]+(DV[3]+DV[4])/2)
    else
      c(input$r_KT[1], switch(checkr_KT(),
                              LOW = input$r_KT[1] + DV[3],
                              HIGH = input$r_KT[1] + DV[4],
                              IN = input$r_KT[2]) )
  })
  
  output$KTError <- renderText({    
    switch(checkr_KT(),
           LOW = paste(" Chosen interval is too small, using", r_KT()[2], "instead as a second bound instead of the one specified"),
           HIGH = paste(" Chosen interval is too big, using", r_KT()[2], "instead as a second bound instead of the one specified"),
           IN = "")
  })
  
  output$KTUI <- renderUI({
    DV <- DVs()
    names(DV) <- NULL
    sliderInput( "r_KT", "Select the range of this temporal window", DV[1], DV[2], c(DV[1], DV[1]+(DV[3]+DV[4])/2), step=DV[5])
  })
  
  
  
  ## KT0 --
  
  # Check for KT0
  checkKTO <- reactive({
    if (is.na(input$KT0))
      "missing"
    else if (input$KT0 >= input$r_KT[1] & input$KT0 <= input$r_KT[2])
      "IN" 
    else
      "OUT"
  })
  
  # Warning message for the KT0 specification
  output$KT0Error <- renderText({    
    switch(checkKTO(),
           missing = paste("Default: Taking last value of the specified range to apply filters"),
           OUT = paste("The value of", input$s_KT, "specified to apply the filter on is not within the range you chose above. Taking last value instead."),
           IN = "")
  })
  
  # Get the appropriate value for KT0
  KT0 <- reactive({
    switch(checkKTO(),
           missing = ,
           OUT = r_KT()[2],
           IN = input$KT0)
  })
  
  
  ## Filters --
  
  renderFilterUI <- function(filterNumber){
    renderUI({
      type <- getTypeFromDB(input[[paste0('Filter',filterNumber)]], connection)
      DV <- getDVFromDB(input[[paste0('Filter',filterNumber)]], connection)
      names(DV) <- NULL
      if (input[[paste0('Filter',filterNumber)]] == 'NONE')
        return(helpText("Select a filter above..."))
      else
        switch(type,
               NUM = sliderInput( paste0('FilterValue',filterNumber), "Choose an interval:", DV[1], DV[2],c(DV[1], DV[2])),
               DISC = sliderInput( paste0('FilterValue',filterNumber), "Choose an interval:", DV[1], DV[2],c(DV[1], DV[2]), step = DV[3]),
               FAC = {list(selectInput( paste0('FilterValue',filterNumber), "Choose a value:", as.list(DV), NULL ), helpText("The category filters are applied in a strict way across the range."))},
               FAC2 = selectInput( paste0('FilterValue',filterNumber), "Choose a value:", as.list(DV), NULL )
        )
    })
  }
  
  output$FilterUI1 <- renderFilterUI(1)
  output$FilterUI2 <- renderFilterUI(2)
  
  #### ENGINE -------------------

  getFilters <- reactive({
    v_s_V <- NULL
    l_V0 <- list()
    i_V <- 1
    for (i in 1:2) {
      s_V <- input[[paste0('Filter', i)]]
      V0 <- input[[paste0('FilterValue', i)]]
      if (s_V != 'NONE') {
        v_s_V[i_V] <- s_V
        l_V0[[i_V]] <- V0
        i_V <- i_V + 1
      }
    }    
    if (is.null(v_s_V)) l_V0 <- NULL
    return(list(v_s_V, l_V0))
  })
  
  argList <- reactive({ list(r_KT = r_KT(),
                             KT0 = KT0(),
                             s_KT = input$s_KT,
                             DBcon = connection,  
                             l_V0 = getFilters()[[2]],
                             v_s_V = getFilters()[[1]])    
  })
  
  df_context <- reactive({ do.call(getContext, c(argList(), list(s_Y= input$s_Y))) })
    
  r_Y <- reactive({
    getDVFromDB(input$s_Y, connection)[1:2]
  })
  
  output$plot <- renderPlot({
    df_context <- df_context()
    print(plotPCT(df_context, NULL, input$s_Y, input$s_KT, r_Y()))
  })
  
  #### EXTRAPOLATION --------------
  
  META <- reactive({ 
    list[test, l_l_Vs, META] <-do.call(getVisits, c(argList(), list(b_META = TRUE)), quote=T)
    return(META)
  })
  
  output$FiltersTable <- renderTable({
    META <- META()
    if (is.numeric(META))
      return(data.frame(0))
    table <- do.call(cbind,lapply(META, '[', 'value'));
    colnames(table) <- names(META)
    rownames(table) <- META[[1]]$name
    return(table)
  })
  
  output$DEBUG <- renderPrint({
    print(argList())
    print(checkKTO())
    print(KT0())
    print(input$FilterValue1)
    print(input$FilterValue2)
  })
    
})