# Server side scripts
shinyServer(function(input, output) {
  
  
  dataForPlot <- reactive({
      
      #Creating multiple plots; each corresponding to an year representing the shares of each state to the market
    
    y <- switch(input$graphChoice,
                "District of Columbia" = (Shared$StateName=='District of Columbia'),
                "Virginia" = (Shared$StateName=='Virginia'),
                "Maryland" = (Shared$StateName=='Maryland'),
                "Delaware" = (Shared$StateName=='Delaware'),
                "West Virginia"=(Shared$StateName=='West Virginia'))
    
    return(y)
    
  })
  
  titlePlot <- reactive({
    paste("Mortgage Portfolio by", input$graphChoice)
  })
  
  output$chart <- renderPlot({
    source("fn.R",local = TRUE)
    plot <- switch(input$graphChoice,
          "District of Columbia" = multiplot(doc_plot2012, doc_plot2013, doc_plot2014, cols=3,layout=NULL),
        "Virginia" = multiplot(vir_plot2012, vir_plot2013, vir_plot2014, cols=3,layout=NULL),
      "Maryland" = multiplot(mary_plot2012, mary_plot2013, mary_plot2014, cols=3,layout=NULL),
      "Delaware" = multiplot(del_plot2012, del_plot2013, del_plot2014, cols=3,layout=NULL),
      "West Virginia" = multiplot(wvir_plot2012, wvir_plot2013, wvir_plot2014, cols=3,layout=NULL)
      )

    
    print(plot)
    
  })
  
  
})
