
shinyUI(fluidPage(theme = "bootstrap.min.css",
  titlePanel("HMDA data analytics"),
  tabsetPanel(
    tabPanel(title = "Reporting by State",
             sidebarLayout(
                           sidebarPanel(
                             selectInput(inputId = "graphChoice",
                                         label = "Choose State:", 
                                         choices = c("District of Columbia","West Virginia","Delaware","Maryland","Virginia"))
                           ),
                           mainPanel(plotOutput("chart"))
             )
             )

  )
))