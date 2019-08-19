setwd("C:/Users/USER/Desktop/Adi_Bbrio/R_GUI_app")

library(car)                    # For VIF (Multicollinearity among independent variables)
library(shiny)
library(shinydashboard)
# library(shinyTime)
library(DT)
library(data.table)
library(shinyWidgets)
# library(plotly)
# library(dplyr)
# library(formattable)
library(shinyjs)
# library(lubridate)


ui <- dashboardPage(
  dashboardHeader(title = "Linear Regression"),
  dashboardSidebar(),
  dashboardBody(
    tabsetPanel(
      tabPanel("UploadData", fluid = TRUE,
               useShinyjs(),
               fluidRow(h6("")),
               fluidRow(h6("")),
               fluidRow(h6("")),
               fluidRow(column(6, fileInput('data_file_upload', 'Upload linear regression data',
                                            accept=c('text/csv',
                                                     'text/comma-separated-values,text/plain',
                                                     '.csv')))
                        # ,
                        # column(6, fileInput('tickets_sold', 'Upload the tickets sold data',
                        #                     accept=c('text/csv',
                        #                              'text/comma-separated-values,text/plain',
                        #                              '.csv')))
               ),
               fluidRow(column(6, downloadButton('formatform1', 'Download Format'))
                        # , 
                        # column(6, downloadButton('formatform2', 'Download Format'))
               ),
               fluidRow(column(6, actionButton("submit_1", "Submit"))),
               fluidRow(h6("")),
               fluidRow(h6("")),
               # fluidRow(column(5, uiOutput("multiInput_1")))
               # selectInput("dependent", "Dependent Variable:", c("x","y","z")),
               uiOutput("dep"),
               fluidRow(h6("")),
               uiOutput("indep"),
               fluidRow(h6("")),
               fluidRow(h6("")),
               fluidRow(column(6, tableOutput("regTab"))),
               
               fluidRow(column(6, tableOutput("vif")))
      )
    )
  )
)


server <- function(input, output) {
  
  # observe({
  #   shinyjs::toggleState(id = "submit_1", condition = (!is.null(input$data_file_upload)))
  # })
  
  
  
  
  observe({
    if(is.null(input$data_file_upload)){
      shinyjs::disable("submit_1")
    } else {
      shinyjs::enable("submit_1")
    }
  })
  
  
  data1 <- eventReactive(input$submit_1, {
    data1 <- read.csv(input$data_file_upload[['datapath']])
    # data1 <- data.frame(c1 = c(1:100), c2 = c(runif(100, 0,20)), c3 = c(runif(100, 200,1000)))
  })
  
  
  output$dep <- renderUI({
    if(!is.null(input$data_file_upload) & input$submit_1){
      # print(colnames(data1()))
      y <- colnames(data1())
      selectInput("dep", "dependent Variables:", choices = y)
    }
  })
  
  output$indep <- renderUI({
    checkboxGroupInput("indep", "Independent Variables:", choices = names(data1())[!names(data1()) %in% input$dep], selected = , names(data1())[!names(data1()) %in% input$dep])
  })
  
  
  runRegression <- reactive({
    lm(as.formula(paste(input$dep," ~ ",paste(input$indep,collapse="+"))), data = data1())
  })
  
  output$regTab <- renderTable({
    if(!is.null(input$indep)){
      # print(input$indep)
      lm_df <- data.frame(Variables = c("Coefficients", input$indep), summary(runRegression())$coefficients)
      colnames(lm_df) <- c("Variables",	"Estimate",	"Std. Error",	"t value",	"p value")
      lm_df
    } else {
      print(data.frame(Warning="Please select Model Parameters."))
      return()
    }
  })
  
  output$vif <- renderTable({
    if(!is.null(input$indep) & length(input$indep) >= 2){
      df <- data.frame('Independent Variables' =  input$indep, VIF = car::vif(runRegression()))
      print(df)
      df
    } else {
      return()
    }
    
  })
  
}

shinyApp(ui, server)


