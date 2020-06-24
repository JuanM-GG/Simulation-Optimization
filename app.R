library(shiny)
library(shinydashboard)
library(markdown)
library(xlsx)
source("analysis_with_kinetic_models.R")

ui = navbarPage("Fermentation Analysis",
                        
                        tabPanel("Simulation",
                                 
                                 sidebarLayout(
                                         
                                         sidebarPanel(
                                                 
                                                 selectInput(inputId = "mod1",label = "Choose a model",
                                                             choices = list("model_1","model_2","model_3","model_4","model_5"),
                                                             selected = "model_1"),
                                                 
                                                 strong("Enter initial conditions"),
                                                 
                                                 numericInput("xint1","X0",0.2),
                                                 numericInput("sint1","S0",40),
                                                 numericInput("pint1","P0",0),
                                                 
                                                 conditionalPanel( condition = "output.model1",
                                                                   fluidRow(
                                                                           
                                                                           column(width = 6,
                                                                                  sliderInput(inputId = "num11",label = "vmax",min = 0.01,max = 2,value = 0.5,step = 0.1),
                                                                                  sliderInput(inputId = "num12",label = "ks",min = 1,max = 200,value = 80,step = 1)
                                                                           ),
                                                                           column(width = 6,
                                                                                  sliderInput(inputId = "num13",label = "yxs",min = 0.01,max = 1.5,value = 0.8,step = 0.1),
                                                                                  sliderInput(inputId = "num14",label = "ypx",min = 1,max = 20,value = 12,step = 1))
                                                                   )),
                                                 
                                                 conditionalPanel( condition = "output.model2",
                                                                   
                                                                   fluidRow(
                                                                           
                                                                           column(width = 6,
                                                                                  sliderInput(inputId = "num21",label = "vmax",min = 0.01,max = 2,value = 0.5,step = 0.1),
                                                                                  sliderInput(inputId = "num22",label = "ks",min = 1,max = 200,value = 80,step = 1),
                                                                                  sliderInput(inputId = "num23",label = "yxs",min = 0.01,max = 1.5,value = 0.8,step = 0.1)
                                                                           ),
                                                                           column(width = 6,
                                                                                  sliderInput(inputId = "num24",label = "ypx",min = 1,max = 20,value = 12,step = 1),
                                                                                  sliderInput(inputId = "num25",label = "kp",min = 1,max = 200,value = 180,step = 1))
                                                                   )
                                                 ),
                                                 
                                                 conditionalPanel( condition = "output.model3",
                                                                   fluidRow(
                                                                           
                                                                           column(width = 6,
                                                                                  sliderInput(inputId = "num31",label = "vmax",min = 0.01,max = 2,value = 0.5,step = 0.1),
                                                                                  sliderInput(inputId = "num32",label = "ks",min = 1,max = 200,value = 80,step = 1),
                                                                                  sliderInput(inputId = "num33",label = "yxs",min = 0.01,max = 1.5,value = 0.8,step = 0.1)
                                                                           ),
                                                                           column(width = 6,
                                                                                  sliderInput(inputId = "num34",label = "alfa",min = 1,max = 20,value = 12,step = 1),
                                                                                  sliderInput(inputId = "num35",label = "beta",min = 0.01,max = 0.5,value = 0.1,step = 0.01))
                                                                   )
                                                 ),
                                                 
                                                 conditionalPanel( condition = "output.model4",
                                                                   fluidRow(
                                                                           
                                                                           column(width = 6,
                                                                                  sliderInput(inputId = "num41",label = "vmax",min = 0.01,max = 2,value = 0.5,step = 0.1),
                                                                                  sliderInput(inputId = "num42",label = "ks",min = 1,max = 200,value = 80,step = 1),
                                                                                  sliderInput(inputId = "num43",label = "yxs",min = 0.01,max = 1.5,value = 0.8,step = 0.1)
                                                                           ),
                                                                           column(width = 6,
                                                                                  sliderInput(inputId = "num44",label = "ypx",min = 1,max = 20,value = 12,step = 1),
                                                                                  sliderInput(inputId = "num45",label = "kd",min = 0.001,max = 1,value = 0.01,step = 0.001))
                                                                   )
                                                ),
                                                 
                                                conditionalPanel( condition = "output.model5",
                                                                  fluidRow(
                                                                          
                                                                          column(width = 6,
                                                                                 sliderInput(inputId = "num51",label = "vmax",min = 0.01,max = 2,value = 0.5,step = 0.1),
                                                                                 sliderInput(inputId = "num52",label = "ks",min = 1,max = 200,value = 80,step = 1),
                                                                                 sliderInput(inputId = "num53",label = "yxs",min = 0.01,max = 1.5,value = 0.8,step = 0.1)
                                                                          ),
                                                                          column(width = 6,
                                                                                 sliderInput(inputId = "num54",label = "ypx",min = 1,max = 20,value = 12,step = 1),
                                                                                 sliderInput(inputId = "num55",label = "km",min = 0.01,max = 0.1,value = 0.05,step = 0.01))
                                                                  )),
                                                 
                                                 
                                                 actionButton("act1","Reset")
                                                 
                                         ),
                                         
                                         mainPanel(
                                                 
                                                 plotOutput("plot1"),
                                                 uiOutput('ex1')
                                                 
                                         )
                                         
                                 )
                                 
                        ),
                        
                        tabPanel("Optimization",
                                 
                                 sidebarLayout(
                                         
                                         sidebarPanel(
                                                 
                                                 fileInput(inputId = "file2",label = "Enter the data", multiple = FALSE,
                                                           accept = c(".xlsx")),
                                                
                                                 checkboxInput(inputId = "check2",label = "Head", value = TRUE),
                                                 
                                                 selectInput(inputId = "sheet",label = "Select sheet",
                                                             choices = list("1","2","3","4","5"),selected = "1"),
                                                 
                                                 selectInput(inputId = "mod2",label = "Choose a model",
                                                             choices = list("model_1","model_2","model_3","model_4","model_5"),
                                                             selected = "model_1"),
                                                 actionButton("act2","Make optimization")
                                                 
                                         ),
                                         
                                         mainPanel(
                                                 
                                                 fluidRow(
                                                         column(3,
                                                                tableOutput("content2")
                                                         ),
                                                         column(8,offset = 1,
                                                                plotOutput("plot2"))
                                                 )
                                                 
                                         )
                                 )
                                 
                        ),
                        
                        navbarMenu("Statistic Analysis",
                                   
                                   tabPanel("t-student",
                                            
                                            sidebarLayout(
                                                    
                                                    sidebarPanel(
                                                            
                                                            fileInput(inputId = "file31",label = "Enter the data",multiple = TRUE),
                                                            
                                                            checkboxInput(inputId = "check31",label = "Header",value = TRUE)
                                                    ),
                                                    
                                                    mainPanel(
                                                            
                                                            
                                                            
                                                    )
                                            )
                                            
                                   ),
                                   
                                   tabPanel("ANOVA",
                                            
                                            sidebarLayout(
                                                    
                                                    sidebarPanel(
                                                            
                                                            fileInput(inputId = "file32",label = "Enter the data",multiple = TRUE),
                                                            
                                                            checkboxInput(inputId = "check32",label = "Header",value = TRUE)
                                                    ),
                                                    
                                                    mainPanel(
                                                    
                                                            )
                                                    
                                            )
                                   ),
                                   
                                   tabPanel("Linear regression",
                                   
                                                     sidebarLayout(
                                                    
                                                             sidebarPanel(
                                                            
                                                                     fileInput(inputId = "file33",label = "Enter the data",
                                                                               multiple = TRUE),
                                                           
                                                                     checkboxInput(inputId = "check33",label = "Header",
                                                                                   value = TRUE)
                                                    ),
                                                    
                                                    mainPanel(
                                                            
                                                    )
                                                    
                                            )
                                            
                                   ),
                                   
                                   tabPanel("Multiple regression",
                                   
                                                     sidebarLayout(
                                            
                                                                     sidebarPanel(
                                                            
                                                                             fileInput(inputId = "file34",label = "Enter the data",
                                                                                       multiple = TRUE),
                                                            
                                                                             checkboxInput(inputId = "check34",label = "Header",
                                                                                           value = TRUE)
                                                                             
                                                    ),
                                                    
                                                    mainPanel(
                                                            
                                                    )
                                                    
                                            )
                                            
                                   )
                                   
                        )
                        
)
        
server = function(input, output, session) {
                # Simulation section ########################################################
                
                # This section is for select the panel with the parameters of the model
                output$model1 <- reactive({
                        ifelse(input$mod1 == "model_1", T,F)
                })
                
                output$model2 <- reactive({
                        ifelse(input$mod1 == "model_2", T,F)
                })
                
                output$model3 <- reactive({
                        ifelse(input$mod1 == "model_3", T,F)
                })
                
                
                output$model4 <- reactive({
                        ifelse(input$mod1 == "model_4", T,F)
                })
                
                output$model5 <- reactive({
                        ifelse(input$mod1 == "model_5", T,F)
                })
                
                outputOptions(output, "model1", suspendWhenHidden = FALSE)
                outputOptions(output, "model2", suspendWhenHidden = FALSE) 
                outputOptions(output, "model3", suspendWhenHidden = FALSE) 
                outputOptions(output, "model4", suspendWhenHidden = FALSE) 
                outputOptions(output, "model5", suspendWhenHidden = FALSE) 
                
                # This section is for load the model the user request and make the simulation, also, plot it 
                # Initial condition entered by the user
                s <- reactive({c(x = input$xint1, p = input$pint1, s = input$sint1)})
                
                # Set parameters for the model
                rv <- reactiveValues(p = vector(mode = "numeric"))
                
                observe({
                
                if (input$mod1 == "model_1") { 
                        
                        rv$p <- c(vmax = input$num11, ks = input$num12, yxs = input$num13, ypx = input$num14)
                }
                else if (input$mod1 == "model_2") { 
                        
                        rv$p <- c(vmax = input$num21, ks = input$num22, yxs = input$num23, ypx = input$num24, kp = input$num25)
                }
                else if (input$mod1 == "model_3") { 
                        
                        rv$p <- c(vmax = input$num31, ks = input$num32, yxs = input$num33, alfa = input$num34, beta = input$num35)
                }
                else if (input$mod1 == "model_4") { 
                        
                        rv$p <- c(vmax = input$num41, ks = input$num42, yxs = input$num43, ypx = input$num44, kd = input$num45)
                }
                else { 
                        
                        rv$p <- c(vmax = input$num51, ks = input$num52, yxs = input$num53, ypx = input$num54, km = input$num55)
                        
                        }
               
                })
                
                output$plot1 <- renderPlot({
                        
                        if (input$mod1 == "model_1") {
                                
                                source("model1.R")
                                
                                
                        }
                        
                        else if(input$mod1 == "model_2") {
                                
                                source("model2.R")
                        }
                        
                        else if(input$mod1 == "model_3") {
                                
                                source("model3.R") 
                        }
                        
                        else if(input$mod1 == "model_4") {
                                
                                source("model4.R")  
                        }
                        
                        else {
                                
                                source("model5.R")  
                        }
                        
                        
                        
                        simMod(s(), rv$p)
                })
                
                
                # This section is for show the mathematic model the user request
                output$ex1 <- renderUI({
                        if (input$mod1 == "model_1") {
                                withMathJax(
                                        helpText('Model 1 (Monod without inhibition by product): $$\\frac{dx}{dt} = v_{max}*\\frac{s}{k_{s}+s}*x$$'),
                                        helpText('$$\\frac{ds}{dt} = \\left(-\\frac{1}{y_{xs}}\\right)*v_{max}*\\frac{s}{k_{s}+s}*x$$'),
                                        helpText('$$\\frac{dp}{dt} = y_{px}* v_{max}*\\frac{s}{k_{s}+s}*x$$')
                                )
                        }
                        else if (input$mod1 == "model_2") {
                                withMathJax(
                                        helpText('Model 2 (Monod with inhibition by product): $$\\frac{dx}{dt} = v_{max}*\\frac{s}{k_{s}+s}*\\frac{k_{p}}{k_{p}+p}*x$$'),
                                        helpText('$$\\frac{ds}{dt} = \\left(-\\frac{1}{y_{xs}}\\right)*v_{max}*\\frac{s}{k_{s}+s}*\\frac{k_{p}}{k_{p}+p}*x$$'),
                                        helpText('$$\\frac{dp}{dt} = y_{px}*v_{max}*\\frac{s}{k_{s}+s}*\\frac{k_{p}}{k_{p}+p}*x$$')
                                )
                        }
                        else if (input$mod1 == "model_3") {
                                withMathJax(
                                        helpText('Model 3 (Monod with product partially linked to growth): $$\\frac{dx}{dt} = v_{max}*\\frac{s}{k_{s}+s}*x$$'),
                                        helpText('$$\\frac{ds}{dt} = \\left(-\\frac{1}{y_{xs}}\\right)*v_{max}*\\frac{s}{k_{s}+s}*x$$'),
                                        helpText('$$\\frac{dp}{dt} = \\beta*v_{max}*\\frac{s}{k_{s}+s}*x + \\alpha*x$$')
                                )
                        }
                        else if (input$mod1 == "model_4") {
                                withMathJax(
                                        helpText('Model 4 (Monod with cell death): $$\\frac{dx}{dt} = v_{max}*\\frac{s}{k_{s}+s}*x - k_{d}*x$$'),
                                        helpText('$$\\frac{ds}{dt} = \\left(-\\frac{1}{y_{xs}}\\right)*v_{max}*\\frac{s}{k_{s}+s}*x$$'),
                                        helpText('$$\\frac{dp}{dt} = y_{px}* v_{max}*\\frac{s}{k_{s}+s}*x$$')
                                )
                        }
                        else {
                                withMathJax(
                                        helpText('Model 5 (Monod with sustrate consumption for maintenance): $$\\frac{dx}{dt} = v_{max}*\\frac{s}{k_{s}+s}*x$$'),
                                        helpText('$$\\frac{ds}{dt} = \\left(-\\frac{1}{y_{xs}}\\right)*v_{max}*\\frac{s}{k_{s}+s}*x - k_{m}*x$$'),
                                        helpText('$$\\frac{dp}{dt} = y_{px}* v_{max}*\\frac{s}{k_{s}+s}*x$$')
                                )
                        }
                        
                })
                #########################################################################################
                ### Optimization section ################################################################
                # This section is for load the data and show it in a table
                
                df <- reactive({
                        read.xlsx(input$file2$datapath, header = TRUE, sheetIndex = as.numeric(input$sheet))
                })
                
                output$content2 <- renderTable({
                        
                        req(input$file2)
                        
                        if(input$check2) {
                                return(head(df()))
                        }
                        else {
                                return(df())
                        }
                        
                })
                
                output$plot2 <- renderPlot({
                        req(input$file2)
                        
                        showPlot(df())
                })
                
                
}



shinyApp(ui, server)
                                           
                                           
