library(shiny)
library(shinydashboard)
library(markdown)
library(xlsx)
source("analysis_with_kinetic_models.R")

shinyApp(
        
        ui = navbarPage("Fermentation Analysis",
                        
                        tabPanel("Simulation",
                                 
                                 sidebarLayout(
                                         
                                         sidebarPanel(
                                                 
                                                 selectInput(inputId = "mod1",label = "Choose a model",
                                                             choices = list("model1","model2","model3","model4","model5"),
                                                             selected = "model5"),
                                                 actionButton("act1","Simulate")
                                                 
                                         ),
                                         
                                         mainPanel(
                                                 
                                                 plotOutput("plot1")
                                                 
                                         )
                                         
                                 )
                                 
                        ),
                        
                        tabPanel("Optimization",
                                 
                                 sidebarLayout(
                                         
                                         sidebarPanel(
                                                 
                                                 fileInput(inputId = "file2",label = "Enter the data", multiple = FALSE,
                                                           accept = c(".xlsx",".csv")),
                                                
                                                 checkboxInput(inputId = "check2",label = "Header", value = TRUE),
                                                 
                                                 selectInput(inputId = "select2",label = "Choose a format",
                                                             choices = list("xlsx","csv"),selected = "xlsx"),
                                                 
                                                 selectInput(inputId = "mod2",label = "Choose a model",
                                                             choices = list("model1","model2","model3","model4","model5"),
                                                             selected = "model5")
                                                 
                                         ),
                                         
                                         mainPanel(
                                                 
                                                 tableOutput("content2")
                                                 
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
                                                            
                                                            plotOutput("plot3")
                                                            
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
                        
        ),
        
        server = function(input, output, session) {
                
                
                plot1 <- eventReactive(input$act1,{
                        
                        if (input$mod1 == "model1") {
                                
                                source("model1.R")
                        }
                        
                        else if(input$mod1 == "model2") {
                                
                                source("model2.R")  
                        }
                        
                        else if(input$mod1 == "model3") {
                                
                                source("model3.R")  
                        }
                        
                        else if(input$mod1 == "model4") {
                                
                                source("model4.R")  
                        }
                        
                        else {
                                source("model5.R")  
                        }
                        
                        simMod()
                })
                
                
                
                output$content2 <- renderTable({
                        
                        # input$file1 will be NULL initially. After the user selects
                        # and uploads a file, head of that data file by default,
                        # or all rows if selected, will be shown.
                        
                        req(input$file2)
                        
                        df <- read.xlsx(input$file2$datapath, header = input$check2,sheetIndex = 1)
                        
                        if(input$check2 == "head") {
                                return(head(df))
                        }
                        else {
                                return(df)
                        }
                        
                })
                
                plot3 <- reactive({
                        
                        hist(rnorm(100,10,1))
                })
                
                output$plot1 <- renderPlot({plot1()})
                
                output$plot2 <- renderPlot({plot2()})
                
                output$plot3 <- renderPlot({plot3()})
                
        }
)
