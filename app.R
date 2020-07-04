# Load libraries #
library(shiny)
library(shinydashboard)
library(markdown)
library(xlsx)
library(shinybusy)
library(shinythemes)
library(deSolve)
library(GA)
library(FME)
library(tidyverse)

# Load function.R script
source("functions.R")

ui = navbarPage("Fermentation Analysis", theme = shinytheme("spacelab"),
                
############## Simulation section ##########################################################################                        
                        tabPanel("Simulation",
                                 
                                 withMathJax(), # For the LaTex code
                                 
                                 add_busy_spinner(spin = "fading-circle"),
                                 
                                 sidebarLayout(
                                         
                                         sidebarPanel(
                                                 
                                                 selectInput(inputId = "mod_int_sim",label = "Choose a model",
                                                             choices = list("Model 1 (Monod without inhibition by product)"="model_1",
                                                                            "Model 2 (Monod with inhibition by product)" = "model_2",
                                                                            "Model 3 (Monod with product partially linked to growth)"="model_3",
                                                                            "Model 4 (Monod with cell death)"="model_4",
                                                                            "Model 5 (Monod with sustrate consumption for maintenance)"="model_5",
                                                                            "Model 6 (Monod with inhibition by product and product partially linked to growth)"="model_6"),
                                                             selected = "model_1"),
                                                 
                                                 
                                                 h5("Enter Initial Conditions"),
                                                 
                                                 numericInput("x_int_sim",helpText('$$X_{0}$$'),0.2),
                                                 numericInput("s_int_sim",helpText('$$S_{0}$$'),40),
                                                 numericInput("p_int_sim",helpText('$$P_{0}$$'),0),
                                                 
                                                 hr(),
                                                 
                                                 
                                                 conditionalPanel(condition = "output.model1_rec_sim",
                                                                  
                                                                   h5("Model Parameters"),
                                                                  
                                                                   fluidRow(
                                                                           
                                                                           column(width = 6,
                                                                                  sliderInput(inputId = "vmax_mod1_int_sim",label = helpText('$$v_{max}$$'),min = 0.01,max = 2,value = 0.5,step = 0.1),
                                                                                  sliderInput(inputId = "ks_mod1_int_sim",label = helpText('$$K_{s}$$'),min = 1,max = 200,value = 80,step = 1)
                                                                           ),
                                                                           column(width = 6,
                                                                                  sliderInput(inputId = "yxs_mod1_int_sim",label = helpText('$$Y_{xs}$$'),min = 0.01,max = 1.5,value = 0.8,step = 0.1),
                                                                                  sliderInput(inputId = "ypx_mod1_int_sim",label = helpText('$$Y_{px}$$'),min = 1,max = 20,value = 12,step = 1))
                                                                   )),
                                                 
                                                 conditionalPanel(condition = "output.model2_rec_sim",
                                                                  
                                                                  h5("Model Parameters"),
                                                                  
                                                                   fluidRow(
                                                                           
                                                                           column(width = 6,
                                                                                  sliderInput(inputId = "vmax_mod2_int_sim",label = helpText('$$v_{max}$$'),min = 0.01,max = 2,value = 0.5,step = 0.1),
                                                                                  sliderInput(inputId = "ks_mod2_int_sim",label = helpText('$$K_{s}$$'),min = 1,max = 200,value = 80,step = 1),
                                                                                  sliderInput(inputId = "yxs_mod2_int_sim",label = helpText('$$Y_{xs}$$'),min = 0.01,max = 1.5,value = 0.8,step = 0.1)
                                                                           ),
                                                                           column(width = 6,
                                                                                  sliderInput(inputId = "ypx_mod2_int_sim",label = helpText('$$Y_{px}$$'),min = 1,max = 20,value = 12,step = 1),
                                                                                  sliderInput(inputId = "kp_mod2_int_sim",label = helpText('$$K_{p}$$'),min = 1,max = 200,value = 180,step = 1))
                                                                   )
                                                 ),
                                                 
                                                 conditionalPanel(condition = "output.model3_rec_sim",
                                                                  
                                                                   h5("Model Parameters"),
                                                                  
                                                                   fluidRow(
                                                                           
                                                                           column(width = 6,
                                                                                  sliderInput(inputId = "vmax_mod3_int_sim",label = helpText('$$v_{max}$$'),min = 0.01,max = 2,value = 0.5,step = 0.1),
                                                                                  sliderInput(inputId = "ks_mod3_int_sim",label = helpText('$$K_{s}$$'),min = 1,max = 200,value = 80,step = 1),
                                                                                  sliderInput(inputId = "yxs_mod3_int_sim",label = helpText('$$Y_{xs}$$'),min = 0.01,max = 1.5,value = 0.8,step = 0.1)
                                                                           ),
                                                                           column(width = 6,
                                                                                  sliderInput(inputId = "alpha_mod3_int_sim",label = helpText('\\(\\alpha\\)'),min = 1,max = 20,value = 12,step = 1),
                                                                                  sliderInput(inputId = "beta_mod3_int_sim",label = helpText('\\(\\beta\\)'),min = 0.01,max = 0.5,value = 0.1,step = 0.01))
                                                                   )
                                                 ),
                                                 
                                                 conditionalPanel(condition = "output.model4_rec_sim",
                                                                  
                                                                   h5("Model Parameters"),
                                                                  
                                                                   fluidRow(
                                                                           
                                                                           column(width = 6,
                                                                                  sliderInput(inputId = "vmax_mod4_int_sim",label = helpText('$$v_{max}$$'),min = 0.01,max = 2,value = 0.5,step = 0.1),
                                                                                  sliderInput(inputId = "ks_mod4_int_sim",label = helpText('$$K_{s}$$'),min = 1,max = 200,value = 80,step = 1),
                                                                                  sliderInput(inputId = "yxs_mod4_int_sim",label = helpText('$$Y_{xs}$$'),min = 0.01,max = 1.5,value = 0.8,step = 0.1)
                                                                           ),
                                                                           column(width = 6,
                                                                                  sliderInput(inputId = "ypx_mod4_int_sim",label = helpText('$$Y_{px}$$'),min = 1,max = 20,value = 12,step = 1),
                                                                                  sliderInput(inputId = "kd_mod4_int_sim",label = helpText('$$k_{d}$$'),min = 0.001,max = 1,value = 0.01,step = 0.001))
                                                                   )
                                                ),
                                                 
                                                conditionalPanel(condition = "output.model5_rec_sim",
                                                                 
                                                                  h5("Model Parameters"),
                                                                 
                                                                  fluidRow(
                                                                          
                                                                          column(width = 6,
                                                                                 sliderInput(inputId = "vmax_mod5_int_sim",label = helpText('$$v_{max}$$'),min = 0.01,max = 2,value = 0.5,step = 0.1),
                                                                                 sliderInput(inputId = "ks_mod5_int_sim",label = helpText('$$K_{s}$$'),min = 1,max = 200,value = 80,step = 1),
                                                                                 sliderInput(inputId = "yxs_mod5_int_sim",label = helpText('$$Y_{xs}$$'),min = 0.01,max = 1.5,value = 0.8,step = 0.1)
                                                                          ),
                                                                          column(width = 6,
                                                                                 sliderInput(inputId = "ypx_mod5_int_sim",label = helpText('$$Y_{px}$$'),min = 1,max = 20,value = 12,step = 1),
                                                                                 sliderInput(inputId = "km_mod5_int_sim",label = helpText('$$k_{m}$$'),min = 0.01,max = 0.1,value = 0.05,step = 0.01))
                                                                  )
                                                ),
                                                
                                                conditionalPanel(condition = "output.model6_rec_sim",
                                                                 
                                                                 h5("Model Parameters"),
                                                                 
                                                                 fluidRow(
                                                                         
                                                                         column(width = 6,
                                                                                sliderInput(inputId = "vmax_mod6_int_sim",label = helpText('$$v_{max}$$'),min = 0.01,max = 2,value = 0.5,step = 0.1),
                                                                                sliderInput(inputId = "ks_mod6_int_sim",label = helpText('$$K_{s}$$'),min = 1,max = 200,value = 80,step = 1),
                                                                                sliderInput(inputId = "yxs_mod6_int_sim",label = helpText('$$Y_{xs}$$'),min = 0.01,max = 1.5,value = 0.8,step = 0.1)
                                                                         ),
                                                                         column(width = 6,
                                                                                sliderInput(inputId = "kp_mod6_int_sim",label = helpText('$$K_{p}$$'),min = 1,max = 200,value = 180,step = 1),
                                                                                sliderInput(inputId = "alpha_mod6_int_sim",label = helpText('\\(\\alpha\\)'),min = 1,max = 20,value = 12,step = 1),
                                                                                sliderInput(inputId = "beta_mod6_int_sim",label = helpText('\\(\\beta\\)'),min = 0.01,max = 0.5,value = 0.1,step = 0.01))
                                                                 )
                                                )
                                                 
                                                 
                                                 
                                         ),
                                         
                                         mainPanel(
                                                 h3("Scatter Plot Simulation"),
                                                 plotOutput("plot_result_out_sim"),
                                                 downloadButton("down_plot_result_out_sim", "Download Plot"),
                                                 hr(),
                                                 fluidRow(
                                                         column(4,offset = 1,
                                                                uiOutput('ui_out_sim')
                                                                ),
                                                         column(4,offset = 1,
                                                                h5("Process Network",style = "color:gray"),
                                                                imageOutput("network_out_sim"))
                                                 )
                                                 
                                         )
                                         
                                 )
                                 
                        ),

#############################################################################################################
############## Optimization section ##########################################################################                        
                        tabPanel("Optimization",
                                 
                                 add_busy_spinner(spin = "fading-circle"),
                                 
                                 sidebarLayout(
                                         
                                         sidebarPanel(
                                                 
                                                 fileInput(inputId = "data_int_opt",label = "Enter the data", multiple = FALSE,
                                                           accept = c(".xlsx")),
                                                
                                                 checkboxInput(inputId = "header_int_opt",label = "Head", value = TRUE),
                                                 
                                                 selectInput(inputId = "sheet_int_opt",label = "Select sheet",
                                                             choices = list("1","2","3","4","5"),selected = "1"),
                                                 
                                                 selectInput(inputId = "mod_int_opt",label = "Choose a model",
                                                             choices = list("Model 1"="model_1","Model 2"="model_2",
                                                                            "Model 3"="model_3","Model 4"="model_4",
                                                                            "Model 5"="model_5"),
                                                             selected = "model_1"),
                                                 
                                                 actionButton("make_opt_int_opt","Make optimization")
                                                 
                                         ),
                                         
                                         mainPanel(
                                                 
                                                 fluidRow(
                                                         column(3,
                                                                h3("Data"),
                                                                tableOutput("table_data_out_opt")
                                                         ),
                                                         column(8,offset = 1,
                                                                h3("Scatter Plot Data"),
                                                                plotOutput("plot_data_out_opt"),
                                                                downloadButton("down_plot_data_out_opt", "Download Plot"))
                                                 ),
                                                 fluidRow(
                                                         column(3,
                                                                h3("Optimized Parameters"),
                                                                tableOutput("table_result_out_opt"),
                                                                downloadButton("down_table_result_out_opt", "Download Results")
                                                         ),
                                                         column(8, offset = 1,
                                                                h3("Scatter Plot Results"),
                                                                plotOutput("plot_result_out_opt"),
                                                                downloadButton("down_plot_result_out_opt", "Download Plot"))  
                                                 )
                                                 
                                         )
                                 )
                                 
                        ),
                        #######################################################################################################################
                        ##### Statistic analysis section #####################################################################################
                        navbarMenu("Statistic Analysis",
                                   
                                   
                                   ##### t-student section ######
                                   tabPanel("t-student",
                                            
                                            add_busy_spinner(spin = "fading-circle"),
                                            
                                            h3("t-student analysis"),
                                            
                                            sidebarLayout(
                                                    
                                                    sidebarPanel(
                                                            
                                                            fileInput(inputId = "data_int_tst",label = "Enter the data",multiple = TRUE),
                                                            
                                                            checkboxInput(inputId = "header_int_tst",label = "Head", value = TRUE),
                                                            
                                                            selectInput(inputId = "sheet_int_tst",label = "Select sheet",
                                                                        choices = list("1","2","3","4"),selected = "1"),
                                                            
                                                            actionButton("make_tst_int_tst", "Make t-test")
                                                            
                                                    ),
                                                    
                                                    mainPanel(
                                                            
                                                            fluidRow(
                                                                    
                                                                    column(3,
                                                                           h3("Data"),
                                                                           tableOutput("table_data_out_tst")
                                                                    ),
                                                                    column(8, offset = 1,
                                                                           h3("Boxplot"),
                                                                           plotOutput("plot_data_out_tst"),
                                                                           downloadButton("down_plot_data_out_tst")
                                                                           )
                                                            ),
                                                            br(),
                                                            fluidRow(
                                                                    column(4,
                                                                           h3("Results"),
                                                                           tableOutput("table_result_out_tst"),
                                                                           downloadButton("down_table_result_out_tst"))
                                                            )
                                                
                                                    )
                                            )
                                            
                                   ),
                                   
                                   ##########################################################################################
                                   ###### ANOVA section #####################################################################
                                   tabPanel("ANOVA",
                                            
                                            add_busy_spinner(spin = "fading-circle"),
                                            
                                            h3("ANOVA analysis"),
                                            
                                            sidebarLayout(
                                                    
                                                    sidebarPanel(
                                                            
                                                            fileInput(inputId = "data_int_anova",label = "Enter the data",multiple = TRUE),
                                                            
                                                            checkboxInput(inputId = "header_int_anova",label = "Head",value = TRUE),
                                                            
                                                            selectInput(inputId = "sheet_int_anova", label = "Select sheet",
                                                                        choices = list("1","2","3","4","5"),selected = "1"),
                                                            
                                                            actionButton("make_anova_int_anova", "Make ANOVA"),
                                                            
                                                            br(),
                                                            br(),
                                                            
                                                            actionButton("make_tukey_int_tukey", "Make Tukey test")
                                                            
                                                            
                                                    ),
                                                    
                                                    mainPanel(
                                                    
                                                            fluidRow(
                                                                    column(3,
                                                                           h3("Data"),
                                                                           tableOutput("table_data_out_anova"),
                                                                           br(),
                                                                           hr(),
                                                                           h3("Means"),
                                                                           tableOutput("table_means_out_anova")
                                                                    ),
                                                                    column(8, offset = 1,
                                                                           h3("Boxplot"),
                                                                           plotOutput("plot_data_out_anova"),
                                                                           downloadButton("down_plot_data_out_anova", "Download Plot")
                                                                    )
                                                            ),
                                                            
                                                            fluidRow(
                                                                    
                                                                    column(6,
                                                                           h3("Results ANOVA"),
                                                                           tableOutput("table_result_out_anova"),
                                                                           downloadButton("down_table_result_out_anova", "Download Table")
                                                                    ),
                                                                    column(6,
                                                                           h3("Results Tukey"),
                                                                           tableOutput("table_result_out_tukey"),
                                                                           downloadButton("down_table_result_out_tukey", "Download Table")
                                                                    )
                                                                    
                                                            )
                                                            
                                                            )
                                                    
                                            )
                                   ),
                                   #######################################################################################################
                                   ##### Regression analysis section #####################################################################
                                   tabPanel("Regression Analysis",
                                            
                                            add_busy_spinner(spin = "fading-circle"),
                                            
                                            h3("Regression Analysis"),
                                   
                                                     sidebarLayout(
                                            
                                                                     sidebarPanel(with = 4,
                                                            
                                                                             fileInput(inputId = "data_int_lr",label = "Enter the data",
                                                                                       multiple = TRUE),
                                                            
                                                                             checkboxInput(inputId = "header_int_lr",label = "Head",
                                                                                           value = TRUE),
                                                                            
                                                                             selectInput(inputId = "sheet_int_lr","Choose a sheet",
                                                                                         list("1","2","3","4","5","6","7")),
                                                                             
                                                                             
                                                                             conditionalPanel(condition = "output.myoutUI",
                                                                                               
                                                                                               uiOutput("more_controls")
                                                                                               
                                                                             ),
                                                                             
                                                                             actionButton(inputId = "make_plot_int_lr","Show plot"),
                                                                             
                                                                             actionButton("make_lr_int_lr","Make Simple Linear Regression"),
                                                                             
                                                                             checkboxInput(inputId = "select_mlr_int_lr",label = "Multiple regression",
                                                                                           value = F),
                                                                             
                                                                             conditionalPanel(condition = "output.myoutUI2",
                                                                                               
                                                                                               uiOutput("more_controls2")
                                                                                               
                                                                             ),
                                                                             
                                                                             actionButton("make_mlr_int_lr", "Make Multiple Linear Regression")
                                                                             
                                                    ),
                                                    
                                                    mainPanel(
                                                            
                                                            fluidRow(
                                                                    
                                                                    column(3,
                                                                           h3("Data"),
                                                                           tableOutput("table_data_out_lr")
                                                                           
                                                                           ),
                                                                    column(8, offset = 1,
                                                                           h3("Scatter Plot"),
                                                                           plotOutput("plot_data_out_lr"))
                                                            
                                                                    ),
                                                            
                                                            fluidRow(
                                                                    
                                                                    column(4,
                                                                           h3("Results: Simple Linear Regression"),
                                                                           tableOutput("table_result_out_lr")),
                                                                    column(4,
                                                                           h3("Results: Multiple Linear Regression"),
                                                                           tableOutput("table_result_out_mlr"))
                                                            )
                                                    
                                                    )
                                                    
                                                    
                                            )
                                            
                                   )
                                   
                        )
                        
)
###########################################################################################################
###########################################################################################################
###########################################################################################################
##### server ##############################################################################################      
server = function(input, output, session) {
        
                # Simulation section ########################################################
        
                # Select the panel with the parameters of the model #
                output$model1_rec_sim <- reactive({
                        ifelse(input$mod_int_sim == "model_1", T,F)
                })
                
                output$model2_rec_sim <- reactive({
                        ifelse(input$mod_int_sim == "model_2", T,F)
                })
                
                output$model3_rec_sim <- reactive({
                        ifelse(input$mod_int_sim == "model_3", T,F)
                })
                
                
                output$model4_rec_sim <- reactive({
                        ifelse(input$mod_int_sim == "model_4", T,F)
                })
                
                output$model5_rec_sim <- reactive({
                        ifelse(input$mod_int_sim == "model_5", T,F)
                })
                
                output$model6_rec_sim <- reactive({
                        ifelse(input$mod_int_sim == "model_6", T,F)
                })
                
                outputOptions(output, "model1_rec_sim", suspendWhenHidden = FALSE)
                outputOptions(output, "model2_rec_sim", suspendWhenHidden = FALSE) 
                outputOptions(output, "model3_rec_sim", suspendWhenHidden = FALSE) 
                outputOptions(output, "model4_rec_sim", suspendWhenHidden = FALSE) 
                outputOptions(output, "model5_rec_sim", suspendWhenHidden = FALSE) 
                outputOptions(output, "model6_rec_sim", suspendWhenHidden = FALSE) 
                
                
                # Set intitial condition  
                s_rec_sim <- reactive({c(x = input$x_int_sim, p = input$p_int_sim, s = input$s_int_sim)})
                
                # Set parameters
                p_rec_sim <- reactiveValues(p = vector(mode = "numeric"))
                
                observe({
                
                if (input$mod_int_sim == "model_1") { 
                        
                        p_rec_sim$p <- c(Vmax = input$vmax_mod1_int_sim, Ks = input$ks_mod1_int_sim, 
                                  Yxs = input$yxs_mod1_int_sim, Ypx = input$ypx_mod1_int_sim)
                }
                        
                else if (input$mod_int_sim == "model_2") { 
                        
                        p_rec_sim$p <- c(Vmax = input$vmax_mod2_int_sim, Ks = input$ks_mod2_int_sim,
                                  Yxs = input$yxs_mod2_int_sim, Ypx = input$ypx_mod2_int_sim,
                                  Kp = input$kp_mod2_int_sim)
                }
                        
                else if (input$mod_int_sim == "model_3") { 
                        
                        p_rec_sim$p <- c(Vmax = input$vmax_mod3_int_sim, Ks = input$ks_mod3_int_sim, 
                                  Yxs = input$yxs_mod3_int_sim, alpha = input$alpha_mod3_int_sim,
                                  beta = input$beta_mod3_int_sim)
                }
                        
                else if (input$mod_int_sim == "model_4") { 
                        
                        p_rec_sim$p <- c(Vmax = input$vmax_mod4_int_sim, Ks = input$ks_mod4_int_sim,
                                  Yxs = input$yxs_mod4_int_sim, Ypx = input$ypx_mod4_int_sim,
                                  Kd = input$kd_mod4_int_sim)
                }
                        
                else if (input$mod_int_sim == "model_5"){ 
                        
                        p_rec_sim$p <- c(Vmax = input$vmax_mod5_int_sim, Ks = input$ks_mod5_int_sim,
                                  Yxs = input$yxs_mod5_int_sim, Ypx = input$ypx_mod5_int_sim,
                                  Km = input$km_mod5_int_sim)
                        
                }
                        
                else {         
                        p_rec_sim$p <- c(Vmax = input$vmax_mod6_int_sim, Ks = input$ks_mod6_int_sim, 
                                 Yxs = input$yxs_mod6_int_sim, alpha = input$alpha_mod6_int_sim, 
                                 Kp = input$kp_mod6_int_sim, beta = input$beta_mod6_int_sim)
                                
                }        
                        
               
                })
                
                # Load the model #
                observe({
                        
                        if (input$mod_int_sim == "model_1") {
                                
                                source("model1.R")
                                
                        }
                        
                        else if(input$mod_int_sim == "model_2") {
                                
                                source("model2.R")
                        }
                        
                        else if(input$mod_int_sim == "model_3") {
                                
                                source("model3.R") 
                        }
                        
                        else if(input$mod_int_sim == "model_4") {
                                
                                source("model4.R")  
                        }
                        
                        else if(input$mod_int_sim == "model_5") {
                                
                                source("model5.R")  
                        }
                        
                        else {
                                
                                source("model6.R")  
                        }
                        
                     
                })
                
                output$plot_result_out_sim <- renderPlot({
                        
                        make_simulation_fun_sim(s_rec_sim(), p_rec_sim$p)
                })
                
                output$down_plot_result_out_sim <- downloadHandler(
                        filename = function() {
                                paste("data-", ".png", sep="")
                        },
                        content = function(file) {
                                
                                ggsave(file,make_simulation_fun_sim(s_rec_sim(), p_rec_sim$p),
                                       width = 10, height = 8)
                        }
                )
                
                # Show mathematical model #
                output$ui_out_sim <- renderUI({
                        if (input$mod_int_sim == "model_1") {
                                withMathJax(
                                        helpText('Model 1 (Monod without inhibition by product): $$\\frac{dx}{dt} = v_{max}*\\frac{s}{k_{s}+s}*x$$'),
                                        helpText('$$\\frac{ds}{dt} = \\left(-\\frac{1}{y_{xs}}\\right)*v_{max}*\\frac{s}{k_{s}+s}*x$$'),
                                        helpText('$$\\frac{dp}{dt} = y_{px}* v_{max}*\\frac{s}{k_{s}+s}*x$$')
                                )
                        }
                        else if (input$mod_int_sim == "model_2") {
                                withMathJax(
                                        helpText('Model 2 (Monod with inhibition by product): $$\\frac{dx}{dt} = v_{max}*\\frac{s}{k_{s}+s}*\\frac{k_{p}}{k_{p}+p}*x$$'),
                                        helpText('$$\\frac{ds}{dt} = \\left(-\\frac{1}{y_{xs}}\\right)*v_{max}*\\frac{s}{k_{s}+s}*\\frac{k_{p}}{k_{p}+p}*x$$'),
                                        helpText('$$\\frac{dp}{dt} = y_{px}*v_{max}*\\frac{s}{k_{s}+s}*\\frac{k_{p}}{k_{p}+p}*x$$')
                                )
                        }
                        else if (input$mod_int_sim == "model_3") {
                                withMathJax(
                                        helpText('Model 3 (Monod with product partially linked to growth): $$\\frac{dx}{dt} = v_{max}*\\frac{s}{k_{s}+s}*x$$'),
                                        helpText('$$\\frac{ds}{dt} = \\left(-\\frac{1}{y_{xs}}\\right)*v_{max}*\\frac{s}{k_{s}+s}*x$$'),
                                        helpText('$$\\frac{dp}{dt} = \\alpha*v_{max}*\\frac{s}{k_{s}+s}*x + \\beta*x$$')
                                )
                        }
                        else if (input$mod_int_sim == "model_4") {
                                withMathJax(
                                        helpText('Model 4 (Monod with cell death): $$\\frac{dx}{dt} = v_{max}*\\frac{s}{k_{s}+s}*x - k_{d}*x$$'),
                                        helpText('$$\\frac{ds}{dt} = \\left(-\\frac{1}{y_{xs}}\\right)*v_{max}*\\frac{s}{k_{s}+s}*x$$'),
                                        helpText('$$\\frac{dp}{dt} = y_{px}* v_{max}*\\frac{s}{k_{s}+s}*x$$')
                                )
                        }
                        else if (input$mod_int_sim == "model_5") {
                                withMathJax(
                                        helpText('Model 5 (Monod with sustrate consumption for maintenance): $$\\frac{dx}{dt} = v_{max}*\\frac{s}{k_{s}+s}*x$$'),
                                        helpText('$$\\frac{ds}{dt} = \\left(-\\frac{1}{y_{xs}}\\right)*v_{max}*\\frac{s}{k_{s}+s}*x - k_{m}*x$$'),
                                        helpText('$$\\frac{dp}{dt} = y_{px}* v_{max}*\\frac{s}{k_{s}+s}*x$$')
                                )
                        }
                        
                        else {
                                withMathJax(
                                        helpText('Model 6 (with inhibition by product and product partially linked to growt):  $$\\frac{dx}{dt} = v_{max}*\\frac{s}{k_{s}+s}*\\frac{k_{p}}{k_{p}+p}*x$$'),
                                        helpText('$$\\frac{ds}{dt} = \\left(-\\frac{1}{y_{xs}}\\right)*v_{max}*\\frac{s}{k_{s}+s}*x - k_{m}*x$$'),
                                        helpText('$$\\frac{dp}{dt} = \\alpha*v_{max}*\\frac{s}{k_{s}+s}*x + \\beta*x$$')
                                )
                        }
                })
                
                # Show network
                output$network_out_sim <- renderImage({
                        if (is.null(input$mod_int_sim))
                                return(NULL)
                        
                        if (input$mod_int_sim == "model_1") {
                                return(list(
                                        src = "www/network1.png",
                                        contentType = "image/png",
                                        alt = "Network",height = 180,width = 320
                                ))
                        } else if (input$mod_int_sim == "model_2") {
                                return(list(
                                        src = "www/network2.png",
                                        contentType = "image/png",
                                        alt = "Network",height = 175,width = 300
                                ))
                        } else if (input$mod_int_sim == "model_3") {
                                return(list(
                                        src = "www/network3.png",
                                        contentType = "image/png",
                                        alt = "Network",height = 220,width = 380
                                ))
                        } else if (input$mod_int_sim == "model_4") {
                                return(list(
                                        src = "www/network4.png",
                                        contentType = "image/png",
                                        alt = "Network",height = 250,width = 290
                                ))
                        } else if (input$mod_int_sim == "model_5") {
                                return(list(
                                        src = "www/network5.png",
                                        contentType = "image/png",
                                        alt = "Network",height = 300,width = 290
                                ))
                        } else {
                                return(list(
                                        src = "www/network6.png",
                                        contentType = "image/png",
                                        alt = "Network",height = 220,width = 380
                                ))
                        }
                        
                }, deleteFile = FALSE)
                
                #########################################################################################
                ### Optimization section ################################################################
                
                # Load the data #
                df_rec_opt <- reactive({
                        read.xlsx(input$data_int_opt$datapath, header = TRUE, 
                                  sheetIndex = as.numeric(input$sheet_int_opt))
                })
                
                # Show data #
                output$table_data_out_opt <- renderTable({
                        
                        req(input$data_int_opt)
                        
                        if(input$header_int_opt) {
                                return(head(df_rec_opt(),8))
                        }
                        else {
                                return(df_rec_opt())
                        }
                        
                })
                
                # Plot the data #
                output$plot_data_out_opt <- renderPlot({
                        
                        req(input$data_int_opt)
                        
                        plot_data_fun_opt(df_rec_opt())
                        
                        
                })
                
                output$down_plot_data_out_opt <- downloadHandler(
                        
                        filename = function() {
                                
                                paste("data_optimization", ".png", sep="")
                        },
                        
                        content = function(file) {
                                
                                ggsave(file,plot_data_fun_opt(df_rec_opt()),
                                       width = 10, height = 8)
                        }
                )
                
                
                # Get the optimized parameters #
                opt_parms_rec_opt <- eventReactive(input$make_opt_int_opt, {
                        
                        if (input$mod_int_opt == "model_1") {
                                
                                source("model1.R")
                                
                        }
                        
                        else if(input$mod_int_opt == "model_2") {
                                
                                source("model2.R")
                        }
                        
                        else if(input$mod_int_opt == "model_3") {
                                
                                source("model3.R") 
                        }
                        
                        else if(input$mod_int_opt == "model_4") {
                                
                                source("model4.R")  
                        }
                        
                        else {
                                
                                source("model5.R")  
                        }
                        
                        get_parms_fun_opt(df_rec_opt())
                })
                
                # Show optimized parameters #
                output$table_result_out_opt <- renderTable({
                        
                        data.frame("Parameter" = c(names(opt_parms_rec_opt()$optimized_parameters),"Fitness value"),
                                   "Results" = c(opt_parms_rec_opt()$optimized_parameters,
                                                 opt_parms_rec_opt()$fitness_value))
                        
                })
                
                output$down_table_result_out_opt <- downloadHandler(
                        
                        filename = function() {
                                
                                paste("result_optimization", ".csv", sep="")
                                
                        },
                        
                        content = function(file) {
                                
                                write.csv(data.frame("Parameter" = c(names(opt_parms_rec_opt()$optimized_parameters),"fitness_value"),
                                                 "Results" = c(opt_parms_rec_opt()$optimized_parameters,
                                                               opt_parms_rec_opt()$fitness_value)), file,row.names = F)
                                
                        }
                )
                
                
                # Plot results of optimization #
                output$plot_result_out_opt <- renderPlot({
                        
                        req(input$data_int_opt)
                        
                        comp_fun_opt(opt_parms_rec_opt()$optimized_parameters)
                })
                
                output$down_plot_result_out_opt <- downloadHandler(
                        filename = function() {
                                paste("result_optimization", ".png", sep="")
                        },
                        content = function(file) {
                                
                                ggsave(file,comp_fun_opt(opt_parms_rec_opt()$optimized_parameters),
                                       width = 10, height = 8)
                        }
                )
                
                
                #############################################################################################
                #### t-test section #########################################################################
                
                df_rec_tst <- reactive({
                        req(input$data_int_tst)
                        read.xlsx(input$data_int_tst$datapath, header = T, sheetIndex = as.numeric(input$sheet_int_tst))
                })
                
                # Show table with data
                output$table_data_out_tst <- renderTable({
                        req(input$data_int_tst)
                        
                        if(input$header_int_tst) {
                                return(head(df_rec_tst()))
                        }
                        else {
                                return(df_rec_tst())
                        }
                        
                })
                
                # Show boxplot
                output$plot_data_out_tst <- renderPlot({
                        
                        make_boxplot_fun_sta(df_rec_tst())
                })
                
                output$down_plot_data_out_tst <- downloadHandler(
                        
                        filename = function() {
                                
                                paste("data_plot",".png",sep = "")
                        },
                        
                        content = function(file) {
                          ggsave(file, make_boxplot_fun_sta(df_rec_tst()), width = 10,height = 8)      
                        })
                
                # Make t-test
                tst_rec_tst <- eventReactive(input$make_tst_int_tst,{
                        
                        make_sts_fun_sts(df_rec_tst())
                })
                
                # Show resutls
                output$table_result_out_tst <- renderTable({
                        
                        tst_rec_tst()
                })
                
                output$down_table_result_out_tst <- downloadHandler(
                        
                        filename = function() {
                                
                                paste("data_result",".csv",sep = "")
                        },
                        
                        content = function(file) {
                                
                                write.csv(tst_rec_tst(),file, row.names = F)
                        }
                )
                
                #############################################################################################################
                ### ANOVA and Tukey section #################################################################################
                
                # Load data
                df_rec_anova <- reactive({
                        req(input$data_int_anova)
                        read.xlsx(input$data_int_anova$datapath, header = TRUE, sheetIndex = as.numeric(input$sheet_int_anova))
                })
                
                # Show table with data
                output$table_data_out_anova <- renderTable({
                        
                        req(input$data_int_anova)
                        
                        if(input$header_int_anova) {
                                
                                return(head(df_rec_anova()))
                        }
                        
                        else {
                                
                                return(df_rec_anova())
                                
                        }
                        
                })
                
                # Show boxplot
                output$plot_data_out_anova <- renderPlot({
                        
                        make_boxplot_fun_sta(df_rec_anova())
                })
                
                output$down_plot_data_out_anova <- downloadHandler(
                        
                        filename = function() {
                                
                                paste("plot_data",".png",sep = "")
                        },
                        
                        content = function(file) {
                                
                                ggsave(file, make_boxplot_fun_sta(df_rec_anova()), width = 12, height = 10)
                        }
                )
                
                # Make ANOVA analysis
                anova_rec_anova <- eventReactive(input$make_anova_int_anova ,{
                        
                        make_anova_fun_anova(df_rec_anova())
                })

                # Show resutls
                output$table_result_out_anova <- renderTable({
                        
                        make_result_anova_fun_anova(anova_rec_anova())

                })
                
                output$down_table_result_out_anova <- downloadHandler(
                        
                        filename = function() {
                                
                                paste("result_anova",".csv",sep = "")
                        },
                        
                        content = function(file) {
                                
                                write.csv(make_result_anova_fun_anova(anova_rec_anova()), file, row.names = F)
                        }
                )
                
                # Make tukey test
                tukey_rec_tukey <- eventReactive(input$make_tukey_int_tukey ,{
                        
                        make_tukey_fun_tukey(df_rec_anova())
                })
                
                # Show resutls
                output$table_result_out_tukey <- renderTable({
                        
                        tukey_rec_tukey()
                })
                
                output$down_table_result_out_tukey <- downloadHandler(
                        
                        filename = function() {
                                paste("result_tukey",".csv",sep = "")
                        },
                        
                        content = function(file) {
                                
                                write.csv(tukey_rec_tukey(),file,row.names = F)
                        }
                )
                
                means_rec_anova <- reactive({
                        
                        make_means_fun_anova(df_rec_anova())
                })
                
                output$table_means_out_anova <- renderTable({
                        
                        means_rec_anova()
                })
                
       
                ###########################################################################################################
                ### Linear regression section #############################################################################
                
                # Load data
                df_rec_lr <- reactive({
                        
                        req(input$data_int_lr)
                        
                        read.xlsx(input$data_int_lr$datapath, header = TRUE, sheetIndex = as.numeric(input$sheet_int_lr))%>% 
                                
                                select_if(~sum(!is.na(.)) > 0)
                })
                
                
                # Get data column names
                colname_rec_lr <- reactive({
                        
                        req(input$data_int_lr)
                        
                        colnames(df_rec_lr())
                }) 
                
                # Create panel 
                output$more_controls <- renderUI({
                        
                        tagList(
                                
                                fluidRow(
                                        column(5,
                                               selectInput("varx", "Choose variable x", colname_rec_lr(),colname_rec_lr()[1])
                                        ),
                                        column(5,
                                               selectInput("vary", "Choose variable y", colname_rec_lr(),colname_rec_lr()[2])
                                        )
                                )
                        )
                })
                
                # Generate panel only if it is required
                output$myoutUI <- reactive({
                        
                        ifelse(class(df_rec_lr) =="data.frame" ,T,F)
                })
                
                outputOptions(output, "myoutUI", suspendWhenHidden = FALSE)
                
                # Create panel for multiple regression
                output$more_controls2 <- renderUI({
                        
                        tagList(
                                
                                fluidRow(
                                        column(5,
                                               checkboxGroupInput("var_ind", "Choose independent variables", colname_rec_lr())
                                        ),
                                        column(5,
                                               selectInput("var_dep", "Choose dependent variable", colname_rec_lr())
                                        )
                                )
                        )
                })
                
                # Generate panel only if it is required
                output$myoutUI2 <- reactive({
                        
                        ifelse(input$select_mlr_int_lr,T,F)
                })
                
                outputOptions(output, "myoutUI2", suspendWhenHidden = FALSE)
                
                # Show table with data
                output$table_data_out_lr <- renderTable({
                        
                        req(input$data_int_lr)
                        
                        if(input$header_int_lr) {
                                
                                return(head(df_rec_lr()))
                        }
                        
                        else {
                                
                                return(df_rec_lr())
                                
                        }
                        
                })
                
                # Make scatter plot
                plot_rec_lr <- eventReactive(input$make_plot_int_lr,{
                        
                        req(input$data_int_lr)
                        
                        make_scatter_fun_lr(df_rec_lr(),input$varx,input$vary)
                })
                
                output$plot_data_out_lr <- renderPlot({
                        
                        plot_rec_lr()
                }) 
                
                # Make linear regression analysis
                make_lr_rec_lr <- eventReactive(input$make_lr_int_lr ,{
                        
                        make_lr_fun_lr(df_rec_lr(),input$varx,input$vary)
                        
                })
                
                # Show resutls
                output$table_result_out_lr <- renderTable({
                        
                        make_lr_rec_lr()
                        
                })
                
                # Make linear regression analysis
                make_mlr_rec_lr <- eventReactive(input$make_mlr_int_lr ,{
                        
                        make_lr_fun_lr(df_rec_lr(),input$var_ind,input$var_dep)
                        
                })
                
                # Show resutls
                output$table_result_out_mlr <- renderTable({
                        
                        make_mlr_rec_lr()
                        
                })    
                
}



shinyApp(ui, server)

                                                   
