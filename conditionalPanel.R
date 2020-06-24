ui <- fluidPage(
        
        selectInput("dataset", "Model", c("model1", "model2", "model3", "model4")),
        conditionalPanel( condition = "output.model1",
                          sliderInput(inputId = "num11",label = "vmax",min = 0,max = 2,value = 0.5,step = 0.1),
                          sliderInput(inputId = "num12",label = "ks",min = 0,max = 200,value = 80,step = 1),
                          sliderInput(inputId = "num13",label = "yxs",min = 0,max = 1,value = 0.5,step = 0.1),
                          sliderInput(inputId = "num14",label = "ypx",min = 0,max = 20,value = 12,step = 1)),
        conditionalPanel( condition = "output.model2",
                          sliderInput(inputId = "num21",label = "vmax",min = 0,max = 2,value = 0.5,step = 0.1),
                          sliderInput(inputId = "num22",label = "ks",min = 0,max = 200,value = 80,step = 1),
                          sliderInput(inputId = "num23",label = "yxs",min = 0,max = 1,value = 0.5,step = 0.1),
                          sliderInput(inputId = "num24",label = "ypx",min = 0,max = 20,value = 12,step = 1),
                          sliderInput(inputId = "num25",label = "kp",min = 0,max = 20,value = 12,step = 1)),
        conditionalPanel( condition = "output.model3",
                          sliderInput(inputId = "num31",label = "vmax",min = 0,max = 2,value = 0.5,step = 0.1),
                          sliderInput(inputId = "num32",label = "ks",min = 0,max = 200,value = 80,step = 1),
                          sliderInput(inputId = "num33",label = "yxs",min = 0,max = 1,value = 0.5,step = 0.1),
                          sliderInput(inputId = "num34",label = "alfa",min = 0,max = 20,value = 12,step = 1),
                          sliderInput(inputId = "num35",label = "beta",min = 0,max = 0.5,value = 0.25,step = 0.01)),
        conditionalPanel( condition = "output.model4",
                          sliderInput(inputId = "num41",label = "vmax",min = 0,max = 2,value = 0.5,step = 0.1),
                          sliderInput(inputId = "num42",label = "ks",min = 0,max = 200,value = 80,step = 1),
                          sliderInput(inputId = "num43",label = "yxs",min = 0,max = 1,value = 0.5,step = 0.1),
                          sliderInput(inputId = "num44",label = "ypx",min = 0,max = 20,value = 12,step = 1),
                          sliderInput(inputId = "num44",label = "kd",min = 0,max = 1,value = 0.5,step = 0.1))
)
server <- function(input, output, session) {
       
        
        output$model1 <- reactive({
                ifelse(input$dataset == "model1", T,F)
        })
        
        output$model2 <- reactive({
                ifelse(input$dataset == "model2", T,F)
        })
        
        output$model3 <- reactive({
                ifelse(input$dataset == "model3", T,F)
        })
        
        
        output$model4 <- reactive({
                ifelse(input$dataset == "model4", T,F)
        })
        
        outputOptions(output, "model1", suspendWhenHidden = FALSE)
        outputOptions(output, "model2", suspendWhenHidden = FALSE) 
        outputOptions(output, "model3", suspendWhenHidden = FALSE) 
        outputOptions(output, "model4", suspendWhenHidden = FALSE) 
}

shinyApp(ui, server)