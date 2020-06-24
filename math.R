library(shiny)

ui <- fluidPage(
        selectInput("mod1","Select model", list("model_1","model_2","model_3","model_4","model_5")),
        uiOutput('ex1')
)

server <- function(input, output, session) {
  
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
}



shinyApp(ui, server)

