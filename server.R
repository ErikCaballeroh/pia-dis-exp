# Define server logic required to draw a histogram ----
# 30 lineas de codigo por cada integrante del equipo
# Una funcion minimo por integrante
# Agregar modo oscuro
# Se entrega el ultimo viernes
# Entregable url del repositorio

server <- function(input, output) {

    # Histogram of the Old Faithful Geyser Data ----
    # with requested number of bins
    # This expression that generates a histogram is wrapped in a call
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot
    output$distPlot <- renderPlot({
        x    <- faithful$waiting
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        hist(x, breaks = bins, col = "#007bc2", border = "white",
            xlab = "Waiting time to next eruption (in mins)",
            main = "Histogram of waiting times"
        )
    })
    
}