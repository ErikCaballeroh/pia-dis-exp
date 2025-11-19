library(shiny)

server <- function(input, output, session) {

    output$contenido <- renderUI({

        # ------------------------------------------------------------
        # ACTIVIDAD 1 - Prueba t pareada Calibradores
        # ------------------------------------------------------------
        if (input$actividad == 1) {

            fluidPage(
                titlePanel("Actividad 1: Prueba t Pareada - Calibradores"),

                h4("Ingrese los datos de los calibradores (12 valores cada uno):"),

                fluidRow(
                    column(6, textInput("cal1", "Calibrador 1",
                                        "0.262, 0.266, 0.264, 0.267, 0.271, 0.268, 0.268, 0.266, 0.267, 0.270, 0.267, 0.262")),
                    column(6, textInput("cal2", "Calibrador 2",
                                        "0.263, 0.267, 0.267, 0.267, 0.264, 0.261, 0.262, 0.261, 0.261, 0.266, 0.264, 0.269"))
                ),

                actionButton("run_ttest", "Ejecutar prueba t", class = "btn btn-primary"),

                hr(),
                h4("Resultado de la prueba t pareada"),
                verbatimTextOutput("ttest_output"),

                h4("Conclusión"),
                textOutput("ttest_conclusion")
            )
        }

        # ------------------------------------------------------------
        # ACTIVIDAD 2 - ANOVA UNA VÍA
        # ------------------------------------------------------------
        else if (input$actividad == 2) {

            fluidPage(
                titlePanel("Actividad 2: ANOVA de una Vía - Crecimiento de Plantas"),

                h4("Ingrese los datos separados por comas (4 valores por área):"),

                fluidRow(
                    column(4, textInput("area1", "Área 1", "6.4, 5.8, 6.9, 8.0")),
                    column(4, textInput("area2", "Área 2", "7.2, 6.6, 7.0, 7.5")),
                    column(4, textInput("area3", "Área 3", "5.6, 6.0, 3.9, 5.7"))
                ),
                fluidRow(
                    column(4, textInput("area4", "Área 4", "5.6, 5.2, 6.4, 4.7")),
                    column(4, textInput("area5", "Área 5", "4.4, 5.2, 6.4, 7.1"))
                ),

                br(),
                actionButton("run_anova", "Ejecutar ANOVA", class = "btn btn-primary"),

                hr(),
                h4("Resultados del ANOVA"),
                verbatimTextOutput("anova_output"),

                h4("Conclusión"),
                textOutput("anova_conclusion")
            )
        }

        # ------------------------------------------------------------
        # ACTIVIDADES 3–8 VACÍAS
        # ------------------------------------------------------------
        else if (input$actividad == 3) h3("Actividad 3 (contenido pendiente)")
        else if (input$actividad == 4) h3("Actividad 4 (contenido pendiente)")
        else if (input$actividad == 5) h3("Actividad 5 (contenido pendiente)")
        else if (input$actividad == 6) h3("Actividad 6 (contenido pendiente)")
        else if (input$actividad == 7) h3("Actividad 7 (contenido pendiente)")
        else if (input$actividad == 8) h3("Actividad 8 (contenido pendiente)")

    })

    # ------------------------------------------------------------
    # LÓGICA ACTIVIDAD 1 - T TEST PAREADO
    # ------------------------------------------------------------
    observeEvent(input$run_ttest, {

        cal1 <- as.numeric(unlist(strsplit(input$cal1, ",")))
        cal2 <- as.numeric(unlist(strsplit(input$cal2, ",")))

        prueba <- t.test(cal1, cal2, paired = TRUE)

        output$ttest_output <- renderPrint({
            prueba
        })

        output$ttest_conclusion <- renderText({
            if (prueba$p.value < 0.05) {
                "Conclusión: p < 0.05. Sí existen diferencias significativas entre los calibradores."
            } else {
                "Conclusión: p ≥ 0.05. No existen diferencias significativas entre los calibradores."
            }
        })
    })

    # ------------------------------------------------------------
    # LÓGICA ACTIVIDAD 2 - ANOVA UNA VÍA
    # ------------------------------------------------------------
    observeEvent(input$run_anova, {

        A1 <- as.numeric(unlist(strsplit(input$area1, ",")))
        A2 <- as.numeric(unlist(strsplit(input$area2, ",")))
        A3 <- as.numeric(unlist(strsplit(input$area3, ",")))
        A4 <- as.numeric(unlist(strsplit(input$area4, ",")))
        A5 <- as.numeric(unlist(strsplit(input$area5, ",")))

        datos <- data.frame(
            crecimiento = c(A1, A2, A3, A4, A5),
            area = factor(rep(1:5, each = 4))
        )

        modelo <- aov(crecimiento ~ area, data = datos)

        output$anova_output <- renderPrint({
            summary(modelo)
        })

        output$anova_conclusion <- renderText({
            p_value <- summary(modelo)[[1]][["Pr(>F)"]][1]

            if (p_value < 0.1) {
                "Conclusión: p < 0.1. El área sí influye en el crecimiento de la planta."
            } else {
                "Conclusión: p ≥ 0.1. No existe evidencia suficiente para afirmar que el área influye."
            }
        })
    })

}
