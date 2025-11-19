server <- function(input, output, session) {

    # ------------------------------------------------------------
    # ACTIVIDAD 1: Prueba t pareada
    # ------------------------------------------------------------
    observeEvent(input$run_ttest, {

        cal1 <- as.numeric(unlist(strsplit(input$cal1, ",")))
        cal2 <- as.numeric(unlist(strsplit(input$cal2, ",")))

        prueba <- t.test(cal1, cal2, paired = TRUE)

        output$ttest_output <- renderPrint({ prueba })

        output$ttest_conclusion <- renderText({
            if (prueba$p.value < 0.05) {
                "Conclusión: p < 0.05. Sí existen diferencias significativas entre los calibradores."
            } else {
                "Conclusión: p ≥ 0.05. No existen diferencias significativas entre los calibradores."
            }
        })
    })

    # ------------------------------------------------------------
    # ACTIVIDAD 2: ANOVA una vía
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

        output$anova_output <- renderPrint({ summary(modelo) })

        output$anova_conclusion <- renderText({

            p_value <- summary(modelo)[[1]][["Pr(>F)"]][1]

            if (p_value < 0.1) {
                "Conclusión: p < 0.1. El área SÍ influye significativamente en el crecimiento de la planta."
            } else {
                "Conclusión: p ≥ 0.1. No existe evidencia significativa de que el área influya."
            }
        })
    })

}
