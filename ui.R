ui <- fluidPage(

    titlePanel("Sistema de Actividades - Diseño de Experimentos"),

    sidebarLayout(

        sidebarPanel(
            selectInput(
                "actividad",
                "Selecciona una actividad:",
                choices = list(
                    "Actividad 1" = 1,
                    "Actividad 2" = 2,
                    "Actividad 3" = 3,
                    "Actividad 4" = 4,
                    "Actividad 5" = 5,
                    "Actividad 6" = 6,
                    "Actividad 7" = 7,
                    "Actividad 8" = 8
                )
            )
        ),

        mainPanel(

            # ------------------------------
            # ACTIVIDAD 1 - Prueba t
            # ------------------------------
            conditionalPanel(
                condition = "input.actividad == 1",

                h3("Actividad 1: Prueba t Pareada (Calibradores)"),

                h4("Ingrese los 12 valores de cada calibrador separados por comas:"),

                fluidRow(
                    column(6, textInput(
                        "cal1",
                        "Calibrador 1",
                        "0.262, 0.266, 0.264, 0.267, 0.271, 0.268, 0.268, 0.266, 0.267, 0.270, 0.267, 0.262"
                    )),
                    column(6, textInput(
                        "cal2",
                        "Calibrador 2",
                        "0.263, 0.267, 0.267, 0.267, 0.264, 0.261, 0.262, 0.261, 0.261, 0.266, 0.264, 0.269"
                    ))
                ),

                actionButton("run_ttest", "Ejecutar prueba t", class = "btn btn-primary"),

                hr(),
                h4("Resultado de la prueba t:"),
                verbatimTextOutput("ttest_output"),

                h4("Conclusión:"),
                textOutput("ttest_conclusion")
            ),

            # ------------------------------
            # ACTIVIDAD 2 - ANOVA UNA VÍA
            # ------------------------------
            conditionalPanel(
                condition = "input.actividad == 2",

                h3("Actividad 2: ANOVA de una vía (Crecimiento de plantas)"),

                h4("Ingrese los 4 valores por área separados por comas:"),

                fluidRow(
                    column(4, textInput("area1", "Área 1", "6.4, 5.8, 6.9, 8.0")),
                    column(4, textInput("area2", "Área 2", "7.2, 6.6, 7.0, 7.5")),
                    column(4, textInput("area3", "Área 3", "5.6, 6.0, 3.9, 5.7"))
                ),

                fluidRow(
                    column(4, textInput("area4", "Área 4", "5.6, 5.2, 6.4, 4.7")),
                    column(4, textInput("area5", "Área 5", "4.4, 5.2, 6.4, 7.1"))
                ),

                actionButton("run_anova", "Ejecutar ANOVA", class = "btn btn-primary"),

                hr(),
                h4("Resultados del ANOVA:"),
                verbatimTextOutput("anova_output"),

                h4("Conclusión:"),
                textOutput("anova_conclusion")
            ),

            # ------------------------------
            # ACTIVIDADES VACÍAS
            # ------------------------------
            conditionalPanel(condition = "input.actividad == 3", h3("Actividad 3 (pendiente)")),
            conditionalPanel(condition = "input.actividad == 4", h3("Actividad 4 (pendiente)")),
            conditionalPanel(condition = "input.actividad == 5", h3("Actividad 5 (pendiente)")),
            conditionalPanel(condition = "input.actividad == 6", h3("Actividad 6 (pendiente)")),
            conditionalPanel(condition = "input.actividad == 7", h3("Actividad 7 (pendiente)")),
            conditionalPanel(condition = "input.actividad == 8", h3("Actividad 8 (pendiente)"))
        )
    )
)
