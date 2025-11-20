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
            # ACTIVIDAD 1
            # ------------------------------
            conditionalPanel(
                condition = "input.actividad == 1",

                h3("Actividad 1: Prueba t Pareada (Calibradores)"),

                h4("Ingrese los 12 valores de cada calibrador separados por comas:"),

                fluidRow(
                    column(6, textInput(
                        "cal1", "Calibrador 1",
                        "0.262, 0.266, 0.264, 0.267, 0.271, 0.268, 0.268, 0.266, 0.267, 0.270, 0.267, 0.262"
                    )),
                    column(6, textInput(
                        "cal2", "Calibrador 2",
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
            # ACTIVIDAD 2
            # ------------------------------
            conditionalPanel(
                condition = "input.actividad == 2",

                h3("Actividad 2: ANOVA de una vía"),

                h4("Ingrese los 4 valores por cada área separados por comas:"),

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
            # ACTIVIDAD 3
            # ------------------------------
            conditionalPanel(
                condition = "input.actividad == 3",

                h3("Actividad 3: ANOVA Bifactorial (Diseño sin interacción)"),
                
                h4("Ejemplo: Puntuaciones de efectividad para 4 detergentes y 3 lavadoras."),
                
                h5("Ingrese las 3 puntuaciones para cada detergente (separadas por comas, un valor por lavadora):"),

                fluidRow(
                    column(6, textInput(
                        "det1", "Detergente 1 (Fila 1)", "53, 50, 59"
                    )),
                    column(6, textInput(
                        "det2", "Detergente 2 (Fila 2)", "54, 54, 60"
                    ))
                ),
                
                fluidRow(
                    column(6, textInput(
                        "det3", "Detergente 3 (Fila 3)", "56, 58, 63"
                    )),
                    column(6, textInput(
                        "det4", "Detergente 4 (Fila 4)", "50, 45, 58"
                    ))
                ),

                actionButton("run_bidimensional_anova", "Ejecutar ANOVA Bifactorial", class = "btn btn-primary"),

                hr(),
                h4("Resultados del ANOVA (Detergente y Lavadora):"),
                verbatimTextOutput("anova_bifactorial_output"),

                h4("Conclusiones:"),
                textOutput("anova_bifactorial_conclusion")
            ),

            # ------------------------------
            # ACTIVIDAD 4
            # ------------------------------
            conditionalPanel(
                condition = "input.actividad == 4",

                h3("Actividad 4: Diseño con factores Fr, Fc, Fl y Fg"),

                h4("1) Captura de datos del experimento"),

                h5("Bloques de tiempos (r1–r4)"),
                fluidRow(
                    column(6, textInput("r1", "r1:", "22.6, 15.4, 27.4, 19.0")),
                    column(6, textInput("r2", "r2:", "19.4, 12.8, 25.1, 16.2"))
                ),
                fluidRow(
                    column(6, textInput("r3", "r3:", "17.6, 26.1, 20.6, 23.1")),
                    column(6, textInput("r4", "r4:", "19.2, 25.1, 19.7, 18.4"))
                ),

                h5("Niveles de Fl (l1–l4)"),
                fluidRow(
                    column(6, textInput("l1", "l1:", "A, B, C, D")),
                    column(6, textInput("l2", "l2:", "B, A, D, C"))
                ),
                fluidRow(
                    column(6, textInput("l3", "l3:", "C, D, A, B")),
                    column(6, textInput("l4", "l4:", "D, C, B, A"))
                ),

                h5("Niveles de Fg (g1–g4)"),
                fluidRow(
                    column(6, textInput("g1", "g1:", "a, b, c, d")),
                    column(6, textInput("g2", "g2:", "c, d, a, b"))
                ),
                fluidRow(
                    column(6, textInput("g3", "g3:", "d, c, b, a")),
                    column(6, textInput("g4", "g4:", "b, a, d, c"))
                ),

                numericInput(
                    "act4_alpha",
                    "Nivel de significancia α:",
                    value = 0.05,
                    min = 0.001,
                    max = 0.2,
                    step = 0.01
                ),

                actionButton("act4_run", "Ejecutar análisis", class = "btn btn-primary"),

                hr(),
                h4("2) Código R:"),
                verbatimTextOutput("act4_codigo_aov"),

                h4("3) Resultado de summary():"),
                verbatimTextOutput("act4_summary"),

                h4("4) Conclusión para cada factor (según α):"),
                verbatimTextOutput("act4_conclusion_factores"),

                h4("5) Prueba de Tukey para Fg:"),
                verbatimTextOutput("act4_tukey"),

                h4("Conclusión de la prueba de Tukey:"),
                verbatimTextOutput("act4_conclusion_tukey")
            ),

            # ------------------------------
            # ACTIVIDADES 5–8
            # ------------------------------
            conditionalPanel(condition = "input.actividad == 5", h3("Actividad 5 (pendiente)")),
            conditionalPanel(condition = "input.actividad == 6", h3("Actividad 6 (pendiente)")),
            conditionalPanel(condition = "input.actividad == 7", h3("Actividad 7 (pendiente)")),
            conditionalPanel(condition = "input.actividad == 8", h3("Actividad 8 (pendiente)"))
        )
    )
)
