library(shiny)

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
            uiOutput("contenido")
        )
    )
)
