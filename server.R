server <- function(input, output, session) {

    # ------------------------------------------------------------
    # ACTIVIDAD 1
    # ------------------------------------------------------------
    observeEvent(input$run_ttest, {

        cal1 <- as.numeric(unlist(strsplit(input$cal1, ",")))
        cal2 <- as.numeric(unlist(strsplit(input$cal2, ",")))

        prueba <- t.test(cal1, cal2, paired = TRUE)

        output$ttest_output <- renderPrint({ prueba })

        output$ttest_conclusion <- renderText({
            if (prueba$p.value < 0.05) {
                "Conclusión: p < 0.05. Sí existen diferencias significativas."
            } else {
                "Conclusión: p ≥ 0.05. No existen diferencias significativas."
            }
        })
    })


    # ------------------------------------------------------------
    # ACTIVIDAD 2
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
                "Conclusión: p < 0.1. El área influye."
            } else {
                "Conclusión: p ≥ 0.1. El área NO influye."
            }
        })
    })

    # ------------------------------------------------------------
    # ACTIVIDAD 3
    # ------------------------------------------------------------
    
observeEvent(input$run_bidimensional_anova, {

    # Datos del ejemplo: Detergentes (filas) y Lavadoras (columnas)
    # Fila 1 (Detergente 1): 53, 50, 59
    # Fila 2 (Detergente 2): 54, 54, 60
    # Fila 3 (Detergente 3): 56, 58, 63
    # Fila 4 (Detergente 4): 50, 45, 58
    
    # Se concatenan los datos en un solo vector (por columnas es típico en R, pero aquí 
    # se sigue el orden de la tabla para facilitar la creación de factores)
    # En la práctica, se ingresarán los datos desde la UI (input$det1, input$det2, etc.)
    # Se asume que los inputs son vectores de números, uno por cada detergente (fila).
    
    # NOTA: Este código asume que los datos están disponibles en las variables de entrada de Shiny, 
    # por ejemplo: input$det1, input$det2, input$det3, input$det4.
    
    det1 <- as.numeric(unlist(strsplit(input$det1, ","))) # Ej: c(53, 50, 59)
    det2 <- as.numeric(unlist(strsplit(input$det2, ","))) # Ej: c(54, 54, 60)
    det3 <- as.numeric(unlist(strsplit(input$det3, ","))) # Ej: c(56, 58, 63)
    det4 <- as.numeric(unlist(strsplit(input$det4, ","))) # Ej: c(50, 45, 58)
    
    # 1. Crear el vector de respuesta Y
    Y <- c(det1, det2, det3, det4) # Vector de todas las puntuaciones: 53, 50, 59, 54, ...
    
    m <- length(det1) # Número de columnas (Lavadoras) = 3
    n <- 4           # Número de filas (Detergentes) = 4
    
    # 2. Crear los factores
    
    # Factor Fila (Detergente): Repite cada nivel 'm' veces (número de lavadoras)
    # 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4
    FactorFila <- factor(rep(1:n, each = m)) 
    
    # Factor Columna (Lavadora): Repite la secuencia de niveles 'n' veces (número de detergentes)
    # 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3
    FactorColumna <- factor(rep(1:m, times = n))
    
    # 3. Crear el data frame
    datos3 <- data.frame(
        Y = Y,
        Detergente = FactorFila,
        Lavadora = FactorColumna
    )
    
    # 4. Realizar el ANOVA Bifactorial (Modelo aditivo sin interacción)
    modelo_bifactorial <- aov(Y ~ Detergente + Lavadora, data = datos3)
    
    # 5. Mostrar los resultados de ANOVA
    output$anova_bifactorial_output <- renderPrint({ summary(modelo_bifactorial) })

    # 6. Conclusión
    output$anova_bifactorial_conclusion <- renderText({
        
        # Extraer los p-valores del resumen del ANOVA
        s <- summary(modelo_bifactorial)
        p_valor_detergente <- s[[1]]["Detergente", "Pr(>F)"]
        p_valor_lavadora <- s[[1]]["Lavadora", "Pr(>F)"]
        alpha <- 0.05 # Nivel de significancia del problema

        conclusion_det <- ifelse(
            p_valor_detergente < alpha,
            "**El Detergente influye** (p-valor < 0.05, Se rechaza H₀).",
            "**El Detergente NO influye** (p-valor ≥ 0.05, No se rechaza H₀)."
        )
        
        conclusion_lav <- ifelse(
            p_valor_lavadora < alpha,
            "**La Lavadora influye** (p-valor < 0.05, Se rechaza H₀).",
            "**La Lavadora NO influye** (p-valor ≥ 0.05, No se rechaza H₀)."
        )

        paste(
            "Conclusiones (α = 0.05):",
            "Factor Fila (Detergente):", conclusion_det,
            "Factor Columna (Lavadora):", conclusion_lav,
            sep = "\n"
        )
    })
})

    # ------------------------------------------------------------
    # ACTIVIDAD 4
    # ------------------------------------------------------------
    observeEvent(input$act4_run, {

        # --- r1–r4: tiempos ---
        r1 <- as.numeric(unlist(strsplit(input$r1, ",")))
        r2 <- as.numeric(unlist(strsplit(input$r2, ",")))
        r3 <- as.numeric(unlist(strsplit(input$r3, ",")))
        r4 <- as.numeric(unlist(strsplit(input$r4, ",")))

        # --- l1–l4: niveles de Fl ---
        l1 <- trimws(unlist(strsplit(input$l1, ",")))
        l2 <- trimws(unlist(strsplit(input$l2, ",")))
        l3 <- trimws(unlist(strsplit(input$l3, ",")))
        l4 <- trimws(unlist(strsplit(input$l4, ",")))

        # --- g1–g4: niveles de Fg ---
        g1 <- trimws(unlist(strsplit(input$g1, ",")))
        g2 <- trimws(unlist(strsplit(input$g2, ",")))
        g3 <- trimws(unlist(strsplit(input$g3, ",")))
        g4 <- trimws(unlist(strsplit(input$g4, ",")))

        Y  <- c(r1, r2, r3, r4)
        Fl_vec <- c(l1, l2, l3, l4)
        Fg_vec <- c(g1, g2, g3, g4)

        n_block <- length(r1)

        # Factores Fr y Fc
        Fr <- factor(rep(1:4, each = n_block))
        Fc <- factor(rep(1:4, times = n_block))
        Fl <- factor(Fl_vec)
        Fg <- factor(Fg_vec)

        datos4 <- data.frame(
            Y  = Y,
            Fr = Fr,
            Fc = Fc,
            Fl = Fl,
            Fg = Fg
        )

        y.aov <- aov(Y ~ Fr + Fc + Fl + Fg, data = datos4)

        # ---- Código R ----
        output$act4_codigo_aov <- renderPrint({
            cat("r1 <- c(", paste(r1, collapse = ", "), ")\n", sep = "")
            cat("r2 <- c(", paste(r2, collapse = ", "), ")\n", sep = "")
            cat("r3 <- c(", paste(r3, collapse = ", "), ")\n", sep = "")
            cat("r4 <- c(", paste(r4, collapse = ", "), ")\n\n", sep = "")

            cat("y <- c(r1, r2, r3, r4)\n")
            cat("Fr <- factor(rep(1:4, each =", n_block, "))\n", sep = "")
            cat("Fc <- factor(rep(1:4, times =", n_block, "))\n\n", sep = "")

            cat("l1 <- c(", paste(sprintf("'%s'", l1), collapse = ", "), ")\n", sep = "")
            cat("l2 <- c(", paste(sprintf("'%s'", l2), collapse = ", "), ")\n", sep = "")
            cat("l3 <- c(", paste(sprintf("'%s'", l3), collapse = ", "), ")\n", sep = "")
            cat("l4 <- c(", paste(sprintf("'%s'", l4), collapse = ", "), ")\n", sep = "")
            cat("Fl <- factor(c(l1, l2, l3, l4))\n\n")

            cat("g1 <- c(", paste(sprintf("'%s'", g1), collapse = ", "), ")\n", sep = "")
            cat("g2 <- c(", paste(sprintf("'%s'", g2), collapse = ", "), ")\n", sep = "")
            cat("g3 <- c(", paste(sprintf("'%s'", g3), collapse = ", "), ")\n", sep = "")
            cat("g4 <- c(", paste(sprintf("'%s'", g4), collapse = ", "), ")\n", sep = "")
            cat("Fg <- factor(c(g1, g2, g3, g4))\n\n")

            cat("y.aov <- aov(y ~ Fr + Fc + Fl + Fg)\n")
        })

        # ---- summary() ----
        output$act4_summary <- renderPrint({
            summary(y.aov)
        })

        # ---- Conclusión automática ----
        output$act4_conclusion_factores <- renderPrint({

            s <- summary(y.aov)
            tabla <- s[[1]]

            col_p <- grep("Pr", colnames(tabla))
            if (length(col_p) == 0) col_p <- ncol(tabla)

            pvals <- tabla[, col_p]
            nombres <- rownames(tabla)
            alpha <- input$act4_alpha

            factores_sig <- c()
            for (i in seq_along(nombres)) {
                if (nombres[i] != "Residuals" && !is.na(pvals[i])) {
                    if (pvals[i] < alpha) {
                        factores_sig <- c(factores_sig, nombres[i])
                    }
                }
            }

            if (length(factores_sig) == 0) {
                cat("Ningún factor es significativo\n")
            } else if (length(factores_sig) == 1) {
                cat("Solo el factor", factores_sig, "influye en el tiempo de ensamblaje\n")
            } else {
                cat("Los factores significativos son:", paste(factores_sig, collapse = ", "), "\n")
            }
        })

        # ---- Tukey ----
        output$act4_tukey <- renderPrint({
            TukeyHSD(y.aov, "Fg")
        })

        # ---- Conclusión Tukey ----
        output$act4_conclusion_tukey <- renderPrint({

            tuk <- TukeyHSD(y.aov, "Fg")
            tabla_tuk <- tuk$Fg

            comps <- rownames(tabla_tuk)
            p_adj <- tabla_tuk[, "p adj"]
            alpha <- input$act4_alpha

            sig <- comps[p_adj < alpha]

            if (length(sig) == 0) {
                cat("No hay diferencias significativas entre niveles de Fg\n")
            } else if (length(sig) == 1) {
                cat("Solo", sig, "es significativo\n")
            } else {
                cat("Las comparaciones significativas son:", paste(sig, collapse = ", "), "\n")
            }
        })
    })
}
