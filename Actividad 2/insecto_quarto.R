library(shiny)
library(readxl)
library(ggplot2)
library(DT)
library(dplyr)
library(car)
library(MASS)
library(DHARMa)
library(betareg)
library(statmod)
library(fitdistrplus)
library(multcomp)
library(emmeans)
library(multcompView)

ui <- fluidPage(
  titlePanel("🪲 Análisis de Captura de Insectos por Tratamiento"),
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo", "📁 Cargar archivo Excel", accept = c(".xlsx")),
      selectInput("modelo", "Modelo a visualizar:",
                  choices = c("ANOVA", "GLM Gaussian", "GLM Poisson", "GLM Binomial Negativa", "GLM Gamma", "GLM Beta")),
      hr(),
      actionButton("analizar", "🔍 Analizar datos")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("📊 Tabla y resumen",
                 DTOutput("tabla"),
                 verbatimTextOutput("resumen")
        ),
        tabPanel("📈 Diagnóstico",
                 plotOutput("diagPlot", height = "400px")
        ),
        tabPanel("📉 Boxplot con letras",
                 plotOutput("boxplotLetras", height = "500px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  datos <- reactiveVal(NULL)
  modelo_final <- reactiveVal(NULL)
  residuos_dharma <- reactiveVal(NULL)
  
  observeEvent(input$archivo, {
    req(input$archivo)
    df <- read_xlsx(input$archivo$datapath)
    df$Tratamiento <- factor(df$Tratamiento, levels = c("Control", "A", "B", "A+B"))
    df$Conteo_mod <- df$Conteo + 0.00001
    datos(df)
  })
  
  observeEvent(input$analizar, {
    req(datos())
    df <- datos()
    
    # Transformación Box-Cox
    boxcox_result <- boxcox(Conteo_mod ~ Tratamiento, data = df, lambda = seq(-4, 4, 0.1))
    lambda <- boxcox_result$x[which.max(boxcox_result$y)]
    df$conteo_trans <- if(abs(lambda) < 0.01) log(df$Conteo_mod + 1) else (((df$Conteo_mod + 1)^lambda - 1)/lambda)
    
    # Conteo escalado para beta
    df$conteo_esc <- (df$Conteo - min(df$Conteo)) / (max(df$Conteo) - min(df$Conteo))
    epsilon <- 1e-4
    df$conteo_esc[df$conteo_esc == 0] <- epsilon
    df$conteo_esc[df$conteo_esc == 1] <- 1 - epsilon
    
    modelo <- switch(input$modelo,
                     "ANOVA" = aov(conteo_trans ~ Tratamiento, data = df),
                     "GLM Gaussian" = glm(Conteo ~ Tratamiento, family = gaussian(link = "identity"), data = df),
                     "GLM Poisson" = glm(Conteo ~ Tratamiento, family = poisson(link = "log"), data = df),
                     "GLM Binomial Negativa" = glm.nb(Conteo ~ Tratamiento, data = df),
                     "GLM Gamma" = glm(Conteo_mod ~ Tratamiento, family = Gamma(link = "log"), data = df),
                     "GLM Beta" = betareg(conteo_esc ~ Tratamiento, data = df)
    )
    
    modelo_final(modelo)
    
    # Residuos
    if (inherits(modelo, "glm") || inherits(modelo, "negbin")) {
      residuos <- simulateResiduals(modelo)
      residuos_dharma(residuos)
    } else {
      residuos_dharma(NULL)
    }
    
    datos(df)
  })
  
  output$tabla <- renderDT({
    req(datos())
    datatable(datos(), options = list(pageLength = 10))
  })
  
  output$resumen <- renderPrint({
    req(modelo_final())
    list(
      `Resumen del modelo` = summary(modelo_final()),
      `ANOVA` = tryCatch(Anova(modelo_final()), error = function(e) "No disponible")
    )
  })
  
  output$diagPlot <- renderPlot({
    req(residuos_dharma())
    plot(residuos_dharma())
  })
  
  output$boxplotLetras <- renderPlot({
    req(datos(), modelo_final())
    df <- datos()
    
    em <- tryCatch(emmeans(modelo_final(), ~Tratamiento), error = function(e) NULL)
    if (is.null(em)) return(NULL)
    
    letras <- cld(em, Letters = letters, adjust = "tukey")
    letras_df <- data.frame(Tratamiento = letras$Tratamiento, Letters = letras$.group)
    posiciones <- df %>% group_by(Tratamiento) %>% summarise(ypos = max(Conteo) + 10)
    
    letras_plot <- merge(posiciones, letras_df, by = "Tratamiento")
    
    ggplot(df, aes(x = Tratamiento, y = Conteo, fill = Tratamiento)) +
      geom_boxplot(color = "black") +
      geom_text(data = letras_plot, aes(x = Tratamiento, y = ypos, label = Letters), size = 5) +
      scale_fill_manual(values = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C")) +
      labs(title = "Boxplot CONTEO con Letras Significativas", y = "Conteo (uds.)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

shinyApp(ui, server)
