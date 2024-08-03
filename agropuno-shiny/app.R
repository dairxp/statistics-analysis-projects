library(readxl)
library(shiny)
library(rsconnect)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinyWidgets)
library(nortest)
library(forecast)
library(imputeTS)

generarHistograma <- function(data, variable) {
  filtered_data <- data %>%
    filter(VARIABLE == variable) %>%
    select(-VARIABLE)
  
  production_data <- data.frame(
    YEAR = as.numeric(colnames(filtered_data)),
    TOTAL = as.numeric(unlist(filtered_data))
  )
  
  hist_data <- hist(production_data$TOTAL, plot = FALSE)
  
  # Comprobar si los datos siguen una distribución normal
  normality_test <- shapiro.test(production_data$TOTAL)
  is_normal <- normality_test$p.value > 0.05
  
  # Generar el gráfico con la curva de normalidad solo si los datos son normales
  if (is_normal) {
    media <- mean(production_data$TOTAL)
    desviacion <- sd(production_data$TOTAL)
    x <- seq(min(hist_data$breaks), max(hist_data$breaks), length.out = 100)
    y <- dnorm(x, mean = media, sd = desviacion)
    curve_data <- data.frame(x = x, y = y)
    
    plot_ly(production_data, x = ~YEAR, y = ~TOTAL, type = 'bar',
            marker = list(color = 'blue'), opacity = 0.7) %>%
      add_trace(data = curve_data, x = ~x, y = ~y, type = 'scatter', mode = 'lines', line = list(color = 'black', width = 3)) %>%
      layout(title = list(text = paste("Histograma de Producción Total para", variable),
                          y = 0.97),
             xaxis = list(title = "Año"),
             yaxis = list(title = "Producción Total"))
  } else {
    # Si los datos no son normales, mostrar solo el histograma
    plot_ly(production_data, x = ~YEAR, y = ~TOTAL, type = 'bar',
            marker = list(color = 'blue'), opacity = 0.7) %>%
      layout(title = list(text = paste("Histograma de Producción Total para", variable),
                          y = 0.97),
             xaxis = list(title = "Año"),
             yaxis = list(title = "Producción Total"))
  }
}

generarMensajeNormalidad <- function(data, variable) {
  filtered_data <- data %>%
    filter(VARIABLE == variable) %>%
    select(-VARIABLE)
  
  production_data <- data.frame(
    YEAR = as.numeric(colnames(filtered_data)),
    TOTAL = as.numeric(unlist(filtered_data))
  )
  
  # Comprobar si los datos siguen una distribución normal
  normality_test <- shapiro.test(production_data$TOTAL)
  is_normal <- normality_test$p.value > 0.05
  
  if (is_normal) {
    mensaje <- "Los datos siguen una distribución normal."
  } else {
    mensaje <- "Los datos no siguen una distribución normal."
  }
  
  mensaje
}

generarBoxPlot <- function(data, variable) {
  filtered_data <- data %>%
    filter(VARIABLE == variable) %>%
    select(-VARIABLE)
  
  production_data <- data.frame(
    YEAR = as.numeric(colnames(filtered_data)),
    TOTAL = as.numeric(unlist(filtered_data))
  )
  
  plot_ly(production_data, y = ~TOTAL, type = 'box',
          marker = list(color = 'blue')) %>%
    layout(title = list(text = paste("Box Plot de Producción Total para", variable),
                        y = 0.97),
           xaxis = list(title = "Año"),
           yaxis = list(title = "Producción Total"))
}

prueba_t <- function(grupo1, grupo2) {
  result <- t.test(grupo1, grupo2)
  resultado <- list(
    estadistico_t = result$statistic,
    valor_p = result$p.value,
    intervalo_confianza = result$conf.int,
    hay_evidencia = result$p.value < 0.05,
    medias_diferentes = result$p.value < 0.05,
    mensaje1 = ifelse(result$p.value < 0.05, "Hay evidencia estadística para rechazar la hipótesis nula. Las medias son significativamente diferentes.", "No hay suficiente evidencia estadística para rechazar la hipótesis nula. Las medias no son significativamente diferentes.")
  )
  
  return(resultado)
}

# Define la función para evaluar la correlación de Pearson y generar el gráfico
evaluar_correlacion_pearson <- function(data) {
  # Convertir a tipo numérico
  variable1 <- as.numeric(data[, "Siembra"])
  variable2 <- as.numeric(data[, "Produccion"])
  
  # Verificar si los datos son numéricos
  if (any(is.na(variable1)) || any(is.na(variable2))) {
    return("Los datos seleccionados contienen valores no numéricos.")
  }
  
  # Calcula la correlación de Pearson
  correlacion <- cor(variable1, variable2)
  
  # Calcula el tamaño de la muestra para ambas variables
  n1 <- length(variable1)
  n2 <- length(variable2)
  
  # Toma el mínimo entre los dos tamaños de muestra
  n <- min(n1, n2)
  
  # Calcula el valor crítico de la correlación de Pearson para un nivel de significancia del 0.05
  critical_value <- qt(0.975, df = n - 2)
  
  # Calcula el intervalo de confianza
  confidence_interval <- c(correlacion - critical_value / sqrt(n - 2), correlacion + critical_value / sqrt(n - 2))
  
  # Comprueba si el intervalo de confianza incluye el valor cero
  if (confidence_interval[1] <= 0 && confidence_interval[2] >= 0) {
    mensaje <- "Correlación Pearson: No hay evidencia suficiente para afirmar una relación significativa."
  } else {
    mensaje <- "Correlación Pearson: Hay evidencia suficiente para afirmar una relación significativa."
  }
  
  # Genera un gráfico de dispersión con línea de tendencia
  plot(variable1, variable2, main = "", xlab = "Variable de Siembra", ylab = "Variable de Producción")
  abline(lm(variable2 ~ variable1), col = "red")
  
  # Devuelve la evaluación de la correlación
  return(mensaje)
}

Agrodata <- read_excel("Agrodata.xlsx")

data <- Agrodata

ui <- shinyUI(
  fluidPage(
    tags$head(
      tags$link(rel="stylesheet", href="estilos.css"),
      tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/boxicons@2.0.9/css/boxicons.min.css"),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@300;400;500;600;700;800;900&display=swap")
    ),
    tags$body(
      header <- tags$header(
        class = "header",
        tags$a(href = "#", class = "logo", "Agrodata"),
        tags$div(class = "bx bx-menu", id = "menu-icon"),
        tags$nav(class = "navbar",
                 tags$ul(
                   tags$li(
                     tags$a(href = "#home", class = "active", "Home")
                   ),
                   tags$li(
                     tags$a(href = "#about", "About")
                   ),
                   tags$li(
                     tags$a(href = "#Data", "Data")
                   ),
                   tags$li(
                     tags$a(href = "#Grafico", "Grafico",
                            tags$ul(
                              tags$li(tags$a(href = "#histograma", "Histograma")),
                              tags$li(tags$a(href = "#circulos", "Circulos")),
                              tags$li(tags$a(href = "#puntos", "Puntos"))
                            )
                     )
                   ),
                   tags$li(
                     tags$a(href = "#Pruebas", "Pruebas",
                            tags$ul(
                              tags$li(tags$a(href = "#normalidad", "Normalidad")),
                              tags$li(tags$a(href = "#t_student", "T STUDENT")),
                              tags$li(tags$a(href = "#pearson", "pearson"))
                            )
                     )
                   ),
                   tags$li(
                     tags$a(href = "#prediccion", "Predicción")
                   )
                 )
        )
        
      ),
      tags$section(class = "home", id = "home",
                   tags$div(class = "home-content",
                            tags$h1("Hola", tags$span("bienvenido")),
                            tags$div(class = "text-animate",
                                     tags$h3("Nuestra aplicación")
                            ),
                            tags$p("¡Bienvenido a nuestra aplicación de visualización de datos! Con nuestra herramienta, podrás explorar y analizar los datos de manera intuitiva y efectiva. Nuestra aplicación está diseñada para facilitar la comprensión y la toma de decisiones."),
                   ),
                   tags$div(class = "home-sci",
                            tags$a(href = "https://www.facebook.com/becam.chani.1", tags$i(class = "bx bxl-facebook-circle")),
                            tags$a(href = "https://forms.gle/YH6MXijjuEPW4WSw7", tags$i(class = "bx bx-envelope")),
                            tags$a(href = "https://www.linkedin.com/in/alenm-jmade-68a186257/", tags$i(class = "bx bxl-linkedin")),
                   ),
                   tags$div(class = "home-imgHover")
      ),
      tags$section(class = "about", id = "about",
                   tags$h2(class = "heading", "Sobre la", tags$span("App")),
                   tags$div(class = "about-img",
                            tags$img(src = "picture/logo.jpg", alt = "logo de la app(AGROPUNO)"),
                            tags$span(class = "circle-spin")
                   ),
                   tags$div(class = "about-content",
                            tags$span("Agrodata"),
                            tags$p("Nuestra aplicación se basa en una base de datos real, lo que significa que los resultados y los análisis se basan en una basen en datos reales. Nuestra aplicación ofrece gráficos interactivos, que te permiten explorar y comprender tus datos de manera intuitiva. Con estas visualizaciones, puedes identificar patrones, realizar comparaciones y extraer información valiosa del data."),
                            
                   )
      ),
      tags$section(class = "Data", id = "Data",
                   tags$div(class = "Analisis_datos",
                            tags$h1("Análisis de ", tags$span("datos")),
                            tags$div(class = "row",
                                     tags$div(class = "opcionesDatos",
                                              sidebarLayout(
                                                sidebarPanel(
                                                  selectInput("variable", "Selecciona la variable:",
                                                              choices = unique(data$VARIABLE),
                                                              selected = unique(data$VARIABLE)[1]),
                                                  
                                                  selectInput("process", "Selecciona el proceso:",
                                                              choices = unique(data$PROCESO),
                                                              selected = unique(data$PROCESO)[1]),
                                                  
                                                  selectInput("province", "Selecciona la provincia:",
                                                              choices = unique(data$PROVINCIA),
                                                              multiple = TRUE),
                                                  
                                                  selectInput("year", "Selecciona el año:",
                                                              choices = colnames(data)[-c(1:4)],
                                                              selected = colnames(data)[-c(1:4)][1])
                                                ),
                                                
                                                mainPanel(
                                                  plotOutput("plot")
                                                )
                                              )
                                     )
                            )
                   ) 
                   
                   
      ),
      tags$section(class = "Grafico", id = "Grafico",
                   tags$div(class = "histograma", id = "histograma",
                            tags$h1(tags$span("histogramas")),
                            fluidRow(
                              column(width = 12, height = 400,
                                     plotlyOutput("graficoBarrasVARIABLE")
                              )
                            ),
                            tags$div(style = "margin-bottom: 20px;"),  # Separación vertical
                            fluidRow(
                              column(width = 6, height = 400,
                                     plotlyOutput("graficoBarrasPROVINCIA")
                              ),
                              column(width = 6, height = 400,
                                     plotlyOutput("graficoBarrasPROCESO")
                              )
                            )
                   ),
                   tags$div(style = "margin-bottom: 20px;"),
                   tags$div(class = "circulos", id = "circulos",
                            
                            tags$h1("Graficos ", tags$span("circulares")),
                            fluidRow(
                              column(width = 6, height = 400,
                                     plotlyOutput("graficoCircularPROVINCIA")
                              ),
                              column(width = 6, height = 400,
                                     plotlyOutput("graficoCircularPROCESO")
                              )
                            ),
                            tags$div(style = "margin-bottom: 20px;"),
                            fluidRow(
                              column(width = 6, height = 400,
                                     plotlyOutput("graficoCircularUnidad")
                              ),
                              column(width = 6, height = 400,
                                     plotlyOutput("graficoCircularVARIABLE")
                              )
                            )
                   ),
                   tags$div(class = "puntos",
                            id = "puntos",
                            tags$h1("Poligonos de", tags$span("frecuencia")),
                            selectInput("variable1",
                                        "Selecciona la variable:",
                                        choices = unique(Agrodata$VARIABLE),
                                        selected = unique(Agrodata$VARIABLE)[1]),
                            mainPanel(
                              plotlyOutput("polygonPlot")
                            ),
                            selectInput("PROCESO",
                                        "Selecciona la PROCESO:",
                                        choices = unique(Agrodata$PROCESO),
                                        selected = unique(Agrodata$PROCESO)[1]),
                            mainPanel(
                              plotlyOutput("polygonPlot1")
                            ),
                            selectInput("PROVINCIA",
                                        "Selecciona la PROVINCIA:",
                                        choices = unique(Agrodata$PROVINCIA),
                                        selected = unique(Agrodata$PROVINCIA)[1]),
                            mainPanel(
                              plotlyOutput("polygonPlot2")
                            )
                   ),
                   tags$div(class = "empty-div"),
                   tags$style(HTML(".puntosvariable, .puntosproceso, .puntosPROVINCIA, .empty-div { width: 150%; box-sizing: border-box; margin-bottom: 20px; }"))
                   ,
      ),
      tags$section(class = "Pruebas", id = "Pruebas",
                   tags$div(class = "normalidad", id = "normalidad",
                            tags$h1("Prueba de ", tags$span("Normalidad")),
                            selectInput("variableHistograma", "Variable para Histograma:", choices = unique(Agrodata$VARIABLE)),
                            plotlyOutput("histogramaPlot"),
                            verbatimTextOutput("mensaje"),
                            selectInput("variableBoxPlot", "Variable para Box Plot:", choices = unique(Agrodata$VARIABLE)),
                            plotlyOutput("boxPlot"),
                   ),
                   tags$div(class = "t_student", id = "t_student",
                            tags$h1("Prueba de ", tags$span("T de student")),
                            selectInput("year1", "Año 1:", choices = colnames(Agrodata)[-c(1:4)]),
                            selectInput("year2", "Año 2:", choices = colnames(Agrodata)[-c(1:4)]),
                            actionButton("calcular", "Calcular prueba t"),
                            verbatimTextOutput("estadisticas"),
                            verbatimTextOutput("mensaje1"),
                            plotOutput("pearsonPlot"),
                            verbatimTextOutput("correlationOutput")
                   ),
                   tags$div(class = "pearson", id = "pearson",
                            tags$h1("Correlación de ", tags$span("pearson")),
                            titlePanel("Correlación entre Siembra y Producción"),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(
                                  inputId = "siembrapear",
                                  label = "Seleccionar variable de Siembra",
                                  choices = unique(Agrodata$VARIABLE[Agrodata$PROCESO == "Siembra"]),
                                  selected = unique(Agrodata$VARIABLE[Agrodata$PROCESO == "Siembra"])[1]
                                ),
                                selectInput(
                                  inputId = "siembrapear",
                                  label = "Seleccionar variable de Producción",
                                  choices = unique(Agrodata$VARIABLE[Agrodata$PROCESO == "Cosecha"]),
                                  selected = unique(Agrodata$VARIABLE[Agrodata$PROCESO == "Cosecha"])[1]
                                ),
                                actionButton("filtrar", "Filtrar")
                              ),
                              mainPanel(
                                plotOutput("pearsonPlot1"),
                                verbatimTextOutput("correlationOutput1")
                              )
                            )
                   ),
                   tags$div(class = "prediccion", id = "prediccion",
                            tags$h1("Prediccion en ", tags$span("series de tiempo")),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("cultivo", "Seleccionar cultivo:", choices = unique(Agrodata$VARIABLE)),
                                actionButton("predecir", "Realizar Predicción")
                              ),
                              mainPanel(
                                plotOutput("serie_tiempo_plot")
                              )
                            ),
                            tags$h1("Análisis de ", tags$span("Regresión Lineal")),
                            sidebarLayout(
                              sidebarPanel(
                                # Dropdown para seleccionar la variable agronómica
                                selectInput("variable3", "Seleccione la Variable:",
                                            choices = unique(Agrodata$VARIABLE)),
                                # Dropdown para seleccionar el año
                                selectInput("anio", "Seleccione el Año:",
                                            choices = colnames(Agrodata)[5:30])
                              ),
                              mainPanel(
                                # Gráfico
                                plotOutput("grafico1"),
                                
                                # Salida para mostrar el resumen del modelo lineal
                                verbatimTextOutput("modelo_summary"),
                                
                                # Mensaje de linealidad
                                uiOutput("mensaje_linealidad")
                              )
                            )
                   )
      )
    )
  )
)


server <- shinyServer(function(input, output) {
  filteredData <- reactive({
    subset(data, PROVINCIA %in% input$province & PROCESO == input$process)
  })
  
  output$title <- renderText({
    paste(input$process, "de", input$variable, "por provincia", input$year)
  })
  
  output$plot <- renderPlot({
    year_col <- which(colnames(data) == input$year)
    data_subset <- data[, c(1:4, year_col)]
    
    ggplot(filteredData(), aes_string(x = "PROVINCIA", y = colnames(data_subset)[5])) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "", x = "Provincia", y = input$variable) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$graficoBarrasVARIABLE <- renderPlotly({
    datos_agrupados <- data %>%
      group_by(VARIABLE) %>%
      summarize(Produccion_Total = sum(TOTAL)) %>%
      arrange(desc(Produccion_Total))
    
    p <- ggplot(datos_agrupados, aes(x = VARIABLE, y = Produccion_Total, fill = VARIABLE)) +
      geom_bar(stat = "identity") +
      labs(x = "VARIABLE", y = "Total", fill = "VARIABLE", title = "Total por VARIABLE") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p) 
  })
  
  output$graficoBarrasPROVINCIA <- renderPlotly({
    datos_agrupados <- data %>%
      group_by(PROVINCIA) %>%
      summarize(Produccion_Total = sum(TOTAL)) %>%
      arrange(desc(Produccion_Total))
    
    p <- ggplot(datos_agrupados, aes(x = PROVINCIA, y = Produccion_Total, fill = PROVINCIA)) +
      geom_bar(stat = "identity") +
      labs(x = "PROVINCIA", y = "Total", fill = "PROVINCIA", title = "Total por PROVINCIA") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p)
  })
  
  output$graficoBarrasPROCESO <- renderPlotly({
    datos_agrupados <- data %>%
      group_by(PROCESO) %>%
      summarize(Produccion_Total = sum(TOTAL)) %>%
      arrange(desc(Produccion_Total))
    
    p <- ggplot(datos_agrupados, aes(x = PROCESO, y = Produccion_Total, fill = PROCESO)) +
      geom_bar(stat = "identity") +
      labs(x = "PROCESO", y = "Total", fill = "PROCESO", title = "Total por PROCESO") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p)
  })
  
  output$graficoBarrasUnidad <- renderPlotly({
    datos_agrupados <- data %>%
      group_by(`Unidad de medida`) %>%
      summarize(Produccion_Total = sum(TOTAL)) %>%
      arrange(desc(Produccion_Total))
    
    p <- ggplot(datos_agrupados, aes(x = `Unidad de medida`, y = Produccion_Total, fill = `Unidad de medida`)) +
      geom_bar(stat = "identity") +
      labs(x = "Unidad de medida", y = "Total", fill = "Unidad de medida", title = "Total por Unidad de medida") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p) 
  })
  
  output$graficoCircularVARIABLE <- renderPlotly({
    datos_agrupados <- data %>%
      group_by(VARIABLE) %>%
      summarize(Produccion_Total = sum(TOTAL)) %>%
      arrange(desc(Produccion_Total))
    
    p <- plot_ly(datos_agrupados, labels = ~VARIABLE, values = ~Produccion_Total, type = "pie") %>%
      layout(title = "Total por VARIABLE")
    
    p
  })
  
  output$graficoCircularPROVINCIA <- renderPlotly({
    datos_agrupados <- data %>%
      group_by(PROVINCIA) %>%
      summarize(Produccion_Total = sum(TOTAL)) %>%
      arrange(desc(Produccion_Total))
    
    p <- plot_ly(datos_agrupados, labels = ~PROVINCIA, values = ~Produccion_Total, type = "pie") %>%
      layout(title = "Total por PROVINCIA")
    
    p
  })
  
  output$graficoCircularPROCESO <- renderPlotly({
    datos_agrupados <- data %>%
      group_by(PROCESO) %>%
      summarize(Produccion_Total = sum(TOTAL)) %>%
      arrange(desc(Produccion_Total))
    
    p <- plot_ly(datos_agrupados, labels = ~PROCESO, values = ~Produccion_Total, type = "pie") %>%
      layout(title = "Total por PROCESO")
    
    p
  })
  
  output$graficoCircularUnidad <- renderPlotly({
    datos_agrupados <- data %>%
      group_by(`Unidad de medida`) %>%
      summarize(Produccion_Total = sum(TOTAL)) %>%
      arrange(desc(Produccion_Total))
    
    p <- plot_ly(datos_agrupados, labels = ~`Unidad de medida`, values = ~Produccion_Total, type = "pie") %>%
      layout(title = "Total por Unidad de medida")
    
    p
  })
  
  output$polygonPlot <- renderPlotly({
    data_df <- Agrodata
    
    filtered_data <- subset(data_df, VARIABLE == input$variable1)
    
    last_year_col <- which(grepl("^\\d{4}$", colnames(filtered_data)))[length(which(grepl("^\\d{4}$", colnames(filtered_data))))]
    
    total_production <- colSums(filtered_data[, 5:last_year_col] %>%
                                  mutate(across(everything(), as.numeric)), na.rm = TRUE)
    
    production_data <- data.frame(YEAR = as.numeric(colnames(filtered_data)[5:last_year_col]), TOTAL = total_production)
    
    production_data <- production_data %>% arrange(YEAR)
    
    plot_ly(production_data, x = ~YEAR, y = ~TOTAL, type = 'scatter', mode = 'lines+markers',
            marker = list(color = 'blue', symbol = 'diamond', size = 10),
            line = list(color = 'skyblue', width = 2)) %>%
      layout(title = list(text = paste("Polígono de Frecuencias para", input$variable),
                          y = 0.97),
             xaxis = list(title = "Año"),
             yaxis = list(title = "Producción Total"))
  })
  
  output$polygonPlot1 <- renderPlotly({
    data_df <- Agrodata
    
    filtered_data <- subset(data_df, PROCESO == input$PROCESO)
    
    last_year_col <- which(grepl("^\\d{4}$", colnames(filtered_data)))[length(which(grepl("^\\d{4}$", colnames(filtered_data))))]
    
    total_production <- colSums(filtered_data[, 5:last_year_col] %>%
                                  mutate(across(everything(), as.numeric)), na.rm = TRUE)
    
    production_data <- data.frame(YEAR = as.numeric(colnames(filtered_data)[5:last_year_col]), TOTAL = total_production)
    
    production_data <- production_data %>% arrange(YEAR)
    
    plot_ly(production_data, x = ~YEAR, y = ~TOTAL, type = 'scatter', mode = 'lines+markers',
            marker = list(color = 'blue', symbol = 'diamond', size = 10),
            line = list(color = 'skyblue', width = 2)) %>%
      layout(title = list(text = paste("Polígono de Frecuencias para", input$PROCESO),
                          y = 0.97),
             xaxis = list(title = "Año"),
             yaxis = list(title = "Producción Total"))
  })
  
  output$polygonPlot2 <- renderPlotly({
    data_df <- Agrodata
    
    filtered_data <- subset(data_df, PROVINCIA == input$PROVINCIA)
    
    last_year_col <- which(grepl("^\\d{4}$", colnames(filtered_data)))[length(which(grepl("^\\d{4}$", colnames(filtered_data))))]
    
    total_production <- colSums(filtered_data[, 5:last_year_col] %>%
                                  mutate(across(everything(), as.numeric)), na.rm = TRUE)
    
    production_data <- data.frame(YEAR = as.numeric(colnames(filtered_data)[5:last_year_col]), TOTAL = total_production)
    
    production_data <- production_data %>% arrange(YEAR)
    
    plot_ly(production_data, x = ~YEAR, y = ~TOTAL, type = 'scatter', mode = 'lines+markers',
            marker = list(color = 'blue', symbol = 'diamond', size = 10),
            line = list(color = 'skyblue', width = 2)) %>%
      layout(title = list(text = paste("Polígono de Frecuencias para", input$PROVINCIA),
                          y = 0.97),
             xaxis = list(title = "Año"),
             yaxis = list(title = "Producción Total"))
  })
  
  output$histogramaPlot <- renderPlotly({
    generarHistograma(Agrodata, input$variableHistograma)
  })
  
  output$mensaje <- renderPrint({
    generarMensajeNormalidad(Agrodata, input$variableHistograma)
  })
  
  output$boxPlot <- renderPlotly({
    generarBoxPlot(Agrodata, input$variableBoxPlot)
  })
  
  output$estadisticas <- renderPrint({
    if (input$calcular > 0) {
      variable1 <- Agrodata[[input$year1]]
      variable2 <- Agrodata[[input$year2]]
      
      resultados <- prueba_t(variable1, variable2)
      cat("Estadístico t:", resultados$estadistico_t, "\n")
      cat("Valor p:", resultados$valor_p, "\n")
      cat("Intervalo de confianza:", resultados$intervalo_confianza, "\n")
    }
  })
  
  output$mensaje1 <- renderPrint({
    if (input$calcular > 0) {
      variable1 <- Agrodata[[input$year1]]
      variable2 <- Agrodata[[input$year2]]
      
      resultados <- prueba_t(variable1, variable2)
      cat(resultados$mensaje, "\n")
    }
  })
  
  filtered_data <- eventReactive(input$filtrar, {
    data <- Agrodata
    data_siembra <- data[data$VARIABLE == input$siembrapear & data$PROCESO == "Siembra", ]
    data_produccion <- data[data$VARIABLE == input$siembrapear & data$PROCESO == "Cosecha", ]
    data_filtered <- data.frame(Siembra = unlist(data_siembra[, 5:ncol(data_siembra)]), Produccion = unlist(data_produccion[, 5:ncol(data_produccion)]))
    data_filtered
  })
  
  output$pearsonPlot1 <- renderPlot({
    data <- filtered_data()
    
    if (is.null(data) || nrow(data) < 2) {
      plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "Variable de Siembra", ylab = "Variable de Producción",
           main = "No hay suficientes datos para graficar y calcular la correlación")
    } else {
      evaluar_correlacion_pearson(data)
    }
  })
  
  output$correlationOutput1 <- renderPrint({
    data <- filtered_data()
    
    if (is.null(data) || nrow(data) < 2) {
      "No hay suficientes datos para calcular la correlación"
    } else {
      evaluar_correlacion_pearson(data)
    }
  })
  
  # Realiza la predicción y grafica la serie de tiempo
  observeEvent(input$predecir, {
    datos_filtrados <- Agrodata %>%
      filter(VARIABLE == input$cultivo, grepl("Produccion", PROCESO))
    
    # Filtrar las columnas correspondientes a los años
    datos_variable <- datos_filtrados %>%
      select(matches("^\\d{4}$")) %>%
      na.omit()
    
    # Obtener los años y los valores de la serie de tiempo
    años <- as.numeric(colnames(datos_variable))
    valores <- as.numeric(unlist(datos_variable))
    serie_tiempo <- ts(valores, start = c(min(años), 1), frequency = 1)
    
    if (length(valores) > 0) {
      # Ajustar un modelo de predicción
      modelo <- ets(serie_tiempo)
      
      # Obtener la predicción
      prediccion <- forecast(modelo, h = 3)
      
      # Graficar la serie de tiempo y la predicción
      output$serie_tiempo_plot <- renderPlot({
        plot(prediccion, main = "Serie de Tiempo y Predicción", xlab = "Año", ylab = "Producción (toneladas)")
      })
    } else {
      # Si no hay suficientes observaciones, muestra un mensaje de error
      output$serie_tiempo_plot <- renderPlot({
        plot(0, main = "Error: No hay suficientes observaciones para realizar la predicción", type = "n", xlab = "", ylab = "")
        text(0, 0, "No hay suficientes observaciones", col = "red", cex = 1.5)
      })
    }
  })
  
  # Función para ajustar el modelo lineal y obtener el resumen
  obtener_resumen_modelo <- function(datos_variable3, anio) {
    modelo <- lm(as.formula(paste0("`", anio, "`", " ~ 1")), data = datos_variable3)
    return(summary(modelo))
  }
  
  # Observador reactivo para actualizar el gráfico y la salida al cambiar la variable o el año seleccionado
  output$grafico1 <- renderPlot({
    # Filtrar los datos para la variable y el año seleccionado
    datos_variable3 <- Agrodata %>% filter(VARIABLE == input$variable3)
    
    # Ajustar el modelo lineal
    modelo <- lm(as.formula(paste0("TOTAL ~ `", input$anio, "`")), data = datos_variable3)
    
    # Gráfico con línea de regresión
    plot(x = datos_variable3[[input$anio]], y = datos_variable3$TOTAL,
         xlab = paste("Año", input$anio), ylab = "Total", pch = 16,
         main = paste("Total vs.", input$anio, "para", input$variable3))
    
    # Agregar línea de regresión al gráfico
    abline(modelo, col = "red")
  })
  
  output$modelo_summary <- renderPrint({
    # Filtrar los datos para la variable y el año seleccionado
    datos_variable3 <- Agrodata %>% filter(VARIABLE == input$variable3)
    resumen_modelo <- obtener_resumen_modelo(datos_variable3, input$anio)
    
    # Mostrar el resumen del modelo lineal
    return(resumen_modelo)
  })
  
  output$mensaje_linealidad <- renderText({
    # Filtrar los datos para la variable y el año seleccionado
    datos_variable3 <- Agrodata %>% filter(VARIABLE == input$variable3)
    
    # Ajustar el modelo lineal
    modelo <- lm(as.formula(paste0("TOTAL ~ `", input$anio, "`")), data = datos_variable3)
    
    # Calcular el coeficiente de determinación (R cuadrado)
    r_squared <- summary(modelo)$r.squared
    
    # Verificar si el coeficiente de determinación es cercano a 1 (indicando linealidad)
    if (abs(r_squared - 1) < 0.1) {
      mensaje_linealidad <- "Los datos presentan linealidad."
    } else {
      mensaje_linealidad <- "Los datos no presentan linealidad."
    }
    
    # Retornar el mensaje
    return(mensaje_linealidad)
  })
  
})

shinyApp(ui, server)
