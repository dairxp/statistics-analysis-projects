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
    select(matches("^\\d{4}$"))
  
  total_por_anio <- colSums(filtered_data, na.rm = TRUE)
  
  production_data <- data.frame(
    YEAR = as.numeric(names(total_por_anio)),
    TOTAL = as.numeric(total_por_anio)
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
    select(matches("^\\d{4}$"))
  
  total_por_anio <- colSums(filtered_data, na.rm = TRUE)
  
  production_data <- data.frame(
    YEAR = as.numeric(names(total_por_anio)),
    TOTAL = as.numeric(total_por_anio)
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
    select(matches("^\\d{4}$"))
  
  total_por_anio <- colSums(filtered_data, na.rm = TRUE)
  
  production_data <- data.frame(
    YEAR = as.numeric(names(total_por_anio)),
    TOTAL = as.numeric(total_por_anio)
  )
  
  plot_ly(production_data, y = ~TOTAL, type = 'box',
          marker = list(color = 'blue')) %>%
    layout(title = list(text = paste("Box Plot de Producción Total para", variable),
                        y = 0.97),
           xaxis = list(title = "Año"),
           yaxis = list(title = "Producción Total"))
}

prueba_t <- function(grupo1, grupo2) {
  grupo1 <- na.omit(as.numeric(grupo1))
  grupo2 <- na.omit(as.numeric(grupo2))
  
  if(length(grupo1) < 3 || length(grupo2) < 3) {
      return(list(
          nombre_prueba = "Error", estadistico = NA, valor_p = NA,
          intervalo_confianza = c(NA, NA),
          mensaje = "No hay suficientes datos (mínimo 3) para realizar la prueba."
      ))
  }
  
  norm1 <- shapiro.test(grupo1)$p.value > 0.05
  norm2 <- shapiro.test(grupo2)$p.value > 0.05
  
  if (norm1 && norm2) {
    result <- t.test(grupo1, grupo2)
    nombre_prueba <- "Prueba T de Student (Paramétrica)"
  } else {
    result <- suppressWarnings(wilcox.test(grupo1, grupo2, conf.int = TRUE, exact = FALSE))
    nombre_prueba <- "Prueba U de Mann-Whitney (No Paramétrica)"
  }
  
  resultado <- list(
    nombre_prueba = nombre_prueba,
    estadistico = result$statistic,
    valor_p = result$p.value,
    intervalo_confianza = if(is.null(result$conf.int)) c(NA, NA) else result$conf.int,
    mensaje = ifelse(result$p.value < 0.05, 
                     paste(nombre_prueba, "- Hay evidencia para rechazar la hipótesis nula. Las diferencias son significativas."), 
                     paste(nombre_prueba, "- No hay suficiente evidencia. No hay diferencias significativas."))
  )
  
  return(resultado)
}

evaluar_correlacion <- function(data) {
  variable1 <- na.omit(as.numeric(data[, "Siembra"]))
  variable2 <- na.omit(as.numeric(data[, "Produccion"]))
  
  if(length(variable1) < 3 || length(variable2) < 3) {
    return("No hay suficientes datos numéricos válidos para calcular la correlación.")
  }
  
  if(sd(variable1) == 0 || sd(variable2) == 0) {
    plot(variable1, variable2, main = "Datos Constantes", xlab = "Siembra", ylab = "Producción", pch = 16)
    return("Una de las variables es constante (no tiene variación). No se puede calcular la correlación.")
  }
  
  norm1 <- shapiro.test(variable1)$p.value > 0.05
  norm2 <- shapiro.test(variable2)$p.value > 0.05
  
  if (norm1 && norm2) {
    metodo <- "pearson"
    nombre_metodo <- "Pearson (Paramétrica)"
  } else {
    metodo <- "spearman"
    nombre_metodo <- "Spearman (No Paramétrica)"
  }
  
  test_result <- suppressWarnings(cor.test(variable1, variable2, method = metodo))
  
  if (test_result$p.value < 0.05) {
    mensaje <- paste("Correlación", nombre_metodo, ": Hay evidencia para afirmar una relación significativa (p < 0.05). rho/r =", round(test_result$estimate, 3))
  } else {
    mensaje <- paste("Correlación", nombre_metodo, ": No hay evidencia suficiente para afirmar una relación significativa. rho/r =", round(test_result$estimate, 3))
  }
  
  plot(variable1, variable2, main = paste("Dispersión -", nombre_metodo), xlab = "Siembra", ylab = "Producción", pch = 16, col="blue")
  abline(lm(variable2 ~ variable1), col = "red", lwd=2)
  
  return(mensaje)
}

Agrodata <- read_excel("Agrodata.xlsx")
year_cols <- grep("^\\d{4}$", colnames(Agrodata))
if(length(year_cols) > 0) {
  Agrodata$TOTAL <- rowSums(Agrodata[, year_cols], na.rm = TRUE)
}

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
                            tags$a(href = "https://www.linkedin.com/in/aldair-maquera-andrade", tags$i(class = "bx bxl-linkedin")),
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
                                                              choices = c("Todas", unique(data$PROVINCIA)),
                                                              selected = "Todas"),
                                                  
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
                                  inputId = "produccionpear",
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
                                selectInput("variable3", "Seleccione el Cultivo para Análisis de Tendencia:",
                                            choices = unique(Agrodata$VARIABLE))
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
    if ("Todas" %in% input$province) {
      subset(data, PROCESO == input$process)
    } else {
      subset(data, PROVINCIA %in% input$province & PROCESO == input$process)
    }
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
      summarize(Produccion_Total = sum(TOTAL, na.rm = TRUE)) %>%
      arrange(desc(Produccion_Total))
    
    plot_ly(datos_agrupados, x = ~VARIABLE, y = ~Produccion_Total, type = 'bar', color = ~VARIABLE) %>%
      layout(title = "Total por VARIABLE", xaxis = list(title = "VARIABLE", tickangle = -45, categoryorder = "total descending"), yaxis = list(title = "Total"), showlegend = FALSE)
  })
  
  output$graficoBarrasPROVINCIA <- renderPlotly({
    datos_agrupados <- data %>%
      group_by(PROVINCIA) %>%
      summarize(Produccion_Total = sum(TOTAL, na.rm = TRUE)) %>%
      arrange(desc(Produccion_Total))
    
    plot_ly(datos_agrupados, x = ~PROVINCIA, y = ~Produccion_Total, type = 'bar', color = ~PROVINCIA) %>%
      layout(title = "Total por PROVINCIA", xaxis = list(title = "PROVINCIA", tickangle = -45, categoryorder = "total descending"), yaxis = list(title = "Total"), showlegend = FALSE)
  })
  
  output$graficoBarrasPROCESO <- renderPlotly({
    datos_agrupados <- data %>%
      group_by(PROCESO) %>%
      summarize(Produccion_Total = sum(TOTAL, na.rm = TRUE)) %>%
      arrange(desc(Produccion_Total))
    
    plot_ly(datos_agrupados, x = ~PROCESO, y = ~Produccion_Total, type = 'bar', color = ~PROCESO) %>%
      layout(title = "Total por PROCESO", xaxis = list(title = "PROCESO", tickangle = -45, categoryorder = "total descending"), yaxis = list(title = "Total"), showlegend = FALSE)
  })
  
  output$graficoBarrasUnidad <- renderPlotly({
    datos_agrupados <- data %>%
      group_by(`Unidad de medida`) %>%
      summarize(Produccion_Total = sum(TOTAL, na.rm = TRUE)) %>%
      arrange(desc(Produccion_Total))
    
    plot_ly(datos_agrupados, x = ~`Unidad de medida`, y = ~Produccion_Total, type = 'bar', color = ~`Unidad de medida`) %>%
      layout(title = "Total por Unidad de medida", xaxis = list(title = "Unidad de medida", tickangle = -45, categoryorder = "total descending"), yaxis = list(title = "Total"), showlegend = FALSE)
  })
  
  output$graficoCircularVARIABLE <- renderPlotly({
    datos_agrupados <- data %>%
      group_by(VARIABLE) %>%
      summarize(Produccion_Total = sum(TOTAL, na.rm = TRUE)) %>%
      arrange(desc(Produccion_Total))
    
    p <- plot_ly(datos_agrupados, labels = ~VARIABLE, values = ~Produccion_Total, type = "pie") %>%
      layout(title = "Total por VARIABLE")
    
    p
  })
  
  output$graficoCircularPROVINCIA <- renderPlotly({
    datos_agrupados <- data %>%
      group_by(PROVINCIA) %>%
      summarize(Produccion_Total = sum(TOTAL, na.rm = TRUE)) %>%
      arrange(desc(Produccion_Total))
    
    p <- plot_ly(datos_agrupados, labels = ~PROVINCIA, values = ~Produccion_Total, type = "pie") %>%
      layout(title = "Total por PROVINCIA")
    
    p
  })
  
  output$graficoCircularPROCESO <- renderPlotly({
    datos_agrupados <- data %>%
      group_by(PROCESO) %>%
      summarize(Produccion_Total = sum(TOTAL, na.rm = TRUE)) %>%
      arrange(desc(Produccion_Total))
    
    p <- plot_ly(datos_agrupados, labels = ~PROCESO, values = ~Produccion_Total, type = "pie") %>%
      layout(title = "Total por PROCESO")
    
    p
  })
  
  output$graficoCircularUnidad <- renderPlotly({
    datos_agrupados <- data %>%
      group_by(`Unidad de medida`) %>%
      summarize(Produccion_Total = sum(TOTAL, na.rm = TRUE)) %>%
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
      cat("Prueba aplicada:", resultados$nombre_prueba, "\n")
      cat("Estadístico:", resultados$estadistico, "\n")
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
    data_siembra <- data[data$VARIABLE == input$siembrapear & grepl("Siembra", data$PROCESO, ignore.case = TRUE), ]
    data_produccion <- data[data$VARIABLE == input$produccionpear & grepl("Producci", data$PROCESO, ignore.case = TRUE), ]
    
    year_cols_s <- grep("^\\d{4}$", colnames(data_siembra))
    year_cols_p <- grep("^\\d{4}$", colnames(data_produccion))
    
    data_filtered <- data.frame(Siembra = as.numeric(unlist(data_siembra[, year_cols_s])), 
                                Produccion = as.numeric(unlist(data_produccion[, year_cols_p])))
    data_filtered
  })
  
  output$pearsonPlot1 <- renderPlot({
    data <- filtered_data()
    
    if (is.null(data) || nrow(data) < 2) {
      plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "Variable de Siembra", ylab = "Variable de Producción",
           main = "No hay suficientes datos para graficar y calcular la correlación")
    } else {
      evaluar_correlacion(data)
    }
  })
  
  output$correlationOutput1 <- renderPrint({
    data <- filtered_data()
    
    if (is.null(data) || nrow(data) < 2) {
      "No hay suficientes datos para calcular la correlación"
    } else {
      evaluar_correlacion(data)
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
  
  # Observador reactivo para actualizar el gráfico y la salida al cambiar la variable seleccionada
  output$grafico1 <- renderPlot({
    datos_variable3 <- Agrodata %>% filter(VARIABLE == input$variable3 & grepl("Producci", PROCESO, ignore.case = TRUE))
    year_cols <- grep("^\\d{4}$", colnames(datos_variable3), value = TRUE)
    
    if(length(year_cols) > 0 && nrow(datos_variable3) > 0) {
      years <- as.numeric(year_cols)
      produccion <- colSums(datos_variable3[, year_cols], na.rm = TRUE)
      
      reg_data <- data.frame(Year = years, Produccion = produccion)
      modelo <- lm(Produccion ~ Year, data = reg_data)
      
      plot(x = reg_data$Year, y = reg_data$Produccion,
           xlab = "Año", ylab = "Producción Total (Cosecha)", pch = 16, col = "blue",
           main = paste("Tendencia Histórica de Producción para", input$variable3))
      
      abline(modelo, col = "red", lwd = 2)
    }
  })
  
  output$modelo_summary <- renderPrint({
    datos_variable3 <- Agrodata %>% filter(VARIABLE == input$variable3 & grepl("Producci", PROCESO, ignore.case = TRUE))
    year_cols <- grep("^\\d{4}$", colnames(datos_variable3), value = TRUE)
    
    if(length(year_cols) > 0 && nrow(datos_variable3) > 0) {
      years <- as.numeric(year_cols)
      produccion <- colSums(datos_variable3[, year_cols], na.rm = TRUE)
      
      reg_data <- data.frame(Year = years, Produccion = produccion)
      modelo <- lm(Produccion ~ Year, data = reg_data)
      
      return(summary(modelo))
    } else {
      cat("No hay datos suficientes para el análisis.\n")
    }
  })
  
  output$mensaje_linealidad <- renderText({
    datos_variable3 <- Agrodata %>% filter(VARIABLE == input$variable3 & grepl("Producci", PROCESO, ignore.case = TRUE))
    year_cols <- grep("^\\d{4}$", colnames(datos_variable3), value = TRUE)
    
    if(length(year_cols) > 0 && nrow(datos_variable3) > 0) {
      years <- as.numeric(year_cols)
      produccion <- colSums(datos_variable3[, year_cols], na.rm = TRUE)
      
      reg_data <- data.frame(Year = years, Produccion = produccion)
      modelo <- lm(Produccion ~ Year, data = reg_data)
      
      p_value <- summary(modelo)$coefficients[2, 4]
      r_squared <- summary(modelo)$r.squared
      
      if (p_value < 0.05) {
        tendencia <- ifelse(coef(modelo)[2] > 0, "Creciente", "Decreciente")
        mensaje <- paste("Tendencia Significativa:", tendencia, "en el tiempo con un R² de", round(r_squared, 3))
      } else {
        mensaje <- paste("No hay una tendencia lineal significativa en el tiempo (p > 0.05).")
      }
      return(mensaje)
    }
  })
  
})

shinyApp(ui, server)
