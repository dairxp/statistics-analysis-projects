# Statistics Analysis Projects

Colección de proyectos de análisis estadístico con dashboards interactivos en Streamlit.

---

## Dashboard 1 - Diagnóstico de Gripe con Naive Bayes

Clasifica pacientes en **con gripe** o **salud normal** a partir de signos vitales, usando el algoritmo Naive Bayes (gaussiano y categórico).

**Variables analizadas:** temperatura, frecuencia cardiaca, saturación de oxígeno, glicemia, tensión arterial.

**Funcionalidades:**
- Comparación entre datos originales (con outliers) y datos limpios
- Control del porcentaje de datos de prueba
- Visualización de la matriz de confusión y métricas de clasificación

**Tecnologías:** Python, Streamlit, scikit-learn, Plotly, pandas

**Demo:** https://redes-bayesinas-afecciones-respiratorias.streamlit.app/

---

## Dashboard 2 - Brecha Digital Móvil en Perú 2023

Analiza la cobertura de telefonía móvil en el Perú a nivel de departamento, zona (urbana/rural) y operadora, con datos de OSIPTEL (marzo 2023).

**Funcionalidades:**
- Mapa de calor interactivo de cobertura por departamento
- Comparación de operadoras por zona geográfica
- Pruebas estadísticas: chi-cuadrado, Kruskal-Wallis, Mann-Whitney
- Regresión para identificar factores asociados a la brecha digital

**Tecnologías:** Python, Streamlit, Plotly, Folium, scipy, statsmodels, pandas

**Demo:** https://brecha-digital-peru.streamlit.app/

---

**Fuente de datos:** OSIPTEL / Plataforma Nacional de Datos Abiertos del Perú

---

## Dashboard 3 - Análisis Agrícola Agropuno

Aplicación web interactiva para el análisis de datos de siembra y producción agrícola en las 13 provincias de la región de Puno (1997 - 2021).

**Funcionalidades:**
- Visualización de histogramas, gráficos circulares y polígonos de frecuencia por provincia y cultivo
- Evaluación estadística inteligente con detección automática de normalidad (Shapiro-Wilk)
- Pruebas de hipótesis (T-Student y Mann-Whitney) y correlaciones (Pearson y Spearman)
- Análisis de tendencia histórica de producción (Regresión Lineal) a lo largo de los años

**Tecnologías:** R, Shiny, Plotly, dplyr, ggplot2

**Demo:** https://dairxp.shinyapps.io/Agropuno/

**Fuente de datos:** [Estadística Agraria Informática (Agropuno)](https://www.agropuno.gob.pe/estadistica-agraria-informatica/agricola/)
