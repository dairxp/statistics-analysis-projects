# Dashboard Diagnóstico de Gripe — Naive Bayes

Proyecto de Estadística Bayesiana que analiza signos vitales de pacientes
para clasificar si tienen **gripe** o están en **salud normal**, usando el
algoritmo Naive Bayes.

---

## Archivos del proyecto

```
AAAA GRIPE/
│
├── streamlit_app/          <- APLICACION PYTHON 
│   ├── app.py              <- Dashboard Streamlit principal
│   ├── gripe.xlsx          <- Dataset: 5,725 pacientes con signos vitales
│   ├── requirements.txt    <- Paquetes necesarios (versiones exactas)
│   └── venv/               <- Entorno virtual Python (ya instalado)
│
├── gripe.R                 <- Script R original: preprocesamiento
├── covidEnfermo bayes.R    <- Script R original: modelo Naive Bayes
├── gripe preparada.csv     <- Datos ya discretizados (salida de gripe.R)
├── maladie_observations.csv<- Dataset fuente (mismo contenido que gripe.xlsx)
├── gripe resultados.txt    <- Resultados del modelo en Netica (error: 8.9%)
└── t.csv                   <- Datos procesados con etiquetas de texto
```

---

## Dataset

| Variable | Columna | Categorias |
|----------|---------|------------|
| Temperatura (C) | T | Normal (<37.2) / Alta (>=37.2) |
| Frecuencia Cardiaca (bpm) | F | Baja (<60) / Normal (60-100) / Alta (>100) |
| Oxigeno (%) | O | Crit. baja (<90) / Baja (90-95) / Normal (>=95) |
| Glicemia (mg/dL) | G | Baja (<70) / Normal (70-99) / Alta (>=99) |
| Tension (mmHg) | E | Baja (<90) / Normal (90-120) / Elevada (120-129) / Alta (>129) |
| **Etiqueta** | **S** | **0 = Salud Normal** / **1 = Con Gripe** |

> **Nota importante:** S=1 = pacientes CON GRIPE (fiebre >=37.2 y taquicardia).
> S=0 = salud normal (sin gripe). Confirmado con Netica: error rate 8.9%, AUC=0.9592.

---

## Como ejecutar la aplicacion

```bash
source venv/Scripts/activate
pip install -r requirements.txt
streamlit run app.py
```

### 4. Ver en el navegador

Streamlit abre automaticamente: **http://localhost:8501**

> Si no abre solo, copia esa direccion en tu navegador.

---

## Contenido del Dashboard (4 pestanas)

| Pestana | Que muestra |
|---------|-------------|
| **Exploracion** | Metricas generales, tabla de datos, distribucion Sano/Enfermo |
| **Distribuciones** | Boxplots e histogramas interactivos por variable |
| **Modelo Naive Bayes** | Accuracy, matriz de confusion, reporte de clasificacion |
| **Prediccion Manual** | Sliders de signos vitales -> predice Con Gripe o Salud Normal |

### Sidebar (panel izquierdo)
- **Modo de datos:** Original (con outliers) vs Limpio (outliers reemplazados)
- **% datos de prueba:** Ajusta el split entrenamiento/prueba (default 70/30)

---

## Rendimiento del modelo

| Metrica | Valor |
|---------|-------|
| Error rate (Netica) | 8.94% |
| AUC (Netica) | 0.9592 |
| Accuracy (Python sklearn) | ~91% |

---

## Tecnologias usadas

| Herramienta | Uso |
|-------------|-----|
| R 4.4.3 | Exploracion y preprocesamiento original |
| Netica 7.01 | Validacion del modelo bayesiano |
| Python 3.12 | Reimplementacion del analisis |
| Streamlit | Dashboard interactivo web |
| scikit-learn | Modelo Naive Bayes (GaussianNB) |
| Plotly | Graficas interactivas |
| Pandas | Manejo de datos |

---

## Referencia Streamlit

Documentacion oficial: https://docs.streamlit.io
