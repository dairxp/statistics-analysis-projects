import streamlit as st
import pandas as pd
import numpy as np
import plotly.express as px
import plotly.graph_objects as go
from sklearn.naive_bayes import GaussianNB, CategoricalNB
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix, accuracy_score, classification_report

st.set_page_config(page_title="Dashboard Gripe - Naive Bayes", layout="wide")

st.title("Dashboard Diagnóstico de Gripe")
st.markdown("Análisis de signos vitales y clasificación con **Naive Bayes**")

# ─────────────────────────────────────────────
# CARGA DE DATOS
# ─────────────────────────────────────────────
@st.cache_data
def cargar_datos():
    df = pd.read_excel("gripe.xlsx")
    df = df[["temperatura", "frecuencia_cardíaca", "oxigeno", "glicemia", "tensión", "etiqueta"]]
    df.columns = ["T", "F", "O", "G", "E", "S"]
    return df

df_raw = cargar_datos()

# ─────────────────────────────────────────────
# SIDEBAR - CONTROLES
# ─────────────────────────────────────────────
st.sidebar.header("Configuración")
modo = st.sidebar.radio(
    "Modo de datos",
    ["Datos originales (con outliers)", "Datos limpios (reemplazar extremos)"]
)
test_size = st.sidebar.slider("% datos de prueba", 10, 40, 30) / 100

# ─────────────────────────────────────────────
# PREPROCESAMIENTO
# ─────────────────────────────────────────────
@st.cache_data
def preprocesar(df, limpiar=False):
    df = df.copy()

    if limpiar:
        rangos = {"T": (35, 45), "F": (50, 100), "O": (90, 120), "G": (70, 120)}
        for col, (lo, hi) in rangos.items():
            mask = df[col].isna() | (df[col] < lo) | (df[col] > hi)
            df.loc[mask, col] = np.random.uniform(lo, hi, mask.sum())

    df = df.dropna()

    # Discretizar
    df["T_cat"] = pd.cut(df["T"], bins=[-np.inf, 37.2, np.inf], labels=["Normal", "Alta"])
    df["F_cat"] = pd.cut(df["F"], bins=[-np.inf, 60, 100, np.inf], labels=["Baja", "Normal", "Alta"])
    df["O_cat"] = pd.cut(df["O"], bins=[-np.inf, 90, 95, np.inf], labels=["Crít. baja", "Baja", "Normal"])
    df["G_cat"] = pd.cut(df["G"], bins=[-np.inf, 70, 99, np.inf], labels=["Baja", "Normal", "Alta"])
    df["E_cat"] = pd.cut(df["E"], bins=[-np.inf, 90, 120, 129, np.inf], labels=["Baja", "Normal", "Elevada", "Alta"])
    df["S_label"] = df["S"].map({0: "Salud Normal", 1: "Con Gripe"})

    # Codificar a numérico para modelo
    df["T_num"] = df["T_cat"].cat.codes + 1
    df["F_num"] = df["F_cat"].cat.codes + 1
    df["O_num"] = df["O_cat"].cat.codes + 1
    df["G_num"] = df["G_cat"].cat.codes + 1
    df["E_num"] = df["E_cat"].cat.codes + 1

    return df

limpiar = (modo == "Datos limpios (reemplazar extremos)")
df = preprocesar(df_raw, limpiar=limpiar)

# ─────────────────────────────────────────────
# TABS
# ─────────────────────────────────────────────
tab1, tab2, tab3, tab4 = st.tabs(["Exploración", "Distribuciones", "Modelo Naive Bayes", "Predicción Manual"])

# ══════════════════════════════════════════════
# TAB 1: EXPLORACIÓN
# ══════════════════════════════════════════════
with tab1:
    st.subheader("Vista General del Dataset")

    col1, col2, col3, col4 = st.columns(4)
    col1.metric("Total registros", len(df))
    col2.metric("Con Gripe (S=1)", int((df["S"] == 1).sum()), f"{(df['S']==1).mean()*100:.1f}%")
    col3.metric("Salud Normal (S=0)", int((df["S"] == 0).sum()), f"{(df['S']==0).mean()*100:.1f}%")
    col4.metric("Variables", 5)

    st.markdown("---")
    col_l, col_r = st.columns(2)

    with col_l:
        st.markdown("**Muestra de datos (primeras 10 filas)**")
        st.dataframe(
            df[["T", "F", "O", "G", "E", "S_label", "T_cat", "F_cat", "O_cat", "G_cat", "E_cat"]].head(10),
            use_container_width=True
        )

    with col_r:
        st.markdown("**Resumen estadístico (datos numéricos)**")
        st.dataframe(df[["T", "F", "O", "G", "E"]].describe().round(2), use_container_width=True)

    st.markdown("---")
    st.subheader("Distribución de Etiqueta (Con Gripe / Salud Normal)")
    fig_bar = px.bar(
        df["S_label"].value_counts().reset_index(),
        x="S_label", y="count",
        color="S_label",
        color_discrete_map={"Con Gripe": "salmon", "Salud Normal": "steelblue"},
        labels={"S_label": "Estado", "count": "Frecuencia"},
        title="Pacientes por Estado de Salud"
    )
    st.plotly_chart(fig_bar, use_container_width=True)

# ══════════════════════════════════════════════
# TAB 2: DISTRIBUCIONES
# ══════════════════════════════════════════════
with tab2:
    st.subheader("Distribución de Variables por Estado de Salud")

    variable = st.selectbox(
        "Selecciona variable numérica para boxplot",
        ["Temperatura (T)", "Frecuencia Cardíaca (F)", "Oxígeno (O)", "Glicemia (G)", "Tensión (E)"]
    )
    var_map = {
        "Temperatura (T)": "T",
        "Frecuencia Cardíaca (F)": "F",
        "Oxígeno (O)": "O",
        "Glicemia (G)": "G",
        "Tensión (E)": "E"
    }
    var_col = var_map[variable]

    col_box, col_hist = st.columns(2)
    with col_box:
        fig_box = px.box(
            df, x="S_label", y=var_col,
            color="S_label",
            color_discrete_map={"Con Gripe": "lightcoral", "Salud Normal": "lightblue"},
            title=f"Boxplot: {variable} por Estado",
            labels={"S_label": "Estado", var_col: variable}
        )
        st.plotly_chart(fig_box, use_container_width=True)

    with col_hist:
        fig_hist = px.histogram(
            df, x=var_col, color="S_label",
            barmode="overlay", opacity=0.7,
            color_discrete_map={"Con Gripe": "lightcoral", "Salud Normal": "lightblue"},
            title=f"Histograma: {variable}",
            labels={var_col: variable, "S_label": "Estado"}
        )
        st.plotly_chart(fig_hist, use_container_width=True)

    st.markdown("---")
    st.subheader("Histogramas de Variables Categorizadas")

    cat_cols = {
        "Temperatura": ("T_cat", "skyblue"),
        "Frec. Cardíaca": ("F_cat", "lightgreen"),
        "Oxígeno": ("O_cat", "lightcoral"),
        "Glicemia": ("G_cat", "gold"),
        "Tensión": ("E_cat", "lightpink"),
    }

    cols = st.columns(3)
    for i, (nombre, (col, color)) in enumerate(cat_cols.items()):
        counts = df[col].value_counts().sort_index().reset_index()
        counts.columns = [nombre, "Frecuencia"]
        fig = px.bar(counts, x=nombre, y="Frecuencia",
                     color_discrete_sequence=[color],
                     title=f"Distribución: {nombre}")
        cols[i % 3].plotly_chart(fig, use_container_width=True)

# ══════════════════════════════════════════════
# TAB 3: MODELO NAIVE BAYES
# ══════════════════════════════════════════════
with tab3:
    st.subheader("Modelo Naive Bayes - Clasificación")

    X = df[["T_num", "F_num", "O_num", "G_num", "E_num"]].values
    y = df["S"].values

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=test_size, random_state=42
    )

    modelo = GaussianNB()
    modelo.fit(X_train, y_train)
    y_pred = modelo.predict(X_test)

    acc = accuracy_score(y_test, y_pred)
    cm = confusion_matrix(y_test, y_pred)

    col_met1, col_met2, col_met3, col_met4 = st.columns(4)
    col_met1.metric("Precisión (Accuracy)", f"{acc*100:.2f}%")
    col_met2.metric("Datos entrenamiento", len(X_train))
    col_met3.metric("Datos prueba", len(X_test))
    col_met4.metric("Split", f"{int((1-test_size)*100)}/{int(test_size*100)}")

    st.markdown("---")
    col_cm, col_rep = st.columns(2)

    with col_cm:
        st.markdown("**Matriz de Confusión**")
        fig_cm = px.imshow(
            cm,
            text_auto=True,
            labels=dict(x="Predicción", y="Real", color="Cantidad"),
            x=["Salud Normal (0)", "Con Gripe (1)"],
            y=["Salud Normal (0)", "Con Gripe (1)"],
            color_continuous_scale="Blues",
            title="Matriz de Confusión"
        )
        st.plotly_chart(fig_cm, use_container_width=True)

    with col_rep:
        st.markdown("**Reporte de Clasificación**")
        report = classification_report(y_test, y_pred, target_names=["Salud Normal", "Con Gripe"], output_dict=True)
        df_report = pd.DataFrame(report).transpose().round(3)
        st.dataframe(df_report, use_container_width=True)

        st.markdown("**Probabilidades A Priori del Modelo**")
        prior_df = pd.DataFrame({
            "Clase": ["Salud Normal (0)", "Con Gripe (1)"],
            "P(clase)": modelo.class_prior_.round(4)
        })
        st.dataframe(prior_df, use_container_width=True)

# ══════════════════════════════════════════════
# TAB 4: PREDICCIÓN MANUAL
# ══════════════════════════════════════════════
with tab4:
    st.subheader("Predecir Estado de un Nuevo Paciente")
    st.markdown("Ingresa los signos vitales del paciente:")

    col_a, col_b, col_c = st.columns(3)
    with col_a:
        temp = st.slider("Temperatura (°C)", 35.0, 45.0, 37.5, 0.1)
        frec = st.slider("Frecuencia Cardíaca (bpm)", 40, 150, 80)
    with col_b:
        oxig = st.slider("Oxígeno (%)", 85.0, 100.0, 97.0, 0.1)
        glic = st.slider("Glicemia (mg/dL)", 60.0, 130.0, 90.0, 0.5)
    with col_c:
        tens = st.slider("Tensión (mmHg)", 85, 145, 115)

    # Categorizar igual que el modelo
    def cat_T(v): return 1 if v < 37.2 else 2
    def cat_F(v): return 1 if v < 60 else (2 if v < 100 else 3)
    def cat_O(v): return 1 if v < 90 else (2 if v < 95 else 3)
    def cat_G(v): return 1 if v < 70 else (2 if v < 99 else 3)
    def cat_E(v): return 1 if v < 90 else (2 if v < 120 else (3 if v <= 129 else 4))

    entrada = np.array([[cat_T(temp), cat_F(frec), cat_O(oxig), cat_G(glic), cat_E(tens)]])

    if st.button("Predecir", type="primary"):
        # Reentrenar con todos los datos
        X_all = df[["T_num", "F_num", "O_num", "G_num", "E_num"]].values
        y_all = df["S"].values
        modelo_full = GaussianNB()
        modelo_full.fit(X_all, y_all)

        pred = modelo_full.predict(entrada)[0]
        proba = modelo_full.predict_proba(entrada)[0]

        estado = "CON GRIPE" if pred == 1 else "SALUD NORMAL"
        color = "red" if pred == 1 else "green"

        st.markdown(f"### Resultado: :{color}[{estado}]")

        col_p1, col_p2 = st.columns(2)
        col_p1.metric("P(Salud Normal)", f"{proba[0]*100:.1f}%")
        col_p2.metric("P(Con Gripe)", f"{proba[1]*100:.1f}%")

        # Mostrar categorías del paciente
        st.markdown("**Categorías asignadas al paciente:**")
        cats = pd.DataFrame({
            "Variable": ["Temperatura", "Frec. Cardíaca", "Oxígeno", "Glicemia", "Tensión"],
            "Valor": [temp, frec, oxig, glic, tens],
            "Categoría": [
                "Normal" if temp < 37.2 else "Alta",
                "Baja" if frec < 60 else ("Normal" if frec < 100 else "Alta"),
                "Crít. baja" if oxig < 90 else ("Baja" if oxig < 95 else "Normal"),
                "Baja" if glic < 70 else ("Normal" if glic < 99 else "Alta"),
                "Baja" if tens < 90 else ("Normal" if tens < 120 else ("Elevada" if tens <= 129 else "Alta"))
            ]
        })
        st.dataframe(cats, use_container_width=True)
