# Dashboard interactivo: Brecha Digital Movil en Peru 2023
# Fuente: OSIPTEL / Plataforma Nacional de Datos Abiertos del Peru
# Periodo: Marzo 2023 (202303)

import warnings
warnings.filterwarnings('ignore')

from pathlib import Path

import streamlit as st
import pandas as pd
import numpy as np
import plotly.express as px
import plotly.graph_objects as go
import folium
from folium.plugins import HeatMap
from streamlit_folium import st_folium
from scipy.stats import chi2_contingency, kruskal, mannwhitneyu
import statsmodels.api as sm

DATA_PATH = Path(__file__).parent / 'cobertura_movil.csv'

st.set_page_config(
    page_title="Brecha Digital Movil - Peru 2023",
    layout="wide",
    initial_sidebar_state="expanded"
)

st.markdown("""
<style>
    .block-container { padding-top: 1.2rem; padding-bottom: 1rem; max-width: 100%; padding-left: 2rem; padding-right: 2rem; }
    .section-label {
        font-size: 0.95rem;
        font-weight: 600;
        color: #0d1b2a;
        border-left: 3px solid #1565C0;
        padding-left: 0.5rem;
        margin-bottom: 0.75rem;
        margin-top: 0.5rem;
    }
    .stTabs [data-baseweb="tab"] {
        font-size: 0.82rem;
        font-weight: 600;
        padding: 6px 14px;
    }
    div[data-testid="stSidebarContent"] .stRadio label {
        font-size: 0.85rem;
    }
    div[data-testid="stSidebarContent"] .stSelectbox label {
        font-size: 0.85rem;
    }
</style>
""", unsafe_allow_html=True)

COSTA = [
    'LIMA', 'CALLAO', 'ICA', 'LAMBAYEQUE', 'LA LIBERTAD',
    'PIURA', 'TUMBES', 'AREQUIPA', 'MOQUEGUA', 'TACNA'
]
SIERRA = [
    'PUNO', 'CUSCO', 'JUNIN', 'ANCASH', 'CAJAMARCA',
    'AYACUCHO', 'APURIMAC', 'HUANCAVELICA', 'PASCO', 'HUANUCO'
]
SELVA = [
    'LORETO', 'UCAYALI', 'AMAZONAS', 'SAN MARTIN', 'MADRE DE DIOS'
]

COORDS_REGIONES = {
    'AMAZONAS':     (-6.23,  -77.87), 'ANCASH':       (-9.53,  -77.53),
    'APURIMAC':     (-13.63, -72.88), 'AREQUIPA':     (-16.40, -71.54),
    'AYACUCHO':     (-13.16, -74.22), 'CAJAMARCA':    (-7.16,  -78.51),
    'CALLAO':       (-12.05, -77.12), 'CUSCO':        (-13.53, -71.97),
    'HUANCAVELICA': (-12.78, -74.97), 'HUANUCO':      (-9.93,  -76.24),
    'ICA':          (-14.07, -75.73), 'JUNIN':        (-12.06, -75.20),
    'LA LIBERTAD':  (-8.11,  -79.03), 'LAMBAYEQUE':   (-6.77,  -79.84),
    'LIMA':         (-12.04, -77.03), 'LORETO':       (-3.75,  -73.25),
    'MADRE DE DIOS':(-12.59, -69.19), 'MOQUEGUA':     (-17.19, -70.93),
    'PASCO':        (-10.69, -76.26), 'PIURA':        (-5.19,  -80.63),
    'PUNO':         (-15.84, -70.02), 'SAN MARTIN':   (-6.49,  -76.37),
    'TACNA':        (-18.01, -70.25), 'TUMBES':       (-3.57,  -80.45),
    'UCAYALI':      (-8.39,  -74.55),
}

COLORES_TECH = {
    'Sin cobertura': '#b0bec5', '2G': '#ef9a9a',
    '3G': '#ffcc80', '4G': '#66bb6a', '5G': '#1565C0',
}

COLORES_ZONA = {'Costa': '#1565C0', 'Sierra': '#E65100', 'Selva': '#2E7D32'}


def asignar_zona(dep):
    if dep in COSTA:  return 'Costa'
    if dep in SIERRA: return 'Sierra'
    if dep in SELVA:  return 'Selva'
    return 'Otro'


# Carga y agrega los datos una sola vez — el resultado queda en cache
@st.cache_data
def cargar_datos():
    df = pd.read_csv(DATA_PATH, sep=';', encoding='latin-1')
    df = df.loc[:, ~df.columns.str.contains('Unnamed')]

    for col in ['2G', '3G', '4G', '5G']:
        df[col] = pd.to_numeric(df[col], errors='coerce').fillna(0).astype(int)

    cols_mbps = [c for c in df.columns if 'MBPS' in c]
    for c in cols_mbps:
        df[c] = pd.to_numeric(df[c], errors='coerce').fillna(0).astype(int)

    mas_mbps = [c for c in cols_mbps if c != 'HASTA_1_MBPS']
    df['MAS_1MBPS'] = df[mas_mbps[0]] if mas_mbps else 0

    for col in ['VOZ', 'SMS', 'MMS', 'HASTA_1_MBPS']:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors='coerce').fillna(0).astype(int)

    df['tiene_4G_o_5G'] = ((df['4G'] == 1) | (df['5G'] == 1)).astype(int)
    df['nivel_tecnologico'] = df[['2G', '3G', '4G', '5G']].apply(
        lambda r: (
            '5G' if r['5G'] == 1 else '4G' if r['4G'] == 1 else
            '3G' if r['3G'] == 1 else '2G' if r['2G'] == 1 else 'Sin cobertura'
        ), axis=1
    )
    df['zona'] = df['DEPARTAMENTO'].apply(asignar_zona)

    # Agregar a nivel de CCPP unico (mejor tecnologia entre todas las operadoras)
    ccpp = df.groupby(['DEPARTAMENTO', 'UBIGEO_CCPP']).agg(
        tiene_4G_5G=('tiene_4G_o_5G', 'max'),
        max_tech=('nivel_tecnologico', lambda x: (
            '5G' if '5G' in x.values else '4G' if '4G' in x.values else
            '3G' if '3G' in x.values else '2G' if '2G' in x.values else 'Sin cobertura'
        )),
        LATITUD=('LATITUD', 'first'),
        LONGITUD=('LONGITUD', 'first'),
    ).reset_index()
    ccpp['zona'] = ccpp['DEPARTAMENTO'].apply(asignar_zona)

    # Brecha digital por departamento
    brecha = ccpp.groupby('DEPARTAMENTO').agg(
        total_ccpp=('UBIGEO_CCPP', 'count'),
        con_4G_5G=('tiene_4G_5G', 'sum')
    ).reset_index()
    brecha['sin_4G_5G']     = brecha['total_ccpp'] - brecha['con_4G_5G']
    brecha['pct_brecha']    = (brecha['sin_4G_5G'] / brecha['total_ccpp'] * 100).round(1)
    brecha['pct_cobertura'] = (100 - brecha['pct_brecha']).round(1)
    brecha['zona']          = brecha['DEPARTAMENTO'].apply(asignar_zona)
    brecha = brecha.sort_values('pct_brecha', ascending=False).reset_index(drop=True)

    # Subconjuntos ya filtrados por zonas validas (excluye 'Otro')
    brecha_valida = brecha[brecha['zona'] != 'Otro'].copy()
    ccpp_valido   = ccpp[ccpp['zona'] != 'Otro'].copy()

    # Cobertura por operadora (pre-computada)
    op = df.groupby('EMPRESA_OPERADORA').agg(
        CCPP_cubiertos=('UBIGEO_CCPP', 'nunique'),
        Reg_2G=('2G', 'sum'), Reg_3G=('3G', 'sum'),
        Reg_4G=('4G', 'sum'), Reg_5G=('5G', 'sum'),
    ).sort_values('CCPP_cubiertos', ascending=False).reset_index()

    # Servicios nacionales (pre-computados)
    servicios = {
        'VOZ': round(df['VOZ'].mean() * 100, 1),
        'SMS': round(df['SMS'].mean() * 100, 1),
        'MMS': round(df['MMS'].mean() * 100, 1),
        'Hasta 1 Mbps': round(df['HASTA_1_MBPS'].mean() * 100, 1),
        'Mas de 1 Mbps': round(df['MAS_1MBPS'].mean() * 100, 1),
    }

    return df, ccpp, brecha, brecha_valida, ccpp_valido, op, servicios


# Regresion logistica — se cachea por separado porque es el calculo mas costoso
@st.cache_data
def ejecutar_regresion(n_obs_key):
    df_m = df[['DEPARTAMENTO', 'EMPRESA_OPERADORA', '4G']].copy()
    df_m['zona'] = df_m['DEPARTAMENTO'].apply(asignar_zona)
    df_m = df_m[df_m['zona'] != 'Otro']

    dummies_zona = pd.get_dummies(df_m['zona'], prefix='zona').astype(int)
    dummies_op   = pd.get_dummies(df_m['EMPRESA_OPERADORA'], prefix='op').astype(int)

    if 'zona_Costa' in dummies_zona.columns:
        dummies_zona = dummies_zona.drop('zona_Costa', axis=1)
    ref = [c for c in dummies_op.columns if 'Viettel' in c]
    if ref:
        dummies_op = dummies_op.drop(columns=ref)

    X = sm.add_constant(pd.concat([dummies_zona, dummies_op], axis=1).astype(float))
    y = df_m['4G'].values
    modelo = sm.Logit(y, X).fit(maxiter=300, disp=False)

    params, conf, pvals = modelo.params, modelo.conf_int(), modelo.pvalues
    tabla = pd.DataFrame({
        'Variable':   params.index,
        'OR':         np.exp(params).round(4),
        'IC inf 95%': np.exp(conf[0]).round(4),
        'IC sup 95%': np.exp(conf[1]).round(4),
        'p-valor':    pvals.round(4),
    })
    tabla['Sig.'] = tabla['p-valor'].apply(
        lambda p: '***' if p < 0.001 else '**' if p < 0.01 else '*' if p < 0.05 else 'ns'
    )
    return tabla, round(modelo.prsquared, 4), round(modelo.aic, 2), int(modelo.nobs)


df, ccpp, brecha, brecha_valida, ccpp_valido, op, servicios = cargar_datos()

# Encabezado
st.title("Brecha Digital Movil en Peru — Analisis por Departamento, Zona y Operadora (2023)")
st.caption("Fuente: OSIPTEL / Plataforma Nacional de Datos Abiertos del Peru — Periodo: Marzo 2023 (202303)")
st.divider()

# Panel lateral — filtros simplificados
with st.sidebar:
    st.markdown("**Filtros de analisis**")
    st.markdown("---")

    zona_sel = st.radio(
        "Zona geografica",
        options=["Todas las zonas", "Costa", "Sierra", "Selva"],
        index=0,
    )

    st.markdown("---")

    deps_validos = sorted(COSTA + SIERRA + SELVA)
    dep_sel = st.selectbox(
        "Departamento (caso de estudio)",
        options=deps_validos,
        index=deps_validos.index('PUNO'),
    )

    st.markdown("---")
    st.caption(
        "Zona: filtra los graficos de resumen y analisis geografico.\n\n"
        "Departamento: se aplica solo en la pestana 'Caso de estudio'."
    )

# Aplicar filtro de zona a los datos agregados (operacion sobre ~25 filas, no sobre 51k)
if zona_sel == "Todas las zonas":
    brecha_vista  = brecha_valida.copy()
    ccpp_vista    = ccpp_valido.copy()
else:
    brecha_vista  = brecha_valida[brecha_valida['zona'] == zona_sel].copy()
    ccpp_vista    = ccpp_valido[ccpp_valido['zona'] == zona_sel].copy()

prom_brecha_vista   = brecha_vista['pct_brecha'].mean()
nacional_4G_vista   = ccpp_vista['tiene_4G_5G'].mean() * 100
total_sin_vista     = (ccpp_vista['tiene_4G_5G'] == 0).sum()

# Definicion de pestanas
tab1, tab2, tab3, tab4, tab5, tab6 = st.tabs([
    "Resumen",
    "Analisis Geografico",
    "Operadoras",
    f"Caso: {dep_sel.title()}",
    "Pruebas Estadisticas",
    "Mapa Interactivo",
])


# PESTANA 1: Resumen
with tab1:
    # Indicador de filtro activo
    if zona_sel != "Todas las zonas":
        st.info(f"Mostrando datos filtrados por zona: **{zona_sel}** ({len(brecha_vista)} departamentos)")

    st.markdown('<div class="section-label">Indicadores generales</div>', unsafe_allow_html=True)

    c1, c2, c3, c4, c5 = st.columns(5)
    c1.metric("Registros totales", f"{len(df):,}")
    c2.metric("CCPP analizados", f"{ccpp_vista['UBIGEO_CCPP'].nunique():,}")
    c3.metric("Operadoras", df['EMPRESA_OPERADORA'].nunique())
    c4.metric("Cobertura 4G/5G", f"{nacional_4G_vista:.1f}%")
    c5.metric("CCPP sin 4G/5G", f"{total_sin_vista:,}")

    st.divider()

    col_izq, col_der = st.columns([3, 2])

    with col_izq:
        st.markdown('<div class="section-label">Brecha digital por departamento (% CCPP sin 4G ni 5G)</div>', unsafe_allow_html=True)

        colores_dep = []
        for dep in brecha_vista['DEPARTAMENTO']:
            if dep == dep_sel:
                colores_dep.append('#d32f2f')
            elif dep in COSTA:
                colores_dep.append(COLORES_ZONA['Costa'])
            elif dep in SIERRA:
                colores_dep.append(COLORES_ZONA['Sierra'])
            else:
                colores_dep.append(COLORES_ZONA['Selva'])

        fig1 = go.Figure()
        fig1.add_trace(go.Bar(
            x=brecha_vista['pct_brecha'],
            y=brecha_vista['DEPARTAMENTO'],
            orientation='h',
            marker_color=colores_dep,
            text=[f"{v}%" for v in brecha_vista['pct_brecha']],
            textposition='outside',
            customdata=brecha_vista[['total_ccpp', 'sin_4G_5G', 'zona']].values,
            hovertemplate=(
                "<b>%{y}</b><br>Brecha: %{x:.1f}%<br>"
                "Total CCPP: %{customdata[0]:,}<br>"
                "Sin 4G/5G: %{customdata[1]:,}<br>"
                "Zona: %{customdata[2]}<extra></extra>"
            ),
        ))
        fig1.add_vline(
            x=prom_brecha_vista, line_dash='dash', line_color='#F9A825',
            annotation_text=f"Promedio: {prom_brecha_vista:.1f}%",
            annotation_position="top right", annotation_font_size=11,
        )
        n_dep = len(brecha_vista)
        alto = max(380, n_dep * 24)
        fig1.update_layout(
            height=alto,
            margin=dict(l=5, r=75, t=10, b=20),
            xaxis_title="% CCPP sin 4G ni 5G",
            xaxis_range=[0, 120],
            yaxis={'categoryorder': 'total ascending'},
            plot_bgcolor='white', paper_bgcolor='white',
            showlegend=False, font=dict(size=11),
        )
        fig1.update_xaxes(gridcolor='#f0f0f0')
        st.plotly_chart(fig1, width='stretch')

    with col_der:
        st.markdown('<div class="section-label">Tecnologia disponible por departamento</div>', unsafe_allow_html=True)

        orden_tech = ['Sin cobertura', '2G', '3G', '4G', '5G']
        pivot = (
            ccpp_vista.groupby(['DEPARTAMENTO', 'max_tech'])
            .size().unstack(fill_value=0)
            .reindex(columns=[c for c in orden_tech if c in ccpp_vista['max_tech'].unique()])
        )
        pivot_melt = pivot.reset_index().melt(
            id_vars='DEPARTAMENTO', var_name='Tecnologia', value_name='CCPP'
        )
        fig2 = px.bar(
            pivot_melt,
            x='CCPP', y='DEPARTAMENTO', color='Tecnologia',
            orientation='h',
            color_discrete_map=COLORES_TECH,
            category_orders={'Tecnologia': orden_tech},
        )
        fig2.update_layout(
            height=alto,
            margin=dict(l=5, r=10, t=10, b=20),
            legend=dict(orientation='h', yanchor='bottom', y=1.01, x=0, font=dict(size=9)),
            plot_bgcolor='white', paper_bgcolor='white',
            xaxis_title="Centros poblados",
            yaxis={'categoryorder': 'total ascending'},
            font=dict(size=10),
        )
        st.plotly_chart(fig2, width='stretch')

    st.divider()

    col_pie, col_serv = st.columns(2)

    with col_pie:
        st.markdown('<div class="section-label">Distribucion tecnologica — registros del conjunto filtrado</div>', unsafe_allow_html=True)
        tech_cnt = ccpp_vista['max_tech'].value_counts().reset_index()
        tech_cnt.columns = ['Tecnologia', 'CCPP']
        fig3 = px.pie(
            tech_cnt, values='CCPP', names='Tecnologia',
            color='Tecnologia', color_discrete_map=COLORES_TECH, hole=0.42,
        )
        fig3.update_layout(height=320, margin=dict(t=10, b=10))
        fig3.update_traces(textinfo='percent+label', textfont_size=11)
        st.plotly_chart(fig3, width='stretch')

    with col_serv:
        st.markdown('<div class="section-label">Cobertura por servicio — nivel nacional</div>', unsafe_allow_html=True)
        df_serv = pd.DataFrame(list(servicios.items()), columns=['Servicio', 'Cobertura (%)'])
        fig4 = px.bar(
            df_serv, x='Cobertura (%)', y='Servicio', orientation='h',
            text='Cobertura (%)', color_discrete_sequence=['#1565C0'],
        )
        fig4.update_traces(texttemplate='%{text:.1f}%', textposition='outside')
        fig4.update_layout(
            height=320,
            margin=dict(l=5, r=60, t=10, b=20),
            xaxis_range=[0, 115], xaxis_title="% de registros",
            plot_bgcolor='white', paper_bgcolor='white', showlegend=False,
        )
        st.plotly_chart(fig4, width='stretch')


# PESTANA 2: Analisis Geografico
with tab2:
    # Esta pestana siempre muestra las 3 zonas para permitir comparacion
    st.markdown('<div class="section-label">Brecha digital por zona geografica — comparacion Costa / Sierra / Selva</div>', unsafe_allow_html=True)

    resumen_zona = brecha_valida.groupby('zona').agg(
        brecha_prom=('pct_brecha', 'mean'),
        total_ccpp=('total_ccpp', 'sum'),
        sin_4G_5G=('sin_4G_5G', 'sum'),
        n_dep=('DEPARTAMENTO', 'count'),
    ).round(1).reset_index()
    resumen_zona.columns = ['Zona', 'Brecha promedio (%)', 'Total CCPP', 'CCPP sin 4G/5G', 'Departamentos']
    resumen_zona = resumen_zona.sort_values('Brecha promedio (%)', ascending=False)

    costa_v  = brecha_valida[brecha_valida['zona'] == 'Costa']['pct_brecha']
    sierra_v = brecha_valida[brecha_valida['zona'] == 'Sierra']['pct_brecha']
    selva_v  = brecha_valida[brecha_valida['zona'] == 'Selva']['pct_brecha']
    stat_kw, p_kw = kruskal(costa_v, sierra_v, selva_v)

    col_iz, col_de = st.columns([2, 3])

    with col_iz:
        st.markdown("**Resumen por zona**")
        st.dataframe(resumen_zona, width='stretch', hide_index=True)

        st.markdown("---")
        st.markdown("**Kruskal-Wallis — diferencias entre zonas**")
        ka, kb = st.columns(2)
        ka.metric("Estadistico H", f"{stat_kw:.4f}")
        kb.metric("p-valor", f"{p_kw:.6f}")
        if p_kw < 0.05:
            st.success(
                f"Se rechaza H0. Diferencias significativas en brecha digital "
                f"entre zonas (H = {stat_kw:.4f}, p = {p_kw:.6f})."
            )

        st.markdown("---")
        st.markdown("**Post-hoc Mann-Whitney**")
        grupos = {'Costa': costa_v, 'Sierra': sierra_v, 'Selva': selva_v}
        pares = [('Costa', 'Sierra'), ('Costa', 'Selva'), ('Sierra', 'Selva')]
        posthoc = []
        for a, b in pares:
            u, p_mw = mannwhitneyu(grupos[a], grupos[b], alternative='two-sided')
            posthoc.append({
                'Comparacion': f"{a} vs {b}",
                'U': round(u, 1),
                'p-valor': round(p_mw, 4),
                'Sig.': 'Si' if p_mw < 0.05 else 'No',
            })
        st.dataframe(pd.DataFrame(posthoc), width='stretch', hide_index=True)

    with col_de:
        fig5 = go.Figure()
        prom_nac = brecha_valida['pct_brecha'].mean()
        for _, fila in resumen_zona.iterrows():
            z = fila['Zona']
            fig5.add_trace(go.Bar(
                x=[z], y=[fila['Brecha promedio (%)']],
                name=z, marker_color=COLORES_ZONA.get(z, '#999'),
                text=[f"{fila['Brecha promedio (%)']:.1f}%"],
                textposition='outside',
                hovertemplate=(
                    f"<b>{z}</b><br>Brecha promedio: {fila['Brecha promedio (%)']:.1f}%<br>"
                    f"Departamentos: {fila['Departamentos']}<extra></extra>"
                ),
            ))
        fig5.add_hline(
            y=prom_nac, line_dash='dash', line_color='#F9A825',
            annotation_text=f"Promedio nacional: {prom_nac:.1f}%",
            annotation_position="top right",
        )
        fig5.update_layout(
            height=320, yaxis_title="% CCPP sin 4G ni 5G", yaxis_range=[0, 100],
            plot_bgcolor='white', paper_bgcolor='white',
            showlegend=False, margin=dict(t=20, b=20),
        )
        st.plotly_chart(fig5, width='stretch')

        fig6 = px.box(
            brecha_valida, x='zona', y='pct_brecha',
            color='zona', color_discrete_map=COLORES_ZONA,
            points='all', hover_data=['DEPARTAMENTO'],
            labels={'pct_brecha': '% CCPP sin 4G/5G', 'zona': 'Zona'},
            category_orders={'zona': ['Costa', 'Sierra', 'Selva']},
        )
        fig6.update_layout(
            height=320, plot_bgcolor='white', paper_bgcolor='white',
            showlegend=False, margin=dict(t=10, b=20),
            yaxis_title="% CCPP sin 4G/5G",
        )
        st.plotly_chart(fig6, width='stretch')

    st.divider()
    st.markdown('<div class="section-label">Todos los departamentos con identificacion de zona</div>', unsafe_allow_html=True)
    brecha_ord = brecha_valida.sort_values('pct_brecha', ascending=True)
    fig7 = px.bar(
        brecha_ord, x='pct_brecha', y='DEPARTAMENTO',
        color='zona', orientation='h',
        color_discrete_map=COLORES_ZONA,
        text='pct_brecha',
        labels={'pct_brecha': '% CCPP sin 4G/5G', 'DEPARTAMENTO': '', 'zona': 'Zona'},
        hover_data=['total_ccpp', 'sin_4G_5G', 'con_4G_5G'],
        category_orders={'zona': ['Costa', 'Sierra', 'Selva']},
    )
    fig7.update_traces(texttemplate='%{text:.1f}%', textposition='outside')
    fig7.update_layout(
        height=620, plot_bgcolor='white', paper_bgcolor='white',
        margin=dict(l=5, r=75, t=10, b=20), xaxis_range=[0, 120],
        xaxis_title="% CCPP sin 4G ni 5G",
    )
    st.plotly_chart(fig7, width='stretch')


# PESTANA 3: Operadoras
with tab3:
    st.markdown('<div class="section-label">Cobertura y concentracion de mercado por empresa operadora</div>', unsafe_allow_html=True)

    op_share = op.set_index('EMPRESA_OPERADORA')['CCPP_cubiertos']
    hhi = ((op_share / op_share.sum()) ** 2).sum() * 10000
    hhi_label = (
        "Concentrado (HHI > 2500)" if hhi > 2500 else
        "Moderadamente concentrado (HHI 1500-2500)" if hhi > 1500 else
        "Competitivo (HHI < 1500)"
    )

    c1, c2, c3 = st.columns(3)
    c1.metric("Operadoras activas", len(op))
    c2.metric("Indice HHI", f"{hhi:.0f}")
    c3.metric("Estructura de mercado", hhi_label)

    st.divider()

    col_a, col_b = st.columns([2, 3])

    with col_a:
        st.markdown("**CCPP cubiertos y registros por tecnologia**")
        tabla_op = op.rename(columns={
            'EMPRESA_OPERADORA': 'Operadora', 'CCPP_cubiertos': 'CCPP unicos',
            'Reg_2G': '2G', 'Reg_3G': '3G', 'Reg_4G': '4G', 'Reg_5G': '5G',
        })
        st.dataframe(tabla_op, width='stretch', hide_index=True)

        st.markdown("---")
        st.markdown("**Porcentaje de tecnologia por operadora**")
        pct_op = op.set_index('EMPRESA_OPERADORA')[['Reg_2G', 'Reg_3G', 'Reg_4G', 'Reg_5G']].copy()
        pct_op.columns = ['2G', '3G', '4G', '5G']
        pct_op = (pct_op.div(pct_op.sum(axis=1), axis=0) * 100).round(1)
        st.dataframe(pct_op.reset_index().rename(columns={'EMPRESA_OPERADORA': 'Operadora'}), width='stretch', hide_index=True)

        st.markdown("---")
        st.markdown("**Chi-cuadrado: operadora vs cobertura 4G**")
        tabla_chi_op = pd.crosstab(df['EMPRESA_OPERADORA'], df['4G'])
        chi2_op, p_op_val, dof_op, _ = chi2_contingency(tabla_chi_op)
        ca, cb, cc = st.columns(3)
        ca.metric("Chi2", f"{chi2_op:.2f}")
        cb.metric("p-valor", f"{p_op_val:.6f}")
        cc.metric("g.l.", dof_op)
        if p_op_val < 0.05:
            st.success(f"Diferencias significativas entre operadoras (p = {p_op_val:.6f}).")

    with col_b:
        st.markdown("**Cobertura por tecnologia — registros totales por operadora**")
        op_melt = op.melt(
            id_vars='EMPRESA_OPERADORA',
            value_vars=['Reg_2G', 'Reg_3G', 'Reg_4G', 'Reg_5G'],
            var_name='Tecnologia', value_name='Registros',
        )
        op_melt['Tecnologia'] = op_melt['Tecnologia'].str.replace('Reg_', '')
        fig8 = px.bar(
            op_melt, x='EMPRESA_OPERADORA', y='Registros',
            color='Tecnologia', color_discrete_map=COLORES_TECH,
            barmode='stack', labels={'EMPRESA_OPERADORA': 'Operadora'},
            category_orders={'Tecnologia': ['2G', '3G', '4G', '5G']},
        )
        fig8.update_layout(
            height=360, plot_bgcolor='white', paper_bgcolor='white',
            margin=dict(t=10, b=20), xaxis_tickangle=-20,
        )
        st.plotly_chart(fig8, width='stretch')

        st.markdown("**Cuota de mercado — CCPP unicos cubiertos**")
        fig9 = px.pie(
            op, values='CCPP_cubiertos', names='EMPRESA_OPERADORA',
            hole=0.4, color_discrete_sequence=px.colors.qualitative.Safe,
        )
        fig9.update_layout(height=310, margin=dict(t=10, b=10))
        fig9.update_traces(textinfo='percent+label', textfont_size=10)
        st.plotly_chart(fig9, width='stretch')


# PESTANA 4: Caso de estudio
with tab4:
    dep_ccpp   = ccpp[ccpp['DEPARTAMENTO'] == dep_sel].copy()
    dep_4G     = dep_ccpp['tiene_4G_5G'].mean() * 100
    dep_sin    = int((dep_ccpp['tiene_4G_5G'] == 0).sum())
    dep_con    = int(dep_ccpp['tiene_4G_5G'].sum())
    dep_total  = len(dep_ccpp)
    nac_4G     = ccpp_valido['tiene_4G_5G'].mean() * 100
    diferencia = dep_4G - nac_4G

    ranking = (
        ccpp_valido.groupby('DEPARTAMENTO')['tiene_4G_5G']
        .mean().sort_values(ascending=False) * 100
    )
    posicion = list(ranking.index).index(dep_sel) + 1 if dep_sel in ranking.index else 'N/D'

    st.markdown(
        f'<div class="section-label">{dep_sel.title()} — Cobertura 4G/5G vs promedio nacional</div>',
        unsafe_allow_html=True
    )

    cm1, cm2, cm3, cm4 = st.columns(4)
    cm1.metric("Cobertura 4G/5G", f"{dep_4G:.1f}%", f"{diferencia:+.1f} pp vs nacional")
    cm2.metric("Total CCPP", f"{dep_total:,}")
    cm3.metric("CCPP con 4G/5G", f"{dep_con:,}")
    cm4.metric("CCPP sin 4G/5G", f"{dep_sin:,}")

    st.caption(
        f"Posicion en ranking nacional: **{posicion} de {len(ranking)}** "
        f"(posicion 1 = mayor cobertura 4G/5G)."
    )

    st.divider()

    col1, col2 = st.columns(2)

    with col1:
        st.markdown(f"**{dep_sel.title()} vs promedio nacional**")
        fig10 = go.Figure()
        fig10.add_trace(go.Bar(
            x=['Promedio\nNacional', dep_sel.title()],
            y=[nac_4G, dep_4G],
            marker_color=['#1565C0', '#d32f2f'],
            text=[f"{nac_4G:.1f}%", f"{dep_4G:.1f}%"],
            textposition='outside', width=0.4,
        ))
        fig10.update_layout(
            height=360, yaxis_title="% CCPP con 4G o 5G",
            yaxis_range=[0, 110], plot_bgcolor='white', paper_bgcolor='white',
            showlegend=False, margin=dict(t=20, b=20), font=dict(size=12),
        )
        st.plotly_chart(fig10, width='stretch')

    with col2:
        st.markdown(f"**Distribucion tecnologica en {dep_sel.title()}**")
        dep_tech = dep_ccpp['max_tech'].value_counts().reset_index()
        dep_tech.columns = ['Tecnologia', 'CCPP']
        dep_tech['%'] = (dep_tech['CCPP'] / dep_tech['CCPP'].sum() * 100).round(1)
        fig11 = px.bar(
            dep_tech, x='Tecnologia', y='CCPP',
            color='Tecnologia', color_discrete_map=COLORES_TECH, text='%',
            category_orders={'Tecnologia': ['Sin cobertura', '2G', '3G', '4G', '5G']},
        )
        fig11.update_traces(texttemplate='%{text:.1f}%', textposition='outside', showlegend=False)
        fig11.update_layout(
            height=360, plot_bgcolor='white', paper_bgcolor='white',
            showlegend=False, yaxis_title="Centros poblados", margin=dict(t=10, b=20),
        )
        st.plotly_chart(fig11, width='stretch')

    st.divider()

    st.markdown(f"**Mann-Whitney: {dep_sel.title()} vs resto del pais**")
    dep_v   = dep_ccpp['tiene_4G_5G']
    resto_v = ccpp_valido[ccpp_valido['DEPARTAMENTO'] != dep_sel]['tiene_4G_5G']
    u_mw, p_mw = mannwhitneyu(dep_v, resto_v, alternative='two-sided')

    mu1, mu2, mu3 = st.columns(3)
    mu1.metric("Estadistico U", f"{u_mw:.1f}")
    mu2.metric("p-valor", f"{p_mw:.6f}")
    mu3.metric("Significativo", "Si" if p_mw < 0.05 else "No")

    if p_mw < 0.05:
        dir_ = "inferior" if dep_4G < nac_4G else "superior"
        st.info(
            f"Diferencia significativa detectada (U = {u_mw:.1f}, p = {p_mw:.4f}). "
            f"La cobertura en {dep_sel.title()} es {dir_} al promedio nacional "
            f"({dep_4G:.1f}% vs {nac_4G:.1f}%)."
        )
    else:
        st.info(f"No se detecta diferencia significativa (p = {p_mw:.4f}).")

    st.divider()

    st.markdown('<div class="section-label">Ranking nacional de cobertura 4G/5G</div>', unsafe_allow_html=True)
    rank_df = ranking.reset_index()
    rank_df.columns = ['Departamento', 'Cobertura 4G/5G (%)']
    rank_df['Cobertura 4G/5G (%)'] = rank_df['Cobertura 4G/5G (%)'].round(1)
    rank_df.insert(0, 'Pos.', range(1, len(rank_df) + 1))
    rank_df['Zona'] = rank_df['Departamento'].apply(asignar_zona)

    def resaltar(row):
        return ['background-color:#FFF8E1;font-weight:600'] * len(row) if row['Departamento'] == dep_sel else [''] * len(row)

    st.dataframe(
        rank_df[['Pos.', 'Departamento', 'Zona', 'Cobertura 4G/5G (%)']].style.apply(resaltar, axis=1),
        width='stretch', hide_index=True, height=420,
    )


# PESTANA 5: Pruebas Estadisticas
with tab5:
    sub_h, sub_r = st.tabs(["Pruebas de hipotesis", "Regresion logistica"])

    with sub_h:
        st.markdown('<div class="section-label">H1 — Zona geografica vs cobertura 4G/5G (Chi-cuadrado)</div>', unsafe_allow_html=True)
        tabla_h1 = pd.crosstab(ccpp_valido['zona'], ccpp_valido['tiene_4G_5G'])
        tabla_h1.columns = ['Sin 4G/5G', 'Con 4G/5G']
        chi2_h1, p_h1, dof_h1, _ = chi2_contingency(tabla_h1)

        h1a, h1b, h1c = st.columns(3)
        h1a.metric("Chi2", f"{chi2_h1:.4f}")
        h1b.metric("p-valor", f"{p_h1:.6f}")
        h1c.metric("g.l.", dof_h1)
        st.markdown("Tabla de contingencia")
        st.dataframe(tabla_h1, width='content')
        if p_h1 < 0.05:
            st.success(f"Se rechaza H0: asociacion significativa entre zona y cobertura 4G/5G (Chi2={chi2_h1:.4f}, p={p_h1:.6f}).")

        st.divider()
        st.markdown('<div class="section-label">H2 — Operadora vs cobertura 4G (Chi-cuadrado)</div>', unsafe_allow_html=True)
        tabla_h2 = pd.crosstab(df['EMPRESA_OPERADORA'], df['4G'])
        chi2_h2, p_h2, dof_h2, _ = chi2_contingency(tabla_h2)
        h2a, h2b, h2c = st.columns(3)
        h2a.metric("Chi2", f"{chi2_h2:.4f}")
        h2b.metric("p-valor", f"{p_h2:.6f}")
        h2c.metric("g.l.", dof_h2)
        if p_h2 < 0.05:
            st.success(f"Se rechaza H0: diferencias significativas entre operadoras (Chi2={chi2_h2:.4f}, p={p_h2:.6f}).")

        st.divider()
        st.markdown('<div class="section-label">H3 — Brecha digital entre zonas (Kruskal-Wallis)</div>', unsafe_allow_html=True)
        stat_kw3, p_kw3 = kruskal(costa_v, sierra_v, selva_v)
        h3a, h3b = st.columns(2)
        h3a.metric("Estadistico H", f"{stat_kw3:.4f}")
        h3b.metric("p-valor", f"{p_kw3:.6f}")
        medias = pd.DataFrame({
            'Zona': ['Costa', 'Sierra', 'Selva'],
            'Brecha promedio (%)': [round(costa_v.mean(), 1), round(sierra_v.mean(), 1), round(selva_v.mean(), 1)],
            'n dep.': [len(costa_v), len(sierra_v), len(selva_v)],
        })
        st.dataframe(medias, width='content', hide_index=True)
        if p_kw3 < 0.05:
            st.success(f"Se rechaza H0: diferencias significativas entre zonas (H={stat_kw3:.4f}, p={p_kw3:.6f}).")

        st.divider()
        st.markdown("**Resumen de hipotesis**")
        resumen_h = pd.DataFrame([
            {'Hipotesis': 'H1: Zona vs cobertura 4G/5G', 'Prueba': 'Chi-cuadrado', 'Estadistico': f"Chi2={chi2_h1:.4f}", 'p-valor': f"{p_h1:.6f}", 'Decision': 'Se rechaza H0' if p_h1 < 0.05 else 'No se rechaza'},
            {'Hipotesis': 'H2: Operadora vs cobertura 4G', 'Prueba': 'Chi-cuadrado', 'Estadistico': f"Chi2={chi2_h2:.4f}", 'p-valor': f"{p_h2:.6f}", 'Decision': 'Se rechaza H0' if p_h2 < 0.05 else 'No se rechaza'},
            {'Hipotesis': 'H3: Brecha entre zonas', 'Prueba': 'Kruskal-Wallis', 'Estadistico': f"H={stat_kw3:.4f}", 'p-valor': f"{p_kw3:.6f}", 'Decision': 'Se rechaza H0' if p_kw3 < 0.05 else 'No se rechaza'},
        ])
        st.dataframe(resumen_h, width='stretch', hide_index=True)

    with sub_r:
        st.markdown('<div class="section-label">Regresion logistica — predictores de cobertura 4G</div>', unsafe_allow_html=True)
        st.caption("Variable dependiente: cobertura 4G (0/1). Referencia: zona=Costa | operadora=Viettel.")

        with st.spinner("Ajustando modelo (primera vez puede tardar ~20 segundos)..."):
            tabla_log, pseudo_r2, aic_val, n_obs = ejecutar_regresion(len(df))

        r1, r2, r3 = st.columns(3)
        r1.metric("Pseudo R2 McFadden", f"{pseudo_r2:.4f}")
        r2.metric("AIC", f"{aic_val:.2f}")
        r3.metric("N observaciones", f"{n_obs:,}")

        st.dataframe(tabla_log, width='stretch', hide_index=True)
        st.caption("*** p<0.001 | ** p<0.01 | * p<0.05 | ns = no significativo.")

        st.divider()
        st.markdown("**Odds Ratios con IC 95%**")
        df_or = tabla_log[tabla_log['Variable'] != 'const'].copy()
        fig12 = go.Figure()
        fig12.add_trace(go.Scatter(
            x=df_or['OR'], y=df_or['Variable'], mode='markers',
            marker=dict(size=8, color='#1565C0'),
            error_x=dict(
                type='data', symmetric=False,
                array=df_or['IC sup 95%'] - df_or['OR'],
                arrayminus=df_or['OR'] - df_or['IC inf 95%'],
                color='#1565C0', thickness=1.5,
            ),
            hovertemplate="<b>%{y}</b><br>OR: %{x:.4f}<extra></extra>",
        ))
        fig12.add_vline(x=1, line_dash='dash', line_color='#d32f2f', annotation_text="OR=1")
        fig12.update_layout(
            height=420, xaxis_title="Odds Ratio",
            plot_bgcolor='white', paper_bgcolor='white',
            margin=dict(l=10, r=20, t=20, b=20),
        )
        fig12.update_xaxes(gridcolor='#f0f0f0')
        st.plotly_chart(fig12, width='stretch')


# PESTANA 6: Mapa Interactivo
with tab6:
    st.markdown('<div class="section-label">Distribucion geografica de la brecha digital — Peru 2023</div>', unsafe_allow_html=True)

    tipo_mapa = st.radio(
        "Visualizacion",
        ["Mapa de calor: CCPP sin 4G/5G", "Circulos proporcionales por departamento"],
        horizontal=True,
    )

    if tipo_mapa == "Mapa de calor: CCPP sin 4G/5G":
        # Filtrar CCPP segun zona seleccionada en sidebar
        ccpp_geo = ccpp_vista[['LATITUD', 'LONGITUD', 'tiene_4G_5G']].dropna()
        sin_cob  = ccpp_geo[ccpp_geo['tiene_4G_5G'] == 0][['LATITUD', 'LONGITUD']]

        mapa = folium.Map(location=[-9.19, -75.015], zoom_start=6, tiles='CartoDB dark_matter')
        if len(sin_cob) > 0:
            HeatMap(
                sin_cob.values.tolist(), radius=8, blur=10, max_zoom=8,
                gradient={'0.2': 'blue', '0.4': 'cyan', '0.6': 'yellow', '0.8': 'orange', '1.0': 'red'},
            ).add_to(mapa)

        st_folium(mapa, width=None, height=540)
        st.caption(f"Representados: {len(sin_cob):,} centros poblados sin 4G ni 5G en zona: {zona_sel}.")

    else:
        mapa2 = folium.Map(location=[-9.19, -75.015], zoom_start=6, tiles='CartoDB positron')

        for _, fila in brecha_vista.iterrows():
            dep = fila['DEPARTAMENTO']
            if dep not in COORDS_REGIONES:
                continue
            lat, lon = COORDS_REGIONES[dep]
            pct   = fila['pct_brecha']
            color = '#d32f2f' if pct > 60 else '#FF9800' if pct > 40 else '#4CAF50'

            folium.CircleMarker(
                location=[lat, lon],
                radius=max(5, pct / 4),
                color=color, fill=True, fill_opacity=0.72,
                popup=folium.Popup(
                    f"<b>{dep}</b><br>Brecha: {pct}%<br>"
                    f"CCPP sin 4G/5G: {int(fila['sin_4G_5G']):,}<br>"
                    f"Total CCPP: {int(fila['total_ccpp']):,}",
                    max_width=220,
                ),
                tooltip=f"{dep}: {pct}%",
            ).add_to(mapa2)

        leyenda = (
            '<div style="position:fixed;bottom:30px;right:10px;background:white;'
            'padding:10px;border-radius:8px;border:1px solid #ccc;font-size:11px;z-index:1000;">'
            '<b>Brecha digital</b><br>'
            '<span style="color:#d32f2f;">&#9679;</span> Mayor de 60%<br>'
            '<span style="color:#FF9800;">&#9679;</span> 40-60%<br>'
            '<span style="color:#4CAF50;">&#9679;</span> Menor de 40%</div>'
        )
        mapa2.get_root().html.add_child(folium.Element(leyenda))
        st_folium(mapa2, width=None, height=540)
        st.caption("Haga clic en cada circulo para ver el detalle del departamento.")


st.divider()
st.markdown(
    '<p style="color:#9E9E9E;font-size:0.78rem;text-align:center;">'
    'OSIPTEL — Cobertura de Servicio Movil por Empresa Operadora. '
    'Elaborado por DairXP.</p>',
    unsafe_allow_html=True,
)
