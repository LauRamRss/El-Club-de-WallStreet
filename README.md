Vista previa de cómo se verá el README en GitHub

# 🌾 Pronóstico del IPAP a 12 meses

**Índice de Precios al Productor Agropecuario de Colombia**

R ≥ 4.2modelo bottom_up32_etsMAPE test 1.16 %validación rolling originlicencia MIT

Torneo rolling origin de 15 candidatos · Modelo elegido: bottom-up de 32 subclases con ETS · Prueba final de 12 meses

Pronóstico del IPAP de oct 2026 a sep 2027 con intervalos calibrados\
*Pronóstico a 12 meses (oct 2026 – sep 2027) con intervalos calibrados al 80 % y 95 %.*

Prueba final: IPAP real vs predicho de oct 2025 a sep 2026\
*Ejercicio de testeo: IPAP real vs predicho (oct 2025 – sep 2026). MAPE del modelo elegido: 1.16 %.*

---

## 📑 Contenido

1. [Resultados en una mirada](#-resultados-en-una-mirada)
2. [Pronóstico a 12 meses](#-pronóstico-a-12-meses-modelo-elegido)
3. [Ejercicio de testeo y MAPE](#-ejercicio-de-testeo-y-mape)
4. [Cómo se eligió el modelo](#-cómo-se-eligió-el-modelo)
5. [Metodología](#-metodología)
6. [Limitaciones y lectura honesta de los resultados](#-limitaciones-y-lectura-honesta-de-los-resultados)
7. [Cómo ejecutarlo](#-cómo-ejecutarlo)
8. [Archivos de resultados](#-archivos-de-resultados)

---

## 🏆 Resultados en una mirada

|  |  |
| --- | --- |
| **Modelo elegido** | `bottom_up32_ets`: ETS (en logaritmos) para cada una de las 32 subclases, agregadas con los pesos publicados de la canasta |
| **MAPE en la prueba final (12 meses)** | **1.16 %** (oct 2025 – sep 2026) |
| **MAPE del Naive en el mismo test** | 3.76 % (el error del modelo elegido equivale al 31 % del error del Naive) |
| **MAE relativo vs Naive en validación** | **0.866** (\< 1 = mejor que el Naive) |
| **MAPE mediano en validación rolling origin** | **5.60 %** a 12 meses, el más bajo de 15 candidatos |
| **Pronóstico sep 2027** | **264.73** vs 254.74 observado en sep 2026 → **+3.92 %** acumulado en 12 meses |

---

## 📈 Pronóstico a 12 meses (modelo elegido)

Pronóstico puntual del IPAP del modelo `bottom_up32_ets`, estimado con toda la serie hasta **sep 2026** (último dato observado: **254.74**).

| Mes | Pronóstico | IC 80 % | IC 95 % |
| --- | --- | --- | --- |
| oct 2026 | **255.60** | 251.7 – 264.1 | 248.5 – 267.5 |
| nov 2026 | **255.42** | 248.9 – 269.4 | 243.7 – 275.1 |
| dic 2026 | **255.82** | 247.1 – 274.6 | 240.1 – 282.2 |
| ene 2027 | **256.98** | 246.1 – 280.2 | 237.5 – 289.6 |
| feb 2027 | **258.16** | 245.3 – 285.6 | 235.1 – 296.8 |
| mar 2027 | **261.30** | 246.5 – 293.0 | 234.7 – 305.9 |
| abr 2027 | **262.74** | 246.1 – 298.4 | 232.8 – 312.9 |
| may 2027 | **262.15** | 243.8 – 301.3 | 229.3 – 317.3 |
| jun 2027 | **261.00** | 241.1 – 303.5 | 225.3 – 320.8 |
| jul 2027 | **261.49** | 240.0 – 307.5 | 222.9 – 326.1 |
| ago 2027 | **262.53** | 239.4 – 312.0 | 221.0 – 332.1 |
| sep 2027 | **264.73** | 239.9 – 317.9 | 220.2 – 339.4 |

Archivo: [`resultados/pronostico_futuro.csv`](resultados/pronostico_futuro.csv) · desglose por subclase: [`resultados/subclases_pronosticadas_futuro.csv`](resultados/subclases_pronosticadas_futuro.csv)

> Los intervalos son **calibrados de forma empírica** con los errores de los orígenes de validación. Son una referencia de la incertidumbre, no una garantía de cobertura (ver [limitaciones](#-limitaciones-y-lectura-honesta-de-los-resultados)).

---

## 🧪 Ejercicio de testeo y MAPE

Los **últimos 12 meses** de la serie (oct 2025 – sep 2026) se reservaron como prueba final. El modelo se reestimó solo con datos hasta **sep 2025** y se pronosticaron esos 12 meses de una sola vez.

| Mes | IPAP real | Predicho | Error abs. | Error % |
| --- | --- | --- | --- | --- |
| oct 2025 | 243.62 | 241.49 | 2.13 | 0.87 % |
| nov 2025 | 238.98 | 241.42 | 2.44 | 1.02 % |
| dic 2025 | 243.25 | 242.02 | 1.24 | 0.51 % |
| ene 2026 | 246.42 | 243.94 | 2.48 | 1.01 % |
| feb 2026 | 250.62 | 245.81 | 4.82 | 1.92 % |
| mar 2026 | 256.72 | 248.89 | 7.83 | 3.05 % |
| abr 2026 | 255.47 | 250.58 | 4.89 | 1.92 % |
| may 2026 | 254.03 | 250.41 | 3.62 | 1.43 % |
| jun 2026 | 253.02 | 250.55 | 2.47 | 0.98 % |
| jul 2026 | 251.18 | 250.91 | 0.27 | 0.11 % |
| ago 2026 | 250.43 | 251.60 | 1.18 | 0.47 % |
| sep 2026 | 254.74 | 253.14 | 1.60 | 0.63 % |
| **Promedio** |  |  | **2.91** | **1.16 %** |

### Comparación con las referencias en el mismo test

| Método | MAPE | MAE (puntos) | MASE |
| --- | --- | --- | --- |
| **bottom_up32_ets (modelo elegido)** | 1.16 % | 2.91 | 1.13 |
| Naive (último dato) | 3.76 % | 9.51 | 3.69 |
| Naive estacional | 6.73 % | 16.88 | 6.55 |
| Drift (mejor modelo directo) | 1.66 % | 4.20 | 1.63 |

- El modelo elegido tiene el **menor MAPE** de los métodos comparados en la prueba.
- El **MASE** usa como escala el cambio mensual medio del entrenamiento (no estacional). Que sea mayor que 1 no implica perder frente al Naive a 12 meses; para eso se comparan directamente los errores de la tabla.
- Los intervalos calibrados contuvieron los 12 valores reales en ambos niveles (80 % y 95 %). Es **un solo origen de 12 meses**: sirve de verificación, no de prueba formal.

Archivos: [`resultados/prueba_historica.csv`](resultados/prueba_historica.csv) · [`resultados/metricas_prueba.csv`](resultados/metricas_prueba.csv) · [`resultados/prueba_referencias.csv`](resultados/prueba_referencias.csv) · [`resultados/comparacion_bottomup_test.csv`](resultados/comparacion_bottomup_test.csv)

---

## 🏁 Cómo se eligió el modelo

Se corrió un **torneo rolling origin** con ventana expansiva: primera estimación con 8 años de datos y un nuevo origen cada 3 meses, pronosticando siempre 12 meses. Hubo **24 orígenes** (dic 2018 – sep 2024); 18 son comparables entre todos los candidatos. Ningún origen de validación llega al tramo de test.

Torneo rolling origin: MAPE mediano por candidato

| # | Candidato | MAPE mediano (18 orígenes) | MAE relativo vs Naive |
| --- | --- | --- | --- |
| 1 | **bottom_up32_ets** | 5.60 % | 0.866 |
| 2 | drift | 6.05 % | 0.964 |
| 3 | ensemble | 6.70 % | 0.972 |
| 4 | naive | 6.72 % | 1.000 |
| 5 | bottom_up32_naive | 6.72 % | 1.000 |
| 6 | tbats | 6.97 % | 1.113 |
| 7 | drift_log | 7.60 % | 1.015 |
| 8 | sarima | 7.63 % | 0.987 |
| 9 | arima | 7.94 % | 1.091 |
| 10 | bottom_up32_sarima | 8.34 % | 1.078 |
| 11 | bottom_up32_drift_ventana | 9.08 % | 1.106 |
| 12 | drift_ventana | 9.08 % | 1.106 |
| 13 | ets_damped | 9.13 % | 1.100 |
| 14 | ets | 9.19 % | 1.199 |
| 15 | snaive | 9.75 % | 1.566 |

Candidatos: Naive, Naive estacional, Drift, Drift en logaritmos, Drift con ventana móvil (48 meses), ETS, ETS amortiguado, ARIMA, SARIMA, TBATS, un ensamble TBATS + Drift con pesos por error histórico, y 4 versiones *bottom-up* (Naive, Drift ventana, ETS y SARIMA aplicados a las 32 subclases).

MAPE por horizonte en validación

Archivos: [`resultados/resumen_torneo.csv`](resultados/resumen_torneo.csv) · [`resultados/torneo_rolling_origin.csv`](resultados/torneo_rolling_origin.csv) · [`resultados/validacion_mae_relativo_naive.csv`](resultados/validacion_mae_relativo_naive.csv) · [`resultados/validacion_mape_por_horizonte.csv`](resultados/validacion_mape_por_horizonte.csv)

---

## 🧭 Metodología

1. **Datos.** `data/IPAP_SUBCLASES.xlsx`: serie mensual desde ene 2011 con `TOTAL_IPAP` y 32 subclases (189 meses hasta sep 2026). Se verifican fechas mensuales sin huecos, sin duplicados y con valores positivos.
2. **Bottom-up.** Cada subclase se pronostica por separado con **el mismo método prefijado** (sin elegir el mejor método por subclase mirando resultados futuros) y se suma con los pesos de la canasta.
3. **Pesos.** Se usan los pesos publicados, **sin renormalizar**. Suman 0.9913; el peso restante (≈0.87 %, fibra) no está disponible y **no se completa con ceros**. El bottom-up es una aproximación, no una reconstrucción de las 33 subclases oficiales. Detalle en [`resultados/ponderaciones_metodologia.csv`](resultados/ponderaciones_metodologia.csv) y [`resultados/auditoria_agregacion.csv`](resultados/auditoria_agregacion.csv).
4. **ETS en logaritmos.** Garantiza pronósticos positivos; con `biasadj = TRUE` las medias se devuelven ajustadas a niveles.
5. **Intervalos.** El bottom-up no produce intervalos propios (no se suman intervalos individuales porque falta la dependencia entre subclases). Se **calibran empíricamente** con los errores relativos de los orígenes de validación, por horizonte. Parámetros en [`resultados/calibracion_intervalos.csv`](resultados/calibracion_intervalos.csv).
6. **Sin fuga de información.** En cada origen, cada candidato solo usa datos hasta ese origen; los pesos del ensamble usan únicamente orígenes ya cerrados.

---

## ⚠️ Limitaciones y lectura honesta de los resultados

- **El test no es virgen.** Algunos candidatos se agregaron después de ver el test de versiones anteriores del script. La evidencia principal es la **validación rolling origin** (18 orígenes); la prueba final es una evaluación retrospectiva complementaria.
- **Los horizontes de validación se solapan** entre orígenes consecutivos, por lo que los errores no son independientes.
- **Los intervalos calibrados subestiman la incertidumbre en validación.** En la calibración secuencial exploratoria (13 orígenes) cubrieron **51.3 %** (nominal 80 %) y **69.9 %** (nominal 95 %), y mejoran con el horizonte. Deben leerse como rangos indicativos. Ver [`resultados/validacion_calibracion_cobertura.csv`](resultados/validacion_calibracion_cobertura.csv).
- **El error crece con el horizonte:** el MAPE de validación pasa de ≈1.6 % a 1 mes a ≈10.2 % a 12 meses.
- **Bottom-up aproximado:** cobertura parcial de la canasta (99.13 % del peso) y brecha con el total observado en algunos meses (ver `auditoria_agregacion.csv`).
- Choques extremos (sequías, paros, cambios regulatorios) pueden quedar fuera de lo que muestra la historia.

---

## 🚀 Cómo ejecutarlo

**Requisitos:** R ≥ 4.2. Los paquetes (`readxl`, `dplyr`, `lubridate`, `forecast`, `ggplot2`) se instalan solos la primera vez.

```bash
git clone https://github.com/LauRamRss/El-Club-de-WallStreet.git
cd El-Club-de-WallStreet
```

Abre R o RStudio **en la carpeta del proyecto** y ejecuta:

```r
source("R/modelo_rolling_origin.R")
```

- Lee `data/IPAP_SUBCLASES.xlsx` y escribe los resultados en `resultados/` (reemplaza los archivos existentes, incluidos los gráficos, que el script genera con `ggplot2`).
- Tarda **bastante** (el torneo reestima 4 versiones bottom-up sobre 32 subclases en cada origen): del orden de horas en un computador con un solo núcleo.
- Semilla fija: `set.seed(123)`.
- Ejecútalo en una sesión nueva de R.

---

## 🗂️ Archivos de resultados

| Archivo | Contenido |
| --- | --- |
| [`pronostico_futuro.csv`](resultados/pronostico_futuro.csv) | Pronóstico a 12 meses con intervalos calibrados 80 / 95 % |
| [`prueba_historica.csv`](resultados/prueba_historica.csv) | Ejercicio de testeo: real, predicho, error y MAPE por mes |
| [`metricas_prueba.csv`](resultados/metricas_prueba.csv) | MAPE, MAE y MASE del modelo elegido y las referencias en el test |
| [`resumen_torneo.csv`](resultados/resumen_torneo.csv) | MAPE mediano de los 15 candidatos en validación |
| [`torneo_rolling_origin.csv`](resultados/torneo_rolling_origin.csv) | MAPE por origen y candidato |
| [`validacion_*.csv`](resultados/) | MAE relativo vs Naive, MAPE por horizonte, cobertura de intervalos |
| [`subclases_pronosticadas_*.csv`](resultados/) | Pronóstico de cada una de las 32 subclases (test y futuro) |
| [`ponderaciones_metodologia.csv`](resultados/ponderaciones_metodologia.csv) | Pesos de la canasta usados en el bottom-up |

## 📂 Estructura del repositorio

```
├── R/
│   └── modelo_rolling_origin.R   # Torneo + modelo elegido + pronóstico
├── data/
│   └── IPAP_SUBCLASES.xlsx       # Serie mensual: TOTAL_IPAP y 32 subclases
├── resultados/                   # Tablas (CSV) y gráficos del modelo elegido
├── README.md
├── LICENSE
└── .gitignore
```

## 🧩 Tecnologías

R · `forecast` · `ggplot2` · `dplyr` · `lubridate` · `readxl`

## 👥 Equipo

**El Club de WallStreet** · Distribuido bajo licencia MIT (ver `LICENSE`).
