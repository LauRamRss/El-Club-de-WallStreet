# Pronóstico del IPAP por subclases

Script en R para pronosticar el **IPAP** (suma ponderada de 32 subclases agropecuarias) a 12 meses, comparando un menú fijo de modelos con validación *walk-forward* (rolling origin).

## Qué hace

- Diagnóstico por subclase: volatilidad, estacionalidad, tendencia, atípicos y efecto ARCH.
- Menú fijo de modelos (ARIMA/Box-Jenkins, ARIMAX con variables externas, VAR, combinaciones anidadas).
- Variables externas asignadas por *prior* económico (TRM, urea, NOAA, precios de maíz, trigo y soya).
- Reconciliación entre pronóstico directo y *bottom-up*.
- Evaluación: MAE relativo al Naive, Diebold-Mariano (HAC), Model Confidence Set, sesgo y MAPE por horizonte.
- Intervalos de predicción por *bootstrap* en bloques, con calibración secuencial.
- Prueba de cointegración de Johansen y gráficos finales (test de 12 meses y pronóstico futuro).

## Estructura

```
ipap-forecast/
├── R/
│   └── IPAP_prototipo8.R      # script principal
├── data/
│   └── IPAP_SUBCLASES.xlsx    # serie mensual (ene-2011 en adelante)
├── .gitignore
└── README.md
```

## Datos

`data/IPAP_SUBCLASES.xlsx`, hoja 1, una fila por mes. Columnas:

| Columna(s) | Descripción |
|---|---|
| `FECHA` | Mes de la observación |
| `TOTAL_IPAP` | Índice total publicado |
| 32 subclases | `BOVINO`, `LECHE`, `POLLO`, `MAIZ`, `ARROZ`, … (índice por subclase; los pesos están en el script, objeto `pesos_bu`) |
| `TRM`, `UREA_USD`, `NOAA`, `MAIZ_USD`, `TRIGO_USD`, `SOYA_USD`, `CAFE_USD_LIBRA` | Variables externas |
| `INFLACION` | Inflación |

> Las fechas deben ser mensuales y sin huecos, y las subclases deben ser positivas. El script lo valida al cargar.

## Requisitos

- R ≥ 4.1
- Paquetes: `readxl`, `lubridate`, `forecast`, `tseries`, `FinTS`, `urca`, `vars`, `ggplot2`, `future.apply`

El script instala automáticamente los que falten.

## Cómo ejecutarlo

1. Clona el repositorio:
   ```bash
   git clone https://github.com/<tu-usuario>/ipap-forecast.git
   cd ipap-forecast
   ```
2. Abre R o RStudio **en la raíz del repositorio** (para que la ruta `data/IPAP_SUBCLASES.xlsx` funcione).
3. Ejecuta:
   ```r
   source("R/IPAP_prototipo8.R")
   ```

Si no encuentra el Excel, el script abre un selector de archivos.

## Parámetros principales

Se editan al inicio del script (sección 1):

| Parámetro | Valor | Significado |
|---|---|---|
| `objetivo` | `"suma"` | `"suma"` = suma ponderada de las 32 subclases; `"total"` = `TOTAL_IPAP` |
| `h` | `12` | Horizonte de pronóstico (meses) |
| `n_test` | `12` | Meses reservados como test |
| `origen_ini` | `96` | Primer origen del *walk-forward* |
| `n_boot` | `1000` | Réplicas del *bootstrap* para intervalos |
| `modelo_ic` | `"Combo_BJ_VAR"` | Modelo al que se le calculan intervalos |

## Notas

- El script usa procesamiento en paralelo (`future.apply`) con todos los núcleos menos uno. La ejecución completa puede tardar.
- Se fija `set.seed(123)` para reproducibilidad.
- Es un **prototipo** (versión 8): los resultados se imprimen en consola y se muestran en gráficos.

## Licencia

Pendiente de definir (por ejemplo MIT). Agrega un archivo `LICENSE` antes de hacer el repositorio público.

## Autor

<tu nombre> — <contacto o institución>
