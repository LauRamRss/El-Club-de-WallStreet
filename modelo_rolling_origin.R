# VERSION 5: VERSION RECALIBRADA + BOTTOM-UP DE 32 SUBCLASES
# Conserva candidatos, rolling origin, cobertura y calibracion exploratoria.
# Bottom-up usa pesos publicados sin renormalizar. No se completa fibra con ceros.
# Su cobertura es parcial: no equivale a reproducir las 33 subclases oficiales.
# El test ya fue consultado en versiones anteriores: evaluacion retrospectiva.
# Los intervalos calibrados son empiricos; no garantizan cobertura nominal.


# IPAP mensual: torneo rolling origin y pronostico a 12 meses
# Entrada unica: IPAP_SUBCLASES.xlsx, columnas FECHA, TOTAL_IPAP y 32 componentes.
# No reconstruye el indice ni utiliza datos futuros como predictores.
# Ejecutar este script completo en una sesion nueva de R.

# 1. PAQUETES Y CONFIGURACION --------------------------------------------
paquetes <- c("readxl", "dplyr", "lubridate", "forecast", "ggplot2")
faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes) > 0) install.packages(faltantes)
library(readxl)
library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)

# Seleccionar la base completa. Si no existe esta ruta se abrira una ventana.
ruta_subclases <- "data/IPAP_SUBCLASES.xlsx"
if (!file.exists(ruta_subclases)) {
  message("Selecciona IPAP_SUBCLASES.xlsx, no el archivo del total solamente.")
  ruta_subclases <- file.choose()
}
dir_salida <- "resultados"
dir.create(dir_salida, showWarnings = FALSE, recursive = TRUE)

h_m <- 12L              # horizonte: 12 meses
n_test <- 12L           # ultimos 12 meses: prueba final reservada
inicio_train <- 96L     # primera estimacion: 8 anios
paso <- 3L              # nuevo origen cada 3 meses
ventana_drift <- 48L    # candidato drift_ventana: ultimos 48 meses
min_origenes_cal <- 8L  # minimo de origenes para calibrar intervalos
min_origenes_pesos <- 3L
min_origenes_comparacion <- 5L
set.seed(123)

# MAPE: porcentaje. MAE: puntos del indice.
# MASE: MAE dividido por el cambio absoluto mensual medio del entrenamiento.
# Esta escala es NO estacional (lag 1). MASE > 1 no demuestra por si solo
# perder frente a naive a 12 meses en el mismo test: comparar ambos errores.
mape <- function(real, pred) 100 * mean(abs((real - pred) / real))
metricas <- function(real, pred, train) {
  mae <- mean(abs(real - pred))
  escala <- mean(abs(diff(as.numeric(train))))
  c(MAPE = mape(real, pred), MAE = mae,
    MASE = if (escala > 0) mae / escala else NA_real_)
}

# 2. LEER Y VERIFICAR LA SERIE OBSERVADA ---------------------------------
datos_subclases <- as.data.frame(read_excel(ruta_subclases, sheet = 1))
names(datos_subclases) <- toupper(trimws(names(datos_subclases)))
if (anyDuplicated(names(datos_subclases))) stop("Hay nombres de columnas duplicados.")
if (!all(c("FECHA", "TOTAL_IPAP") %in% names(datos_subclases))) {
  stop("Selecciona la base con FECHA, TOTAL_IPAP y las subclases.")
}
# En el archivo enviado CEBOLLA coincide en los 189 meses con Cebada, otros
# del archivo original Comparativo niveles IPAP por sector (4).xlsx.
# Correccion de etiqueta documentada, no nueva clasificacion de cebolla.
if ("CEBOLLA" %in% names(datos_subclases)) {
  if ("CEBADA" %in% names(datos_subclases)) stop("Existen CEBOLLA y CEBADA: revisar.")
  names(datos_subclases)[names(datos_subclases) == "CEBOLLA"] <- "CEBADA"
  message("Etiqueta CEBOLLA tratada como CEBADA, segun cotejo con la base original.")
}
datos_subclases$FECHA <- floor_date(as.Date(datos_subclases$FECHA), "month")
datos_subclases <- datos_subclases[order(datos_subclases$FECHA), ]
datos <- data.frame(Date = datos_subclases$FECHA, IPAP = datos_subclases$TOTAL_IPAP)
if (!all(c("Date", "IPAP") %in% names(datos))) {
  stop("El Excel debe tener las columnas Date e IPAP.")
}
if (!is.numeric(datos$IPAP)) stop("IPAP debe ser una columna numerica en Excel.")
ipap_completo <- datos %>%
  transmute(fecha = floor_date(as.Date(Date), "month"), IPAP = IPAP) %>%
  arrange(fecha)
if (anyNA(ipap_completo) || any(!is.finite(ipap_completo$IPAP))) {
  stop("Hay fechas o niveles faltantes/no finitos. Revisar antes de estimar.")
}
if (anyDuplicated(ipap_completo$fecha)) stop("Hay meses duplicados.")
if (any(ipap_completo$IPAP <= 0)) stop("Los modelos logaritmicos requieren IPAP > 0.")
fechas_esperadas <- seq(min(ipap_completo$fecha), max(ipap_completo$fecha), by = "month")
if (!identical(as.character(ipap_completo$fecha), as.character(fechas_esperadas))) {
  stop("Faltan meses. No se comprimira el calendario ni se imputara automaticamente.")
}

y_m <- ts(ipap_completo$IPAP,
          start = c(year(ipap_completo$fecha[1]), month(ipap_completo$fecha[1])),
          frequency = 12)
n <- length(y_m)
corte <- n - n_test
if (corte - h_m < inicio_train) stop("No hay suficientes datos para el torneo configurado.")
origenes_m <- seq(inicio_train, corte - h_m, by = paso)
crear_train <- function(o) window(y_m, end = time(y_m)[o])

cat("\nMeses:", n, "| Desde", format(min(ipap_completo$fecha)),
    "hasta", format(max(ipap_completo$fecha)), "\n")
cat("Entrenamiento final hasta:", format(ipap_completo$fecha[corte]), "\n")
cat("Prueba reservada:", format(ipap_completo$fecha[corte + 1]), "a",
    format(ipap_completo$fecha[n]), "\n")


# 2b. PONDERACIONES Y AUDITORIA DE AGREGACION ----------------------------
# Fuente: BMC, Metodologia IPAP, julio de 2022, apendice pp. impresas 15-16.
# Columna Reponderacion, NO Ponderacion original. Valores en fraccion.
# Cebada es una subclase distinta de cebolla; ver correccion de etiqueta arriba.
pesos_bu <- c(
  BOVINO = 0.169370782, LECHE = 0.116490732, POLLO = 0.089743128,
  HUEVOS = 0.074275778, MAIZ = 0.060446759, PLATANO = 0.059920606,
  ARROZ = 0.052097672, PAPAS = 0.045723827, OTROS_FRUTOS = 0.043908222,
  ANIMALES_VIVOS = 0.041594821, HORTALIZA_RAIZ = 0.022852431,
  TRIGO = 0.021589547, LEGUMBRES_SECAS = 0.020733145, CITRICOS = 0.020703863,
  FRUTAS_PEPITA = 0.018993176, TOMATES = 0.017760004, YUCA = 0.017590443,
  LEGUMBRES_VERDES = 0.012988126, HORTALIZA_HOJA = 0.012987726,
  FRUTAS_TROPICALES = 0.012833636, HORTALIZA_FRUTO = 0.009426576,
  PESCADO = 0.008586980, NUECES = 0.006815703, SORGO = 0.006007886,
  SOJA = 0.005512721, GRANO_CACAO = 0.005429492, FLORES_CAPULLOS = 0.005369985,
  BANANO = 0.005220701, CEBADA = 0.003239188, UVAS = 0.001586038,
  SEMILLA = 0.000784934, CRUSTACEOS = 0.000729849
)
peso_fibra_no_disponible <- 0.008685522
faltan_columnas <- setdiff(names(pesos_bu), names(datos_subclases))
extras <- setdiff(names(datos_subclases), c("FECHA", "TOTAL_IPAP", names(pesos_bu)))
if (length(faltan_columnas)) stop(paste("Faltan subclases:", paste(faltan_columnas, collapse = ", ")))
if (length(extras)) stop(paste("Hay columnas sin correspondencia metodologica:", paste(extras, collapse = ", ")))
if (!all(vapply(datos_subclases[, names(pesos_bu)], is.numeric, logical(1)))) {
  stop("Las 32 columnas de subclases deben ser numericas.")
}
matriz_subclases <- as.matrix(datos_subclases[, names(pesos_bu)])
if (anyNA(matriz_subclases) || any(!is.finite(matriz_subclases)) || any(matriz_subclases <= 0)) {
  stop("Hay subclases faltantes, no finitas o no positivas. Revisar, no imputar con el futuro.")
}
if (!identical(as.character(datos_subclases$FECHA), as.character(ipap_completo$fecha))) {
  stop("El calendario del total y el de las subclases no coinciden.")
}
auditoria_agregacion <- data.frame(
  fecha = ipap_completo$fecha,
  total_observado = ipap_completo$IPAP,
  suma_32_ponderada = as.numeric(matriz_subclases %*% pesos_bu)
)
auditoria_agregacion$brecha <- with(auditoria_agregacion, total_observado - suma_32_ponderada)
auditoria_agregacion$requiere_revision <- abs(auditoria_agregacion$brecha) > 0.01
tabla_pesos <- data.frame(subclase = c(names(pesos_bu), "CULTIVOS_FIBRA"),
                         peso = c(as.numeric(pesos_bu), peso_fibra_no_disponible),
                         disponible = c(rep(TRUE, length(pesos_bu)), FALSE))
cat("\n=== AUDITORIA: 32 SUBCLASES DISPONIBLES ===\n")
cat("Suma de pesos usados:", sum(pesos_bu), "| Sin renormalizar.\n")
cat("Peso de fibra no disponible:", peso_fibra_no_disponible, "\n")
print(auditoria_agregacion[auditoria_agregacion$requiere_revision, ])
cat("Bottom_up32 es aproximado: la brecha no se interpreta como fibra ni se elimina.\n")
# Esta auditoria de toda la base es descriptiva. No se usa para escoger pesos,
# corregir el total ni ajustar los pronosticos con informacion futura.
write.csv(auditoria_agregacion, file.path(dir_salida, "auditoria_agregacion.csv"), row.names = FALSE)
write.csv(tabla_pesos, file.path(dir_salida, "ponderaciones_metodologia.csv"), row.names = FALSE)

# 3. MODELOS Y CONTROL DE AVISOS ----------------------------------------
# Candidatos nuevos (v2): drift_log, drift_ventana y ets_damped se agregaron
# DESPUES de ver el test de la v1. Si uno de ellos gana, el test ya no es
# completamente virgen: reportarlo asi y priorizar la validacion rolling origin.
modelos_base <- c("naive", "snaive", "drift", "drift_log", "drift_ventana",
                  "ets", "ets_damped", "arima", "sarima", "tbats")
# Un metodo prefijado para las 32 subclases por candidato, sin seleccionar
# el mejor metodo de cada subclase mirando sus resultados futuros.
metodos_bu <- c(bottom_up32_naive = "naive",
                 bottom_up32_drift_ventana = "drift_ventana",
                 bottom_up32_ets = "ets", bottom_up32_sarima = "sarima")
modelos_bu <- names(metodos_bu)
modelos <- c(modelos_base, "ensemble", modelos_bu)
registro_avisos <- data.frame(modelo = character(), origen = integer(),
                              mensaje = character())

# Todos devuelven pronosticos en NIVELES.
# ETS/ARIMA usan logaritmos internamente; biasadj=TRUE pide medias
# ajustadas al volver a niveles. Naive y drift se estiman en niveles.
ajustar <- function(nombre, train, h) {
  switch(nombre,
         naive = naive(train, h = h, level = c(80, 95)),
         snaive = snaive(train, h = h, level = c(80, 95)),
         drift = rwf(train, h = h, drift = TRUE, level = c(80, 95)),
         # Drift en logaritmos: crecimiento medio porcentual constante.
         drift_log = rwf(train, h = h, drift = TRUE, lambda = 0,
                         biasadj = TRUE, level = c(80, 95)),
         # Drift con ventana movil: pendiente de los ultimos meses.
         drift_ventana = {
           k <- min(ventana_drift, length(train))
           rwf(window(train, start = time(train)[length(train) - k + 1]),
               h = h, drift = TRUE, level = c(80, 95))
         },
         ets = forecast(ets(train, lambda = 0), h = h,
                        level = c(80, 95), biasadj = TRUE),
         # ETS con tendencia aditiva amortiguada, sin estacionalidad.
         ets_damped = forecast(ets(train, model = "ZAN", damped = TRUE, lambda = 0),
                               h = h, level = c(80, 95), biasadj = TRUE),
         arima = forecast(auto.arima(train, lambda = 0, seasonal = FALSE,
                                     stepwise = TRUE, approximation = FALSE),
                          h = h, level = c(80, 95), biasadj = TRUE),
         sarima = forecast(auto.arima(train, lambda = 0, seasonal = TRUE,
                                      stepwise = TRUE, approximation = FALSE),
                           h = h, level = c(80, 95), biasadj = TRUE),
         tbats = forecast(tbats(train), h = h, level = c(80, 95), biasadj = TRUE)
  )
}

pronostico_seguro <- function(nombre, train, h, origen, etiqueta = nombre) {
  avisos <- character()
  fc <- tryCatch(withCallingHandlers(
    ajustar(nombre, train, h),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = function(e) {
      avisos <<- c(avisos, paste("ERROR:", conditionMessage(e)))
      NULL
    })
  if (length(avisos) > 0) {
    registro_avisos <<- rbind(registro_avisos, data.frame(
      modelo = etiqueta, origen = origen, mensaje = avisos))
    # Excluir ajustes con avisos de convergencia, no solo esconder el aviso.
    if (any(grepl("converg|optim\\(\\)", avisos, ignore.case = TRUE))) fc <- NULL
  }
  if (!is.null(fc) && (length(fc$mean) != h || any(!is.finite(fc$mean)))) {
    registro_avisos <<- rbind(registro_avisos, data.frame(
      modelo = etiqueta, origen = origen, mensaje = "Pronostico no finito o longitud incorrecta"))
    fc <- NULL
  }
  fc
}

pesos <- function(errores_tb, errores_dr) {
  inv <- 1 / pmax(c(median(errores_tb), median(errores_dr)), 1e-8)
  setNames(inv / sum(inv), c("tbats", "drift"))
}


# 3b. PRONOSTICO BOTTOM-UP: SOLO SUBCLASES PRONOSTICADAS ------------------
# Cache evita volver a estimar un mismo candidato/origen al preparar tablas.
cache_bu <- new.env(parent = emptyenv())
pronostico_bu <- function(nombre, o, h) {
  clave <- paste(nombre, o, h, sep = "_")
  if (exists(clave, envir = cache_bu, inherits = FALSE)) {
    return(get(clave, envir = cache_bu, inherits = FALSE))
  }
  metodo <- unname(metodos_bu[nombre])
  if (length(metodo) != 1 || is.na(metodo)) stop("Candidato bottom-up desconocido.")
  pred_sub <- matrix(NA_real_, h, length(pesos_bu),
                     dimnames = list(NULL, names(pesos_bu)))
  for (j in seq_along(pesos_bu)) {
    # Ventana termina estrictamente en el origen o. No usar valores reales t+h.
    train_j <- ts(matriz_subclases[seq_len(o), j], start = start(y_m), frequency = 12)
    fc_j <- pronostico_seguro(metodo, train_j, h, o,
                              etiqueta = paste(nombre, names(pesos_bu)[j], sep = ":"))
    if (is.null(fc_j)) {
      assign(clave, NULL, envir = cache_bu)
      return(NULL)  # no omitir una subclase, ni sustituirla por cero
    }
    pred_sub[, j] <- as.numeric(fc_j$mean)
  }
  total <- as.numeric(pred_sub %*% pesos_bu)
  if (any(!is.finite(total)) || any(total <= 0)) {
    assign(clave, NULL, envir = cache_bu)
    return(NULL)
  }
  # No sumar intervalos individuales: faltaria la dependencia entre subclases.
  sin_ic <- matrix(NA_real_, h, 2, dimnames = list(NULL, c("80%", "95%")))
  resultado <- list(mean = total, lower = sin_ic, upper = sin_ic,
                    componentes = pred_sub)
  assign(clave, resultado, envir = cache_bu)
  resultado
}

# 4. TORNEO ROLLING ORIGIN ----------------------------------------------
# Ventana expansiva: se reestima cada candidato desde el comienzo hasta o.
# Cada origen predice 12 meses. Ninguna evaluacion alcanza el test final.
# Los horizontes de validacion SI se solapan entre si; esto es intencional.
res_torneo <- data.frame(origen = origenes_m,
                         fecha = ipap_completo$fecha[origenes_m])
for (nombre in modelos) res_torneo[[nombre]] <- NA_real_
res_torneo$w_tb <- NA_real_
res_torneo$w_dr <- NA_real_

# Errores por origen y horizonte (para MAPE por horizonte y Diebold-Mariano).
errores_largo <- data.frame()
# Cobertura empirica de intervalos 80% y 95% por origen y horizonte.
cob_largo <- data.frame()
guardar_cobertura <- function(modelo, o, real, fc) {
  l80 <- as.numeric(fc$lower[, "80%"]); u80 <- as.numeric(fc$upper[, "80%"])
  l95 <- as.numeric(fc$lower[, "95%"]); u95 <- as.numeric(fc$upper[, "95%"])
  media <- as.numeric(fc$mean)
  data.frame(modelo = modelo, origen = o, h = seq_along(real),
             cubre80 = real >= l80 & real <= u80,
             cubre95 = real >= l95 & real <= u95,
             ancho95_pct = 100 * (u95 - l95) / real,
             err_rel_pct = 100 * (real - media) / media,
             stringsAsFactors = FALSE)
}
guardar_errores <- function(modelo, o, real, pred) {
  data.frame(modelo = modelo, origen = o, h = seq_along(real),
             ape = 100 * abs((real - pred) / real),
             ae = abs(real - pred), stringsAsFactors = FALSE)
}

for (i in seq_along(origenes_m)) {
  o <- origenes_m[i]
  cat("Origen", format(ipap_completo$fecha[o]), "\n")
  train_m <- crear_train(o)
  real_m <- as.numeric(y_m[(o + 1):(o + h_m)])
  fc_o <- setNames(lapply(modelos_base, function(nombre) {
    pronostico_seguro(nombre, train_m, h_m, o)
  }), modelos_base)
  # Incorporar los candidatos bottom-up DENTRO del mismo origen.
  for (nombre in modelos_bu) {
    cat("  ", nombre, "(32 subclases)\n")
    fc_o[nombre] <- list(pronostico_bu(nombre, o, h_m))
  }
  for (nombre in c(modelos_base, modelos_bu)) {
    if (!is.null(fc_o[[nombre]])) {
      res_torneo[i, nombre] <- mape(real_m, as.numeric(fc_o[[nombre]]$mean))
      errores_largo <- rbind(errores_largo, guardar_errores(
        nombre, o, real_m, as.numeric(fc_o[[nombre]]$mean)))
      cob_largo <- rbind(cob_largo, guardar_cobertura(
        nombre, o, real_m, fc_o[[nombre]]))
    }
  }
  # Solo errores de horizontes ya terminados a la fecha del origen actual.
  validos <- which(res_torneo$origen + h_m <= o &
                     is.finite(res_torneo$tbats) & is.finite(res_torneo$drift))
  if (length(validos) >= min_origenes_pesos &&
      !is.null(fc_o$tbats) && !is.null(fc_o$drift)) {
    w <- pesos(res_torneo$tbats[validos], res_torneo$drift[validos])
    res_torneo$w_tb[i] <- w["tbats"]
    res_torneo$w_dr[i] <- w["drift"]
    pred_ens <- w["tbats"] * as.numeric(fc_o$tbats$mean) +
      w["drift"] * as.numeric(fc_o$drift$mean)
    res_torneo$ensemble[i] <- mape(real_m, pred_ens)
    errores_largo <- rbind(errores_largo, guardar_errores(
      "ensemble", o, real_m, as.numeric(pred_ens)))
    cob_largo <- rbind(cob_largo, data.frame(
      modelo = "ensemble", origen = o, h = seq_along(real_m),
      cubre80 = NA, cubre95 = NA, ancho95_pct = NA,
      err_rel_pct = 100 * (real_m - as.numeric(pred_ens)) / as.numeric(pred_ens),
      stringsAsFactors = FALSE))
  }
}

# Comparar en los MISMOS origenes, incluidos los del ensemble tras su arranque.
comunes <- apply(res_torneo[, modelos, drop = FALSE], 1,
                 function(x) all(is.finite(x)))
write.csv(res_torneo, file.path(dir_salida, "torneo_rolling_origin.csv"), row.names = FALSE)
write.csv(registro_avisos, file.path(dir_salida, "avisos_modelos.csv"), row.names = FALSE)
if (sum(comunes) < min_origenes_comparacion) {
  stop("Pocos origenes comunes validos. Revisar avisos_modelos.csv antes de seleccionar.")
}
medianas <- vapply(res_torneo[comunes, modelos, drop = FALSE], median, numeric(1))
resumen_torneo <- data.frame(modelo = modelos, n_origenes = sum(comunes),
                             MAPE_mediano = as.numeric(medianas)) %>%
  arrange(MAPE_mediano)
# Elegir con precision completa; redondear solo para mostrar/exportar tablas.
ganador <- resumen_torneo$modelo[1]
mejor_bu <- names(which.min(medianas[modelos_bu]))
mejor_directo <- names(which.min(medianas[c(modelos_base, "ensemble")]))
cat("\nMejor bottom-up en validacion:", mejor_bu,
    "| Mejor directo:", mejor_directo, "\n")
cat("\n=== TORNEO: ORIGENES COMUNES ===\n")
print(resumen_torneo)
cat("\nGANADOR SELECCIONADO EN VALIDACION:", ganador, "\n")

# 4b. DIAGNOSTICO DE LA VALIDACION --------------------------------------
# No cambia el ganador: solo describe cuanta confianza dar a la seleccion.
orig_comunes <- res_torneo$origen[comunes]
err_c <- errores_largo[errores_largo$origen %in% orig_comunes, ]

# (a) Distribucion del MAPE por origen, no solo la mediana.
dist_mape <- do.call(rbind, lapply(modelos, function(m) {
  x <- res_torneo[comunes, m]
  data.frame(modelo = m, n_origenes = length(x), media = mean(x),
             p25 = unname(quantile(x, 0.25)), mediana = median(x),
             p75 = unname(quantile(x, 0.75)), maximo = max(x))
}))
dist_mape <- dist_mape[order(dist_mape$mediana), ]
cat("\n=== DISTRIBUCION DEL MAPE POR ORIGEN (origenes comunes) ===\n")
print(dist_mape, row.names = FALSE, digits = 4)

# (b) MAPE por horizonte (mes 1 a 12) y MAE relativo frente a naive.
mh <- with(err_c, tapply(ape, list(h, modelo), mean))
mape_horizonte <- data.frame(h = as.integer(rownames(mh)),
                             mh[, dist_mape$modelo, drop = FALSE])
cat("\n=== MAPE POR HORIZONTE (meses adelante) ===\n")
print(round(mape_horizonte, 2), row.names = FALSE)
mae_m <- tapply(err_c$ae, err_c$modelo, mean)
mae_relativo <- data.frame(modelo = names(mae_m), MAE = as.numeric(mae_m),
                           MAE_rel_naive = as.numeric(mae_m / mae_m["naive"]))
mae_relativo <- mae_relativo[order(mae_relativo$MAE_rel_naive), ]
cat("\n=== MAE RELATIVO A NAIVE (<1 = mejor que naive) ===\n")
print(mae_relativo, row.names = FALSE, digits = 4)

# (c) Comparacion DESCRIPTIVA de perdidas por origen, sin p-valores.
# El ganador fue seleccionado con esta validacion y los horizontes se solapan.
# No tratar esta comparacion como prueba formal de superioridad.
rivales <- setdiff(modelos, ganador)
comparacion_perdidas <- do.call(rbind, lapply(rivales, function(r) {
  dif <- res_torneo[comunes, ganador] - res_torneo[comunes, r]
  data.frame(modelo_1 = ganador, modelo_2 = r,
             diferencia_media_MAPE_pp = mean(dif),
             proporcion_origenes_gana_modelo_1 = mean(dif < 0))
}))
cat("\n=== DIFERENCIAS DE PERDIDA (descriptivas, no prueba formal) ===\n")
print(comparacion_perdidas, row.names = FALSE, digits = 4)

g_horizonte <- ggplot(
  err_c[err_c$modelo %in% unique(c("naive", "drift", "drift_log", "drift_ventana",
                                   "ets_damped", mejor_bu, ganador)), ] %>%
    group_by(modelo, h) %>% summarise(MAPE = mean(ape), .groups = "drop"),
  aes(h, MAPE, colour = modelo)) +
  geom_line(linewidth = 0.8) + scale_x_continuous(breaks = 1:h_m) +
  labs(title = "MAPE de validacion por horizonte", x = "Meses adelante",
       y = "MAPE (%)", colour = NULL) + theme_minimal()
print(g_horizonte)

write.csv(dist_mape, file.path(dir_salida, "validacion_distribucion_mape.csv"), row.names = FALSE)
write.csv(mape_horizonte, file.path(dir_salida, "validacion_mape_por_horizonte.csv"), row.names = FALSE)
write.csv(mae_relativo, file.path(dir_salida, "validacion_mae_relativo_naive.csv"), row.names = FALSE)
write.csv(comparacion_perdidas, file.path(dir_salida, "validacion_diferencias_perdidas.csv"), row.names = FALSE)
write.csv(errores_largo, file.path(dir_salida, "errores_por_origen_horizonte.csv"), row.names = FALSE)
ggsave(file.path(dir_salida, "validacion_mape_por_horizonte.png"), g_horizonte,
       width = 9, height = 5, dpi = 150)

# 4c. COBERTURA EMPIRICA DE LOS INTERVALOS ------------------------------
# Un intervalo de 95% bien calibrado deberia cubrir ~95% de los reales.
# Las observaciones por origen/horizonte estan correlacionadas (solapamiento),
# asi que la cobertura es una guia, no una prueba formal.
cob_c <- cob_largo[cob_largo$origen %in% orig_comunes & !is.na(cob_largo$cubre95), ]
cob_modelo <- do.call(rbind, lapply(split(cob_c, cob_c$modelo), function(d) {
  data.frame(modelo = d$modelo[1], cobertura_80 = 100 * mean(d$cubre80),
             cobertura_95 = 100 * mean(d$cubre95),
             ancho_95_pct_medio = mean(d$ancho95_pct))
}))
cob_modelo <- cob_modelo[order(match(cob_modelo$modelo, dist_mape$modelo)), ]
cat("\n=== COBERTURA EMPIRICA (nominal 80% y 95%) ===\n")
print(cob_modelo, row.names = FALSE, digits = 4)

cob_h <- do.call(rbind, lapply(unique(c(ganador, "drift")), function(m) {
  d <- cob_c[cob_c$modelo == m, ]
  if (nrow(d) == 0) return(NULL)   # el ensemble no tiene intervalos
  data.frame(modelo = m, h = sort(unique(d$h)),
             cobertura_80 = 100 * as.numeric(tapply(d$cubre80, d$h, mean)),
             cobertura_95 = 100 * as.numeric(tapply(d$cubre95, d$h, mean)))
}))
cat("\n=== COBERTURA POR HORIZONTE: ganador y drift ===\n")
print(cob_h, row.names = FALSE, digits = 4)

write.csv(cob_modelo, file.path(dir_salida, "validacion_cobertura_modelo.csv"), row.names = FALSE)
write.csv(cob_h, file.path(dir_salida, "validacion_cobertura_horizonte.csv"), row.names = FALSE)
write.csv(cob_largo, file.path(dir_salida, "cobertura_por_origen_horizonte.csv"), row.names = FALSE)

# 4d. CALIBRACION EMPIRICA DE INTERVALOS --------------------------------
# Intervalo = pronostico * (1 + h^b * q / 100), con q = cuantiles de los
# errores relativos estandarizados z = err_rel / h^b del ganador en el rolling
# origin. b se estima de como crece el error medio con el horizonte (acotado a
# [0.3, 1]). Capta asimetria y crecimiento del error, sin suponer normalidad.
# Limitaciones: ~6 ventanas anuales independientes, errores solapados y el
# supuesto de que los errores futuros se parecen a los pasados.
cal_df <- cob_largo[cob_largo$modelo == ganador, c("origen", "h", "err_rel_pct")]
niveles_cal <- c(0.80, 0.95)
calibrar_intervalos <- function(d) {
  d <- d[is.finite(d$err_rel_pct) & is.finite(d$h), ]
  if (length(unique(d$origen)) < 3) stop("Pocos origenes para calibrar intervalos.")
  mae_h <- tapply(abs(d$err_rel_pct), d$h, mean)
  b <- unname(coef(lm(log(pmax(as.numeric(mae_h), 1e-8)) ~
                       log(as.numeric(names(mae_h)))))[2])
  if (!is.finite(b)) b <- 0.5
  b <- min(max(b, 0.3), 1)
  z <- d$err_rel_pct / d$h^b
  q <- sapply(niveles_cal, function(nv)
    quantile(z, c((1 - nv) / 2, 1 - (1 - nv) / 2), names = FALSE))
  dimnames(q) <- list(c("inf", "sup"), c("n80", "n95"))
  list(b = b, q = q, n_origenes = length(unique(d$origen)))
}
aplicar_calibracion <- function(pred, cal) {
  esc <- seq_along(pred)^cal$b
  data.frame(
    inf_cal_80 = pred * (1 + esc * cal$q["inf", "n80"] / 100),
    sup_cal_80 = pred * (1 + esc * cal$q["sup", "n80"] / 100),
    inf_cal_95 = pred * (1 + esc * cal$q["inf", "n95"] / 100),
    sup_cal_95 = pred * (1 + esc * cal$q["sup", "n95"] / 100))
}
cal_final <- calibrar_intervalos(cal_df)
cat("\n=== CALIBRACION DE INTERVALOS (", ganador, ") ===\n", sep = "")
cat("Exponente de crecimiento del error con el horizonte (b):",
    round(cal_final$b, 3), "| origenes usados:", cal_final$n_origenes, "\n")
print(round(cal_final$q, 3))

# Diagnostico secuencial de calibracion: solo horizontes cerrados.
# El modelo ganador se eligio con toda la validacion: esto NO es una
# validacion anidada de la seleccion completa, ni prueba de cobertura exacta.
orig_cal <- sort(unique(cal_df$origen))
oos <- do.call(rbind, lapply(orig_cal, function(o) {
  previos <- cal_df[cal_df$origen + h_m <= o, ]
  if (length(unique(previos$origen)) < min_origenes_cal) return(NULL)
  cal <- calibrar_intervalos(previos)
  act <- cal_df[cal_df$origen == o, ]
  zz <- act$err_rel_pct / act$h^cal$b
  data.frame(origen = o, h = act$h,
             cubre80 = zz >= cal$q["inf", "n80"] & zz <= cal$q["sup", "n80"],
             cubre95 = zz >= cal$q["inf", "n95"] & zz <= cal$q["sup", "n95"])
}))
if (!is.null(oos) && nrow(oos) > 0) {
  cob_conv <- cob_c[cob_c$modelo == ganador & cob_c$origen %in% oos$origen, ]
  tab_oos <- data.frame(
    metodo = c("convencional", "calibrado secuencial exploratorio"),
    origenes = length(unique(oos$origen)),
    cobertura_80 = c(if (nrow(cob_conv)) 100 * mean(cob_conv$cubre80) else NA_real_,
                     100 * mean(oos$cubre80)),
    cobertura_95 = c(if (nrow(cob_conv)) 100 * mean(cob_conv$cubre95) else NA_real_,
                     100 * mean(oos$cubre95)))
  cat("\n=== COBERTURA DE LOS INTERVALOS CALIBRADOS (nominal 80% / 95%) ===\n")
  print(tab_oos, row.names = FALSE, digits = 4)
  oos$bloque <- cut(oos$h, c(0, 4, 8, h_m), labels = c("h 1-4", "h 5-8", "h 9-12"))
  tab_oos_h <- data.frame(
    bloque = levels(oos$bloque),
    cobertura_80 = 100 * as.numeric(tapply(oos$cubre80, oos$bloque, mean)),
    cobertura_95 = 100 * as.numeric(tapply(oos$cubre95, oos$bloque, mean)))
  print(tab_oos_h, row.names = FALSE, digits = 4)
  write.csv(tab_oos, file.path(dir_salida, "validacion_calibracion_cobertura.csv"), row.names = FALSE)
  write.csv(tab_oos_h, file.path(dir_salida, "validacion_calibracion_por_bloque.csv"), row.names = FALSE)
} else {
  cat("\nPocos origenes para validar la calibracion fuera de muestra.\n")
}
write.csv(data.frame(parametro = c("b", "q_inf_80", "q_sup_80", "q_inf_95", "q_sup_95"),
                     valor = c(cal_final$b, cal_final$q["inf", "n80"], cal_final$q["sup", "n80"],
                               cal_final$q["inf", "n95"], cal_final$q["sup", "n95"])),
          file.path(dir_salida, "calibracion_intervalos.csv"), row.names = FALSE)

# Pesos fijados antes de evaluar el test. No se ajustan con sus resultados.
validos_final <- which(res_torneo$origen + h_m <= corte &
                         is.finite(res_torneo$tbats) & is.finite(res_torneo$drift))
w_final <- if (length(validos_final) >= min_origenes_pesos) {
  pesos(res_torneo$tbats[validos_final], res_torneo$drift[validos_final])
} else NULL

# Devuelve puntos e intervalos del MISMO modelo. Ensemble: solo puntos.
obtener_pronostico <- function(nombre, train, h, origen, w) {
  if (nombre %in% modelos_bu) {
    fc <- pronostico_bu(nombre, origen, h)
    if (is.null(fc)) return(NULL)
    return(list(media = as.numeric(fc$mean), inferior = rep(NA_real_, h),
                superior = rep(NA_real_, h), componentes = fc$componentes))
  }
  if (nombre != "ensemble") {
    fc <- pronostico_seguro(nombre, train, h, origen)
    if (is.null(fc)) return(NULL)
    return(list(media = as.numeric(fc$mean),
                inferior = as.numeric(fc$lower[, "95%"]),
                superior = as.numeric(fc$upper[, "95%"])))
  }
  if (is.null(w)) return(NULL)
  ft <- pronostico_seguro("tbats", train, h, origen)
  fd <- pronostico_seguro("drift", train, h, origen)
  if (is.null(ft) || is.null(fd)) return(NULL)
  list(media = as.numeric(w["tbats"] * ft$mean + w["drift"] * fd$mean),
       inferior = rep(NA_real_, h), superior = rep(NA_real_, h))
}

# 5. PRUEBA FINAL RESERVADA ---------------------------------------------
train_final <- crear_train(corte)
real_final <- as.numeric(y_m[(corte + 1):n])
fc_final <- obtener_pronostico(ganador, train_final, n_test, corte, w_final)
if (is.null(fc_final)) stop("Fallo el ganador en el ajuste final. Consultar registro_avisos.")
pred_final <- fc_final$media
metricas_finales <- metricas(real_final, pred_final, train_final)
tabla_historica <- data.frame(
  fecha = ipap_completo$fecha[(corte + 1):n],
  ipap_real = real_final, ipap_predicho = pred_final,
  error_absoluto = abs(real_final - pred_final),
  error_pct_absoluto = 100 * abs((real_final - pred_final) / real_final),
  limite_inferior_95 = fc_final$inferior, limite_superior_95 = fc_final$superior)

# Referencias evaluadas en el MISMO test y horizonte. No cambiar el ganador
# mirando este test; cualquier redisenio posterior requiere otra validacion.
pred_n <- as.numeric(naive(train_final, h = n_test)$mean)
pred_sn <- as.numeric(snaive(train_final, h = n_test)$mean)
pred_d <- as.numeric(rwf(train_final, h = n_test, drift = TRUE)$mean)
comparacion_test <- data.frame(
  metodo = c(paste0("seleccionado_", ganador), "referencia_naive",
             "referencia_snaive", "referencia_drift"),
  rbind(metricas_finales, metricas(real_final, pred_n, train_final),
        metricas(real_final, pred_sn, train_final),
        metricas(real_final, pred_d, train_final)), row.names = NULL)
fc_bu_test <- obtener_pronostico(mejor_bu, train_final, n_test, corte, w_final)
fc_directo_test <- obtener_pronostico(mejor_directo, train_final, n_test, corte, w_final)
if (is.null(fc_bu_test) || is.null(fc_directo_test)) {
  stop("Fallo el mejor bottom-up o directo en el test; revisar registro_avisos.")
}
comparacion_test <- rbind(comparacion_test,
  data.frame(metodo = paste0("mejor_bu_validacion_", mejor_bu),
             t(metricas(real_final, fc_bu_test$media, train_final))),
  data.frame(metodo = paste0("mejor_directo_validacion_", mejor_directo),
             t(metricas(real_final, fc_directo_test$media, train_final))))
comparacion_bottomup <- data.frame(fecha = ipap_completo$fecha[(corte + 1):n],
  ipap_real = real_final, directo = fc_directo_test$media,
  bottom_up32 = fc_bu_test$media)
cat("\n=== PRUEBA FINAL: GANADOR Y REFERENCIAS ===\n")
print(comparacion_test)
print(tabla_historica)
cat("\nMASE usa escala no estacional del entrenamiento; ver comparacion directa con referencias.\n")

# 5b. INTERVALOS CALIBRADOS EN LA PRUEBA FINAL --------------------------
# cal_final usa solo origenes de validacion (todos terminan antes del test).
# Es un solo origen de 12 meses: sirve de verificacion, no de prueba formal.
ic_test <- aplicar_calibracion(pred_final, cal_final)
tabla_historica <- cbind(tabla_historica, ic_test)
cob_test <- data.frame(
  intervalo = c("convencional 95%", "calibrado 80%", "calibrado 95%"),
  cobertura_pct = 100 * c(
    mean(real_final >= tabla_historica$limite_inferior_95 &
           real_final <= tabla_historica$limite_superior_95),
    mean(real_final >= ic_test$inf_cal_80 & real_final <= ic_test$sup_cal_80),
    mean(real_final >= ic_test$inf_cal_95 & real_final <= ic_test$sup_cal_95)))
cat("\n=== COBERTURA EN LA PRUEBA FINAL (12 meses) ===\n")
print(cob_test, row.names = FALSE, digits = 4)

# 6. PRONOSTICO FUTURO CON TODA LA SERIE ---------------------------------
# Reestimar el ganador con datos hasta el ultimo mes observado.
# Mantener la regla de seleccion y los pesos congelados de validacion.
fc_futuro <- obtener_pronostico(ganador, y_m, h_m, n, w_final)
if (is.null(fc_futuro)) stop("Fallo el ganador al pronosticar el futuro. Consultar registro_avisos.")
fechas_futuras <- seq(tail(ipap_completo$fecha, 1), by = "month", length.out = h_m + 1)[-1]
tabla_futuro <- data.frame(
  fecha = fechas_futuras, ipap_pronosticado = fc_futuro$media,
  limite_inferior_95 = fc_futuro$inferior,
  limite_superior_95 = fc_futuro$superior)
cat("\n=== PRONOSTICO FUTURO:", ganador, "===\n")
print(tabla_futuro)
# Intervalos empiricos del ganador: no aseguran cobertura nominal.
# Revisar diagnosticos antes de preferirlos a los convencionales.
# Las columnas limite_inferior_95/limite_superior_95 son las convencionales.
tabla_futuro <- cbind(tabla_futuro,
                      aplicar_calibracion(tabla_futuro$ipap_pronosticado, cal_final))
cat("\n=== PRONOSTICO FUTURO CON INTERVALOS CALIBRADOS ===\n")
print(tabla_futuro[, c("fecha", "ipap_pronosticado", "inf_cal_80", "sup_cal_80",
                       "inf_cal_95", "sup_cal_95")], digits = 5)
if (ganador %in% c("ensemble", modelos_bu)) {
  cat("Sin intervalos convencionales para esta agregacion; NA no significa incertidumbre nula.\n")
  cat("Los intervalos empiricos calibrados se presentan como exploratorios.\n")
}
# Los intervalos de los modelos individuales son sus intervalos convencionales.
# Su cobertura debe evaluarse; este script NO implementa ajuste HAC ni SVECM.


# 6b. DETALLE DEL MEJOR BOTTOM-UP (AUN SI NO GANA EL TORNEO) -------------
fc_bu_futuro <- obtener_pronostico(mejor_bu, y_m, h_m, n, w_final)
fc_directo_futuro <- obtener_pronostico(mejor_directo, y_m, h_m, n, w_final)
if (is.null(fc_bu_futuro) || is.null(fc_directo_futuro)) {
  stop("Fallo un candidato al generar la comparacion futura; revisar registro_avisos.")
}
tabla_futuro_comparada <- data.frame(fecha = fechas_futuras,
  directo = fc_directo_futuro$media, bottom_up32 = fc_bu_futuro$media,
  seleccionado = fc_futuro$media)
detalle_componentes <- function(fc, fechas) {
  h <- length(fechas)
  data.frame(fecha = rep(fechas, times = length(pesos_bu)),
    subclase = rep(names(pesos_bu), each = h),
    ponderacion = rep(as.numeric(pesos_bu), each = h),
    indice_pronosticado = as.numeric(fc$componentes),
    aporte_al_nivel_total = as.numeric(sweep(fc$componentes, 2, pesos_bu, "*")))
}
detalle_bu_test <- detalle_componentes(fc_bu_test, comparacion_bottomup$fecha)
detalle_bu_futuro <- detalle_componentes(fc_bu_futuro, fechas_futuras)
cat("\n=== COMPARACION FUTURA: DIRECTO VS BOTTOM-UP32 ===\n")
print(tabla_futuro_comparada)

# 7. GRAFICAS -----------------------------------------------------------
g_test <- ggplot(tabla_historica, aes(x = fecha)) +
  geom_ribbon(aes(ymin = inf_cal_95, ymax = sup_cal_95),
              fill = "#007C91", alpha = 0.12) +
  geom_line(aes(y = ipap_real, colour = "Observado"), linewidth = 0.8) +
  geom_line(aes(y = ipap_predicho, colour = "Pronosticado"), linewidth = 0.8) +
  labs(title = paste("IPAP: prueba final -", ganador), x = NULL,
       y = "Puntos del IPAP", colour = NULL) + theme_minimal()
# La linea del pronostico parte del ultimo dato observado.
linea_fc <- rbind(
  data.frame(fecha = tail(ipap_completo$fecha, 1), ipap_pronosticado = tail(ipap_completo$IPAP, 1)),
  tabla_futuro[, c("fecha", "ipap_pronosticado")])
g_futuro <- ggplot(tail(ipap_completo, 60), aes(fecha, IPAP)) +
  geom_line(colour = "grey35", linewidth = 0.8) +
  geom_ribbon(data = tabla_futuro, inherit.aes = FALSE,
              aes(x = fecha, ymin = inf_cal_95, ymax = sup_cal_95),
              fill = "#007C91", alpha = 0.12) +
  geom_ribbon(data = tabla_futuro, inherit.aes = FALSE,
              aes(x = fecha, ymin = inf_cal_80, ymax = sup_cal_80),
              fill = "#007C91", alpha = 0.22) +
  geom_line(data = linea_fc, aes(fecha, ipap_pronosticado),
            colour = "#007C91", linewidth = 0.9) +
  labs(title = paste("IPAP: pronostico a 12 meses -", ganador),
       x = NULL, y = "Puntos del IPAP") + theme_minimal()
print(g_test)
print(g_futuro)

g_bottomup <- ggplot(comparacion_bottomup, aes(fecha)) +
  geom_line(aes(y = ipap_real, colour = "Observado"), linewidth = 0.9) +
  geom_line(aes(y = directo, colour = "Mejor directo"), linewidth = 0.8) +
  geom_line(aes(y = bottom_up32, colour = "Mejor bottom-up32"), linewidth = 0.8) +
  labs(title = "Prueba retrospectiva: directo vs bottom-up de 32 subclases",
       subtitle = paste(mejor_directo, "vs", mejor_bu), x = NULL,
       y = "Puntos del IPAP", colour = NULL) + theme_minimal()
print(g_bottomup)

mes <- as.Date("2026-03-01")
i <- match(mes, ipap_completo$fecha)
h <- match(mes, comparacion_bottomup$fecha)

error_marzo <- data.frame(
  subclase = names(pesos_bu),
  error_ponderado = as.numeric(
    (matriz_subclases[i, ] - fc_bu_test$componentes[h, ]) * pesos_bu
  )
)

head(error_marzo[order(-abs(error_marzo$error_ponderado)), ], 5)

# 8. EXPORTAR -----------------------------------------------------------
# Numeros sin redondear: evita alterar las metricas antes de calcularlas.
write.csv(ipap_completo, file.path(dir_salida, "ipap_mensual_observado.csv"), row.names = FALSE)
write.csv(resumen_torneo, file.path(dir_salida, "resumen_torneo.csv"), row.names = FALSE)
write.csv(tabla_historica, file.path(dir_salida, "prueba_historica.csv"), row.names = FALSE)
write.csv(comparacion_test, file.path(dir_salida, "metricas_prueba.csv"), row.names = FALSE)
write.csv(tabla_futuro, file.path(dir_salida, "pronostico_futuro.csv"), row.names = FALSE)
write.csv(registro_avisos, file.path(dir_salida, "avisos_modelos.csv"), row.names = FALSE)
ggsave(file.path(dir_salida, "prueba_historica.png"), g_test, width = 9, height = 5, dpi = 150)
ggsave(file.path(dir_salida, "pronostico_futuro.png"), g_futuro, width = 9, height = 5, dpi = 150)
write.csv(comparacion_bottomup, file.path(dir_salida, "comparacion_bottomup_test.csv"), row.names = FALSE)
write.csv(tabla_futuro_comparada, file.path(dir_salida, "comparacion_bottomup_futuro.csv"), row.names = FALSE)
write.csv(detalle_bu_test, file.path(dir_salida, "subclases_pronosticadas_test.csv"), row.names = FALSE)
write.csv(detalle_bu_futuro, file.path(dir_salida, "subclases_pronosticadas_futuro.csv"), row.names = FALSE)
ggsave(file.path(dir_salida, "comparacion_bottomup_test.png"), g_bottomup,
       width = 9, height = 5, dpi = 150)
saveRDS(list(ganador = ganador, mejor_bottomup = mejor_bu,
  mejor_directo = mejor_directo, ponderaciones_subclases = pesos_bu,
  auditoria_agregacion = auditoria_agregacion,
  comparacion_bottomup = comparacion_bottomup, pesos = w_final, parametros = list(
  h = h_m, n_test = n_test, inicio_train = inicio_train, paso = paso),
  calibracion = cal_final, torneo = res_torneo, resumen = resumen_torneo, prueba = tabla_historica,
  metricas = comparacion_test, futuro = tabla_futuro, session = sessionInfo()),
  file.path(dir_salida, "resultados_modelo.rds"))
capture.output(sessionInfo(), file = file.path(dir_salida, "sesion_R.txt"))
cat("\nProceso terminado. Resultados en:", dir_salida, "\n")
