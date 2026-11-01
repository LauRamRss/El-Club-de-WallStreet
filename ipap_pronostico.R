# ============================================================
# IPAP SUBCLASES - PRONOSTICO A 12 MESES (v9)
# Indice de Precios al Productor Agropecuario (Colombia)
#
# Objetivo: pronosticar la suma ponderada de las 32 subclases del IPAP
# Enfoque : menu de modelos + combinaciones + bottom-up + intervalos
#           calibrados, evaluado con walk-forward sin fuga de informacion.
#
# Uso     : colocar IPAP_SUBCLASES.xlsx en data/ y ejecutar desde la
#           raiz del repositorio:  source("R/ipap_pronostico.R")
# Salidas : carpeta outputs/ (CSV y graficos)
# ============================================================

# 1. SETUP ---------------------------------------------------------------
paquetes <- c("readxl", "lubridate", "forecast", "tseries", "FinTS",
              "urca", "vars", "ggplot2", "future.apply", "glmnet")

faltan <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan)) install.packages(faltan, dependencies = TRUE)

suppressPackageStartupMessages({
  library(readxl);       library(lubridate);  library(forecast)
  library(tseries);      library(FinTS);      library(urca)
  library(vars);         library(ggplot2);    library(future.apply)
  library(glmnet)
})

plan(multisession, workers = max(1, parallel::detectCores() - 1))
set.seed(123)

# --- Parametros globales ------------------------------------------------
objetivo    <- "suma"         # "suma" (ponderada 32 subclases) o "total" (TOTAL_IPAP)
h           <- 12L            # horizonte (meses)
n_test      <- 12L            # ultimos 12 meses = test
origen_ini  <- 96L            # primer origen del walk-forward
inicio_val  <- as.Date("2020-06-01")
paso_wf     <- 1L
pub_lag     <- 1L             # las externas se conocen hasta o - 1
lag_x_fijo  <- 3L             # rezago fijo de las externas en el ARIMAX
t_ini_x     <- 30L            # primera fila usada para ajustar ARIMA/ARIMAX
p_max       <- 3L
q_max       <- 3L
reid_cada   <- 12L            # re-identificar BJ cada 12 origenes
min_pesos   <- 12L            # origenes cerrados minimos para pesos anidados
vol_ventana <- 24L            # meses para volatilidad que escala IC
modelo_ic   <- "Combo_final"
n_boot      <- 1000L
bloque_boot <- 12L
alpha_mcs   <- 0.10
min_cal     <- 24L

# --- Rutas (relativas a la raiz del repositorio) --------------------------
ruta    <- file.path("data", "IPAP_SUBCLASES.xlsx")
dir_out <- "outputs"
dir.create(dir_out, showWarnings = FALSE)
if (!file.exists(ruta)) {
  message("No se encontro ", ruta, ". Selecciona el archivo manualmente.")
  ruta <- file.choose()
}

# --- Funcion utilitaria de impresion ------------------------------------
mostrar <- function(nombre, x, d = 3) {
  if (is.data.frame(x)) {
    rownames(x) <- NULL
    x[] <- lapply(x, function(v) if (is.numeric(v)) round(v, d) else v)
  }
  print(setNames(list(x), nombre))
}

# 2. CARGA Y VALIDACION --------------------------------------------------
datos <- as.data.frame(read_excel(ruta, sheet = 1))
names(datos) <- toupper(trimws(names(datos)))
datos$FECHA <- floor_date(as.Date(datos$FECHA), "month")
datos <- datos[order(datos$FECHA), ]
n <- nrow(datos)
corte <- n - n_test

stopifnot(
  !anyDuplicated(datos$FECHA),
  identical(as.character(datos$FECHA),
            as.character(seq(datos$FECHA[1], datos$FECHA[n], by = "month"))),
  !anyNA(datos[1:corte, -1])
)

pos_cols <- setdiff(names(datos), "FECHA")
stopifnot(all(as.matrix(datos[, pos_cols, drop = FALSE]) > 0))

# --- Deteccion de variables externas -------------------------------------
# El codigo funciona aunque NO existan las externas; solo desactiva ARIMAX/VAR.
candidatas_ext <- c("TRM", "UREA_USD", "NOAA", "MAIZ_USD", "TRIGO_USD",
                    "SOYA_USD", "CAFE_USD_LIBRA", "INFLACION")
externas_disponibles <- intersect(candidatas_ext, names(datos))
if (length(externas_disponibles) == 0) {
  warning("No se encontraron variables externas en el Excel. ",
          "Se desactivan ARIMAX y VAR recursivo. ",
          "El pipeline sigue funcionando solo con series internas.")
}

# 3. SERIE OBJETIVO, PONDERACIONES Y MAPA DE EXTERNAS -------------------
mensual <- function(x) ts(x,
                          start = c(year(datos$FECHA[1]), month(datos$FECHA[1])),
                          frequency = 12)

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
  SEMILLA = 0.000784934, CRUSTACEOS = 0.000729849)
subclases <- names(pesos_bu)

stopifnot(all(c("TOTAL_IPAP", subclases) %in% names(datos)))

Y <- as.matrix(datos[, subclases, drop = FALSE])
suma32 <- as.numeric(Y %*% pesos_bu)
brecha <- datos$TOTAL_IPAP - suma32
idx_br <- which(abs(brecha) > 0.01)
if (length(idx_br)) {
  mostrar("Meses con diferencia entre TOTAL_IPAP y la suma ponderada",
          data.frame(fecha = format(datos$FECHA[idx_br], "%Y-%m"),
                     total = datos$TOTAL_IPAP[idx_br],
                     suma32 = suma32[idx_br],
                     brecha = brecha[idx_br]))
}

y_obj   <- if (objetivo == "suma") suma32 else datos$TOTAL_IPAP
ipap_ts <- mensual(y_obj)
y_vec   <- as.numeric(ipap_ts)
ly      <- log(y_vec)
y_te    <- y_vec[(corte + 1):n]
fechas_test <- datos$FECHA[(corte + 1):n]

# --- Mapa de externas por subclase (solo las disponibles) ---------------
grupo_fert <- c("ARROZ", "PAPAS", "YUCA", "PLATANO", "BANANO", "TOMATES",
                "HORTALIZA_RAIZ", "HORTALIZA_HOJA", "HORTALIZA_FRUTO",
                "LEGUMBRES_SECAS", "LEGUMBRES_VERDES", "CITRICOS",
                "FRUTAS_PEPITA", "FRUTAS_TROPICALES", "OTROS_FRUTOS")

mapa_x_raw <- c(
  list(MAIZ = c("MAIZ_USD", "TRM"), TRIGO = c("TRIGO_USD", "TRM"),
       CEBADA = c("TRIGO_USD", "TRM"), SORGO = c("MAIZ_USD", "TRM"),
       SOJA = c("SOYA_USD", "TRM"),
       POLLO = c("MAIZ_USD", "SOYA_USD"), HUEVOS = c("MAIZ_USD", "SOYA_USD"),
       LECHE = c("MAIZ_USD", "SOYA_USD"), BOVINO = c("MAIZ_USD"),
       AGREGADO = c("UREA_USD", "TRM", "NOAA")),
  setNames(rep(list(c("UREA_USD", "NOAA")), length(grupo_fert)), grupo_fert))

mapa_x <- lapply(mapa_x_raw, function(v) intersect(v, externas_disponibles))
vars_de <- function(s) if (is.null(mapa_x[[s]])) character(0) else mapa_x[[s]]

# 4. DIAGNOSTICO POR SUBCLASE (solo entrenamiento) ----------------------
diag_sub <- do.call(rbind, lapply(subclases, function(s) {
  y  <- datos[[s]][1:corte]
  lys <- log(y)
  dl  <- diff(lys)
  yt  <- ts(lys, frequency = 12)
  st  <- tryCatch(stl(yt, s.window = "periodic", robust = TRUE)$time.series,
                  error = function(e) NULL)
  Fs <- if (is.null(st)) NA_real_ else
    max(0, 1 - var(st[, "remainder"]) / var(st[, "seasonal"] + st[, "remainder"]))
  Ft <- if (is.null(st)) NA_real_ else
    max(0, 1 - var(st[, "remainder"]) / var(st[, "trend"] + st[, "remainder"]))
  data.frame(
    subclase = s,
    peso = unname(pesos_bu[s]),
    vol_mensual_pct = 100 * sd(dl),
    fuerza_estac = Fs,
    fuerza_tend = Ft,
    atipicos = tryCatch(length(tsoutliers(yt)$index),
                        error = function(e) NA_integer_),
    max_abs_var_pct = 100 * max(abs(dl)),
    pct_dif_cero = 100 * mean(diff(y) == 0),
    pct_dif2_cero = 100 * mean(abs(diff(y, differences = 2)) < 1e-8),
    ARCH_p = tryCatch(ArchTest(dl, lags = 12)$p.value,
                      error = function(e) NA_real_),
    externas_prior = paste(if (length(vars_de(s))) vars_de(s) else "-",
                           collapse = "+"))
}))
mostrar("Diagnostico por subclase (entrenamiento, ordenado por peso)",
        diag_sub[order(-diag_sub$peso), ], 3)

# 5. FUNCIONES AUXILIARES ------------------------------------------------
mape_vec <- function(r, p) 100 * mean(abs((r - p) / r))
ape_fila <- function(r, p) 100 * rowMeans(abs((r - p) / r))
mae_fila <- function(r, p) rowMeans(abs(r - p))
dif_alineada <- function(z, d) if (d == 0) z else c(rep(NA_real_, d), diff(z, differences = d))

etq <- function(s) sprintf("ARIMA(%d,%d,%d)(%d,%d,%d)[12]",
                           s$order[1], s$order[2], s$order[3],
                           s$seas[1], s$seas[2], s$seas[3])

# --- Box-Jenkins con rejilla -------------------------------------------
ajusta <- function(y, spec) {
  Arima(y,
        order = spec$order,
        seasonal = list(order = spec$seas, period = 12),
        include.drift = (spec$order[2] + spec$seas[2]) == 1,
        lambda = 0, biasadj = TRUE)
}

diferenciar <- function(ylog) {
  D <- tryCatch(min(nsdiffs(ylog, test = "ocsb", max.D = 1), 1L),
                error = function(e) 0L)
  x <- if (D > 0) diff(ylog, lag = 12, differences = D) else ylog
  list(D = D, d = ndiffs(x, test = "kpss", max.d = 2))
}

ajustar_grid <- function(y_train, d, D) {
  g <- expand.grid(p = 0:p_max, q = 0:q_max, PQ = 1:3)
  g$P <- ifelse(g$PQ == 2, 1L, 0L)
  g$Q <- ifelse(g$PQ == 3, 1L, 0L)
  filas <- lapply(seq_len(nrow(g)), function(i) {
    spec <- list(order = c(g$p[i], d, g$q[i]),
                 seas  = c(g$P[i], D, g$Q[i]))
    fit <- tryCatch(suppressWarnings(ajusta(y_train, spec)),
                    error = function(e) NULL)
    if (is.null(fit) || !is.finite(fit$aicc)) return(NULL)
    e <- na.omit(as.numeric(residuals(fit)))
    k <- g$p[i] + g$q[i] + g$P[i] + g$Q[i]
    lb <- tryCatch(
      Box.test(e,
               lag = max(k + 2L, min(24L, floor(length(e) / 5))),
               type = "Ljung-Box", fitdf = k)$p.value,
      error = function(e) NA_real_)
    data.frame(p = g$p[i], q = g$q[i], P = g$P[i], Q = g$Q[i],
               AICc = fit$aicc, BIC = fit$bic, LB_p = lb)
  })
  tabla <- do.call(rbind, filas)
  if (is.null(tabla)) stop("Ningun candidato convergio.")
  tabla[order(tabla$AICc), ]
}

especificar <- function(y_tr) {
  dd <- diferenciar(log(y_tr))
  tabla <- ajustar_grid(y_tr, dd$d, dd$D)
  ok <- tabla[!is.na(tabla$LB_p) & tabla$LB_p > 0.05, ]
  el <- if (nrow(ok) > 0) ok[which.min(ok$AICc), ] else tabla[which.min(tabla$AICc), ]
  spec <- list(order = c(el$p, dd$d, el$q), seas = c(el$P, dd$D, el$Q))
  list(spec = spec, fit = ajusta(y_tr, spec), fila = el, dd = dd)
}

# --- Walk-forward -------------------------------------------------------
correr_wf <- function(real_vec, f_pred, origenes) {
  k <- length(origenes)
  real <- matrix(NA_real_, k, h)
  fc   <- matrix(NA_real_, k, h)
  avisos <- integer(k)
  fallo  <- logical(k)
  for (i in seq_len(k)) {
    o <- origenes[i]
    real[i, ] <- real_vec[(o + 1):(o + h)]
    fc[i, ] <- tryCatch(
      withCallingHandlers(
        as.numeric(f_pred(o)),
        warning = function(w) {
          avisos[i] <<- avisos[i] + 1L
          invokeRestart("muffleWarning")
        }),
      error = function(e) rep(NA_real_, h))
    if (length(fc[i, ]) != h || !all(is.finite(fc[i, ]))) {
      fallo[i] <- TRUE
      fc[i, ] <- rep(real_vec[o], h)
    }
  }
  list(origenes = origenes, real = real, fc = fc,
       avisos = avisos, fallo = fallo)
}

# --- Evaluacion ---------------------------------------------------------
evaluar <- function(lista, nv, filas) {
  real   <- nv$real[filas, , drop = FALSE]
  mae_nv <- mae_fila(real, nv$fc[filas, , drop = FALSE])
  bloque <- cut(seq_along(filas), 3, labels = c("T1", "T2", "T3"))
  res <- do.call(rbind, lapply(names(lista), function(m) {
    fc  <- lista[[m]]$fc[filas, , drop = FALSE]
    mae <- mae_fila(real, fc)
    rel <- sapply(levels(bloque), function(b) sum(mae[bloque == b]) / sum(mae_nv[bloque == b]))
    data.frame(
      modelo = m,
      origenes = length(filas),
      MAPE_media = mean(ape_fila(real, fc)),
      MAE_rel = sum(mae) / sum(mae_nv),
      T1 = unname(rel[1]), T2 = unname(rel[2]), T3 = unname(rel[3]),
      pct_gana_naive = 100 * mean(mae < mae_nv))
  }))
  res[order(res$MAE_rel), ]
}

bloques_fechas <- function(filas, origenes_ref) {
  b <- cut(seq_along(filas), 3, labels = c("T1", "T2", "T3"))
  do.call(rbind, lapply(levels(b), function(x) {
    f <- datos$FECHA[origenes_ref[filas][b == x]]
    data.frame(bloque = x,
               origen_desde = format(min(f), "%Y-%m"),
               origen_hasta = format(max(f), "%Y-%m"))
  }))
}

# --- Diebold-Mariano HAC ------------------------------------------------
dm_hac <- function(d, bw) {
  n <- length(d); u <- d - mean(d)
  g <- sapply(0:(bw - 1), function(k) sum(u[(k + 1):n] * u[1:(n - k)]) / n)
  w <- if (bw > 1) c(1, 2 * (1 - (1:(bw - 1)) / bw)) else 1
  v <- max(sum(w * g) / n, 1e-12)
  corr <- sqrt(max((n + 1 - 2 * bw + bw * (bw - 1) / n) / n, 1e-6))
  stat <- mean(d) / sqrt(v) * corr
  list(stat = stat, p = 2 * pt(-abs(stat), df = n - 1))
}

tabla_dm <- function(lista, filas, ref = "Naive") {
  real  <- lista[[ref]]$real[filas, , drop = FALSE]
  L_ref <- mae_fila(real, lista[[ref]]$fc[filas, , drop = FALSE])
  r <- do.call(rbind, lapply(setdiff(names(lista), ref), function(m) {
    L  <- mae_fila(real, lista[[m]]$fc[filas, , drop = FALSE])
    dm <- dm_hac(L - L_ref, h)
    data.frame(modelo = m, dif_MAE = mean(L - L_ref),
               DM = dm$stat, p = dm$p)
  }))
  r$p_holm <- p.adjust(r$p, "holm")
  r[order(r$p), ]
}

# --- Model Confidence Set ----------------------------------------------
mcs <- function(L, alpha = 0.10, B = 1000, l = 12) {
  Tn <- nrow(L); m <- ncol(L); nb <- ceiling(Tn / l)
  idx <- replicate(B, {
    s <- sample.int(Tn, nb, replace = TRUE)
    ((rep(s, each = l) + rep(0:(l - 1), nb) - 1) %% Tn + 1)[1:Tn]
  })
  MB <- t(apply(idx, 2, function(ix) colMeans(L[ix, , drop = FALSE])))
  m0 <- colMeans(L)
  activos <- seq_len(m); p_act <- 0
  pv <- setNames(rep(NA_real_, m), colnames(L))
  pasos <- list()
  repeat {
    S <- activos; k <- length(S)
    if (k == 1) { pv[S] <- 1; break }
    par_ij <- which(upper.tri(matrix(0, k, k)), arr.ind = TRUE)
    i1 <- S[par_ij[, 1]]; i2 <- S[par_ij[, 2]]
    d0 <- m0[i1] - m0[i2]
    dB <- MB[, i1, drop = FALSE] - MB[, i2, drop = FALSE]
    se <- pmax(apply(dB, 2, sd), 1e-12)
    tt <- d0 / se
    t0 <- max(abs(tt))
    tB <- apply(abs(sweep(sweep(dB, 2, d0), 2, se, "/")), 1, max)
    p_act <- max(p_act, mean(tB >= t0))
    if (p_act >= alpha) { pv[S] <- p_act; break }
    w <- which.max(abs(tt))
    peor <- if (tt[w] > 0) i1[w] else i2[w]
    pv[peor] <- p_act
    pasos[[length(pasos) + 1]] <- data.frame(
      eliminado = colnames(L)[peor], T_R = t0, p = p_act)
    activos <- setdiff(S, peor)
  }
  list(tabla = data.frame(modelo = colnames(L), MAE_medio = m0,
                          p_MCS = pv, en_MCS = pv >= alpha)[order(-pv, m0), ],
       pasos = if (length(pasos)) do.call(rbind, pasos) else NULL)
}

L_mat <- function(lista, filas) sapply(lista, function(w)
  mae_fila(w$real[filas, , drop = FALSE], w$fc[filas, , drop = FALSE]))

# --- Intervalos: empirico, escala, escala_h + conformal simple ---------
dy_tot <- c(NA, diff(ly))
sig_o <- function(o) sd(dy_tot[(o - vol_ventana + 1):o])
metodos_ic <- c("empirico_h", "escala", "escala_h", "conformal")

ic_calibrado <- function(metodo, pred, sig_pred, E, sig_cal) {
  esc <- sqrt(seq_len(h))
  Q <- function(M, p) apply(M, 2, quantile, probs = p, names = FALSE)
  if (metodo == "empirico_h") {
    l80 <- cummin(Q(E, 0.10)); u80 <- cummax(Q(E, 0.90))
    l95 <- cummin(Q(E, 0.025)); u95 <- cummax(Q(E, 0.975))
  } else if (metodo == "conformal") {
    # Conformal simple: cuantiles de |E| por horizonte
    absE <- abs(E)
    q80 <- Q(absE, 0.80); q95 <- Q(absE, 0.95)
    l80 <- -q80; u80 <- q80
    l95 <- -q95; u95 <- q95
  } else {
    U <- E / outer(sig_cal, esc)
    s <- sig_pred * esc
    if (metodo == "escala") {
      q <- quantile(as.numeric(U), c(0.10, 0.90, 0.025, 0.975), names = FALSE)
      l80 <- q[1] * s; u80 <- q[2] * s
      l95 <- q[3] * s; u95 <- q[4] * s
    } else {
      l80 <- cummin(Q(U, 0.10)) * s; u80 <- cummax(Q(U, 0.90)) * s
      l95 <- cummin(Q(U, 0.025)) * s; u95 <- cummax(Q(U, 0.975)) * s
    }
  }
  data.frame(inf80 = pred * (1 + l80), sup80 = pred * (1 + u80),
             inf95 = pred * (1 + l95), sup95 = pred * (1 + u95))
}

cobertura_seq <- function(w, metodo, filas, origenes_ref) {
  E_all <- w$real / w$fc - 1
  res <- lapply(filas, function(i) {
    cerr <- intersect(which(origenes_ref + h <= origenes_ref[i]), filas)
    if (length(cerr) < min_cal) return(NULL)
    ic <- ic_calibrado(metodo, w$fc[i, ], sig_o(origenes_ref[i]),
                       E_all[cerr, , drop = FALSE],
                       sapply(origenes_ref[cerr], sig_o))
    data.frame(i = i, h = seq_len(h),
               c80 = w$real[i, ] >= ic$inf80 & w$real[i, ] <= ic$sup80,
               c95 = w$real[i, ] >= ic$inf95 & w$real[i, ] <= ic$sup95,
               ancho95 = 100 * (ic$sup95 - ic$inf95) / w$fc[i, ])
  })
  do.call(rbind, res)
}

# 6. ORIGENES Y MENU DE MODELOS -----------------------------------------
origenes     <- seq(origen_ini, corte - h, by = paso_wf)
origenes_ext <- c(origenes, corte, n)
filas_todas  <- seq_along(origenes)
filas_val    <- which(datos$FECHA[origenes] >= inicio_val)
fila_test    <- length(origenes) + 1L
fila_fin     <- length(origenes) + 2L
ids_all      <- sort(unique(c(origenes[seq(1, length(origenes), by = reid_cada)],
                              corte, n)))

mostrar("Configuracion",
        data.frame(
          meses = n, train = corte, test = n_test,
          origenes = length(origenes),
          origenes_ventana_val = length(filas_val),
          reidentificaciones_BJ = length(ids_all),
          objetivo = objetivo,
          externas_disponibles = length(externas_disponibles)))
mostrar("Bloques de origenes - ventana completa",
        bloques_fechas(filas_todas, origenes))
mostrar("Bloques de origenes - ventana desde inicio_val",
        bloques_fechas(filas_val, origenes))

identificar_nested <- function(y) {
  setNames(lapply(ids_all, function(oo)
    tryCatch(
      especificar(ts(y[1:oo], start = start(ipap_ts), frequency = 12))$spec,
      error = function(e) list(order = c(0, 1, 0), seas = c(0, 0, 0)))),
    as.character(ids_all))
}

spec_para <- function(specs, o) specs[[as.character(max(ids_all[ids_all <= o]))]]

modelos_menu <- c("Naive", "Drift", "ETS_amort", "ARIMA_sd", "BJ_deriva",
                  "STL_ETS", "Theta", "TBATS")

pred_modelo <- function(m, y, o, specs) {
  yt <- ts(y[1:o], start = start(ipap_ts), frequency = 12)
  switch(m,
    Naive = rep(y[o], h),
    Drift = y[o] + (y[o] - y[1]) / (o - 1) * seq_len(h),
    ETS_amort = {
      # Error aditivo, tendencia amortiguada, sin estacionalidad (sobre log)
      fit <- tryCatch(
        ets(yt, model = "ZAN", damped = TRUE, lambda = 0, biasadj = TRUE),
        error = function(e) ets(yt, lambda = 0, biasadj = TRUE))
      as.numeric(forecast(fit, h = h)$mean)
    },
    ARIMA_sd = {
      fit <- auto.arima(yt, lambda = 0,
                        allowdrift = FALSE,
                        seasonal = TRUE,
                        stepwise = TRUE,
                        approximation = FALSE)
      as.numeric(forecast(fit, h = h, biasadj = TRUE)$mean)
    },
    BJ_deriva = as.numeric(forecast(ajusta(yt, spec_para(specs, o)),
                                    h = h)$mean),
    STL_ETS = as.numeric(stlf(yt, h = h, s.window = "periodic",
                              robust = TRUE,
                              lambda = 0, biasadj = TRUE)$mean),
    Theta = as.numeric(forecast(thetaf(yt, h = h),
                                level = c(80, 95))$mean),
    TBATS = {
      fit <- tryCatch(tbats(yt, use.box.cox = TRUE,
                            use.trend = TRUE,
                            use.damped.trend = TRUE,
                            use.arma.errors = TRUE),
                      error = function(e) NULL)
      if (is.null(fit)) rep(y[o], h)
      else as.numeric(forecast(fit, h = h)$mean)
    },
    stop("modelo desconocido"))
}

# 7. EXTERNAS: ARIMAX CON PRIOR ECONOMICO ------------------------------
mx <- NULL
if (length(externas_disponibles) > 0) {
  crear_x <- function(vars_x) {
    h_l <- h; pub_l <- pub_lag; lag_l <- lag_x_fijo; t_ini <- t_ini_x
    X_mat <- sapply(vars_x, function(v) {
      z <- datos[[v]]
      if (v == "NOAA") z else dif_alineada(log(z), 1)
    })
    colnames(X_mat) <- vars_x
    cache <- new.env(parent = emptyenv())
    build_M <- function(o) {
      key <- as.character(o)
      if (!is.null(cache[[key]])) return(cache[[key]])
      M <- matrix(NA_real_, o + h_l, length(vars_x),
                  dimnames = list(NULL, vars_x))
      for (v in vars_x) {
        z   <- X_mat[seq_len(o - pub_l), v]
        ini <- which(!is.na(z))[1]
        ult <- max(which(!is.na(z)))
        zz  <- z[ini:ult]
        M[ini:ult, v] <- zz
        M[(ult + 1):(o + h_l), v] <- tryCatch(
          as.numeric(forecast(auto.arima(ts(zz, frequency = 12),
                                         seasonal = TRUE,
                                         max.p = 1, max.q = 1),
                              h = o + h_l - ult)$mean),
          error = function(e) rep(mean(tail(zz, 12)), o + h_l - ult))
      }
      assign(key, M, envir = cache)
      M
    }
    f_serie <- function(lyv, vars_s, usar_x = TRUE) {
      dyv <- c(NA, diff(lyv))
      function(o) {
        dy <- dyv[t_ini:o]
        if (usar_x && length(vars_s) > 0) {
          M   <- build_M(o)
          xtr <- do.call(cbind, lapply(vars_s,
                                       function(v) M[t_ini:o - lag_l, v]))
          xfu <- do.call(cbind, lapply(vars_s,
                                       function(v) M[(o + 1):(o + h_l) - lag_l, v]))
          colnames(xtr) <- colnames(xfu) <- vars_s
          cen <- colMeans(xtr); esc <- apply(xtr, 2, sd)
          esc[!is.finite(esc) | esc < 1e-10] <- 1
          xtr <- sweep(sweep(xtr, 2, cen), 2, esc, "/")
          xfu <- sweep(sweep(xfu, 2, cen), 2, esc, "/")
          fit <- auto.arima(dy, xreg = xtr, d = 0, D = 0,
                            max.p = 2, max.q = 2,
                            seasonal = FALSE, stepwise = TRUE)
          dh <- as.numeric(forecast(fit, h = h_l, xreg = xfu)$mean)
        } else {
          fit <- auto.arima(dy, d = 0, D = 0,
                            max.p = 2, max.q = 2,
                            seasonal = FALSE, stepwise = TRUE)
          dh <- as.numeric(forecast(fit, h = h_l)$mean)
        }
        exp(lyv[o] + cumsum(dh))
      }
    }
    list(f_serie = f_serie, X_mat = X_mat)
  }
  mx <- crear_x(externas_disponibles)
}

# 8. WALK-FORWARD POR SERIE (32 subclases + agregado), EN PARALELO ------
corre_serie <- function(s) {
  y <- if (s == "AGREGADO") y_vec else datos[[s]]
  lys <- log(y)
  specs <- identificar_nested(y)
  menu <- setNames(
    lapply(modelos_menu, function(m)
      correr_wf(y, function(o) pred_modelo(m, y, o, specs), origenes_ext)),
    modelos_menu)

  if (!is.null(mx)) {
    vs   <- vars_de(s)
    ctrl <- correr_wf(y, mx$f_serie(lys, vs, FALSE), origenes_ext)
    arimax <- if (length(vs) == 0) ctrl else
      correr_wf(y, mx$f_serie(lys, vs, TRUE), origenes_ext)
  } else {
    ctrl <- NULL; arimax <- NULL
  }
  c(menu, list(ARIMA_dlog = ctrl, ARIMAX_prior = arimax))
}

res_series <- future_lapply(c(subclases, "AGREGADO"), corre_serie,
                            future.seed = TRUE,
                            future.packages = c("forecast", "tseries", "FinTS"))
names(res_series) <- c(subclases, "AGREGADO")

# --- Combinaciones: promedios, pesos anidados y stacking ----------------
combos_serie <- function(w_menu) {
  real <- w_menu[[1]]$real
  fcs <- lapply(w_menu[modelos_menu], `[[`, "fc")
  mods <- names(fcs); k <- nrow(real); nm <- length(mods)
  mae_om <- sapply(fcs, function(f) mae_fila(real, f))
  tend <- setdiff(mods, "Naive")

  # Pesos anidados: inverso del MAE historico (solo origenes ya cerrados)
  pes <- matrix(NA_real_, k, h)
  for (i in seq_len(k)) {
    cerr <- which(origenes_ext + h <= origenes_ext[i])
    w <- if (length(cerr) >= min_pesos) {
      v <- 1 / pmax(colMeans(mae_om[cerr, , drop = FALSE]), 1e-8)
      v / sum(v)
    } else rep(1 / nm, nm)
    pes[i, ] <- Reduce(`+`, Map(function(f, wm) f[i, ] * wm, fcs, w))
  }

  # Stacking elastic net (glmnet), un modelo por horizonte.
  # X_stack tiene columnas [modelo1_h1..h12, modelo2_h1..h12, ...];
  # para el horizonte hh se usan las columnas (m - 1) * h + hh de cada modelo m.
  stack_fc <- matrix(NA_real_, k, h)
  X_stack <- do.call(cbind, fcs)
  for (i in seq_len(k)) {
    cerr <- which(origenes_ext + h <= origenes_ext[i])
    if (length(cerr) >= max(min_pesos, 24)) {
      Y_st <- real[cerr, , drop = FALSE]
      for (hh in seq_len(h)) {
        cols_h <- (seq_len(nm) - 1) * h + hh
        X_st <- X_stack[cerr, cols_h, drop = FALSE]
        fit <- tryCatch(cv.glmnet(X_st, Y_st[, hh], alpha = 0.5, nfolds = 5),
                        error = function(e) NULL)
        stack_fc[i, hh] <- if (!is.null(fit)) {
          as.numeric(predict(fit, s = "lambda.min",
                             newx = matrix(X_stack[i, cols_h], nrow = 1)))
        } else mean(X_stack[i, cols_h])
      }
    } else {
      for (hh in seq_len(h))
        stack_fc[i, hh] <- mean(X_stack[i, (seq_len(nm) - 1) * h + hh])
    }
  }

  mk <- function(fc) list(origenes = origenes_ext, real = real, fc = fc)
  list(Media_menu = mk(Reduce(`+`, fcs) / length(fcs)),
       Media_tend = mk(Reduce(`+`, fcs[tend]) / length(tend)),
       Pesos_nested = mk(pes),
       Stack_glmnet = mk(stack_fc))
}

cand <- lapply(res_series, function(r) {
  base <- r[modelos_menu]
  extras <- list()
  if (!is.null(r$ARIMA_dlog))   extras$ARIMA_dlog   <- r$ARIMA_dlog
  if (!is.null(r$ARIMAX_prior)) extras$ARIMAX_prior <- r$ARIMAX_prior
  c(base, extras, combos_serie(base))
})

# 9. DESEMPENO POR SUBCLASE ---------------------------------------------
mods_tab <- c("Drift", "ETS_amort", "ARIMA_sd", "BJ_deriva", "STL_ETS",
              "Theta", "TBATS",
              "Media_menu", "Media_tend", "Pesos_nested", "Stack_glmnet",
              "ARIMA_dlog", "ARIMAX_prior")
mods_tab <- intersect(mods_tab, names(cand[[1]]))

suma_mae <- function(w) sum(mae_fila(w$real[filas_todas, , drop = FALSE],
                                     w$fc[filas_todas, , drop = FALSE]))
mae_sub <- sapply(subclases, function(s)
  sapply(c("Naive", mods_tab), function(m) suma_mae(cand[[s]][[m]])))
rel_sub <- t(sweep(mae_sub[mods_tab, , drop = FALSE], 2,
                   mae_sub["Naive", ], "/"))
aporte <- 100 * pesos_bu * mae_sub["Naive", ] /
  sum(pesos_bu * mae_sub["Naive", ])
tab_sub <- data.frame(subclase = subclases,
                      peso = unname(pesos_bu),
                      aporte_error_naive_pct = unname(aporte),
                      rel_sub)
mostrar("Por subclase: MAE relativo a su Naive y aporte al error del agregado",
        tab_sub[order(-tab_sub$aporte_error_naive_pct), ], 3)

mostrar("Por modelo, a nivel subclase (MAE_rel < 1 = mejor que Naive)",
        data.frame(
          modelo = mods_tab,
          subclases_que_ganan = sapply(mods_tab, function(m) sum(rel_sub[, m] < 1)),
          peso_que_gana_pct = sapply(mods_tab, function(m)
            100 * sum(pesos_bu[rel_sub[, m] < 1])),
          MAE_rel_ponderado = sapply(mods_tab, function(m)
            sum(pesos_bu * mae_sub[m, ]) / sum(pesos_bu * mae_sub["Naive", ]))), 3)

cuenta <- function(campo, m) sum(sapply(subclases, function(s)
  sum(cand[[s]][[m]][[campo]][filas_todas])))
mostrar("Diagnostico de ajustes en subclases (respaldos naive y avisos)",
        data.frame(
          modelo = setdiff(modelos_menu, "Naive"),
          respaldos = sapply(setdiff(modelos_menu, "Naive"),
                             function(m) cuenta("fallo", m)),
          avisos = sapply(setdiff(modelos_menu, "Naive"),
                          function(m) cuenta("avisos", m))), 0)

# 10. AGREGADO: DIRECTO, BOTTOM-UP, VAR Y COMBINACION FINAL --------------
nm_cand <- names(cand[["AGREGADO"]])
wf_naive <- cand[["AGREGADO"]]$Naive
real_ag  <- wf_naive$real

dd <- setNames(cand[["AGREGADO"]], paste0("D_", nm_cand))
bu <- setNames(lapply(nm_cand, function(m)
  list(origenes = origenes_ext, real = real_ag,
       fc = Reduce(`+`, lapply(subclases, function(s)
         cand[[s]][[m]]$fc * pesos_bu[s])))),
  paste0("BU_", nm_cand))

# VAR recursivo (solo si hay NOAA, UREA_USD, TRM)
wf_var <- NULL
if (all(c("NOAA", "UREA_USD", "TRM") %in% externas_disponibles)) {
  Z <- cbind(NOAA = mx$X_mat[, "NOAA"],
             UREA_USD = mx$X_mat[, "UREA_USD"],
             TRM = mx$X_mat[, "TRM"],
             IPAP = c(NA, diff(ly)))
  f_var <- function(o) {
    z <- Z[2:o, , drop = FALSE]
    z <- z[complete.cases(z), , drop = FALSE]
    o_u <- 1 + nrow(z)
    p <- unname(VARselect(z, lag.max = 4, type = "const")$selection["HQ(n)"])
    if (is.na(p) || p < 1) p <- 1
    fit <- VAR(z, p = p, type = "const")
    tail(exp(ly[o_u] + cumsum(predict(fit, n.ahead = h + (o - o_u))$
                                 fcst$IPAP[, "fcst"])), h)
  }
  wf_var <- correr_wf(y_vec, f_var, origenes_ext)
}

mezcla <- function(ms, ws) list(origenes = origenes_ext, real = real_ag,
                                fc = Reduce(`+`, Map(function(m, w) m$fc * w, ms, ws)))

todos <- c(
  list(Naive = wf_naive),
  dd[setdiff(names(dd), "D_Naive")],
  bu[setdiff(names(bu), "BU_Naive")])

if (!is.null(wf_var)) {
  todos$VAR_recursivo <- wf_var
  todos$Combo_BJ_VAR <- mezcla(list(dd$D_BJ_deriva, wf_var), c(0.5, 0.5))
}

# Mejor directo y mejor bottom-up (segun MAE en la ventana de validacion)
mejor_directo <- names(dd)[which.min(sapply(dd[setdiff(names(dd), "D_Naive")],
                                            function(w)
                                              sum(mae_fila(w$real[filas_val, ],
                                                           w$fc[filas_val, ]))))]
mejor_bu <- names(bu)[which.min(sapply(bu[setdiff(names(bu), "BU_Naive")],
                                       function(w)
                                         sum(mae_fila(w$real[filas_val, ],
                                                      w$fc[filas_val, ]))))]
todos$Rec_optimo <- mezcla(list(dd[[mejor_directo]], bu[[mejor_bu]]), c(0.5, 0.5))

# Combo final: promedio de mejor directo, mejor BU y los dos stackings
candidatos_final <- c(mejor_directo, mejor_bu, "D_Stack_glmnet", "BU_Stack_glmnet")
candidatos_final <- intersect(candidatos_final, names(todos))
if (length(candidatos_final) > 0) {
  w_final <- rep(1 / length(candidatos_final), length(candidatos_final))
  todos$Combo_final <- mezcla(todos[candidatos_final], w_final)
}

# 11. WALK-FORWARD DEL AGREGADO -----------------------------------------
eval_completa <- evaluar(todos, wf_naive, filas_todas)
eval_val      <- evaluar(todos, wf_naive, filas_val)
mostrar("WALK-FORWARD - ventana completa (MAE_rel < 1 = mejor que Naive)",
        eval_completa)
mostrar("WALK-FORWARD - ventana desde inicio_val", eval_val)

principales <- c("Naive", "D_Drift", "D_BJ_deriva", "D_Media_tend",
                 "D_Pesos_nested", "D_Stack_glmnet",
                 "BU_Media_tend", "BU_BJ_deriva", "BU_Stack_glmnet")
if (!is.null(wf_var)) principales <- c(principales, "VAR_recursivo", "Combo_BJ_VAR")
principales <- c(principales, "Rec_optimo", "Combo_final")
principales <- intersect(principales, names(todos))

mape_h <- sapply(todos[principales], function(w)
  colMeans(100 * abs((w$real[filas_val, ] - w$fc[filas_val, ]) /
                       w$real[filas_val, ])))
mostrar("MAPE por horizonte (ventana desde inicio_val)",
        data.frame(h = 1:h, mape_h), 2)

# 12. TEST 12 MESES ------------------------------------------------------
test_preds  <- lapply(todos, function(w) w$fc[fila_test, ])
final_preds <- lapply(todos, function(w) w$fc[fila_fin, ])

tab_test <- data.frame(
  modelo      = names(test_preds),
  MAPE        = sapply(test_preds, function(p) mape_vec(y_te, p)),
  MAE         = sapply(test_preds, function(p) mean(abs(y_te - p))),
  max_abs_err = sapply(test_preds, function(p) max(abs(y_te - p))),
  sesgo_medio = sapply(test_preds, function(p) mean(y_te - p)),
  meses_gana_naive = sapply(test_preds, function(p)
    sum(abs(y_te - p) < abs(y_te - test_preds$Naive))))
mostrar("TEST 12 MESES (periodo ya consultado)",
        tab_test[order(tab_test$MAE), ], 2)

# 13. DIEBOLD-MARIANO (HAC) Y MCS ---------------------------------------
mostrar("DM vs Naive - ventana completa",
        tabla_dm(todos, filas_todas))
mostrar("DM vs Naive - ventana desde inicio_val",
        tabla_dm(todos, filas_val))
mostrar("DM vs D_Drift - ventana completa",
        tabla_dm(todos, filas_todas, ref = "D_Drift"))

set.seed(123)
mcs_todas <- mcs(L_mat(todos, filas_todas), alpha_mcs, n_boot, bloque_boot)
mostrar("MCS - ventana completa", mcs_todas$tabla)
if (!is.null(mcs_todas$pasos))
  mostrar("MCS - eliminaciones (ventana completa)", mcs_todas$pasos)

set.seed(123)
mcs_val <- mcs(L_mat(todos, filas_val), alpha_mcs, n_boot, bloque_boot)
mostrar("MCS - ventana desde inicio_val", mcs_val$tabla)
if (!is.null(mcs_val$pasos))
  mostrar("MCS - eliminaciones (ventana inicio_val)", mcs_val$pasos)

# 14. ERRORES CON SIGNO: SESGO Y DEPENDENCIA ENTRE MODELOS --------------
modelos_div <- c("D_BJ_deriva", "D_ETS_amort", "D_Media_tend",
                 "BU_BJ_deriva", "BU_Media_tend", "D_Drift")
if (!is.null(wf_var)) modelos_div <- c(modelos_div, "VAR_recursivo")
modelos_div <- intersect(modelos_div, names(todos))

err_s <- function(m, hh) todos[[m]]$real[filas_val, hh] -
  todos[[m]]$fc[filas_val, hh]
mostrar("Error medio con signo (real - pronostico), ventana desde inicio_val",
        data.frame(
          modelo = modelos_div,
          h1  = sapply(modelos_div, function(m) mean(err_s(m, 1))),
          h3  = sapply(modelos_div, function(m) mean(err_s(m, 3))),
          h6  = sapply(modelos_div, function(m) mean(err_s(m, 6))),
          h12 = sapply(modelos_div, function(m) mean(err_s(m, 12)))), 2)
mostrar("Correlacion de errores con signo, h = 12",
        round(cor(sapply(modelos_div, function(m) err_s(m, 12))), 2))

# 15. INTERVALOS (calibracion secuencial) -------------------------------
stopifnot(modelo_ic %in% names(todos))
w_ic <- todos[[modelo_ic]]
cov_tab <- do.call(rbind, lapply(metodos_ic, function(mt) {
  cs <- cobertura_seq(w_ic, mt, filas_todas, origenes_ext)
  data.frame(metodo = mt, origenes = length(unique(cs$i)),
             cobertura_80 = 100 * mean(cs$c80),
             cobertura_95 = 100 * mean(cs$c95),
             ancho_95_pct = mean(cs$ancho95))
}))
mostrar(paste(modelo_ic, ": cobertura fuera de muestra por metodo (nominal 80/95)"),
        cov_tab, 1)

metodo_ic_sel <- cov_tab$metodo[which.min(
  abs(cov_tab$cobertura_80 - 80) + abs(cov_tab$cobertura_95 - 95))]
mostrar("Metodo de intervalo con cobertura mas cercana a la nominal",
        data.frame(metodo = metodo_ic_sel))

E_wf    <- (w_ic$real / w_ic$fc - 1)[filas_todas, , drop = FALSE]
sig_cal <- sapply(origenes, sig_o)
ic_test <- ic_calibrado(metodo_ic_sel, test_preds[[modelo_ic]], sig_o(corte),
                        E_wf, sig_cal)
mostrar("Cobertura en el test (12 meses, un solo origen)",
        data.frame(
          cobertura_80 = 100 * mean(y_te >= ic_test$inf80 & y_te <= ic_test$sup80),
          cobertura_95 = 100 * mean(y_te >= ic_test$inf95 & y_te <= ic_test$sup95),
          ancho_medio_95_pct = mean(100 * (ic_test$sup95 - ic_test$inf95) /
                                      test_preds[[modelo_ic]])), 1)

# 16. PRONOSTICO FINAL Y VARIACIONES ------------------------------------
fechas_fut <- seq(datos$FECHA[n], by = "month", length.out = h + 1)[-1]
E_fin   <- rbind(E_wf, y_te / test_preds[[modelo_ic]] - 1)
sig_fin <- c(sig_cal, sig_o(corte))
ic_fut  <- ic_calibrado(metodo_ic_sel, final_preds[[modelo_ic]], sig_o(n),
                        E_fin, sig_fin)

cols_fut <- c("D_BJ_deriva", "D_Stack_glmnet", "D_Media_tend",
              "BU_Media_tend", "Rec_optimo", "Combo_final", "D_Drift", "Naive")
cols_fut <- intersect(cols_fut, names(final_preds))

tabla_futuro <- data.frame(
  fecha = format(fechas_fut, "%Y-%m"),
  pronostico = final_preds[[modelo_ic]],
  ic_fut,
  final_preds[cols_fut])
names(tabla_futuro)[2] <- modelo_ic
mostrar(paste("PRONOSTICO 12 MESES -", modelo_ic, "con IC", metodo_ic_sel),
        tabla_futuro, 1)

mostrar("Registro del pronostico (para evaluarlo cuando salgan los datos)",
        data.frame(generado = format(Sys.Date()),
                   ultimo_dato = format(datos$FECHA[n], "%Y-%m"),
                   objetivo = objetivo,
                   modelo = modelo_ic,
                   metodo_ic = metodo_ic_sel))

var_pct <- function(x, base) 100 * (x / base - 1)
idx_t <- (corte + 1):n
base_t <- y_vec[corte]
real_tab <- data.frame(
  fecha = format(fechas_test, "%Y-%m"),
  indice = y_te,
  var_mensual = 100 * (y_vec[idx_t] / y_vec[idx_t - 1] - 1),
  var_anual   = 100 * (y_vec[idx_t] / y_vec[idx_t - 12] - 1),
  var_acum    = var_pct(y_te, base_t))
mostrar("Variacion real del IPAP en el test (%)", real_tab, 2)

mod_v <- c("Naive", "D_Drift", "D_BJ_deriva", "D_Media_tend",
           "BU_Media_tend", "Combo_final")
mod_v <- intersect(mod_v, names(test_preds))
acum_pred <- sapply(test_preds[mod_v], function(p) var_pct(p, base_t))
err_acum  <- real_tab$var_acum - acum_pred
mostrar("Variacion acumulada desde el origen: real vs pronostico (%)",
        data.frame(fecha = format(fechas_test, "%Y-%m"),
                   real = real_tab$var_acum, acum_pred), 2)
mostrar("Error de la variacion acumulada en puntos porcentuales",
        data.frame(modelo = mod_v,
                   MAE_pp = colMeans(abs(err_acum)),
                   sesgo_pp = colMeans(err_acum),
                   error_h12_pp = err_acum[h, ]), 2)

pc <- final_preds[[modelo_ic]]
tabla_var <- data.frame(
  fecha = format(fechas_fut, "%Y-%m"),
  var_mensual = 100 * (pc / c(y_vec[n], pc[-h]) - 1),
  var_acum = var_pct(pc, y_vec[n]),
  inf80 = var_pct(ic_fut$inf80, y_vec[n]),
  sup80 = var_pct(ic_fut$sup80, y_vec[n]),
  inf95 = var_pct(ic_fut$inf95, y_vec[n]),
  sup95 = var_pct(ic_fut$sup95, y_vec[n]),
  var_anual = 100 * (pc / y_vec[n + seq_len(h) - 12] - 1))
mostrar(paste("Variacion pronosticada -", modelo_ic, "(%)"), tabla_var, 2)

# 17. COINTEGRACION (JOHANSEN) ------------------------------------------
datos$IPAP_OBJ <- y_obj
johansen_tabla <- function(cols, K = 3) {
  jo <- ca.jo(log(as.matrix(datos[1:corte, cols])),
              type = "trace", ecdet = "const", K = K, spec = "transitory")
  k <- length(cols)
  data.frame(
    hipotesis = c(paste("r <= ", (k - 1):1), "r = 0"),
    estadistico = jo@teststat,
    crit_10 = jo@cval[, "10pct"],
    crit_5  = jo@cval[, "5pct"],
    crit_1  = jo@cval[, "1pct"],
    rechaza_5 = jo@teststat > jo@cval[, "5pct"])
}
if (all(c("IPAP_OBJ", "UREA_USD", "TRM") %in% names(datos))) {
  mostrar("Johansen (traza), K = 3 - objetivo, UREA, TRM",
          johansen_tabla(c("IPAP_OBJ", "UREA_USD", "TRM"), 3), 2)
} else {
  message("Johansen omitido: faltan UREA_USD o TRM en los datos.")
}

# 18. GRAFICOS ----------------------------------------------------------
df_test <- do.call(rbind, lapply(mod_v, function(m)
  data.frame(fecha = fechas_test, real = y_te,
             predicho = test_preds[[m]], modelo = m)))
g_test <- ggplot(df_test, aes(x = fecha)) +
  geom_line(aes(y = real), colour = "black", linewidth = 1) +
  geom_line(aes(y = predicho), colour = "#1671AD", linewidth = 0.8) +
  geom_point(aes(y = predicho), colour = "#1671AD", size = 1.3) +
  facet_wrap(~ modelo, ncol = 4) +
  labs(title = "Test 12 meses: real vs predicho", x = NULL, y = "IPAP") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
print(g_test)

hist_df <- data.frame(fecha = tail(datos$FECHA, 48), IPAP = tail(y_vec, 48))
fut_df  <- data.frame(fecha = fechas_fut,
                      pred = final_preds[[modelo_ic]], ic_fut)
g_fut <- ggplot() +
  geom_line(data = hist_df, aes(fecha, IPAP),
            colour = "grey30", linewidth = 0.9) +
  geom_ribbon(data = fut_df, aes(fecha, ymin = inf95, ymax = sup95),
              fill = "#1671AD", alpha = 0.12) +
  geom_ribbon(data = fut_df, aes(fecha, ymin = inf80, ymax = sup80),
              fill = "#1671AD", alpha = 0.22) +
  geom_line(data = fut_df, aes(fecha, pred),
            colour = "#1671AD", linewidth = 1) +
  labs(title = paste("IPAP:", modelo_ic, "a 12 meses (IC 80% y 95%)"),
       x = NULL, y = "Puntos del IPAP") +
  theme_minimal()
print(g_fut)

# 19. EXPORTAR RESULTADOS ------------------------------------------------
write.csv(tabla_futuro, file.path(dir_out, "pronostico_12m.csv"), row.names = FALSE)
write.csv(tabla_var,    file.path(dir_out, "variaciones_pronostico.csv"), row.names = FALSE)
write.csv(eval_val,     file.path(dir_out, "evaluacion_walkforward_val.csv"), row.names = FALSE)
write.csv(eval_completa, file.path(dir_out, "evaluacion_walkforward_completa.csv"), row.names = FALSE)
write.csv(tab_test[order(tab_test$MAE), ], file.path(dir_out, "test_12m.csv"), row.names = FALSE)
ggsave(file.path(dir_out, "grafico_test.png"),       g_test, width = 11, height = 6, dpi = 200)
ggsave(file.path(dir_out, "grafico_pronostico.png"), g_fut,  width = 9,  height = 5, dpi = 200)

plan(sequential)

message("\n=== PIPELINE COMPLETADO ===")
message("Modelos evaluados: ", paste(names(todos), collapse = ", "))
message("Mejor modelo en test (MAE): ",
        tab_test$modelo[which.min(tab_test$MAE)])
message("Metodo de intervalo seleccionado: ", metodo_ic_sel)
message("Resultados guardados en: ", normalizePath(dir_out))
