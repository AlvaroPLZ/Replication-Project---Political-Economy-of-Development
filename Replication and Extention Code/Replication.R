### Title: Replication of "The Miracle of Microfinance?" by Banerjee et al.
### Author: Álvaro Pérez
### Last update: April 16, 2025
### Course: Political Economy of Development

setwd("/Users/alvaroperezlopez/Library/CloudStorage/OneDrive-Personal/Documentos/LICENCIATURA_ITAM/11 - ONCEAVO SEMESTRE/Economía Política II/Replicación/113599-V1/2013-0533_data--TO-SUBMIT-")

# ******************************************************************************
# -------- I. Libraries --------
# ******************************************************************************

library(tidyverse)
library(dplyr)
library(haven)
library(ggplot2)
library(stargazer)
library(estimatr)
library(xtable)
install.packages("quantreg")
library(quantreg)   # Para regresiones por cuantiles
install.packages("boot")
library(boot)       # Para bootstrap
install.packages("Matrix")
library(parallel)
install.packages("tikzDevice")
library(tikzDevice)

# ******************************************************************************
# -------- II. Loading Data --------
# ******************************************************************************

endlines1and2 <- read_dta("2013-0533_data_endlines1and2.dta")
baseline <- read_dta("2013-0533_data_baseline.dta")
census <- read_dta("2013-0533_data_census.dta")
businesstype <- read_dta("2013-0533_data_endline1businesstype.dta")

# ******************************************************************************
# -------- III. Table 2. Credit --------
# ******************************************************************************

# Definir variables de control para las variables de base 
area_controls <- c("area_pop_base", "area_debt_total_base", "area_business_total_base",
                   "area_exp_pc_mean_base", "area_literate_head_base", "area_literate_base")

# ------------- Panel A. Endline 1 ----

# Lista de variables de resultado para el Panel A
vars_a <- c("spandana_1", "othermfi_1", "anymfi_1", "anybank_1", "anyinformal_1",
            "anyloan_1", "everlate_1", "mfi_loan_cycles_1",
            "spandana_amt_1", "othermfi_amt_1", "anymfi_amt_1", "bank_amt_1",
            "informal_amt_1", "anyloan_amt_1", "credit_index_1")

# Lista para guardar los resultados de cada modelo
results_panel_a <- lapply(vars_a, function(var) {
  # Fórmula: variable de resultado en función de treatment y controles de área
  form <- as.formula(paste0(var, " ~ treatment + ",
                            paste(area_controls, collapse = " + ")))
  
  # Estimando el modelo usando lm_robust (con pesos pweight = w1 y cluster en areaid)
  mod <- lm_robust(form, data = endlines1and2, weights = w1, clusters = areaid)
  
  # Extraer coeficiente, error estándar y t-valor para "treatment"
  coef_treat <- coef(mod)["treatment"]
  se_treat   <- mod$std.error["treatment"]
  t_val      <- coef_treat / se_treat
  
  # Grados de libertad (usamos los residuales del modelo)
  df_mod <- mod$df.residual
  
  # Calcular el p-valor (bilateral)
  p_val <- 2 * pt(abs(t_val), df = df_mod, lower.tail = FALSE)
  
  # Obtener las observaciones usadas en el modelo. Si hay NA, mod$na.action contiene 
  # las filas omitidas.
  used_index <- if (!is.null(mod$na.action)) {
    setdiff(seq_len(nrow(endlines1and2)), mod$na.action)
  } else {
    seq_len(nrow(endlines1and2))
  }
  
  # De las observaciones usadas, seleccionar las del grupo de control (treatment == 0)
  sub_data <- endlines1and2[used_index, ]
  sub_data <- sub_data[sub_data$treatment == 0, ]
  
  # Calcular la media y desviación estándar de la variable de resultado en el grupo control
  mean_val <- mean(sub_data[[var]], na.rm = TRUE)
  sd_val   <- sd(sub_data[[var]], na.rm = TRUE)
  
  # Tamaño de muestra utilizado en el modelo
  n_obs <- nobs(mod)
  
  # Extraer el R² (se usa el r² del summary del modelo; para lm_robust es equivalente al de lm)
  r2_val <- summary(mod)$r.squared
  
  # Retornar un data frame con los resultados para esta variable
  data.frame(Variable    = var,
             Coefficient = round(coef_treat, 4),
             SE          = round(se_treat, 4),
             r2          = round(r2_val, 4),
             Mean        = round(mean_val, 4),
             SD          = round(sd_val, 4),
             N           = n_obs,
             pvalue      = round(p_val, 4),
             stringsAsFactors = FALSE)
})

# Combinar los resultados del Panel A en un solo data frame
results_panel_a_df <- do.call(rbind, results_panel_a)

# ------------- Panel B. Endline 2 ----

# Lista de variables de resultado para el Panel B
vars_b <- c("spandana_2", "othermfi_2", "anymfi_2", "anybank_2", "anyinformal_2",
            "anyloan_2", "everlate_2", "mfi_loan_cycles_2",
            "spandana_amt_2", "othermfi_amt_2", "anymfi_amt_2", "bank_amt_2",
            "informal_amt_2", "anyloan_amt_2", "credit_index_2")

# Lista para guardar los resultados de cada modelo del Panel B
results_panel_b <- lapply(vars_b, function(var) {
  # Construir la fórmula para el modelo
  form <- as.formula(paste0(var, " ~ treatment + ",
                            paste(area_controls, collapse = " + ")))
  
  # Estimar el modelo con pesos pweight = w2 y cluster en areaid
  mod <- lm_robust(form, data = endlines1and2, weights = w2, clusters = areaid)
  
  # Extraer coeficiente, error estándar, t-valor y p-valor para treatment
  coef_treat <- coef(mod)["treatment"]
  se_treat   <- mod$std.error["treatment"]
  t_val      <- coef_treat / se_treat
  df_mod     <- mod$df.residual
  p_val      <- 2 * pt(abs(t_val), df = df_mod, lower.tail = FALSE)
  
  # Obtener las observaciones usadas en el modelo
  used_index <- if (!is.null(mod$na.action)) {
    setdiff(seq_len(nrow(endlines1and2)), mod$na.action)
  } else {
    seq_len(nrow(endlines1and2))
  }
  
  # Filtrar para las observaciones en el grupo de control (treatment == 0)
  sub_data <- endlines1and2[used_index, ]
  sub_data <- sub_data[sub_data$treatment == 0, ]
  
  # Calcular media y desviación estándar en el grupo control
  mean_val <- mean(sub_data[[var]], na.rm = TRUE)
  sd_val   <- sd(sub_data[[var]], na.rm = TRUE)
  
  # Tamaño de muestra empleado
  n_obs <- nobs(mod)
  
  # Extraer R² del modelo
  r2_val <- summary(mod)$r.squared
  
  # Retornar los resultados en un data frame
  data.frame(Variable    = var,
             Coefficient = round(coef_treat, 4),
             SE          = round(se_treat, 4),
             r2          = round(r2_val, 4),
             Mean        = round(mean_val, 4),
             SD          = round(sd_val, 4),
             N           = n_obs,
             pvalue      = round(p_val, 4),
             stringsAsFactors = FALSE)
})

# Combinar los resultados del Panel B
results_panel_b_df <- do.call(rbind, results_panel_b)

### Combinar resultados de Panel A y Panel B y guardar la tabla

results_table2 <- rbind(results_panel_a_df, results_panel_b_df)

# Guardar la tabla en "table2.txt" (separador de tabulaciones, sin nombres de fila ni comillas)
write.table(results_table2, file = "table2.txt", sep = "\t",
            row.names = FALSE, quote = FALSE)

table2 <- read.table("table2.txt", header = TRUE, sep = "\t")
table2 <- xtable(table2)
print(table2, file = "table2_results.tex")

# ******************************************************************************
# -------- IV. Table 3. Self-employment activities (all households) --------
# ******************************************************************************

# ------------- Panel A. Endline 1 ----

# Vector de variables para Panel A
vars_a <- c("bizassets_1", "bizinvestment_1", "bizrev_1", "bizexpense_1",
            "bizprofit_1", "any_biz_1", "total_biz_1", "any_new_biz_1",
            "biz_stop_1", "newbiz_1", "female_biz_new_1", "biz_index_all_1")

# Procesar cada variable y almacenar resultados en una lista
results_panel_a <- lapply(vars_a, function(var) {
  # Construir la fórmula:
  #   Ejemplo: bizassets_1 ~ treatment + area_pop_base + area_debt_total_base + ...
  form <- as.formula(paste0(var, " ~ treatment + ", paste(area_controls, collapse = " + ")))
  
  # Ajustar el modelo con ponderación (w1) y errores estándar clusterizados en areaid
  mod <- lm_robust(form, data = endlines1and2, weights = w1, clusters = areaid)
  
  # Extraer el coeficiente y error estándar de la variable treatment
  coef_treat <- coef(mod)["treatment"]
  se_treat   <- mod$std.error["treatment"]
  t_val      <- coef_treat / se_treat
  # Calcular el p-valor (dos colas)
  p_val      <- 2 * pt(abs(t_val), df = mod$df.residual, lower.tail = FALSE)
  
  # Determinar las observaciones empleadas en el modelo (simulando e(sample))
  used_index <- if (!is.null(mod$na.action)) {
    setdiff(seq_len(nrow(endlines1and2)), mod$na.action)
  } else {
    seq_len(nrow(endlines1and2))
  }
  
  # Filtrar solo las observaciones del grupo de control (treatment == 0)
  sub_data <- endlines1and2[used_index, ]
  sub_data <- sub_data[sub_data$treatment == 0, ]
  
  # Calcular la media y desviación estándar de la variable para el grupo de control
  mean_val <- mean(sub_data[[var]], na.rm = TRUE)
  sd_val   <- sd(sub_data[[var]], na.rm = TRUE)
  
  # Obtener el tamaño de muestra utilizado en el modelo
  n_obs <- nobs(mod)
  
  # Extraer el R² del modelo (redondeamos, por ejemplo, a 3 decimales)
  r2_val <- summary(mod)$r.squared
  
  # Devolver un data frame con los resultados para la variable
  data.frame(Variable    = var,
             Coefficient = round(coef_treat, 3),
             SE          = round(se_treat, 3),
             r2          = round(r2_val, 3),
             Mean        = round(mean_val, 3),
             SD          = round(sd_val, 3),
             N           = n_obs,
             pvalue      = round(p_val, 3),
             stringsAsFactors = FALSE)
})

# Combinar resultados del Panel A en un único data frame
results_panel_a_df <- do.call(rbind, results_panel_a)
# Agregar una columna para identificar el Panel
results_panel_a_df$Panel <- "Endline 1"

# ------------- Panel B. Endline 2 ----

# Definir el vector de variables para Panel B
vars_b <- c("bizassets_2", "bizinvestment_2", "bizrev_2", "bizexpense_2",
            "bizprofit_2", "any_biz_2", "total_biz_2", "any_new_biz_2",
            "biz_stop_2", "newbiz_2", "female_biz_new_2", "biz_index_all_2")

# Procesar cada variable del Panel B en una lista
results_panel_b <- lapply(vars_b, function(var) {
  form <- as.formula(paste0(var, " ~ treatment + ", paste(area_controls, collapse = " + ")))
  
  # Usar los pesos pweight = w2 para Endline 2
  mod <- lm_robust(form, data = endlines1and2, weights = w2, clusters = areaid)
  
  coef_treat <- coef(mod)["treatment"]
  se_treat   <- mod$std.error["treatment"]
  t_val      <- coef_treat / se_treat
  p_val      <- 2 * pt(abs(t_val), df = mod$df.residual, lower.tail = FALSE)
  
  used_index <- if (!is.null(mod$na.action)) {
    setdiff(seq_len(nrow(endlines1and2)), mod$na.action)
  } else {
    seq_len(nrow(endlines1and2))
  }
  
  sub_data <- endlines1and2[used_index, ]
  sub_data <- sub_data[sub_data$treatment == 0, ]
  
  mean_val <- mean(sub_data[[var]], na.rm = TRUE)
  sd_val   <- sd(sub_data[[var]], na.rm = TRUE)
  n_obs    <- nobs(mod)
  r2_val   <- summary(mod)$r.squared
  
  data.frame(Variable    = var,
             Coefficient = round(coef_treat, 3),
             SE          = round(se_treat, 3),
             r2          = round(r2_val, 3),
             Mean        = round(mean_val, 3),
             SD          = round(sd_val, 3),
             N           = n_obs,
             pvalue      = round(p_val, 3),
             stringsAsFactors = FALSE)
})

results_panel_b_df <- do.call(rbind, results_panel_b)
results_panel_b_df$Panel <- "Endline 2"

# Combinar resultados de Panel A y Panel B y guardar la tabla
results_table3 <- rbind(results_panel_a_df, results_panel_b_df)

# Opcionalmente, puedes reordenar columnas para que Panel aparezca como la primera columna
results_table3 <- results_table3[, c("Panel", "Variable", "Coefficient", "SE", "r2", "Mean", "SD", "N", "pvalue")]

# Guardar la tabla en "table3.txt" (en formato tab-delimited)
write.table(results_table3, file = "table3.txt", sep = "\t",
            row.names = FALSE, quote = FALSE)

# ******************************************************************************
# -------- V. Table 3.B Self-employment activities (old businesses) --------
# ******************************************************************************

# ------------- Panel A. Endline 1 ----
#(solo hogares con any_old_biz == 1)

# Variables de resultado para el Panel A
vars_a <- c("bizassets_1", "bizinvestment_1", "bizrev_1", "bizexpense_1", 
            "bizprofit_1", "bizemployees_1", "biz_index_old_1")

results_panel_a <- lapply(vars_a, function(var) {
  # Construir la fórmula: variable ~ treatment + controles de área
  form <- as.formula(paste0(var, " ~ treatment + ", paste(area_controls, collapse = " + ")))
  
  # Subconjunto de datos: solo hogares con old business
  data_sub <- subset(endlines1and2, any_old_biz == 1)
  
  # Estimar el modelo con peso w1 y clúster en areaid
  mod <- lm_robust(form, data = data_sub, weights = w1, clusters = areaid)
  
  # Extraer coeficiente y error estándar de treatment
  coef_treat <- coef(mod)["treatment"]
  se_treat   <- mod$std.error["treatment"]
  t_val      <- coef_treat / se_treat
  p_val      <- 2 * pt(abs(t_val), df = mod$df.residual, lower.tail = FALSE)
  
  # Determinar las observaciones empleadas (simulando e(sample))
  used_index <- if (!is.null(mod$na.action)) {
    setdiff(seq_len(nrow(data_sub)), mod$na.action)
  } else {
    seq_len(nrow(data_sub))
  }
  
  sub_data <- data_sub[used_index, ]
  # Calcular media y desviación estándar para el grupo de control (treatment==0)
  sub_ctrl <- sub_data[sub_data$treatment == 0, ]
  mean_val <- mean(sub_ctrl[[var]], na.rm = TRUE)
  sd_val   <- sd(sub_ctrl[[var]], na.rm = TRUE)
  
  # Número de observaciones utilizadas en el modelo
  n_obs <- nobs(mod)
  
  # Extraer R² del modelo
  r2_val <- summary(mod)$r.squared
  
  # Retornar un data frame con los resultados de esta variable
  data.frame(Variable    = var,
             Coefficient = round(coef_treat, 3),
             SE          = round(se_treat, 3),
             r2          = round(r2_val, 3),
             Mean        = round(mean_val, 3),
             SD          = round(sd_val, 3),
             N           = n_obs,
             pvalue      = round(p_val, 3),
             stringsAsFactors = FALSE)
})

# Combinar resultados del Panel A
results_panel_a_df <- do.call(rbind, results_panel_a)
results_panel_a_df$Panel <- "Endline 1"

# ------------- Panel B. Endline 2 ----

# Variables de resultado para el Panel B
vars_b <- c("bizassets_2", "bizinvestment_2", "bizrev_2", "bizexpense_2", 
            "bizprofit_2", "bizemployees_2", "biz_index_old_2")

results_panel_b <- lapply(vars_b, function(var) {
  form <- as.formula(paste0(var, " ~ treatment + ", paste(area_controls, collapse = " + ")))
  
  # Subconjunto: solo hogares con old business
  data_sub <- subset(endlines1and2, any_old_biz == 1)
  
  # Estimar el modelo con peso w2 y clúster en areaid
  mod <- lm_robust(form, data = data_sub, weights = w2, clusters = areaid)
  
  coef_treat <- coef(mod)["treatment"]
  se_treat   <- mod$std.error["treatment"]
  t_val      <- coef_treat / se_treat
  p_val      <- 2 * pt(abs(t_val), df = mod$df.residual, lower.tail = FALSE)
  
  used_index <- if (!is.null(mod$na.action)) {
    setdiff(seq_len(nrow(data_sub)), mod$na.action)
  } else {
    seq_len(nrow(data_sub))
  }
  
  sub_data <- data_sub[used_index, ]
  sub_ctrl <- sub_data[sub_data$treatment == 0, ]
  mean_val <- mean(sub_ctrl[[var]], na.rm = TRUE)
  sd_val   <- sd(sub_ctrl[[var]], na.rm = TRUE)
  
  n_obs <- nobs(mod)
  r2_val <- summary(mod)$r.squared
  
  data.frame(Variable    = var,
             Coefficient = round(coef_treat, 3),
             SE          = round(se_treat, 3),
             r2          = round(r2_val, 3),
             Mean        = round(mean_val, 3),
             SD          = round(sd_val, 3),
             N           = n_obs,
             pvalue      = round(p_val, 3),
             stringsAsFactors = FALSE)
})

results_panel_b_df <- do.call(rbind, results_panel_b)
results_panel_b_df$Panel <- "Endline 2"

### Combinar resultados de Panel A y Panel B y exportar la tabla

results_table3b <- rbind(results_panel_a_df, results_panel_b_df)

# Escribir la tabla en un archivo de texto (separado por tabulaciones)
write.table(results_table3b, file = "table3b.txt", sep = "\t",
            row.names = FALSE, quote = FALSE)

# ******************************************************************************
# -------- VI. Table 3.C Self-employment activities (new businesses) --------
# ******************************************************************************

# Vector de variables de resultado para el análisis
vars_newbiz <- c("bizassets_1", "bizinvestment_1", "bizrev_1", 
                 "bizexpense_1", "bizprofit_1", "bizemployees_1", 
                 "biz_index_new_1")

# Iterar sobre cada variable para estimar el modelo en el subconjunto 
# donde newbiz_1 > 0 y no es missing
results_newbiz <- lapply(vars_newbiz, function(var) {
  
  # Subconjunto de datos: hogares con newbiz_1 > 0 y no NA
  data_sub <- subset(endlines1and2, newbiz_1 > 0 & !is.na(newbiz_1))
  
  # Construir la fórmula del modelo:
  # Ejemplo: bizassets_1 ~ treatment + area_pop_base + area_debt_total_base + ...
  formula_mod <- as.formula(paste0(var, " ~ treatment + ", 
                                   paste(area_controls, collapse = " + ")))
  
  # Ajustar el modelo usando lm_robust con peso w1 y clustering en areaid
  mod <- lm_robust(formula_mod, data = data_sub, weights = w1, clusters = areaid)
  
  # Extraer el coeficiente y error estándar de "treatment"
  coef_treat <- coef(mod)["treatment"]
  se_treat   <- mod$std.error["treatment"]
  t_val      <- coef_treat / se_treat
  # Calcular el p-valor (dos colas)
  p_val      <- 2 * pt(abs(t_val), df = mod$df.residual, lower.tail = FALSE)
  
  # Determinar las observaciones usadas en el modelo (simula e(sample))
  used_index <- if (!is.null(mod$na.action)) {
    setdiff(seq_len(nrow(data_sub)), mod$na.action)
  } else {
    seq_len(nrow(data_sub))
  }
  
  # Filtrar el subconjunto de control: treatment == 0
  sub_data <- data_sub[used_index, ]
  sub_ctrl <- sub_data[sub_data$treatment == 0, ]
  
  # Calcular la media y la desviación estándar para la variable en el grupo control
  mean_val <- mean(sub_ctrl[[var]], na.rm = TRUE)
  sd_val   <- sd(sub_ctrl[[var]], na.rm = TRUE)
  
  # Número de observaciones utilizadas en el modelo
  n_obs    <- nobs(mod)
  
  # Extraer el R² del modelo
  r2_val   <- summary(mod)$r.squared
  
  # Devolver un data frame con los resultados de esta variable
  data.frame(Variable    = var,
             Coefficient = round(coef_treat, 3),
             SE          = round(se_treat, 3),
             r2          = round(r2_val, 3),
             Mean        = round(mean_val, 3),
             SD          = round(sd_val, 3),
             N           = n_obs,
             pvalue      = round(p_val, 3),
             stringsAsFactors = FALSE)
})

# Combinar todos los resultados en un data frame
results_newbiz_df <- do.call(rbind, results_newbiz)

# Guardar la tabla en "table3c.txt" (archivo de texto con separador de tabulaciones)
write.table(results_newbiz_df, file = "table3c.txt", sep = "\t",
            row.names = FALSE, quote = FALSE)

# ******************************************************************************
# -------- VII. Table 4. Income --------
# ******************************************************************************

# ------------- Panel A. Endline 1 -----

# Definir las variables a utilizar para el Panel A
vars_a <- c("bizprofit_1", "wages_nonbiz_1", "income_index_1")

# Iterar sobre las variables para estimar el modelo y extraer los parámetros
results_panel_a <- lapply(vars_a, function(var) {
  
  # Construir la fórmula: ej. bizprofit_1 ~ treatment + area_pop_base + area_debt_total_base + ...
  formula_mod <- as.formula(paste0(var, " ~ treatment + ", paste(area_controls, collapse = " + ")))
  
  # Estimar el modelo usando lm_robust, con ponderación w1 y cluster en areaid
  mod <- lm_robust(formula_mod, data = endlines1and2, weights = w1, clusters = areaid)
  
  # Extraer el coeficiente y error estándar para "treatment"
  coef_treat <- coef(mod)["treatment"]
  se_treat   <- mod$std.error["treatment"]
  t_val      <- coef_treat / se_treat
  
  # Calcular el p-valor (dos colas)
  p_val      <- 2 * pt(abs(t_val), df = mod$df.residual, lower.tail = FALSE)
  
  # Identificar las observaciones usadas en el modelo (simulación de e(sample))
  used_index <- if (!is.null(mod$na.action)) {
    setdiff(seq_len(nrow(endlines1and2)), mod$na.action)
  } else {
    seq_len(nrow(endlines1and2))
  }
  
  # Seleccionar el subconjunto de control: treatment == 0 (dentro de las observaciones usadas)
  sub_data <- endlines1and2[used_index, ]
  sub_ctrl <- sub_data[sub_data$treatment == 0, ]
  
  # Calcular la media y desviación estándar de la variable en el grupo control
  mean_val <- mean(sub_ctrl[[var]], na.rm = TRUE)
  sd_val   <- sd(sub_ctrl[[var]], na.rm = TRUE)
  
  # Obtener el número de observaciones utilizadas en el modelo
  n_obs <- nobs(mod)
  
  # Extraer el R² del modelo
  r2_val <- summary(mod)$r.squared
  
  # Devolver un data frame con los resultados para esta variable
  data.frame(Variable    = var,
             Coefficient = round(coef_treat, 3),
             SE          = round(se_treat, 3),
             r2          = round(r2_val, 3),
             Mean        = round(mean_val, 3),
             SD          = round(sd_val, 3),
             N           = n_obs,
             pvalue      = round(p_val, 3),
             stringsAsFactors = FALSE)
})

# Combinar los resultados en un data frame para Panel A y agregar identificador del panel
results_panel_a_df <- do.call(rbind, results_panel_a)
results_panel_a_df$Panel <- "Endline 1"

# ------------- Panel B. Endline 2 -----

# Definir las variables a utilizar para el Panel B
vars_b <- c("bizprofit_2", "wages_nonbiz_2", "income_index_2")

# Iterar sobre las variables para Panel B
results_panel_b <- lapply(vars_b, function(var) {
  
  # Construir la fórmula: variable ~ treatment + controles de área
  formula_mod <- as.formula(paste0(var, " ~ treatment + ", paste(area_controls, collapse = " + ")))
  
  # Estimar el modelo usando lm_robust, con ponderación w2 y cluster en areaid
  mod <- lm_robust(formula_mod, data = endlines1and2, weights = w2, clusters = areaid)
  
  coef_treat <- coef(mod)["treatment"]
  se_treat   <- mod$std.error["treatment"]
  t_val      <- coef_treat / se_treat
  p_val      <- 2 * pt(abs(t_val), df = mod$df.residual, lower.tail = FALSE)
  
  used_index <- if (!is.null(mod$na.action)) {
    setdiff(seq_len(nrow(endlines1and2)), mod$na.action)
  } else {
    seq_len(nrow(endlines1and2))
  }
  
  sub_data <- endlines1and2[used_index, ]
  sub_ctrl <- sub_data[sub_data$treatment == 0, ]
  
  mean_val <- mean(sub_ctrl[[var]], na.rm = TRUE)
  sd_val   <- sd(sub_ctrl[[var]], na.rm = TRUE)
  
  n_obs <- nobs(mod)
  r2_val <- summary(mod)$r.squared
  
  data.frame(Variable    = var,
             Coefficient = round(coef_treat, 3),
             SE          = round(se_treat, 3),
             r2          = round(r2_val, 3),
             Mean        = round(mean_val, 3),
             SD          = round(sd_val, 3),
             N           = n_obs,
             pvalue      = round(p_val, 3),
             stringsAsFactors = FALSE)
})

results_panel_b_df <- do.call(rbind, results_panel_b)

### Combinar ambos paneles y organizar las columnas

results_table4 <- rbind(results_panel_a_df, results_panel_b_df)

# Exportar la tabla a un archivo de texto (separado por tabuladores)
write.table(results_table4, file = "table4.txt", sep = "\t",
            row.names = FALSE, quote = FALSE)

# ******************************************************************************
# -------- VIII. Table 5. Household labor hours --------
# ******************************************************************************

# ------------- Panel A. Endline 1 -----

# Variables a analizar en Endline 1
vars_panel_a <- c("hours_week_1", "hours_week_biz_1", "hours_week_outside_1", 
                  "hours_girl1620_week_1", "hours_boy1620_week_1", 
                  "hours_headspouse_week_1", "hours_headspouse_biz_1", 
                  "hours_headspouse_outside_1", "labor_index_1")

results_panel_a <- lapply(vars_panel_a, function(var) {
  # Construir la fórmula del modelo:
  formula_mod <- as.formula(paste0(var, " ~ treatment + ", 
                                   paste(area_controls, collapse = " + ")))
  
  # Estimar el modelo usando lm_robust() con peso w1 y clustering en areaid
  mod <- lm_robust(formula_mod, data = endlines1and2, weights = w1, clusters = areaid)
  
  # Extraer el coeficiente y error estándar de 'treatment'
  coef_treat <- coef(mod)["treatment"]
  se_treat   <- mod$std.error["treatment"]
  t_val      <- coef_treat / se_treat
  # Cálculo del p-valor bilateral
  p_val      <- 2 * pt(abs(t_val), df = mod$df.residual, lower.tail = FALSE)
  
  # Determinar el subconjunto de observaciones utilizadas (simulando e(sample))
  used_index <- if (!is.null(mod$na.action)) {
    setdiff(seq_len(nrow(endlines1and2)), mod$na.action)
  } else {
    seq_len(nrow(endlines1and2))
  }
  
  # Filtrar el grupo de control (treatment == 0) dentro de las observaciones usadas
  sub_data <- endlines1and2[used_index, ]
  sub_ctrl <- sub_data[sub_data$treatment == 0, ]
  
  # Calcular media y desviación estándar en el grupo de control
  mean_val <- mean(sub_ctrl[[var]], na.rm = TRUE)
  sd_val   <- sd(sub_ctrl[[var]], na.rm = TRUE)
  
  # Número de observaciones usadas en el modelo
  n_obs    <- nobs(mod)
  
  # Extraer el R² del modelo
  r2_val   <- summary(mod)$r.squared
  
  # Armar el data frame con los resultados para la variable
  data.frame(Variable    = var,
             Coefficient = round(coef_treat, 3),
             SE          = round(se_treat, 3),
             r2          = round(r2_val, 3),
             Mean        = round(mean_val, 3),
             SD          = round(sd_val, 3),
             N           = n_obs,
             pvalue      = round(p_val, 3),
             stringsAsFactors = FALSE)
})

# Combinar los resultados del Panel A
results_panel_a_df <- do.call(rbind, results_panel_a)

# ------------- Panel B. Endline 2 -----

# Variables a analizar en Endline 2
vars_panel_b <- c("hours_week_2", "hours_week_biz_2", "hours_week_outside_2", 
                  "hours_girl1620_week_2", "hours_boy1620_week_2", 
                  "hours_headspouse_week_2", "hours_headspouse_biz_2", 
                  "hours_headspouse_outside_2", "labor_index_2")

results_panel_b <- lapply(vars_panel_b, function(var) {
  # Construir la fórmula del modelo
  formula_mod <- as.formula(paste0(var, " ~ treatment + ", 
                                   paste(area_controls, collapse = " + ")))
  
  # Estimar el modelo usando lm_robust() con peso w2 y cluster en areaid
  mod <- lm_robust(formula_mod, data = endlines1and2, weights = w2, clusters = areaid)
  
  coef_treat <- coef(mod)["treatment"]
  se_treat   <- mod$std.error["treatment"]
  t_val      <- coef_treat / se_treat
  p_val      <- 2 * pt(abs(t_val), df = mod$df.residual, lower.tail = FALSE)
  
  used_index <- if (!is.null(mod$na.action)) {
    setdiff(seq_len(nrow(endlines1and2)), mod$na.action)
  } else {
    seq_len(nrow(endlines1and2))
  }
  
  sub_data <- endlines1and2[used_index, ]
  sub_ctrl <- sub_data[sub_data$treatment == 0, ]
  mean_val <- mean(sub_ctrl[[var]], na.rm = TRUE)
  sd_val   <- sd(sub_ctrl[[var]], na.rm = TRUE)
  
  n_obs    <- nobs(mod)
  r2_val   <- summary(mod)$r.squared
  
  data.frame(Variable    = var,
             Coefficient = round(coef_treat, 3),
             SE          = round(se_treat, 3),
             r2          = round(r2_val, 3),
             Mean        = round(mean_val, 3),
             SD          = round(sd_val, 3),
             N           = n_obs,
             pvalue      = round(p_val, 3),
             stringsAsFactors = FALSE)
})

# Combinar los resultados del Panel B
results_panel_b_df <- do.call(rbind, results_panel_b)

### Unir Panel A y Panel B y reordenar columnas

results_table5 <- rbind(results_panel_a_df, results_panel_b_df)

# Exportar la tabla a un archivo de texto (separador de tabulaciones)
write.table(results_table5, file = "table5.txt", sep = "\t",
            row.names = FALSE, quote = FALSE)

# ******************************************************************************
# -------- IX. Table 6. Consumption --------
# ******************************************************************************

# ------------- Panel A. Endline 1 -----

# Variables a analizar en Endline 1
vars_a <- c("total_exp_mo_pc_1", "durables_exp_mo_pc_1", "nondurable_exp_mo_pc_1", 
            "food_exp_mo_pc_1", "health_exp_mo_pc_1", "educ_exp_mo_pc_1", 
            "temptation_exp_mo_pc_1", "festival_exp_mo_pc_1", "home_durable_index_1")

results_panel_a <- lapply(vars_a, function(var) {
  
  # Construir la fórmula: ej. total_exp_mo_pc_1 ~ treatment + controles
  formula_mod <- as.formula(paste0(var, " ~ treatment + ", 
                                   paste(area_controls, collapse = " + ")))
  
  # Estimar el modelo con peso w1, cluster en areaid y nivel de confianza 90%
  mod <- lm_robust(formula_mod, data = endlines1and2, weights = w1, clusters = areaid)
  
  # Extraer coeficiente y error estándar de treatment
  coef_treat <- coef(mod)["treatment"]
  se_treat   <- mod$std.error["treatment"]
  t_val      <- coef_treat / se_treat
  # P-valor bilateral
  p_val      <- 2 * pt(abs(t_val), df = mod$df.residual, lower.tail = FALSE)
  
  # Obtener las observaciones usadas en el modelo (simulando e(sample))
  used_index <- if (!is.null(mod$na.action)) {
    setdiff(seq_len(nrow(endlines1and2)), mod$na.action)
  } else {
    seq_len(nrow(endlines1and2))
  }
  
  # Del subconjunto utilizado, extraer las observaciones de control (treatment == 0)
  sub_data <- endlines1and2[used_index, ]
  sub_ctrl <- sub_data[sub_data$treatment == 0, ]
  
  # Calcular media y desviación estándar en el grupo de control
  mean_val <- mean(sub_ctrl[[var]], na.rm = TRUE)
  sd_val   <- sd(sub_ctrl[[var]], na.rm = TRUE)
  
  # Obtener el número de observaciones y el R² del modelo
  n_obs  <- nobs(mod)
  r2_val <- summary(mod)$r.squared
  
  data.frame(Variable    = var,
             Coefficient = round(coef_treat, 3),
             SE          = round(se_treat, 3),
             r2          = round(r2_val, 3),
             Mean        = round(mean_val, 3),
             SD          = round(sd_val, 3),
             N           = n_obs,
             pvalue      = round(p_val, 3),
             stringsAsFactors = FALSE)
})

results_panel_a_df <- do.call(rbind, results_panel_a)

# ------------- Panel B. Endline 2 -----

vars_b <- c("total_exp_mo_pc_2", "durables_exp_mo_pc_2", "nondurable_exp_mo_pc_2", 
            "food_exp_mo_pc_2", "health_exp_mo_pc_2", "educ_exp_mo_pc_2", 
            "temptation_exp_mo_pc_2", "festival_exp_mo_pc_2", "home_durable_index_2")

results_panel_b <- lapply(vars_b, function(var) {
  
  formula_mod <- as.formula(paste0(var, " ~ treatment + ", 
                                   paste(area_controls, collapse = " + ")))
  
  # Estimar el modelo usando peso w2 y cluster en areaid; se usa el nivel por defecto (95%)
  mod <- lm_robust(formula_mod, data = endlines1and2, weights = w2, clusters = areaid)
  
  coef_treat <- coef(mod)["treatment"]
  se_treat   <- mod$std.error["treatment"]
  t_val      <- coef_treat / se_treat
  p_val      <- 2 * pt(abs(t_val), df = mod$df.residual, lower.tail = FALSE)
  
  used_index <- if (!is.null(mod$na.action)) {
    setdiff(seq_len(nrow(endlines1and2)), mod$na.action)
  } else {
    seq_len(nrow(endlines1and2))
  }
  
  sub_data <- endlines1and2[used_index, ]
  sub_ctrl <- sub_data[sub_data$treatment == 0, ]
  mean_val <- mean(sub_ctrl[[var]], na.rm = TRUE)
  sd_val   <- sd(sub_ctrl[[var]], na.rm = TRUE)
  
  n_obs  <- nobs(mod)
  r2_val <- summary(mod)$r.squared
  
  data.frame(Variable    = var,
             Coefficient = round(coef_treat, 3),
             SE          = round(se_treat, 3),
             r2          = round(r2_val, 3),
             Mean        = round(mean_val, 3),
             SD          = round(sd_val, 3),
             N           = n_obs,
             pvalue      = round(p_val, 3),
             stringsAsFactors = FALSE)
})

results_panel_b_df <- do.call(rbind, results_panel_b)

#### Combinar Panel A y Panel B y exportar la tabla

results_table6 <- rbind(results_panel_a_df, results_panel_b_df)

# Exportar la tabla a "table6.txt" (separado por tabulaciones)
write.table(results_table6, file = "table6.txt", sep = "\t",
            row.names = FALSE, quote = FALSE)

# ******************************************************************************
# -------- X. Table 7. Social Effects --------
# ******************************************************************************

# ------------- Panel A. Endline 1 -----

# Definir el vector de variables para el Panel A
vars_panel_a <- c("girl515_school_1", "boy515_school_1", 
                 "girl515_workhrs_pc_1", "boy515_workhrs_pc_1", 
                 "girl1620_school_1", "boy1620_school_1", 
                 "women_emp_index_1", "female_biz_new_1", "social_index_1")

results_panel_a <- lapply(vars_panel_a, function(var) {
  # Construir la fórmula del modelo (por ejemplo, "girl515_school_1 ~ treatment + controles")
  formula_mod <- as.formula(paste0(var, " ~ treatment + ", paste(area_controls, collapse = " + ")))
  
  # Estimar el modelo usando peso w1 y cluster en areaid
  mod <- lm_robust(formula_mod, data = endlines1and2, weights = w1, clusters = areaid)
  
  # Extraer el coeficiente y el error estándar para treatment
  coef_treat <- coef(mod)["treatment"]
  se_treat   <- mod$std.error["treatment"]
  t_val      <- coef_treat / se_treat
  # Calcular el p-valor bilateral
  p_val      <- 2 * pt(abs(t_val), df = mod$df.residual, lower.tail = FALSE)
  
  # Obtener los índices de las observaciones utilizadas (simula e(sample))
  used_index <- if (!is.null(mod$na.action)) {
    setdiff(seq_len(nrow(endlines1and2)), mod$na.action)
  } else {
    seq_len(nrow(endlines1and2))
  }
  
  # Del subconjunto utilizado, seleccionar aquellas observaciones donde treatment == 0 (grupo control)
  sub_data <- endlines1and2[used_index, ]
  sub_ctrl <- sub_data[sub_data$treatment == 0, ]
  
  # Calcular media y desviación estándar de la variable en el grupo control
  mean_val <- mean(sub_ctrl[[var]], na.rm = TRUE)
  sd_val   <- sd(sub_ctrl[[var]], na.rm = TRUE)
  
  # Número de observaciones utilizadas y R² del modelo
  n_obs  <- nobs(mod)
  r2_val <- summary(mod)$r.squared
  
  # Devolver un data frame con los resultados para la variable
  data.frame(Variable    = var,
             Coefficient = round(coef_treat, 3),
             SE          = round(se_treat, 3),
             r2          = round(r2_val, 3),
             Mean        = round(mean_val, 3),
             SD          = round(sd_val, 3),
             N           = n_obs,
             pvalue      = round(p_val, 3),
             stringsAsFactors = FALSE)
})

results_panel_a_df <- do.call(rbind, results_panel_a)

# ------------- Panel B. Endline 2 -----

# Definir el vector de variables para el Panel B
vars_panel_b <- c("girl515_school_2", "boy515_school_2", 
                 "girl515_workhrs_pc_2", "boy515_workhrs_pc_2", 
                 "girl1620_school_2", "boy1620_school_2", 
                 "women_emp_index_2", "female_biz_new_2", "social_index_2")

results_panel_b <- lapply(vars_panel_b, function(var) {
  formula_mod <- as.formula(paste0(var, " ~ treatment + ", paste(area_controls, collapse = " + ")))
  
  # Estimar el modelo con peso w2 y cluster en areaid
  mod <- lm_robust(formula_mod, data = endlines1and2, weights = w2, clusters = areaid)
  
  coef_treat <- coef(mod)["treatment"]
  se_treat   <- mod$std.error["treatment"]
  t_val      <- coef_treat / se_treat
  p_val      <- 2 * pt(abs(t_val), df = mod$df.residual, lower.tail = FALSE)
  
  used_index <- if (!is.null(mod$na.action)) {
    setdiff(seq_len(nrow(endlines1and2)), mod$na.action)
  } else {
    seq_len(nrow(endlines1and2))
  }
  
  sub_data <- endlines1and2[used_index, ]
  sub_ctrl <- sub_data[sub_data$treatment == 0, ]
  mean_val <- mean(sub_ctrl[[var]], na.rm = TRUE)
  sd_val   <- sd(sub_ctrl[[var]], na.rm = TRUE)
  
  n_obs  <- nobs(mod)
  r2_val <- summary(mod)$r.squared
  
  data.frame(Variable    = var,
             Coefficient = round(coef_treat, 3),
             SE          = round(se_treat, 3),
             r2          = round(r2_val, 3),
             Mean        = round(mean_val, 3),
             SD          = round(sd_val, 3),
             N           = n_obs,
             pvalue      = round(p_val, 3),
             stringsAsFactors = FALSE)
})

results_panel_b_df <- do.call(rbind, results_panel_b)

#### Combinar resultados de ambos paneles y exportar la tabla

results_table7 <- rbind(results_panel_a_df, results_panel_b_df)

# Guardar la tabla en "table7.txt" (archivo de texto con separador de tabulaciones)
write.table(results_table7, file = "table7.txt", sep = "\t",
            row.names = FALSE, quote = FALSE)

# ******************************************************************************
# -------- XI. Figure. Quantile Treatment Effect Regressions --------
# ******************************************************************************

df <- endlines1and2

# Generar las variables de profit para los distintos subgrupos:
df$bizprofit_1_old <- ifelse(df$any_old_biz == 1, df$bizprofit_1, NA)
df$bizprofit_1_new <- ifelse(df$any_old_biz == 0 & df$any_biz_1 == 1, df$bizprofit_1, NA)
df$bizprofit_2_biz <- ifelse(df$any_biz_2 == 1, df$bizprofit_2, NA)

# Fijar semilla para replicabilidad
set.seed(65209844)

# Lista de variables dependientes a analizar
outcome_vars <- c("informal_amt_1", "bizprofit_1_old", "bizprofit_1_new", "bizprofit_2_biz")

# Función personalizada para bootstrap con clusters para regresión por cuantiles.
# Esta función toma:
#   - data: data frame
#   - formula: fórmula del modelo (por ejemplo, outcome ~ treatment)
#   - tau: cuantíl a estimar
#   - cluster_var: nombre de la variable de cluster (en este caso "areaid")
#   - R: número de repeticiones de bootstrap (500)
cluster_bootstrap_rq <- function(data, formula, tau, cluster_var, R = 500) {
  clusters <- unique(data[[cluster_var]])
  n_clusters <- length(clusters)
  estimates <- numeric(R)
  
  for (r in 1:R) {
    # Muestreo de clusters con reemplazo
    sampled_clusters <- sample(clusters, size = n_clusters, replace = TRUE)
    # Reunir todas las filas correspondientes a los clusters seleccionados
    boot_sample <- do.call(rbind, lapply(sampled_clusters, function(cl) {
      data[data[[cluster_var]] == cl, ]
    }))
    
    # Estimar la regresión por cuantiles en la muestra bootstrapeada
    fit <- rq(formula, tau = tau, data = boot_sample)
    estimates[r] <- coef(fit)["treatment"]
  }
  
  return(estimates)
}

# Lista donde se guardarán los resultados para cada variable dependiente
results_list <- list()

# Función de bootstrap con clustering optimizada y paralela.
cluster_bootstrap_rq_parallel <- function(data, formula, tau, cluster_var, R = 500) {
  clusters <- unique(data[[cluster_var]])
  n_clusters <- length(clusters)
  
  # Función interna: ejecuta una iteración de bootstrap.
  bootstrap_iteration <- function(dummy) {
    # Seleccionar clusters de forma aleatoria con reemplazo
    sampled_clusters <- sample(clusters, size = n_clusters, replace = TRUE)
    # Reunir las filas correspondientes a cada cluster seleccionado
    boot_sample <- do.call(rbind, lapply(sampled_clusters, function(cl) {
      data[data[[cluster_var]] == cl, ]
    }))
    # Ajustar la regresión por cuantiles y devolver el coeficiente de treatment
    fit <- rq(formula, tau = tau, data = boot_sample)
    coef(fit)["treatment"]
  }
  
  # Utilizar mclapply para ejecutar R iteraciones en paralelo.
  # Para Windows, se recomienda usar foreach/doParallel en lugar de mclapply.
  estimates <- mclapply(1:R, bootstrap_iteration, mc.cores = detectCores() - 1)
  
  return(unlist(estimates))
}

# Lista para guardar resultados por cada variable
results_list <- list()

# Bucle sobre cada variable de interés
for (var in outcome_vars) {
  # Seleccionar casos completos para la variable dependiente, treatment y cluster.
  subdata <- df[!is.na(df[[var]]) & !is.na(df$treatment) & !is.na(df$areaid), ]
  
  # Preasignar la estructura del data frame de resultados.
  # Se define la secuencia de cuantiles (por ejemplo, de 0.05 a 0.97 en pasos de 0.05).
  quantiles <- seq(0.05, 0.97, by = 0.05)
  nq <- length(quantiles)
  results <- data.frame(qtile = quantiles,
                        ols_treatment = rep(NA, nq),
                        treatment = rep(NA, nq),
                        treatment_cilo = rep(NA, nq),
                        treatment_cihi = rep(NA, nq))
  
  # Calcular el valor crítico t para el 90% de confianza (se asume N_eff constante para la variable actual)
  N_eff <- nrow(subdata)
  ts <- qt(0.95, df = N_eff - 1)
  
  # Fórmula base para ambas regresiones
  current_formula <- as.formula(paste(var, "~ treatment"))
  
  # Ajustar la regresión OLS una sola vez si no varía por cuantíl
  ols_fit <- lm(current_formula, data = subdata)
  betaols <- coef(ols_fit)["treatment"]
  
  # Bucle sobre cada cuantíl
  for (i in seq_along(quantiles)) {
    q <- quantiles[i]
    cat("Procesando", var, "en cuantíl:", q, "\n")
    
    # Regresión por cuantiles
    rq_fit <- rq(current_formula, tau = q, data = subdata)
    beta <- coef(rq_fit)["treatment"]
    
    # Bootstrap con clustering para obtener error estándar (utilizando la versión paralela)
    boot_estimates <- cluster_bootstrap_rq_parallel(subdata,
                                                    formula = current_formula,
                                                    tau = q,
                                                    cluster_var = "areaid",
                                                    R = 500)
    sigma <- sd(boot_estimates)
    
    # Calcular los intervalos de confianza usando el valor crítico t calculado
    cil <- beta - (ts * sigma)
    ciu <- beta + (ts * sigma)
    
    # Guardar los resultados preasignados
    results$ols_treatment[i] <- betaols
    results$treatment[i] <- beta
    results$treatment_cilo[i] <- cil
    results$treatment_cihi[i] <- ciu
  }
  
  # Guardar los resultados para la variable actual en un archivo RDS
  filename <- paste0(var, "_qreg_results_optimized.RDS")
  saveRDS(results, file = filename)
  
  # Almacenar también en la lista para uso posterior si se requiere
  results_list[[var]] <- results
}


# ******************************************************************************
# -------- XII. Fig. 1. Quantile Treatment Effects, Informal Borrowing --------
# ******************************************************************************

# Leer los resultados para 'informal_amt_1'
results <- readRDS("informal_amt_1_qreg_results.RDS")

# Transformar: crear Percentile y la variable y0
results <- results %>%
  mutate(Percentile = qtile * 100,
         # y0 es cero, pero se vuelve NA si Percentile es menor a 5 o mayor a 95
         y0 = ifelse(Percentile < 5 | Percentile > 95, NA, 0))

# Truncar la cota superior del intervalo de confianza:
results <- results %>%
  mutate(treatment_cihi = ifelse(treatment_cihi > 20000, 20000, treatment_cihi),
         treatment_cihi = ifelse(Percentile == 95, NA, treatment_cihi))

# Construir el gráfico
tikz("figure1.tex", standAlone = TRUE, width = 8, height = 6)
p1 <- ggplot(results, aes(x = Percentile)) +
  # Línea OLS: discontinua
  geom_line(aes(y = ols_treatment, linetype = "OLS"), size = 1, na.rm = TRUE) +
  # Línea de tratamiento cuantílico: sólida, más gruesa y en gris
  geom_line(aes(y = treatment, linetype = "Quantile treatment effect"),
            size = 1.2, color = "gray", na.rm = TRUE) +
  # Línea base (y0): negra
  geom_line(aes(y = y0, linetype = "Baseline"),
            color = "black", size = 1, na.rm = TRUE) +
  # Líneas del intervalo de confianza: delgado, tipo dotdash y color navy
  geom_line(aes(y = treatment_cihi, linetype = "90\\% C.I."), 
            color = "navy", size = 0.5, na.rm = TRUE) +
  geom_line(aes(y = treatment_cilo, linetype = "90\\% C.I."), 
            color = "navy", size = 0.5, na.rm = TRUE) +
  # Configuración de ejes
  scale_x_continuous(breaks = c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95)) +
  scale_y_continuous(breaks = c(-18000, -6000, 0, 6000, 18000)) +
  # Títulos y etiquetas
  labs(title = "Treatment effect on informal borrowing",
       subtitle = "(Endline 1)",
       x = "Percentile",
       y = "Treatment effect") +
  theme_minimal() +
  theme(legend.position = "top") + 
  scale_linetype_manual(
  name = NULL,
  values = c("OLS" = "dashed",
             "Quantile treatment effect" = "solid",
             "90\\% C.I." = "dotdash",
             "Baseline" = "solid"),
  breaks = c("OLS", "Quantile treatment effect", "90\\% C.I."))
print(p1) 
dev.off()


# ******************************************************************************
# -------- XII. Fig. 2. Quantile Treatment Effects, E1 Profits (Old Businesses) --------
# ******************************************************************************

results <- readRDS("bizprofit_1_old_qreg_results_optimized.RDS")

# Transformar: crear Percentile y la variable y0
results <- results %>%
  mutate(Percentile = qtile * 100,
         # y0 es cero, pero se vuelve NA si Percentile es menor a 5 o mayor a 95
         y0 = ifelse(Percentile < 5 | Percentile > 95, NA, 0))

# Truncar la cota superior del intervalo de confianza:
results <- results %>%
  mutate(treatment_cihi = ifelse(treatment_cihi > 20000, 20000, treatment_cihi),
         treatment_cihi = ifelse(Percentile == 95, NA, treatment_cihi))

# Gráfico
tikz("figure2.tex", standAlone = TRUE, width = 8, height = 6)
p2 <- ggplot(results, aes(x = Percentile)) +
  # Línea OLS: discontinua
  geom_line(aes(y = ols_treatment, linetype = "OLS"), size = 1, na.rm = TRUE) +
  # Línea de tratamiento cuantílico: sólida, más gruesa y en gris
  geom_line(aes(y = treatment, linetype = "Quantile treatment effect"),
            size = 1.2, color = "gray", na.rm = TRUE) +
  # Línea base (y0): negra
  geom_line(aes(y = y0, linetype = "Baseline"),
            color = "black", size = 1, na.rm = TRUE) +
  # Líneas del intervalo de confianza: delgado, tipo dotdash y color navy
  geom_line(aes(y = treatment_cihi, linetype = "90\\% C.I."), 
            color = "navy", size = 0.5, na.rm = TRUE) +
  geom_line(aes(y = treatment_cilo, linetype = "90\\% C.I."), 
            color = "navy", size = 0.5, na.rm = TRUE) +
  # Configuración de ejes
  scale_x_continuous(breaks = c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95)) +
  scale_y_continuous(breaks = c(-3000, 0, 3000, 6000)) +
  # Títulos y etiquetas
  labs(title = "Treatment effect on business profits",
       subtitle = "(Old Business, Endline 1)",
       x = "Percentile",
       y = "Treatment effect") +
  theme_minimal() +
  theme(legend.position = "top") + 
  scale_linetype_manual(
    name = NULL, 
    values = c("OLS" = "dashed",
               "Quantile treatment effect" = "solid",
               "90\\% C.I." = "dotdash", 
               "Baseline" = "solid"),
    breaks = c("OLS", "Quantile treatment effect", "90\\% C.I."))
print(p2)
dev.off()

# ******************************************************************************
# -------- XII. Fig. 3. Quantile Treatment Effects, E1 Profits (New Businesses) --------
# ******************************************************************************

results <- readRDS("bizprofit_1_new_qreg_results_optimized.RDS")

# Transformar: crear Percentile y la variable y0
results <- results %>%
  mutate(Percentile = qtile * 100,
         # y0 es cero, pero se vuelve NA si Percentile es menor a 5 o mayor a 95
         y0 = ifelse(Percentile < 5 | Percentile > 95, NA, 0))
results <- results %>% 
  filter(Percentile <= 94)

# Truncar la cota superior del intervalo de confianza:
results <- results %>%
  mutate(treatment_cihi = ifelse(treatment_cihi > 20000, 20000, treatment_cihi),
         treatment_cihi = ifelse(Percentile == 95, NA, treatment_cihi))

# Gráfico
tikz("figure3.tex", standAlone = TRUE, width = 8, height = 6)
p3 <- ggplot(results, aes(x = Percentile)) +
  # Línea OLS: discontinua
  geom_line(aes(y = ols_treatment, linetype = "OLS"), size = 1, na.rm = TRUE) +
  # Línea de tratamiento cuantílico: sólida, más gruesa y en gris
  geom_line(aes(y = treatment, linetype = "Quantile treatment effect"),
            size = 1.2, color = "gray", na.rm = TRUE) +
  # Línea base (y0): negra
  geom_line(aes(y = y0, linetype = "Baseline"),
            color = "black", size = 1, na.rm = TRUE) +
  # Líneas del intervalo de confianza: delgado, tipo dotdash y color navy
  geom_line(aes(y = treatment_cihi, linetype = "90\\% C.I."), 
            color = "navy", size = 0.5, na.rm = TRUE) +
  geom_line(aes(y = treatment_cilo, linetype = "90\\% C.I."), 
            color = "navy", size = 0.5, na.rm = TRUE) +
  # Configuración de ejes
  scale_x_continuous(breaks = c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95)) +
  scale_y_continuous(breaks = c(-5000, 0, 5000)) +
  # Títulos y etiquetas
  labs(title = "Treatment effect on business profits",
       subtitle = "(New business, Endline 1)",
       x = "Percentile",
       y = "Treatment effect") +
  theme_minimal() +
  theme(legend.position = "top") + 
  scale_linetype_manual(
    name = NULL, 
    values = c("OLS" = "dashed",
               "Quantile treatment effect" = "solid", 
               "90\\% C.I." = "dotdash", 
               "Baseline" = "solid"),
    breaks = c("OLS", "Quantile treatment effect", "90\\% C.I."))
print(p3)
dev.off()

# ******************************************************************************
# -------- XII. Fig. 4. Quantile Treatment Effects, E2 Profits (All Businesses) --------
# ******************************************************************************

results <- readRDS("bizprofit_2_biz_qreg_results_optimized.RDS")

# Transformar: crear Percentile y la variable y0
results <- results %>%
  mutate(Percentile = qtile * 100,
         # y0 es cero, pero se vuelve NA si Percentile es menor a 5 o mayor a 95
         y0 = ifelse(Percentile < 5 | Percentile > 95, NA, 0))

# Truncar la cota superior del intervalo de confianza:
results <- results %>%
  mutate(treatment_cihi = ifelse(treatment_cihi > 20000, 20000, treatment_cihi),
         treatment_cihi = ifelse(Percentile == 95, NA, treatment_cihi))

# Gráfico
tikz("figure4.tex", standAlone = TRUE, width = 8, height = 6)
p4 <- ggplot(results, aes(x = Percentile)) +
  # Línea OLS: discontinua
  geom_line(aes(y = ols_treatment, linetype = "OLS"), size = 1, na.rm = TRUE) +
  # Línea de tratamiento cuantílico: sólida, más gruesa y en gris
  geom_line(aes(y = treatment, linetype = "Quantile treatment effect"),
            size = 1.2, color = "gray", na.rm = TRUE) +
  # Línea base (y0): negra
  geom_line(aes(y = y0, linetype = "Baseline"),
            color = "black", size = 1, na.rm = TRUE) +
  # Líneas del intervalo de confianza: delgado, tipo dotdash y color navy
  geom_line(aes(y = treatment_cihi, linetype = "90\\% C.I."), 
            color = "navy", size = 0.5, na.rm = TRUE) +
  geom_line(aes(y = treatment_cilo, linetype = "90\\% C.I."), 
            color = "navy", size = 0.5, na.rm = TRUE) +
  # Configuración de ejes
  scale_x_continuous(breaks = c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95)) +
  scale_y_continuous(breaks = c(-1500, 0, 1500, 3000, 4500)) +
  # Títulos y etiquetas
  labs(title = "Treatment effect on business profits",
       subtitle = "(All business, Endline 2)",
       x = "Percentile",
       y = "Treatment effect") +
  theme_minimal() +
  theme(legend.position = "top") + 
  scale_linetype_manual(
    name = NULL, 
    values = c("OLS" = "dashed",
               "Quantile treatment effect" = "solid",
               "90\\% C.I." = "dotdash",
               "Baseline" = "solid"), 
    breaks = c("OLS", "Quantile treatment effect", "90\\% C.I."))
print(p4)
dev.off()

