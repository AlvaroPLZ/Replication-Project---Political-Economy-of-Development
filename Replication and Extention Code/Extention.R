### Author: Álvaro Pérez
### Title: Paper extention
### Last update: May 18, 2025
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
library(quantreg)   # Para regresiones por cuantiles
library(boot)       # Para bootstrap
library(parallel)
library(tikzDevice)
library(purrr)
install.packages("modelsummary")
library(modelsummary)
library(knitr)
install.packages("kableExtra")
library(kableExtra)
install.packages("interactions")
library(interactions)
install.packages("ggeffects")
library(ggeffects)
install.packages("grf")
library(grf) # For Causal Forest

# ******************************************************************************
# -------- II. Loading Data --------
# ******************************************************************************

endlines1and2 <- read_dta("2013-0533_data_endlines1and2.dta")
baseline <- read_dta("2013-0533_data_baseline.dta")
census <- read_dta("2013-0533_data_census.dta")
businesstype <- read_dta("2013-0533_data_endline1businesstype.dta")

## Merge by Household ID
df <- endlines1and2 %>% 
  left_join(businesstype, by = "hhid")

# ******************************************************************************
# -------- III. Controls and Outcomes --------
# ******************************************************************************

area_controls <- c(
  "area_pop_base", "area_debt_total_base", "area_business_total_base",
  "area_exp_pc_mean_base", "area_literate_head_base", "area_literate_base"
)

outcomes <- c(
  "bizprofit_1", "bizassets_1", "bizinvestment_1",
  "anymfi_1", "bank_amt_1", "credit_index_1"
)

outcomes_2 <- c(
  "bizprofit_2", "bizassets_2", "bizinvestment_2",
  "anymfi_2", "bank_amt_2", "credit_index_2"
)

# ******************************************************************************
# -------- IV. Dummies by sector --------
# ******************************************************************************

df <- df %>%
  mutate(biz_type = as.factor(business_type_aggregate_1)) %>%
  mutate(dummy = 1) %>%
  pivot_wider(names_from = biz_type,
              values_from = dummy,
              values_fill  = 0,
              names_prefix = "sector_")

# Lista de nombres de dummies de sector
sector_vars <- df %>% select(starts_with("sector_")) %>% names()
sector_vars <- sector_vars[sector_vars != "sector_NA"]

df <- df %>%
  # renombra treatment.x → treatment, y areaid.x → areaid
  rename(
    treatment = treatment.x,
    areaid    = areaid.x
  ) %>%
  # quita las columnas sobrantes
  select(-treatment.y, -areaid.y)

# ******************************************************************************
# -------- V. Function for interaction treatment x sector --------
# ******************************************************************************

run_interaction <- function(data, Y, Z) {
  # 1) Construye la fórmula
  f <- as.formula(
    paste0(Y, " ~ treatment * ", Z, " + ", paste(area_controls, collapse = " + "))
  )
  # 2) Estima el modelo
  m <- lm_robust(f, data = data,
                 weights  = w1,
                 clusters = areaid)
  # 3) Tidy y diagnóstico (opcional)
  td <- broom::tidy(m)
  # cat("Terms for", Y, "×", Z, ":\n"); print(td$term)
  
  # 4) Filtra cualquier término que contenga "treatment" seguido de ":" y el nombre del sector
  ix <- grepl(paste0("^treatment.*:", Z, "$"), td$term)
  if (!any(ix)) {
    warning(sprintf("No encuentro interaction para %s × %s", Y, Z))
    return(tibble(
      outcome   = Y,
      sector    = Z,
      estimate  = NA_real_,
      std.error = NA_real_,
      p.value   = NA_real_
    ))
  }
  inter <- td[ix, ]
  
  # 5) Devuelve sólo las columnas que queremos
  inter %>% transmute(
    outcome   = Y,
    sector    = Z,
    estimate  = estimate,
    std.error = std.error,
    p.value   = p.value
  )
}

# ******************************************************************************
# -------- VI. Run Combinations treatment x sector --------
# ******************************************************************************

results_raw <- cross2(outcomes, sector_vars) %>%
  map_dfr(~ run_interaction(df, .x[[1]], .x[[2]]))

# ******************************************************************************
# -------- VII. P-values adjust for multiple test --------
# ******************************************************************************

results <- results_raw_2 %>%
  mutate(p.adj = p.adjust(p.value, method = "hochberg"))

# ******************************************************************************
# -------- VIII. Table with β₃ for each sector-outcome  --------
# ******************************************************************************

results_cells <- results %>%
  # 3.1. Crear asteriscos según p.adj
  mutate(
    stars = case_when(
      p.adj < 0.01 ~ "***",
      p.adj < 0.05 ~ "**",
      p.adj < 0.10 ~ "*",
      TRUE         ~ ""
    ),
    # 3.2. Formatear coeficiente y error en una sola cadena, con \\ para salto de línea en LaTeX
    cell = paste0(
      sprintf("%.3f", estimate), stars,     # coef + stars
      "\\\\\n(",                          # salto + paréntesis
      sprintf("%.3f", std.error),        # error estándar
      ")"                                 # cierre
    )
  ) %>%
  select(sector, outcome, cell)

# 4. Pivotar a formato ancho: filas = sector, columnas = outcome
table_wide <- results_cells %>%
  pivot_wider(
    names_from  = outcome,
    values_from = cell
  ) %>%
  # (opcional) reordenar sectores en un orden lógico
  arrange(sector)

# 5. Crear la tabla en LaTeX con kableExtra
latex_code <- table_wide %>%
  kable(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,                   # para que \\ y paréntesis salgan sin escapar
    caption   = "Heterogeneidad del efecto por tipo de negocio (\\(\\beta_3\\))",
    align     = paste0("l", rep("c", ncol(table_wide)-1), collapse = "")
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down")
  )

# 6. Imprimir en consola
cat(latex_code)

# ******************************************************************************
# -------- VIII. Margin effects (Plot)  --------
# ******************************************************************************

# Tomemos un sector de ejemplo, p.ej. "sector_food"
mod_food <- lm_robust(
  bizprofit_1 ~ treatment * sector_1 + 
    area_pop_base + area_debt_total_base + area_business_total_base +
    area_exp_pc_mean_base + area_literate_head_base + area_literate_base,
  data     = df,
  weights  = w1,
  clusters = areaid
)

# 2. Calcula y asigna el QR de la matriz de diseño
Xmat <- model.matrix(formula(mod_food), data = df)
mod_food$qr <- qr(Xmat)

# 3. Cambia la clase para heredar de "lm"
class(mod_food) <- c("lm", class(mod_food))

# 4. Ahora ggpredict debe funcionar
gge <- ggpredict(mod_food, terms = c("treatment", "sector_1 [0,1]"))
plot(gge)

# Gráfico
ggplot(gge, aes(x = x, y = predicted, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(
    title  = "Efecto del tratamiento según sector: Food",
    x      = "Sector food (0 vs 1)",
    y      = "Predicción de Biz profit",
    color  = "Treatment"
  ) +
  theme_minimal()


# ******************************************************************************
# -------- IX. Quantile Regressions  --------
# ******************************************************************************

# Función para correr rq con interacción en un cuantil dado
run_rq <- function(data, Y, Z, tau){
  f <- as.formula(
    paste0(Y, " ~ treatment * ", Z, " + ", paste(area_controls, collapse = " + "))
  )
  # intentamos ajustar rq(), si falla devolvemos NA
  mod <- tryCatch(
    rq(f, tau = tau, data = data, weights = w1),
    error = function(e) return(NULL)
  )
  if (is.null(mod)) {
    warning("rq() failed for sector=", Z, " tau=", tau)
    return(tibble(
      sector    = Z,
      tau       = tau,
      estimate  = NA_real_,
      std.error = NA_real_
    ))
  }
  # si succeed, tratamos de extraer coeficiente y SE
  s <- tryCatch(
    summary(mod, se="nid")$coefficients,
    error = function(e) return(NULL)
  )
  if (is.null(s)) {
    warning("summary.rq() failed for sector=", Z, " tau=", tau)
    return(tibble(
      sector    = Z,
      tau       = tau,
      estimate  = NA_real_,
      std.error = NA_real_
    ))
  }
  term <- paste0("treatment:", Z)
  if (!(term %in% rownames(s))) {
    warning("interaction term not found for sector=", Z, " tau=", tau)
    return(tibble(
      sector    = Z,
      tau       = tau,
      estimate  = NA_real_,
      std.error = NA_real_
    ))
  }
  tibble(
    sector    = Z,
    tau       = tau,
    estimate  = s[term, "Value"],
    std.error = s[term, "Std. Error"]
  )
}

tau <- c(0.75,0.85,0.95)

# Correr para cada sector y cuantil
quant_results <- cross2(sector_vars, c(0.75,0.85,0.95)) %>%
  map_dfr(~ run_rq(df, "biz_index_all_1", .x[[1]], .x[[2]]))

# Results
print(quant_results)

# 1) Calcula t-stat y p-valor aproximado (normal) para cada fila
quant_adj <- quant_results %>%
  mutate(
    t_stat = estimate / std.error,
    p_raw  = 2 * pnorm(-abs(t_stat)),
    p_adj  = p.adjust(p_raw, method = "hochberg"),
    signif = case_when(
      p_adj < 0.01 ~ "***",
      p_adj < 0.05 ~ "**",
      p_adj < 0.10 ~ "*",
      TRUE         ~ ""
    )
  )

# 2) Mira la tabla con p-ajustados y estrellas
print(quant_adj)

# 3) Gráfico de líneas con IC 95% y estrellas de significancia

quant_adj2 <- quant_adj %>%
  filter(signif != "") %>%
  mutate(signif = factor(signif, levels = c("*","**","***")))

ggplot(quant_adj, aes(x = tau, y = estimate, color = sector)) +
  geom_ribbon(aes(ymin = estimate - 1.96*std.error,
                  ymax = estimate + 1.96*std.error,
                  fill = sector),
              alpha = 0.1, color = NA) +
  geom_line(size = 1) +
  geom_point(data = quant_adj,
             aes(shape = signif),
             size = 2, show.legend = FALSE) +
  scale_shape_manual(values = c("*"=8, "**"=15, "***"=17)) +
  facet_wrap(~ sector) +
  labs(
    x        = expression(tau),
    y        = expression(hat(beta)[3](tau)),
    title    = "Treatment × sector interaction in high quantiles",
    subtitle = "Bandwith = IC 95%, points = significant (Hochberg)",
    color    = "Sector",
    fill     = "Sector"
  ) +
  theme_minimal() +
  theme(
    strip.text      = element_text(face="bold"),
    legend.position = "bottom"
  )

# ******************************************************************************
# ---- X. Bootstrap cluster para errores ----
# ******************************************************************************

# 1) Prepara la fórmula y el término de interés
tau    <- 0.75
sector <- "sector_1"
Y      <- "bizprofit_1"
term   <- paste0("treatment:", sector)
f      <- as.formula(
  paste0(Y, " ~ treatment * ", sector, 
         " + ", paste(area_controls, collapse=" + "))
)

# 2) Identifica tus clusters (áreas) únicos
clusters <- unique(df$areaid)

# 3) Número de réplicas bootstrap
B <- 500  # mínimo 200–500; para mayor precisión sube a 1000

# 4) Función que ajusta rq() en una muestra bootstrap de clusters
boot_coef <- function() {
  # a) remuestrea los clusters con reemplazo
  sampled_clusters <- sample(clusters, length(clusters), replace = TRUE)
  # b) arma el data.frame bootstrap uniendo todas las obs. de cada cluster
  df_boot <- do.call(rbind, lapply(sampled_clusters, function(cl) {
    df %>% filter(areaid == cl)
  }))
  # c) ajusta la cuantílica en esa muestra
  mod_boot <- rq(f, tau = tau, data = df_boot, weights = w1)
  # d) extrae y devuelve el coeficiente de la interacción
  cf <- coef(mod_boot)
  if (term %in% names(cf)) {
    return(cf[term])
  } else {
    return(NA_real_)
  }
}

# 5) Ejecuta las B réplicas
set.seed(12345)
boot_estimates <- replicate(B, boot_coef())

# 6) Calcula el error estándar bootstrap y la media como estimador
se_boot <- sd(boot_estimates, na.rm = TRUE)
beta_hat <- mean(boot_estimates, na.rm = TRUE)

# 7) Intervalo de confianza (por percentiles)
ci_low  <- quantile(boot_estimates, 0.025, na.rm = TRUE)
ci_high <- quantile(boot_estimates, 0.975, na.rm = TRUE)

# 8) Resultado
cat(sprintf("τ = %.2f, sector = %s\n", tau, sector))
cat(sprintf("Coef. bootstrapped = %.3f\n", beta_hat))
cat(sprintf("SE (bootstrap)       = %.3f\n", se_boot))
cat(sprintf("95%% CI bootstrapped = [%.3f, %.3f]\n", ci_low, ci_high))

# ******************************************************************************
# -------- XI. Causal Forest  --------
# ******************************************************************************

df1 <- zap_labels(df)

## Preparar datos
vars_necesarias <- c(
  "biz_index_all_1", "treatment", "w1", "areaid",
  sector_vars,   # tus dummies sector_1, sector_2, …
  area_controls  # tus controles de área
)

# 2) Crea un data frame sin NAs en esas columnas
df_cf <- df1 %>%
  select(all_of(vars_necesarias)) %>%
  tidyr::drop_na()

# 3) Reconstruye X, Y, W, clusters y sw desde df_cf
X        <- df_cf %>% select(starts_with("sector_"), all_of(area_controls)) %>% as.matrix()
Y        <- as.numeric(df_cf$biz_index_all_1)
W        <- as.numeric(df_cf$treatment)
clusters <- as.integer(df_cf$areaid)
sw       <- as.numeric(df_cf$w1)

# 2.3 Ajusta el bosque causal
cf <- causal_forest(
  X, Y, W,
  num.trees = 2000,
  clusters  = clusters,   # clusteriza si quieres
  sample.weights = sw    # usa peso si lo deseas
)

# 2.4 Estima los CATEs (efectos individuales)
cates <- predict(cf)$predictions

# 2.5 Importancia de variables para explicar la heterogeneidad
vi <- variable_importance(cf)
# vi es un vector con puntajes para cada columna de X
imp <- tibble(
  variable = colnames(X),
  importance = vi
) %>% arrange(desc(importance))
print(imp)

# Asigna los CATE generados a tu data.frame limpio:
df_cf <- df_cf %>% mutate(CATE = cates)

# 2.6 Promedios de CATE por sector
avg_by_sector <- map_df(sector_vars, function(sector){
  sel <- df_cf[[sector]] == 1
  tibble(
    sector     = sector,
    mean_CATE  = mean(df_cf$CATE[sel], na.rm = TRUE),
    N          = sum(sel)
  )
})

print(avg_by_sector)

# 1) “Long” de los sectores
df_long <- df_cf %>%
  select(CATE, all_of(sector_vars)) %>%
  pivot_longer(
    cols      = all_of(sector_vars),
    names_to  = "sector",
    values_to = "in_sector"
  ) %>%
  filter(in_sector == 1)  # sólo hogares en cada sector

# 2) Resume media, sd, se, t y p por sector
signif_by_sector <- df_long %>%
  group_by(sector) %>%
  summarise(
    mean_CATE = mean(CATE, na.rm = TRUE),
    sd_CATE   = sd(CATE,   na.rm = TRUE),
    N         = n(),
    se_CATE   = sd_CATE / sqrt(N),
    t_value   = mean_CATE / se_CATE,
    p_value   = 2 * pt(-abs(t_value), df = N - 1)
  ) %>%
  ungroup() %>%
  mutate(
    stars = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE           ~ ""
    )
  )

print(signif_by_sector)

signif_by_sector %>%
  ggplot(aes(x = reorder(sector, mean_CATE), y = mean_CATE)) +
  geom_col(fill="steelblue") +
  geom_errorbar(aes(ymin = mean_CATE - se_CATE, ymax = mean_CATE + se_CATE),
                width = 0.2) +
  coord_flip() +
  labs(x="Sector", y="Efecto promedio (CATE)", 
       title="Media de CATE por sector con IC 95%")

### ANOVA ###
df_sector <- df_cf %>%
  # pivot_longer convierte las dummies a largo
  pivot_longer(
    cols      = starts_with("sector_"),
    names_to  = "sector",
    values_to = "in_sector"
  ) %>%
  filter(in_sector == 1) %>%      # sólo la fila con dummy==1
  select(-in_sector)              # ya no necesitamos el indicador

# Asegúrate que sector sea factor
df_sector <- df_sector %>%
  mutate(sector = factor(sector))

# 2) ANOVA: ajusta el modelo y revisa tabla de ANOVA
aov_mod <- aov(CATE ~ sector, data = df_sector)
summary(aov_mod)

# 3) Tukey HSD: comparaciones múltiples por pares
tukey_res <- TukeyHSD(aov_mod, "sector", conf.level = 0.95)
print(tukey_res)

# 4) (Opcional) tabla de resultados limpia
library(broom)
tukey_tab <- broom::tidy(tukey_res)
print(tukey_tab)

# ******************************************************************************
# ---- XII. Función completa ----
# ******************************************************************************

# Función integral para analizar heterogeneidad por sector
run_sector_analysis <- function(df_input, 
                                Yvar,           # e.g. "bizprofit_1" or "bizprofit_2"
                                weight_var,     # e.g. "w1" or "w2"
                                sector_vars,    # vector de nombres "sector_1", "sector_2", …
                                area_controls,  # vector de nombres de controles
                                taus = c(0.75, 0.90),  # cuantiles a estimar
                                B    = 500      # réplicas bootstrap para cuantílicas
) {
  library(dplyr); library(purrr); library(broom)
  library(estimatr); library(quantreg); library(grf)
  
  # 1) Función para regresiones lineales con interacción
  run_interaction <- function(data, Y, Z) {
    f <- as.formula(paste0(Y, " ~ treatment * ", Z, " + ", paste(area_controls, collapse=" + ")))
    m <- lm_robust(f, data = data, weights = data[[weight_var]], clusters = areaid)
    td <- broom::tidy(m)
    # Extraer β₃
    term <- paste0("treatment:", Z)
    if (!(term %in% td$term)) return(tibble(sector=Z, beta3=NA, se=NA, p.value=NA))
    row <- td %>% filter(term==term)
    tibble(sector   = Z,
           beta3    = row$estimate,
           se       = row$std.error,
           p.value  = row$p.value)
  }
  interaction_res <- map_df(sector_vars, ~ run_interaction(df_input, Yvar, .x))
  
  # 2) Función bootstrap para regresiones cuantílicas
  bootstrap_rq <- function(data, Y, Z, tau) {
    f    <- as.formula(paste0(Y, " ~ treatment * ", Z, " + ", paste(area_controls, collapse=" + ")))
    term <- paste0("treatment:", Z)
    clusters <- unique(data$areaid)
    boot_coef <- function() {
      samp <- sample(clusters, length(clusters), replace = TRUE)
      dfb  <- bind_rows(lapply(samp, function(c) filter(data, areaid==c)))
      modb <- tryCatch(rq(f, tau=tau, data=dfb, weights=dfb[[weight_var]]), error=function(e) NULL)
      if (is.null(modb)) return(NA_real_)
      cf <- coef(modb)
      cf[term] %||% NA_real_
    }
    ests <- replicate(B, boot_coef())
    tibble(sector   = Z,
           tau      = tau,
           estimate = mean(ests, na.rm=TRUE),
           se_boot  = sd(ests, na.rm=TRUE),
           ci_low   = quantile(ests, 0.025, na.rm=TRUE),
           ci_high  = quantile(ests, 0.975, na.rm=TRUE))
  }
  quant_res <- cross2(sector_vars, taus) %>%
    map_dfr(~ bootstrap_rq(df_input, Yvar, .x[[1]], .x[[2]]))
  
  # 3) Causal forest
  # 3a) Filtrar NAs
  vars_needed <- c(Yvar, "treatment", weight_var, "areaid", sector_vars, area_controls)
  df_cf <- df_input %>% select(all_of(vars_needed)) %>% tidyr::drop_na()
  # 3b) Construir X, Y, W, clusters, sw
  X        <- df_cf %>% select(all_of(sector_vars), all_of(area_controls)) %>% as.matrix()
  Y        <- as.numeric(df_cf[[Yvar]])
  W        <- as.numeric(df_cf$treatment)
  clusters <- as.integer(df_cf$areaid)
  sw       <- as.numeric(df_cf[[weight_var]])
  # 3c) Ajustar
  cf <- causal_forest(X, Y, W,
                      num.trees      = 2000,
                      clusters       = clusters,
                      sample.weights = sw)
  # 3d) Importancia y CATEs
  vi  <- variable_importance(cf)
  imp <- tibble(variable = colnames(X), importance = vi) %>% arrange(desc(importance))
  cates <- predict(cf)$predictions
  df_cf <- df_cf %>% mutate(CATE = cates)
  avg_cate <- map_df(sector_vars, function(sec) {
    sel <- df_cf[[sec]]==1
    tibble(sector   = sec,
           mean_CATE = mean(df_cf$CATE[sel], na.rm=TRUE),
           N         = sum(sel))
  })
  
  # 4) ANOVA + Tukey
  df_sector <- df_cf %>%
    select(CATE, all_of(sector_vars)) %>%
    pivot_longer(cols=all_of(sector_vars), names_to="sector", values_to="in_sector") %>%
    filter(in_sector==1) %>% mutate(sector = factor(sector))
  aov_mod      <- aov(CATE ~ sector, data=df_sector)
  anova_table  <- summary(aov_mod)
  tukey_res    <- TukeyHSD(aov_mod, "sector")
  tukey_tab    <- broom::tidy(tukey_res)
  
  # 5) Devolver todos los resultados en una lista
  list(
    interactions    = interaction_res,
    quantile_boot   = quant_res,
    var_importance  = imp,
    avg_CATE_by_sec = avg_cate,
    anova_summary   = anova_table,
    tukey           = tukey_tab
  )
}
res1 <- run_sector_analysis(
  df_input      = df,
  Yvar          = "bizprofit_1",
  weight_var    = "w1",
  sector_vars   = sector_vars,
  area_controls = area_controls
)
