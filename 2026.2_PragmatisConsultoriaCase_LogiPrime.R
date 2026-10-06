# Importações e unificação dos dados

setwd("C:\\Users\\Joelson Costa\\Documents\\Estágios\\Pragmatis_Consultoria\\PS2026.2_PRG00-CaseM&ALogistica\\database")

# install.packages("tidyverse")
# install.packages("ggplot2", type = "binary")
# install.packages("gt", type = "binary")
if (!require("fmsb")) install.packages("fmsb", dependencies = TRUE)
library(fmsb)
if (!require("gridExtra")) install.packages("gridExtra")
library(gridExtra)
library(grid)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(lubridate)
library(scales)
library(purrr)
library(forcats)
library(gt)

dados <- list.files(
  pattern = "\\.csv$",  # seleciona apenas arquivos que terminam em .csv
  full.names = TRUE     # traz o caminho completo do arquivo
)

dados <- map_dfr(dados, read_csv)

#####################################################################
# Checagem, limpeza e criação de novo banco de dados 

glimpse(dados)
# Vemos que os tipos das variáveis estão corretos

# Novo banco de dados limpo
dados_limpos <- dados %>%
  # Excluir registros inconsistentes
  filter(
    !erro_missing_km,
    !erro_missing_tempo,
    !erro_negativo_km,
    !erro_negativo_tempo
  ) %>%
  # Remover colunas de erros e textos redundantes para otimizar a memória
  select(
    -starts_with("erro_"),
    -ends_with("_txt")
  ) %>%
  # Converter variáveis de texto repetitivas para factor, para aprimorar o desempenho
  mutate(
    empresa      = as.factor(empresa),
    uf           = as.factor(uf),
    tipo_veiculo = as.factor(tipo_veiculo)
  )

# Noas variáveis úteis para análises
dados_limpos <- dados_limpos %>%
  group_by(codigo_rota) %>%
  mutate(
    seq_entrega_corrigida   = row_number(),
    num_entregas_rota_calc = n(),
    km_total_rota_calc     = sum(km_trecho, na.rm = TRUE),
    peso_kg_total_calc     = sum(peso_kg_entrega, na.rm = TRUE),
    volume_m3_total_calc   = sum(volume_m3_entrega, na.rm = TRUE)
  ) %>%
  ungroup()

cat("--- Cobertura Temporal da Base Limpa ---\n")
cat("Data inicial:", as.character(min(dados_limpos$data_rota)), "\n")
cat("Data final  :", as.character(max(dados_limpos$data_rota)), "\n")
cat("Dias distintos cobertos:", n_distinct(dados_limpos$data_rota), "\n\n")

glimpse(dados_limpos)
# Checagem do novo banco de dados

cat("Linhas da base original:", nrow(dados), "\n")
cat("Linhas mantidas na base limpa:", nrow(dados_limpos), "\n")

# Comparativo de perda de dados da base original com a limpa, por UF
vies_uf <- dados %>%
  group_by(uf) %>%
  summarise(
    total_original = n(),
    com_erro       = sum(erro_missing_km | erro_missing_tempo | erro_negativo_km | erro_negativo_tempo),
    pct_perda      = (com_erro / total_original) * 100
  )

perda_global <- dados %>%
  summarise(
    pct_global = mean(
      erro_missing_km | erro_missing_tempo | erro_negativo_km | erro_negativo_tempo, 
      na.rm = TRUE
    ) * 100
  ) %>%
  pull(pct_global)

# Gráfico de comparação entre porcentagens de perda de dados por UF
ggplot(vies_uf, aes(x = reorder(uf, -pct_perda), y = pct_perda, fill = uf)) +
  geom_col(show.legend = FALSE, width = 0.6, alpha = 0.85) +
  geom_text(
    aes(label = sprintf("%.2f%%", pct_perda)), 
    vjust = -0.7,             # Ajuste fino para distanciar o texto do topo da barra/linha
    fontface = "bold", 
    size = 3.8
  ) +
  geom_hline(
    aes(yintercept = perda_global, color = sprintf("Perda Global (%.2f%%)", perda_global)), 
    linetype = "dashed", 
    linewidth = 1
  ) +
  scale_color_manual(
    name = NULL, 
    values = c("red")
  ) +
  scale_y_continuous(
    limits = c(0, max(vies_uf$pct_perda) * 1.15), # Aumenta o teto para dar folga aos rótulos
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Porcentagem de Perda de Dados por Estado (UF)",
    subtitle = "Comparativo entre a perda por estado e a referência global",
    x = "Estado (UF)",
    y = "Porcentagem de Perda (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )

#####################################################################
# Perfis operacionais das rotas das empresas

# Tabela
rotas_resumo <- dados_limpos %>%
  group_by(codigo_rota) %>%
  summarise(
    empresa              = first(empresa),
    uf                   = first(uf),
    tipo_veiculo         = first(tipo_veiculo),
    
    # Paradas e Entregas
    n_paradas            = n_distinct(pdv_id, na.rm = TRUE),
    
    # Quilometragem e Tempos
    km_total_rota        = sum(km_trecho, na.rm = TRUE),
    tempo_transito_min   = sum(tempo_trecho_min, na.rm = TRUE),
    tempo_espera_min     = sum(tempo_espera_pdv_min, na.rm = TRUE),
    tempo_descarga_min   = sum(tempo_descarga_pdv_min, na.rm = TRUE),
    tempo_total_rota_min = tempo_transito_min + tempo_espera_min + tempo_descarga_min,
    hora_extra_min       = first(hora_extra_min),
    
    # Cargas Totais da Rota
    peso_total_kg        = sum(peso_kg_entrega, na.rm = TRUE),
    volume_total_m3      = sum(volume_m3_entrega, na.rm = TRUE),
    
    # Ocupação da Frota
    ocup_peso_pct        = first(ocup_kg_rota) * 100,
    ocup_vol_pct         = first(ocup_vol_rota) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    # Indicadores Derivados por Rota
    velocidade_media_kmh = ifelse(tempo_transito_min > 0, (km_total_rota / (tempo_transito_min / 60)), NA),
    produtividade_m3_h   = ifelse(tempo_total_rota_min > 0, (volume_total_m3 / (tempo_total_rota_min / 60)), NA),
    carga_por_parada_kg  = ifelse(n_paradas > 0, peso_total_kg / n_paradas, NA),
    carga_por_parada_m3  = ifelse(n_paradas > 0, volume_total_m3 / n_paradas, NA)
  )

tabela_perfil_operacional <- rotas_resumo %>%
  group_by(empresa) %>%
  summarise(
    # Distância e Paradas
    `Km Total por Rota`     = paste0(round(mean(km_total_rota), 1), " / ", round(median(km_total_rota), 1), " km"),
    `Nº de Paradas / PDVs`            = paste0(round(mean(n_paradas), 1), " / ", round(median(n_paradas), 0)),
    
    # Cargas e Ocupação
    `Carga Peso Total (kg)`           = paste0(format(round(mean(peso_total_kg), 0), big.mark = "."), " / ", format(round(median(peso_total_kg), 0), big.mark = ".")),
    `Carga Volume Total (m³)`         = paste0(round(mean(volume_total_m3), 1), " / ", round(median(volume_total_m3), 1)),
    `Ocupação Peso (%)`              = paste0(round(mean(ocup_peso_pct), 1), "% / ", round(median(ocup_peso_pct), 1), "%"),
    `Ocupação Volume (%)`            = paste0(round(mean(ocup_vol_pct), 1), "% / ", round(median(ocup_vol_pct), 1), "%"),
    
    # Velocidade e Eficiência
    `Velocidade Média (km/h)`         = paste0(round(mean(velocidade_media_kmh, na.rm=T), 1), " / ", round(median(velocidade_media_kmh, na.rm=T), 1), " km/h"),
    `Produtividade (m³/h)`            = paste0(round(mean(produtividade_m3_h, na.rm=T), 2), " / ", round(median(produtividade_m3_h, na.rm=T), 2)),
    `Carga por Parada (kg)`           = paste0(round(mean(carga_por_parada_kg, na.rm=T), 0), " / ", round(median(carga_por_parada_kg, na.rm=T), 0)),
    
    # Tempos em Minutos
    `Tempo de Trânsito (min)`         = paste0(round(mean(tempo_transito_min), 0), " / ", round(median(tempo_transito_min), 0), " min"),
    `Tempo de Espera PDV (min)`       = paste0(round(mean(tempo_espera_min), 0), " / ", round(median(tempo_espera_min), 0), " min"),
    `Tempo de Descarga PDV (min)`     = paste0(round(mean(tempo_descarga_min), 0), " / ", round(median(tempo_descarga_min), 0), " min"),
    `Horas Extras por Rota (min)`     = paste0(round(mean(hora_extra_min), 0), " / ", round(median(hora_extra_min), 0), " min"),
    .groups = "drop"
  ) %>%
  pivot_longer(-empresa, names_to = "Indicador Operacional", values_to = "Valor") %>%
  pivot_wider(names_from = empresa, values_from = Valor)

tabela_perfil_operacional %>%
  gt() %>%
  tab_header(
    title = md("**Perfil Operacional Comparativo (Métricas Absolutas)**"),
    subtitle = md("Valores mostrados no formato: **Média / Mediana** por rota realizada em 2024")
  ) %>%
  cols_label(
    `Indicador Operacional` = md("**Indicador / Variável**"),
    LogiPrime = md("**LogiPrime (SP/RJ)**"),
    RotaSul = md("**RotaSul (PR/SC/RS)**")
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#F0F4F8"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    table.width = pct(100),
    data_row.padding = px(6)
  )

# Gráficos de composição da frota, distância e entregas por rota
perfil_rotas_unicas <- dados_limpos %>%
  group_by(codigo_rota, empresa) %>%
  summarise(
    km_total_rota_medio       = first(km_total_rota_calc),
    entregas_por_rota_media   = first(num_entregas_rota_calc),
    peso_total_kg_medio       = first(peso_kg_total_calc),
    tipo_veiculo_predominante = first(tipo_veiculo),
    tempo_total_min_medio     = first(tempo_total_rota_min),
    hora_extra_min_media      = first(hora_extra_min),
    .groups = "drop"
  )

rotas_por_veiculo <- perfil_rotas_unicas %>%
  count(empresa, tipo_veiculo = tipo_veiculo_predominante) %>%
  group_by(empresa) %>%
  mutate(pct_do_total = (n / sum(n)) * 100) %>%
  ungroup()

g1 <- ggplot(rotas_por_veiculo, aes(x = empresa, y = pct_do_total, fill = tipo_veiculo)) +
  geom_col(width = 0.55, color = "black", alpha = 0.85) +
  geom_text(aes(label = paste0(tipo_veiculo, "\n", round(pct_do_total, 1), "%")),
            position = position_stack(vjust = 0.5), 
            size = 3.2, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("VUC" = "#E69F00", "TOCO" = "#D55E00", "TRUCK" = "#0072B2", "CARRETA" = "#009E73")) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25)) +
  labs(
    title = "Composição da Frota",
    subtitle = "Proporção de veículos (%)",
    x = NULL, y = "% do Total"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, color = "grey30"),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
  )

g2a <- ggplot(perfil_rotas_unicas, aes(x = empresa, y = km_total_rota_medio, fill = empresa)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.05, outlier.size = 0.4, width = 0.5) +
  scale_fill_manual(values = c("LogiPrime" = "#D55E00", "RotaSul" = "#0072B2")) +
  labs(
    title = "Distância / Rota",
    subtitle = "Quilometragem total (km)",
    x = NULL, y = "Km Total"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, color = "grey30"),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
  )

g2b <- ggplot(perfil_rotas_unicas, aes(x = empresa, y = entregas_por_rota_media, fill = empresa)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.05, outlier.size = 0.4, width = 0.5) +
  scale_fill_manual(values = c("LogiPrime" = "#D55E00", "RotaSul" = "#0072B2")) +
  labs(
    title = "Pulverização / Rota",
    subtitle = "Número de entregas",
    x = NULL, y = "Nº de Paradas"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, color = "grey30"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

grid.arrange(
  g1, g2a, g2b, 
  ncol = 3, 
  widths = c(1.1, 1, 1),
  top = textGrob("Perfil Operacional por Empresa: Frota, Distância e Pulverização", 
                 gp = gpar(fontsize = 13, fontface = "bold"))
)

# Função auxiliar de normalização
normalizar_relativo_max <- function(matriz_num) {
  # Identifica o valor máximo de cada coluna (variável)
  max_vals <- apply(matriz_num, 2, max, na.rm = TRUE)
  
  # Evita divisão por zero caso alguma coluna seja toda zerada
  max_vals[max_vals == 0] <- 1 
  
  # Divide cada valor pelo máximo da coluna e multiplica por 100
  matriz_norm <- as.data.frame(
    sweep(matriz_num, 2, max_vals, FUN = "/") * 100
  )
  
  return(matriz_norm)
}
colors_border <- c("#D55E00", "#0072B2")
colors_in     <- c(rgb(0.83, 0.36, 0.0, 0.35), rgb(0.0, 0.44, 0.69, 0.35))

# ==============================================================================
# 1. GRÁFICO RADAR: CARGA, OCUPAÇÃO, KM E PARADAS (MACRO)
# ==============================================================================
perfil_rotas_macro <- dados_limpos %>%
  group_by(codigo_rota) %>%
  summarise(
    empresa           = first(empresa),
    uf                = first(uf),
    tipo_veiculo      = first(tipo_veiculo),
    km_total          = first(km_total_rota_calc),
    num_entregas      = first(num_entregas_rota_calc),
    peso_total_kg     = first(peso_kg_total_calc),
    volume_total_m3   = first(volume_m3_total_calc),
    ocupacao_kg       = first(ocup_kg_rota),
    ocupacao_vol      = first(ocup_vol_rota),
    tempo_total_min   = first(tempo_total_rota_min),
    jornada_plan_min  = first(jornada_planejada_min),
    hora_extra_min    = first(hora_extra_min),
    .groups           = "drop"
  )

dados_radar <- perfil_rotas_macro %>%
  group_by(empresa) %>%
  summarise(
    `Carga Peso` = mean(peso_total_kg, na.rm = TRUE),
    `Carga Vol`  = mean(volume_total_m3, na.rm = TRUE),
    `Ocup. Peso` = mean(ocupacao_kg * 100, na.rm = TRUE),
    `Ocup. Vol`  = mean(ocupacao_vol * 100, na.rm = TRUE),
    `Km Total`   = mean(km_total, na.rm = TRUE),
    `Nº Paradas` = mean(num_entregas, na.rm = TRUE)
  )

matriz_num <- dados_radar %>% select(-empresa)

# Normalização Relativa
matriz_norm <- normalizar_relativo_max(matriz_num)

df_fmsb <- rbind(rep(100, ncol(matriz_norm)), rep(0, ncol(matriz_norm)), matriz_norm)
rownames(df_fmsb) <- c("Max", "Min", as.character(dados_radar$empresa))

par(mar = c(3, 2, 3, 2))

radarchart(
  df_fmsb, 
  axistype = 1,
  seg = 4,
  pcol = colors_border, 
  pfcol = colors_in, 
  plwd = 2.5, 
  plty = 1,
  cglcol = "grey70", 
  cglty = 2, 
  axislw = 0.8, 
  cglwd = 0.8,
  vlcex = 0.85,
  caxislabels = c("0%", "25%", "50%", "75%", "100%"),
  axistoptag = TRUE,
  calcex = 0.75,
  title = "Perfil Comparativo de Carga, Ocupação, Km e Paradas"
)

legend(
  x = "topright", 
  inset = c(0.08, 0.05),
  legend = c("LogiPrime", "RotaSul"), 
  bty = "n", 
  pch = 20, 
  col = colors_border, 
  text.col = "black", 
  cex = 0.9, 
  pt.cex = 1.8
)


# ==============================================================================
# 2. GRÁFICO RADAR: TRECHO, PDV E ENTREGA (MICRO)
# ==============================================================================
dados_radar_micro <- dados_limpos %>%
  group_by(empresa) %>%
  summarise(
    `Dist. Trecho (km)`   = mean(km_trecho, na.rm = TRUE),
    `Tempo Trecho (min)` = mean(tempo_trecho_min, na.rm = TRUE),
    `Espera PDV (min)`   = mean(tempo_espera_pdv_min, na.rm = TRUE),
    `Descarga PDV (min)` = mean(tempo_descarga_pdv_min, na.rm = TRUE),
    `Entrega (kg)`       = mean(peso_kg_entrega, na.rm = TRUE),
    `Entrega (m³)`       = mean(volume_m3_entrega, na.rm = TRUE)
  )

matriz_micro_num <- dados_radar_micro %>% select(-empresa)

# Normalização Relativa
matriz_micro_norm <- normalizar_relativo_max(matriz_micro_num)

df_fmsb_micro <- rbind(
  rep(100, ncol(matriz_micro_norm)), 
  rep(0, ncol(matriz_micro_norm)), 
  matriz_micro_norm
)
rownames(df_fmsb_micro) <- c("Max", "Min", as.character(dados_radar_micro$empresa))

par(mar = c(3, 2, 3, 2))

radarchart(
  df_fmsb_micro, 
  axistype = 1,
  seg = 4,
  pcol = colors_border, 
  pfcol = colors_in, 
  plwd = 2.5, 
  plty = 1,
  cglcol = "grey70", 
  cglty = 2, 
  axislw = 0.8, 
  cglwd = 0.8,
  vlcex = 0.85,
  caxislabels = c("0%", "25%", "50%", "75%", "100%"),
  axistoptag = TRUE,
  calcex = 0.75,
  title = "Perfil Comparativo de Trecho, PDV e Entrega"
)

legend(
  x = "topright", 
  inset = c(0.08, 0.05),
  legend = c("LogiPrime", "RotaSul"), 
  bty = "n", 
  pch = 20, 
  col = colors_border, 
  text.col = "black", 
  cex = 0.9, 
  pt.cex = 1.8
)


# ==============================================================================
# PAINEL GGPLOT (TEMPOS, JORNADA E COBERTURA GEOGRÁFICA)
# ==============================================================================
rotas_uf_individuais <- perfil_rotas_macro %>%
  count(empresa, uf) %>%
  group_by(empresa) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

rotas_uf_combinada <- perfil_rotas_macro %>%
  count(uf) %>%
  mutate(
    empresa = "Empresa Combinada",
    pct = n / sum(n) * 100
  ) %>%
  select(empresa, uf, n, pct)

rotas_uf_completo <- bind_rows(rotas_uf_individuais, rotas_uf_combinada) %>%
  mutate(empresa = factor(empresa, levels = c("LogiPrime", "RotaSul", "Empresa Combinada")))

g_tempo <- ggplot(perfil_rotas_macro, aes(x = empresa, y = tempo_total_min / 60, fill = empresa)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.05, outlier.size = 0.4, width = 0.4) +
  scale_fill_manual(values = c("LogiPrime" = "#D55E00", "RotaSul" = "#0072B2")) +
  labs(title = "Tempo Total de Rota", x = NULL, y = "Horas") +
  theme_minimal(base_size = 11) + 
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 10))

he_summary <- perfil_rotas_macro %>%
  group_by(empresa) %>%
  summarise(pct_he = mean(hora_extra_min > 0, na.rm = TRUE) * 100)

g_he <- ggplot(he_summary, aes(x = empresa, y = pct_he, fill = empresa)) +
  geom_col(width = 0.4, alpha = 0.85, color = "black") +
  geom_text(aes(label = paste0(round(pct_he, 1), "%")), vjust = -0.5, fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("LogiPrime" = "#D55E00", "RotaSul" = "#0072B2")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "% Rotas com Hora Extra", x = NULL, y = "% das Rotas") +
  theme_minimal(base_size = 11) + 
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 10))

g_uf <- ggplot(rotas_uf_completo, aes(x = empresa, y = pct, fill = uf)) +
  geom_col(width = 0.5, color = "black", alpha = 0.85) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribuição Geográfica (UF)", 
    x = NULL, 
    y = "% do Total de Rotas", 
    fill = "UF"
  ) +
  theme_minimal(base_size = 11) + 
  theme(
    plot.title = element_text(face = "bold", size = 10),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

grid.arrange(
  g_tempo, g_he, g_uf, ncol = 3,
  top = grid::textGrob(
    "Diagnóstico Comparativo de Tempos, Jornada e Cobertura Geográfica", 
    gp = grid::gpar(fontsize = 12, fontface = "bold")
  )
)


# ==============================================================================
# 3. GRÁFICO RADAR: FROTA, VELOCIDADE E EFICIÊNCIA OPERACIONAL
# ==============================================================================
radar_eficiencia <- perfil_rotas_macro %>%
  group_by(empresa) %>%
  summarise(
    `Velocidade (km/h)`    = mean((km_total / (tempo_total_min / 60)), na.rm = TRUE),
    `Minutos Extra/Estouro`= mean(hora_extra_min[hora_extra_min > 0], na.rm = TRUE),
    `Mix Frota Pesada %`  = mean(toupper(tipo_veiculo) %in% c("TRUCK", "CARRETA"), na.rm = TRUE) * 100,
    `Carga/Parada (kg)`    = mean(peso_total_kg / num_entregas, na.rm = TRUE),
    `Produtividade (m³/h)` = mean(volume_total_m3 / (tempo_total_min / 60), na.rm = TRUE)
  )

matriz_ef <- radar_eficiencia %>% select(-empresa)

# Normalização Relativa
matriz_ef_norm <- normalizar_relativo_max(matriz_ef)

df_radar_ef <- rbind(rep(100, ncol(matriz_ef_norm)), rep(0, ncol(matriz_ef_norm)), matriz_ef_norm)
rownames(df_radar_ef) <- c("Max", "Min", as.character(radar_eficiencia$empresa))

par(mar = c(3, 2, 3, 2))

radarchart(
  df_radar_ef, axistype = 1, seg = 4,
  pcol = colors_border, pfcol = colors_in, plwd = 2.5, plty = 1,
  cglcol = "grey70", cglty = 2, axislw = 0.8, cglwd = 0.8, vlcex = 0.85,
  caxislabels = c("0%", "25%", "50%", "75%", "100%"), axistoptag = TRUE, calcex = 0.75,
  title = "Perfil Comparativo de Frota, Velocidade e Eficiência Operacional"
)

legend(
  x = "topright", inset = c(0.08, 0.05), legend = c("LogiPrime", "RotaSul"),
  bty = "n", pch = 20, col = colors_border, pt.cex = 1.8, cex = 0.9
)

#####################################################################
# Economias potenciais na RotaSul com a aquisição pela LogiPrime

rotas_rotasul_otim <- dados_limpos %>%
  filter(empresa == "RotaSul") %>%
  group_by(codigo_rota) %>%
  summarise(
    km_original         = first(km_total_rota_calc),
    
    tempo_transito_orig = sum(tempo_trecho_min, na.rm = TRUE),
    tempo_espera_orig   = sum(tempo_espera_pdv_min, na.rm = TRUE),
    tempo_descarga_orig = sum(tempo_descarga_pdv_min, na.rm = TRUE),
    
    tempo_total_orig    = tempo_transito_orig + tempo_espera_orig + tempo_descarga_orig,
    
    # Reduções operacionais (-23% no trânsito e -35% na espera no PDV)
    tempo_transito_otim = tempo_transito_orig * 0.77,
    tempo_espera_otim   = tempo_espera_orig * 0.65,
    
    tempo_total_otim    = tempo_transito_otim + tempo_espera_otim + tempo_descarga_orig,
    
    # Cálculo Efetivo da Hora Extra (Excedente > 440 min/dia)
    he_orig_min         = pmax(0, tempo_total_orig - 440),
    he_otim_min         = pmax(0, tempo_total_otim - 440),
    
    .groups = "drop"
  )

km_total_rotasul <- sum(rotas_rotasul_otim$km_original, na.rm = TRUE)
km_reduzidos     <- km_total_rotasul * 0.17

gasto_manutencao   <- km_total_rotasul * 1.31
gasto_combustivel  <- km_total_rotasul * 4.07

saving_manutencao  <- km_reduzidos * 1.31
saving_combustivel <- km_reduzidos * 4.07

gasto_he_atual    <- sum(rotas_rotasul_otim$he_orig_min / 60 * 58, na.rm = TRUE)
saving_hora_extra <- sum((rotas_rotasul_otim$he_orig_min - rotas_rotasul_otim$he_otim_min) / 60 * 58, na.rm = TRUE)

gasto_total_opex  <- gasto_manutencao + gasto_combustivel + gasto_he_atual
saving_total_opex <- saving_manutencao + saving_combustivel + saving_hora_extra

df_barras <- data.frame(
  Categoria = factor(
    c("Manutenção", "Combustível", "Hora Extra", "TOTAL OPEX"),
    levels = c("Manutenção", "Combustível", "Hora Extra", "TOTAL OPEX")
  ),
  `Economia (Saving)` = c(saving_manutencao, saving_combustivel, saving_hora_extra, saving_total_opex),
  `Custo Residual`    = c(gasto_manutencao - saving_manutencao, 
                          gasto_combustivel - saving_combustivel, 
                          gasto_he_atual - saving_hora_extra, 
                          gasto_total_opex - saving_total_opex),
  Gasto_Total         = c(gasto_manutencao, gasto_combustivel, gasto_he_atual, gasto_total_opex),
  check.names = FALSE
)

df_plot <- df_barras %>%
  pivot_longer(
    cols = c("Economia (Saving)", "Custo Residual"),
    names_to = "Tipo",
    values_to = "Valor_RS"
  )

df_totais <- df_barras %>%
  mutate(
    label_total = paste0("Total: R$ ", format(round(Gasto_Total / 1e6, 2), nsmall = 2, decimal.mark = ","), " M\n(Saving: R$ ", format(round(`Economia (Saving)` / 1e6, 2), nsmall = 2, decimal.mark = ","), " M)")
  )

ggplot() +
  geom_bar(
    data = df_plot,
    aes(x = Categoria, y = Valor_RS, fill = Tipo),
    stat = "identity", width = 0.55
  ) +
  geom_text(
    data = df_totais,
    aes(x = Categoria, y = Gasto_Total, label = label_total),
    vjust = -0.3,
    color = "black", 
    fontface = "bold", 
    size = 3.6
  ) +
  scale_fill_manual(values = c("Economia (Saving)" = "#2E7D32", "Custo Residual" = "#424242")) +
  scale_y_continuous(
    labels = label_currency(prefix = "R$ ", big.mark = ".", decimal.mark = ","),
    limits = c(0, max(df_barras$Gasto_Total) * 1.18)
  ) +
  labs(
    title = "Oportunidade de Redução de OPEX - Operação RotaSul",
    subtitle = "Comparativo de Custo Total Atual vs. Economia (Saving) Projetada em R$",
    x = "Categoria de Custo",
    y = "Valor Total (R$)",
    fill = "Composição do Gasto"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )


#####################################################################
# Expansão de capacidade operacional da RotaSul

rotas_diarias <- dados_limpos %>%
  filter(empresa == "RotaSul") %>%
  group_by(codigo_rota) %>%
  summarise(
    id_veiculo_linha    = first(str_extract(codigo_rota, "[A-Z]-[A-Z]{3}-[0-9]+$")),
    entregas_orig       = n_distinct(pdv_id, na.rm = TRUE),
    tempo_transito_orig = sum(tempo_trecho_min, na.rm = TRUE),
    tempo_espera_orig   = sum(tempo_espera_pdv_min, na.rm = TRUE),
    tempo_descarga_orig = sum(tempo_descarga_pdv_min, na.rm = TRUE),
    tempo_total_orig    = tempo_transito_orig + tempo_espera_orig + tempo_descarga_orig,
    .groups = "drop"
  )

expansao_rotasul <- rotas_diarias %>%
  group_by(id_veiculo_linha) %>%
  summarise(
    entregas_orig       = mean(entregas_orig, na.rm = TRUE),
    tempo_transito_orig = mean(tempo_transito_orig, na.rm = TRUE),
    tempo_espera_orig   = mean(tempo_espera_orig, na.rm = TRUE),
    tempo_descarga_orig = mean(tempo_descarga_orig, na.rm = TRUE),
    tempo_total_orig    = tempo_transito_orig + tempo_espera_orig + tempo_descarga_orig,
    
    # Otimização (-23% trânsito, -35% espera)
    tempo_transito_otim = tempo_transito_orig * 0.77,
    tempo_espera_otim   = tempo_espera_orig * 0.65,
    tempo_total_otim    = tempo_transito_otim + tempo_espera_otim + tempo_descarga_orig,
    
    # Economia
    minutos_salvos      = tempo_total_orig - tempo_total_otim,
    horas_salvas        = minutos_salvos / 60,
    pct_reducao_jornada = (minutos_salvos / tempo_total_orig) * 100,
    
    # Produtividade (Entregas por Hora de Jornada)
    prod_hora_atual     = entregas_orig / (tempo_total_orig / 60),
    prod_hora_otim      = entregas_orig / (tempo_total_otim / 60),
    ganho_prod_pct      = ((prod_hora_otim - prod_hora_atual) / prod_hora_atual) * 100,
    
    .groups = "drop"
  )

# ==============================================================================
# GRÁFICO 1: REDUÇÃO DO TEMPO DE JORNADA E ECONOMIA EM HORAS
# ==============================================================================
df_plot_jornada <- expansao_rotasul %>%
  arrange(desc(minutos_salvos)) %>%
  head(10) %>%
  select(id_veiculo_linha, tempo_total_otim, minutos_salvos) %>%
  pivot_longer(
    cols = c("tempo_total_otim", "minutos_salvos"),
    names_to = "Componente",
    values_to = "Minutos"
  ) %>%
  mutate(
    Componente = factor(
      Componente, 
      levels = c("minutos_salvos", "tempo_total_otim"),
      labels = c("Tempo Economizado", "Nova Jornada Otimizada")
    )
  )

ggplot(df_plot_jornada, aes(x = reorder(id_veiculo_linha, Minutos, sum), y = Minutos / 60, fill = Componente)) +
  geom_col(width = 0.65, alpha = 0.9) +
  coord_flip() +
  scale_fill_manual(values = c("Tempo Economizado" = "#2E7D32", "Nova Jornada Otimizada" = "#1565C0")) +
  labs(
    title = "Impacto da Otimização na Jornada de Trabalho - RotaSul",
    subtitle = "Os 10 veículos com maiores reduções em jornadas (-23% trânsito, -35% espera)",
    x = "Veículo / Linha",
    y = "Duração da Jornada (Horas)",
    fill = "Composição do Tempo"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

# ==============================================================================
# GRÁFICO: CAMINHÕES COM AS MENORES JORNADAS OTIMIZADAS (TOP 15 MAIS RÁPIDOS)
# ==============================================================================
df_plot_menores_jornadas <- expansao_rotasul %>%
  arrange(tempo_total_otim) %>% # Ordena de forma crescente pela nova jornada
  head(10) %>%                  # Seleciona os 10 caminhões com menores jornadas
  select(id_veiculo_linha, tempo_total_otim, minutos_salvos) %>%
  pivot_longer(
    cols = c("tempo_total_otim", "minutos_salvos"),
    names_to = "Componente",
    values_to = "Minutos"
  ) %>%
  mutate(
    Componente = factor(
      Componente, 
      levels = c("minutos_salvos", "tempo_total_otim"),
      labels = c("Tempo Economizado", "Nova Jornada Otimizada")
    )
  )

ggplot(df_plot_menores_jornadas, aes(x = reorder(id_veiculo_linha, -Minutos, sum), y = Minutos / 60, fill = Componente)) +
  geom_col(width = 0.65, alpha = 0.9) +
  coord_flip() +
  scale_fill_manual(values = c("Tempo Economizado" = "#2E7D32", "Nova Jornada Otimizada" = "#1565C0")) +
  labs(
    title = "Caminhões com Menores Jornadas de Trabalho Otimizadas - RotaSul",
    subtitle = "Os 10 veículos com as rotas mais curtas e rápidas após otimizações",
    x = "Veículo / Linha",
    y = "Duração da Jornada (Horas)",
    fill = "Composição do Tempo"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )

# ==============================================================================
# GRÁFICO 2: GANHO DE PRODUTIVIDADE OPERACIONAL (% DE AUMENTO DE CAPACIDADE)
# ==============================================================================
df_plot_prod <- expansao_rotasul %>%
  arrange(desc(ganho_prod_pct)) %>%
  head(10)

ggplot(df_plot_prod, aes(x = reorder(id_veiculo_linha, ganho_prod_pct), y = ganho_prod_pct)) +
  geom_col(fill = "#2E7D32", width = 0.65) +
  geom_text(
    aes(label = paste0("+", round(ganho_prod_pct, 1), "%")), 
    hjust = -0.15, size = 3.5, fontface = "bold", color = "#1B5E20"
  ) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Ganho de Produtividade Operacional por Veículo - RotaSul",
    subtitle = "Aumento percentual na taxa de entregas por hora trabalhada",
    x = "Veículo / Linha",
    y = "Aumento de Produtividade (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )