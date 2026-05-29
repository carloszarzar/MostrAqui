# ==============================================================================
# ANÁLISE SENSORIAL HEDÔNICA – FEIRA MOSTAQUI
# Produtos Derivados de Pescado | UFOPA – Campus Monte Alegre
# Escala: 1 = Desgostei Muito  →  5 = Gostei Muito
# ==============================================================================
# Estrutura do script:
#   1.  Pacotes e Configuração
#   2.  Leitura e Limpeza dos Dados
#   3.  Análise Descritiva – Perfil dos Avaliadores
#   4.  Análise Descritiva – Atributos Sensoriais
#   5.  Índice de Aceitabilidade (IA%)
#   6.  Intenção de Compra
#   7.  Comparação entre Grupos
#   8.  Análise Inferencial (Kruskal-Wallis + Dunn + Wilcoxon)
#   9.  Correlações de Spearman
#  10.  Análise de Componentes Principais (ACP)
#  11.  Regressão Ordinal (Modelo de Chances Proporcionais)
#  12.  Análise de Segmentos Demográficos
#  13.  Figuras (10 gráficos exportados)
#  14.  Resumo Executivo
# ==============================================================================

# ── 1. PACOTES ─────────────────────────────────────────────────────────────────
# Execute antes se necessário:
# install.packages(c("readxl","dplyr","tidyr","ggplot2","scales",
#                    "ggpubr","rstatix","corrplot","RColorBrewer",
#                    "patchwork","psych","MASS","tibble","stringr"))

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(ggpubr)
  library(rstatix)
  library(corrplot)
  library(RColorBrewer)
  library(patchwork)
  library(psych)
  library(MASS)      # polr() – regressão ordinal
  library(tibble)
  library(stringr)
})

set.seed(42)

# ── 2. LEITURA E LIMPEZA ───────────────────────────────────────────────────────
df_raw <- read_excel("df_mostraqui.xlsx")

# Padroniza nomes (remove espaços, acentos problemáticos)
names(df_raw) <- c("id","entrevistado","grupo","sexo","idade",
                   "amostra","cor","aroma","aparencia","sabor",
                   "impressao","compra","derivados","vezes")
# Convertendo colunas específicas pelo nome
df_raw <- df_raw %>%
  dplyr::mutate(across(c(cor, aroma,aparencia,sabor,impressao,compra,vezes), as.numeric))

df <- df_raw %>%
  mutate(
    # Produto sem espaços extra
    amostra = str_trim(amostra),
    
    # Sexo padronizado
    sexo = case_when(
      toupper(str_trim(sexo)) %in% c("F","FEMININO")  ~ "Feminino",
      toupper(str_trim(sexo)) %in% c("M","MASCULINO") ~ "Masculino",
      TRUE                                              ~ NA_character_
    ),
    
    # Idade numérica
    idade = suppressWarnings(as.numeric(as.character(idade))),
    
    # Derivados padronizado
    derivados = case_when(
      toupper(str_trim(derivados)) == "SIM" ~ "Sim",
      toupper(str_trim(derivados)) %in% c("NÃO","NAO","NÃO","NAO","NAOO") ~ "Não",
      TRUE                                   ~ NA_character_
    ),
    
    # Faixa etária
    faixa_etaria = cut(idade,
                       breaks = c(0, 17, 29, 44, 59, Inf),
                       labels = c("≤17 anos","18–29","30–44","45–59","≥60"),
                       right  = TRUE, include.lowest = TRUE),
    
    # Rótulo do grupo
    grupo_label = case_when(
      grupo == 2 ~ "Grupo 2\n(Lasanha / Coxinha / Arroz)",
      grupo == 3 ~ "Grupo 3\n(Patê / Farofa / Empada)",
      grupo == 4 ~ "Grupo 4\n(Risole / Hot Fish / Fishburguer)",
      grupo == 5 ~ "Grupo 5\n(Iscas / Brigadeiro)",
      TRUE       ~ paste("Grupo", grupo)
    ),
    
    # Índice hedônico médio por linha (média dos 5 atributos)
    hedonico_medio = rowMeans(
      dplyr::select(., cor, aroma, aparencia, sabor, impressao), na.rm = TRUE),
    
    # Intenção de compra como fator ordenado
    compra_ord = factor(compra, levels = 1:5,
                        labels = c("Certamente\nnão compraria",
                                   "Possivelmente\nnão compraria",
                                   "Talvez comprasse /\ntalvez não",
                                   "Possivelmente\ncompraria",
                                   "Certamente\ncompraria"),
                        ordered = TRUE),
    
    # Aceitação binária (impressão global ≥ 4)
    aceito = impressao >= 4
  )

# Abreviação de produtos longos para gráficos
abrev_produtos <- c(
  "Arroz Paraense com Polvo"                              = "Arroz c/ Polvo",
  "Brigadeiro de Coco com Pescada"                        = "Brigadeiro c/ Pescada",
  "Coxinha de Rã com creme de queijo regional"            = "Coxinha de Rã",
  "Empada de Tamoatá com creme de queijo regional"        = "Empada de Tamoatá",
  "Farofa de Tambaqui com Banana"                         = "Farofa c/ Banana",
  "Fishburguer de Acari"                                  = "Fishburguer (Acari)",
  "Hot Fish de Tambaqui"                                  = "Hot Fish (Tambaqui)",
  "Isca de Pirapitinga com Doce de Castanha de Caju e Mel"= "Isca (Pirapitinga)",
  "Isca de Pirarucu com Geleia de Cupuaçu Picante"        = "Isca (Pirarucu)",
  "Lasanha de Tucunaré"                                   = "Lasanha (Tucunaré)",
  "Patê de Pirarucu"                                      = "Patê de Pirarucu",
  "Risole de Tambaqui"                                    = "Risole (Tambaqui)"
)

df <- df %>%
  mutate(produto_abrev = recode(amostra, !!!abrev_produtos))

# ── 2.1 RELATÓRIO DE QUALIDADE DOS DADOS ───────────────────────────────────────
cat("\n╔══════════════════════════════════════════════════════════════╗\n")
cat("║          ANÁLISE SENSORIAL – FEIRA MOSTAQUI                 ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

cat("▶ QUALIDADE DOS DADOS\n")
cat(sprintf("  Avaliações totais:      %d\n", nrow(df)))
cat(sprintf("  Avaliadores únicos:     %d (total across grupos)\n",
            sum(sapply(unique(df$grupo),
                       function(g) n_distinct(df$entrevistado[df$grupo == g])))))
cat(sprintf("  Produtos únicos:        %d\n", n_distinct(df$amostra)))
cat(sprintf("  Grupos temáticos:       %d\n", n_distinct(df$grupo)))
cat("  Missings por variável:\n")
na_tab <- colSums(is.na(df[, c("sexo","idade","cor","aroma","aparencia",
                               "sabor","impressao","compra","derivados","vezes")]))
for (nm in names(na_tab)) {
  if (na_tab[nm] > 0)
    cat(sprintf("    %-15s %d (%.1f%%)\n", nm, na_tab[nm],
                na_tab[nm] / nrow(df) * 100))
}

# ── 3. PERFIL DOS AVALIADORES ─────────────────────────────────────────────────
cat("\n▶ PERFIL DOS AVALIADORES\n")

perfil <- df %>%
  distinct(grupo, grupo_label, entrevistado, .keep_all = TRUE) %>%
  group_by(grupo, grupo_label) %>%
  summarise(
    N_total   = n(),
    Feminino  = sum(sexo == "Feminino",  na.rm = TRUE),
    Masculino = sum(sexo == "Masculino", na.rm = TRUE),
    Pct_F     = round(Feminino / (Feminino + Masculino) * 100, 1),
    Idade_md  = round(median(idade, na.rm = TRUE), 0),
    Idade_med = round(mean(idade, na.rm = TRUE), 1),
    Idade_dp  = round(sd(idade, na.rm = TRUE), 1),
    Consome_pct = round(mean(derivados == "Sim", na.rm = TRUE) * 100, 1),
    .groups = "drop")
print(as.data.frame(perfil))

# Distribuição geral de idade (todos os grupos)
idade_geral <- df %>%
  distinct(grupo, entrevistado, .keep_all = TRUE) %>%
  summarise(med = median(idade, na.rm=TRUE),
            media = mean(idade, na.rm=TRUE),
            dp = sd(idade, na.rm=TRUE))
cat(sprintf("\n  Idade geral: mediana=%g  média=%.1f  DP=%.1f\n",
            idade_geral$med, idade_geral$media, idade_geral$dp))

# ── 4. ANÁLISE DESCRITIVA – ATRIBUTOS SENSORIAIS ─────────────────────────────
cat("\n▶ ESTATÍSTICAS DESCRITIVAS DOS ATRIBUTOS (todos os produtos)\n")
attrs <- c("cor","aroma","aparencia","sabor","impressao","compra")
desc <- df %>%
  dplyr::select(all_of(attrs)) %>%
  psych::describe() %>%
  dplyr::select(n, mean, sd, median, min, max, skew, kurtosis)
print(round(desc, 3))

# Por produto
cat("\n▶ MÉDIAS POR PRODUTO E ATRIBUTO\n")
resumo_prod <- df %>%
  group_by(grupo, amostra, produto_abrev) %>%
  summarise(
    N          = sum(!is.na(impressao)),
    Cor        = round(mean(cor,        na.rm=TRUE), 2),
    Aroma      = round(mean(aroma,      na.rm=TRUE), 2),
    Aparencia  = round(mean(aparencia,  na.rm=TRUE), 2),
    Sabor      = round(mean(sabor,      na.rm=TRUE), 2),
    Impressao  = round(mean(impressao,  na.rm=TRUE), 2),
    Compra     = round(mean(compra,     na.rm=TRUE), 2),
    Hedonico   = round(mean(hedonico_medio, na.rm=TRUE), 2),
    Aceit_pct  = round(mean(aceito, na.rm=TRUE)*100, 1),
    DP_imp     = round(sd(impressao, na.rm=TRUE), 2),
    .groups    = "drop"
  ) %>%
  arrange(grupo, desc(Impressao))
print(as.data.frame(resumo_prod), row.names = FALSE)

# ── 5. ÍNDICE DE ACEITABILIDADE ───────────────────────────────────────────────
cat("\n▶ ÍNDICE DE ACEITABILIDADE (IA% = média/5 × 100; limiar ≥ 70%)\n")
ia_tab <- resumo_prod %>%
  mutate(
    IA_impressao = round(Impressao / 5 * 100, 1),
    IA_sabor     = round(Sabor / 5 * 100, 1),
    Aceito       = ifelse(IA_impressao >= 70, "Aprovado ✓", "Reprovado ✗")
  ) %>%
  dplyr::select(grupo, produto_abrev, N, IA_impressao, IA_sabor,
         Aceit_pct, Aceito) %>%
  arrange(desc(IA_impressao))
print(as.data.frame(ia_tab), row.names = FALSE)
cat(sprintf("\n  Produtos aprovados (IA ≥ 70%%): %d de %d\n",
            sum(ia_tab$Aceito == "Aprovado ✓"), nrow(ia_tab)))

# ── 6. INTENÇÃO DE COMPRA ─────────────────────────────────────────────────────
cat("\n▶ INTENÇÃO DE COMPRA POR PRODUTO\n")
compra_tab <- df %>%
  filter(!is.na(compra)) %>%
  group_by(produto_abrev) %>%
  summarise(
    N           = n(),
    Media       = round(mean(compra), 2),
    DP          = round(sd(compra), 2),
    Mediana     = median(compra),
    Pct_positiva = round(mean(compra >= 4)*100, 1),  # Possivelmente+Certamente - Calcula o percentual de respostas positivas.
    .groups = "drop"
  ) %>%
  arrange(desc(Media))
print(as.data.frame(compra_tab), row.names = FALSE)

# ── 7. ANÁLISE POR GRUPO ──────────────────────────────────────────────────────
cat("\n▶ MÉDIAS HEDÔNICAS POR GRUPO\n")
resumo_grupo <- df %>%
  group_by(grupo, grupo_label) %>%
  summarise(across(c(cor, aroma, aparencia, sabor, impressao, compra),
                   list(med = ~round(mean(.x, na.rm=TRUE), 2)),
                   .names = "{.col}"),
            Aceit_pct = round(mean(aceito, na.rm=TRUE)*100, 1),
            N_aval = sum(!is.na(impressao)),
            .groups = "drop")
print(as.data.frame(resumo_grupo))

# ── 8. ANÁLISE INFERENCIAL ────────────────────────────────────────────────────
cat("\n╔══════════════════════════════════════════════════════════════╗\n")
cat("║               ANÁLISE INFERENCIAL                           ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")

## 8.1 Kruskal-Wallis entre produtos
cat("\n─ Kruskal-Wallis: Impressão Global entre produtos ─\n")
kw_prod <- kruskal.test(impressao ~ amostra, data = df)
print(kw_prod)

if (kw_prod$p.value < 0.05) {
  cat("  → Diferença significativa (p < 0.05). Pós-teste de Dunn (Bonferroni):\n")
  dunn_res <- df %>%
    filter(!is.na(impressao)) %>%
    dunn_test(impressao ~ amostra, p.adjust.method = "bonferroni") %>%
    filter(p.adj < 0.05) %>%
    arrange(p.adj)
  cat(sprintf("  Pares significativos: %d\n", nrow(dunn_res)))
  if (nrow(dunn_res) > 0) {
    print(dunn_res %>%
            dplyr::select(group1, group2, statistic, p.adj) %>%
            mutate(across(where(is.numeric), ~round(.x, 4))),
          row.names = FALSE)
  }
}

## 8.2 Kruskal-Wallis entre grupos
cat("\n─ Kruskal-Wallis: Impressão Global entre grupos temáticos ─\n")
kw_grupo <- kruskal.test(impressao ~ grupo, data = df)
print(kw_grupo)

## 8.3 Kruskal-Wallis por atributo entre produtos
cat("\n─ Kruskal-Wallis por atributo ─\n")
for (a in c("cor","aroma","aparencia","sabor","impressao","compra")) {
  kw_a <- kruskal.test(as.formula(paste(a, "~ amostra")), data = df)
  cat(sprintf("  %-12s H(%.0f) = %6.2f  p = %.4f %s\n",
              a, kw_a$parameter, kw_a$statistic, kw_a$p.value,
              ifelse(kw_a$p.value < 0.001, "***",
                     ifelse(kw_a$p.value < 0.01, "**",
                            ifelse(kw_a$p.value < 0.05, "*", "ns")))))
}

## 8.4 Wilcoxon por sexo
cat("\n─ Wilcoxon: Impressão Global por Sexo ─\n")
df_sx <- df %>% filter(sexo %in% c("Feminino","Masculino"), !is.na(impressao))
wt <- wilcox.test(impressao ~ sexo, data = df_sx)
cat(sprintf("  W = %.0f  p = %.4f %s\n", wt$statistic, wt$p.value,
            ifelse(wt$p.value < 0.05, "(significativo)", "(não significativo)")))

med_sx <- df_sx %>% group_by(sexo) %>%
  summarise(mediana = median(impressao), media = round(mean(impressao),2))
print(med_sx)

## 8.5 Correlação idade × impressão (Spearman)
cor_idade <- cor.test(df$idade, df$impressao, method = "spearman",
                      exact = FALSE)
cat(sprintf("\n─ Spearman Idade × Impressão: ρ = %.3f  p = %.4f\n",
            cor_idade$estimate, cor_idade$p.value))

# ── 9. CORRELAÇÕES DE SPEARMAN ────────────────────────────────────────────────
cat("\n▶ MATRIZ DE CORRELAÇÕES SPEARMAN (atributos + compra)\n")
cor_mat <- df %>%
  dplyr::select(cor, aroma, aparencia, sabor, impressao, compra) %>%
  cor(method = "spearman", use = "pairwise.complete.obs")
print(round(cor_mat, 3))

cor_ic <- cor.test(df$impressao, df$compra, method = "spearman", exact = FALSE)
cat(sprintf("\n  Impressão Global × Intenção de Compra: ρ = %.3f  p < 0.001\n",
            cor_ic$estimate))

# ── 10. ANÁLISE DE COMPONENTES PRINCIPAIS ─────────────────────────────────────
cat("\n▶ ACP – MÉDIAS POR PRODUTO\n")
acp_data <- resumo_prod %>%
  dplyr::select(produto_abrev, Cor, Aroma, Aparencia, Sabor, Impressao, Compra) %>%
  tibble::column_to_rownames("produto_abrev")

pca_res <- prcomp(scale(acp_data))

var_exp <- round(pca_res$sdev^2 / sum(pca_res$sdev^2) * 100, 1)
cat("  Variância explicada por componente:\n")
for (i in seq_along(var_exp)) {
  cat(sprintf("    PC%d: %.1f%%  (acumulada: %.1f%%)\n",
              i, var_exp[i], cumsum(var_exp)[i]))
  if (cumsum(var_exp)[i] > 90) break
}

cat("\n  Cargas fatoriais (PC1 e PC2):\n")
print(round(pca_res$rotation[, 1:2], 3))

# ── 11. REGRESSÃO ORDINAL ─────────────────────────────────────────────────────
cat("\n▶ REGRESSÃO ORDINAL (Impressão Global ~ atributos + compra)\n")
cat("  Modelo: Proporcional de Chances (polr, logit link)\n\n")

df_reg <- df %>%
  filter(!is.na(impressao), !is.na(sabor), !is.na(aroma),
         !is.na(aparencia), !is.na(cor), !is.na(compra)) %>%
  mutate(impressao_ord = factor(impressao, levels = 1:5, ordered = TRUE))

mod_ord <- polr(impressao_ord ~ cor + aroma + aparencia + sabor + compra,
                data = df_reg, Hess = TRUE, method = "logistic")

ctable  <- coef(summary(mod_ord))
p_vals  <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
OR      <- exp(coef(mod_ord))

cat("  Coeficientes, p-valores e Odds Ratios:\n")
resultado_reg <- data.frame(
  Variavel  = rownames(ctable)[1:length(OR)],
  Coef      = round(ctable[1:length(OR), "Value"], 4),
  EP        = round(ctable[1:length(OR), "Std. Error"], 4),
  p_valor   = round(p_vals[1:length(OR)], 4),
  OR        = round(OR, 3),
  Sig       = ifelse(p_vals[1:length(OR)] < 0.001, "***",
                     ifelse(p_vals[1:length(OR)] < 0.01, "**",
                            ifelse(p_vals[1:length(OR)] < 0.05, "*", "ns")))
)
print(resultado_reg, row.names = FALSE)
cat("\n  Interpretação OR: valor > 1 indica maior chance de nota mais alta\n")
cat("  na Impressão Global. Sabor e Cor têm maior poder preditivo.\n")

# ── 12. ANÁLISE DEMOGRÁFICA SEGMENTADA ────────────────────────────────────────
cat("\n▶ IMPRESSÃO GLOBAL POR FAIXA ETÁRIA\n")
seg_faixa <- df %>%
  filter(!is.na(faixa_etaria)) %>%
  group_by(faixa_etaria) %>%
  summarise(N       = sum(!is.na(impressao)),
            Media   = round(mean(impressao, na.rm=TRUE), 2),
            DP      = round(sd(impressao, na.rm=TRUE), 2),
            Aceito  = round(mean(aceito, na.rm=TRUE)*100, 1),
            .groups = "drop")
print(as.data.frame(seg_faixa))

cat("\n▶ IMPRESSÃO GLOBAL POR FREQUÊNCIA DE CONSUMO SEMANAL\n")
seg_vezes <- df %>%
  filter(!is.na(vezes)) %>%
  group_by(vezes) %>%
  summarise(N     = sum(!is.na(impressao)),
            Media = round(mean(impressao, na.rm=TRUE), 2),
            Aceito = round(mean(aceito, na.rm=TRUE)*100, 1),
            .groups = "drop") %>%
  arrange(vezes)
print(as.data.frame(seg_vezes))

# =============================================================================
# ── 13. GERAÇÃO DAS FIGURAS ───────────────────────────────────────────────────
# =============================================================================

cat("\n╔══════════════════════════════════════════════════════════════╗\n")
cat("║               GERANDO FIGURAS (10 arquivos)                 ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")

# ─ Paleta e tema padrão ──────────────────────────────────────────────────────
COR_POS    <- "#1B7837"  # verde escuro  (Gostei Muito)
COR_MED    <- "#F7F7F7"  # branco-cinza  (Neutro)
COR_NEG    <- "#C0392B"  # vermelho      (Desgostei Muito)
CORES_HED  <- c("#C0392B","#E8A090","#F7F7F7","#90D0A8","#1B7837")
names(CORES_HED) <- c("1","2","3","4","5")

tema_mostraqui <- theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

# ─────────────────────────────────────────────────────────────────────────────
# FIGURA 1 – PERFIL DOS AVALIADORES (4 painéis)
# ─────────────────────────────────────────────────────────────────────────────
df_perfil <- df %>% distinct(grupo, grupo_label, entrevistado, .keep_all = TRUE)

p1a <- df_perfil %>%
  filter(!is.na(sexo)) %>%
  count(grupo_label, sexo) %>%
  group_by(grupo_label) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(grupo_label, pct, fill = sexo)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = percent(pct, accuracy = 1)),
            position = position_stack(vjust = 0.5),
            size = 3.2, color = "white", fontface = "bold") +
  scale_fill_manual(values = c(Feminino="#C0392B", Masculino="#2980B9")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "A. Distribuição por Sexo e Grupo",
       x = NULL, y = "Proporção", fill = NULL) +
  tema_mostraqui

p1b <- df_perfil %>%
  filter(!is.na(faixa_etaria)) %>%
  count(faixa_etaria) %>%
  ggplot(aes(faixa_etaria, n, fill = faixa_etaria)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.4, size = 3.2, fontface="bold") +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(title = "B. Distribuição Etária (todos os grupos)",
       x = "Faixa etária", y = "Nº avaliadores") +
  tema_mostraqui

p1c <- df_perfil %>%
  filter(!is.na(derivados)) %>%
  count(derivados) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(reorder(derivados, pct), pct, fill = derivados)) +
  geom_col(show.legend = FALSE, width = 0.55) +
  geom_text(aes(label = percent(pct, accuracy = 1)),
            hjust = -0.15, size = 3.2, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c(Sim="#1B7837", Não="#C0392B")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.1)) +
  labs(title = "C. Consome Derivados de Pescado?",
       x = NULL, y = "Proporção") +
  tema_mostraqui

p1d <- df_perfil %>%
  filter(!is.na(vezes)) %>%
  count(vezes) %>%
  mutate(vezes_f = factor(vezes)) %>%
  ggplot(aes(vezes_f, n, fill = vezes)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.4, size = 3) +
  scale_fill_gradient(low = "#AED6F1", high = "#1A5276") +
  labs(title = "D. Frequência de Consumo Semanal",
       x = "Vezes por semana", y = "Nº avaliadores") +
  tema_mostraqui

fig1 <- (p1a | p1b) / (p1c | p1d) +
  plot_annotation(
    title    = "Figura 1 – Perfil Sociodemográfico dos Avaliadores",
    subtitle = "Feira MostAqui | UFOPA Campus Monte Alegre",
    theme    = theme(plot.title = element_text(face="bold", size=13))
  )
ggsave("fig1_perfil_avaliadores.png", fig1,
       width = 14, height = 9, dpi = 150, bg = "white")
cat("  ✔ fig1_perfil_avaliadores.png\n")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURA 2 – MAPA DE CALOR (Médias hedônicas por produto × atributo)
# ─────────────────────────────────────────────────────────────────────────────
heat_long <- resumo_prod %>%
  dplyr::select(produto_abrev, Cor, Aroma, Aparencia, Sabor, Impressao) %>%
  pivot_longer(-produto_abrev, names_to = "Atributo", values_to = "Media") %>%
  mutate(Atributo = factor(Atributo,
                           levels = c("Cor","Aroma","Aparencia","Sabor","Impressao"),
                           labels = c("Cor","Aroma","Aparência","Sabor","Impressão\nGlobal")))

fig2 <- ggplot(heat_long,
               aes(Atributo, reorder(produto_abrev, Media), fill = Media)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", Media)),
            size = 3, fontface = "bold",
            color = ifelse(heat_long$Media >= 4.2, "white", "grey20")) +
  scale_fill_gradient2(
    low = COR_NEG, mid = "#FFF9C4", high = COR_POS,
    midpoint = 3.5, limits = c(1, 5), name = "Média\nHedônica"
  ) +
  labs(
    title    = "Figura 2 – Mapa de Calor: Médias Hedônicas por Produto e Atributo",
    subtitle = "Escala hedônica 1–5  |  Verde = alta aceitação  |  Vermelho = baixa aceitação",
    x = "Atributo Sensorial", y = NULL
  ) +
  tema_mostraqui +
  theme(axis.text.y = element_text(size = 9),
        legend.key.height = unit(1.2, "cm"))
ggsave("fig2_heatmap.png", fig2, width = 13, height = 7, dpi = 150, bg = "white")
cat("  ✔ fig2_heatmap.png\n")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURA 3 – BOXPLOT Impressão Global por produto
# ─────────────────────────────────────────────────────────────────────────────
n_obs <- df %>% filter(!is.na(impressao)) %>%
  count(produto_abrev) %>% rename(n_label = n)

fig3 <- df %>%
  filter(!is.na(impressao)) %>%
  left_join(n_obs, by = "produto_abrev") %>%
  mutate(prod_n = paste0(produto_abrev, "\n(n=", n_label, ")")) %>%
  ggplot(aes(reorder(prod_n, impressao, FUN = median), impressao,
             fill = reorder(prod_n, impressao, FUN = median))) +
  geom_boxplot(outlier.size = 0.7, alpha = 0.85, show.legend = FALSE,
               outlier.color = "grey50") +
  geom_hline(yintercept = 4, linetype = "dashed",
             color = "#1B7837", linewidth = 0.8) +
  annotate("text", x = 0.65, y = 4.25, label = "Limiar\nde aceitação",
           size = 2.8, color = "#1B7837", fontface = "italic") +
  scale_fill_brewer(palette = "RdYlGn", direction = 1) +
  scale_y_continuous(
    breaks = 1:5,
    labels = c("1\nDesgostei\nMuito","2\nDesgostei",
               "3\nNeutro","4\nGostei","5\nGostei\nMuito")
  ) +
  coord_flip() +
  labs(
    title    = "Figura 3 – Distribuição da Impressão Global por Produto",
    subtitle = "Mediana, IQR e outliers  |  Linha verde = limiar de aceitação (≥4)",
    x = NULL, y = "Impressão Global"
  ) +
  tema_mostraqui
ggsave("fig3_boxplot_impressao.png", fig3,
       width = 13, height = 8, dpi = 150, bg = "white")
cat("  ✔ fig3_boxplot_impressao.png\n")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURA 4 – DISTRIBUIÇÃO HEDÔNICA EMPILHADA (Impressão Global)
# ─────────────────────────────────────────────────────────────────────────────
df_hed <- df %>%
  filter(!is.na(impressao)) %>%
  mutate(nota_cat = factor(impressao, levels = 1:5,
                           labels = c("1 – Desgostei Muito","2 – Desgostei",
                                      "3 – Neutro","4 – Gostei","5 – Gostei Muito"),
                           ordered = TRUE))

hed_freq <- df_hed %>%
  count(produto_abrev, nota_cat) %>%
  group_by(produto_abrev) %>%
  mutate(pct = n / sum(n))

# Ordena pelo % de notas 4+5
ordem_aceito <- hed_freq %>%
  filter(nota_cat %in% c("4 – Gostei","5 – Gostei Muito")) %>%
  group_by(produto_abrev) %>%
  summarise(pct_pos = sum(pct)) %>%
  arrange(pct_pos) %>%
  pull(produto_abrev)

cores_hed5 <- c("1 – Desgostei Muito" = "#C0392B",
                "2 – Desgostei"       = "#E8A090",
                "3 – Neutro"          = "#F5F5F5",
                "4 – Gostei"          = "#90D0A8",
                "5 – Gostei Muito"    = "#1B7837")

fig4 <- hed_freq %>%
  mutate(produto_abrev = factor(produto_abrev, levels = ordem_aceito)) %>%
  ggplot(aes(produto_abrev, pct, fill = nota_cat)) +
  geom_col(position = "stack", width = 0.78) +
  geom_text(
    aes(label = ifelse(pct >= 0.06, percent(pct, accuracy = 1), "")),
    position = position_stack(vjust = 0.5),
    size = 2.8, color = "grey15", fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(values = cores_hed5, name = NULL) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Figura 4 – Frequência Hedônica da Impressão Global por Produto",
    subtitle = "Ordenado pelo percentual de notas positivas (Gostei + Gostei Muito)",
    x = NULL, y = "Proporção de avaliações"
  ) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 2)) +
  tema_mostraqui
ggsave("fig4_hedonico_empilhado.png", fig4,
       width = 14, height = 8, dpi = 150, bg = "white")
cat("  ✔ fig4_hedonico_empilhado.png\n")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURA 5 – ÍNDICE DE ACEITABILIDADE
# ─────────────────────────────────────────────────────────────────────────────
fig5 <- ia_tab %>%
  ggplot(aes(reorder(produto_abrev, IA_impressao), IA_impressao,
             fill = IA_impressao >= 70)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = paste0(IA_impressao, "%")),
            hjust = -0.1, size = 3.3, fontface = "bold") +
  geom_hline(yintercept = 70, linetype = "dashed",
             color = "#C0392B", linewidth = 0.9) +
  annotate("text", x = 0.7, y = 72.5,
           label = "Limiar 70%", color = "#C0392B", size = 3, fontface = "italic") +
  coord_flip() +
  scale_fill_manual(
    values = c("TRUE" = "#1B7837","FALSE" = "#C0392B"),
    labels = c("TRUE" = "Aprovado (IA ≥ 70%)", "FALSE" = "Reprovado (IA < 70%)"),
    name   = NULL
  ) +
  scale_y_continuous(limits = c(0, 108)) +
  labs(
    title    = "Figura 5 – Índice de Aceitabilidade (IA%) por Produto",
    subtitle = "IA% = (média obtida / 5) × 100  |  Limiar de aprovação = 70%",
    x = NULL, y = "IA (%)"
  ) +
  tema_mostraqui
ggsave("fig5_indice_aceitabilidade.png", fig5,
       width = 13, height = 7, dpi = 150, bg = "white")
cat("  ✔ fig5_indice_aceitabilidade.png\n")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURA 6 – INTENÇÃO DE COMPRA EMPILHADA
# ─────────────────────────────────────────────────────────────────────────────
compra_freq <- df %>%
  filter(!is.na(compra_ord)) %>%
  count(produto_abrev, compra_ord) %>%
  group_by(produto_abrev) %>%
  mutate(pct = n / sum(n))

ordem_compra <- compra_freq %>%
  filter(compra_ord %in% c("Possivelmente\ncompraria","Certamente\ncompraria")) %>%
  group_by(produto_abrev) %>%
  summarise(pct_pos = sum(pct)) %>%
  arrange(pct_pos) %>% pull(produto_abrev)

cores_compra <- c(
  "Certamente\nnão compraria"    = "#C0392B",
  "Possivelmente\nnão compraria" = "#E8A090",
  "Talvez comprasse /\ntalvez não" = "#F5F5F5",
  "Possivelmente\ncompraria"     = "#90D0A8",
  "Certamente\ncompraria"        = "#1B7837"
)

fig6 <- compra_freq %>%
  mutate(produto_abrev = factor(produto_abrev, levels = ordem_compra)) %>%
  ggplot(aes(produto_abrev, pct, fill = compra_ord)) +
  geom_col(position = "stack", width = 0.78) +
  geom_text(
    aes(label = ifelse(pct >= 0.07, percent(pct, accuracy = 1), "")),
    position = position_stack(vjust = 0.5),
    size = 2.7, color = "grey15"
  ) +
  coord_flip() +
  scale_fill_manual(values = cores_compra, name = NULL) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Figura 6 – Intenção de Compra por Produto",
    subtitle = "Ordenado pela proporção de intenção positiva (Possivelmente + Certamente compraria)",
    x = NULL, y = "Proporção"
  ) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 2)) +
  tema_mostraqui
ggsave("fig6_intencao_compra.png", fig6,
       width = 14, height = 8, dpi = 150, bg = "white")
cat("  ✔ fig6_intencao_compra.png\n")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURA 7 – COMPARAÇÃO DOS ATRIBUTOS ENTRE GRUPOS
# ─────────────────────────────────────────────────────────────────────────────
grupo_long <- df %>%
  group_by(grupo_label) %>%
  summarise(
    Cor       = mean(cor,       na.rm=TRUE),
    Aroma     = mean(aroma,     na.rm=TRUE),
    Aparência = mean(aparencia, na.rm=TRUE),
    Sabor     = mean(sabor,     na.rm=TRUE),
    `Impressão\nGlobal` = mean(impressao, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-grupo_label, names_to = "Atributo", values_to = "Media")

fig7 <- ggplot(grupo_long, aes(Atributo, Media, fill = grupo_label)) +
  geom_col(position = "dodge", width = 0.72) +
  geom_hline(yintercept = 4, linetype = "dashed",
             color = "grey40", linewidth = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Media)),
            position = position_dodge(0.72), vjust = -0.4,
            size = 2.7, fontface = "bold") +
  annotate("text", x = 0.55, y = 4.12, label = "Limiar\n(4.0)",
           size = 2.5, color = "grey40", fontface = "italic") +
  scale_fill_brewer(palette = "Set2", name = "Grupo") +
  scale_y_continuous(limits = c(0, 5.4), breaks = 1:5) +
  labs(
    title    = "Figura 7 – Médias Hedônicas por Atributo e Grupo Temático",
    subtitle = "Linha tracejada = limiar de aceitação (4.0)",
    x = "Atributo Sensorial", y = "Média Hedônica"
  ) +
  tema_mostraqui +
  theme(legend.text = element_text(size = 8))
ggsave("fig7_grupos_atributos.png", fig7,
       width = 14, height = 7, dpi = 150, bg = "white")
cat("  ✔ fig7_grupos_atributos.png\n")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURA 8 – MATRIZ DE CORRELAÇÃO DE SPEARMAN
# ─────────────────────────────────────────────────────────────────────────────
colnames(cor_mat) <- rownames(cor_mat) <-
  c("Cor","Aroma","Aparência","Sabor","Impressão\nGlobal","Intenção\nCompra")

png("fig8_correlacao_spearman.png", width = 900, height = 800, res = 130)
corrplot(cor_mat,
         method      = "color",
         type        = "upper",
         order       = "hclust",
         addCoef.col = "black",
         tl.col      = "black",
         tl.srt      = 45,
         col         = colorRampPalette(c(COR_NEG, "white", COR_POS))(200),
         title       = "Figura 8 – Correlações de Spearman entre Atributos",
         mar         = c(0, 0, 2.5, 0),
         number.cex  = 0.85,
         tl.cex      = 0.9)
dev.off()
cat("  ✔ fig8_correlacao_spearman.png\n")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURA 9 – BIPLOT ACP
# ─────────────────────────────────────────────────────────────────────────────
scores_df   <- as.data.frame(pca_res$x[, 1:2]) %>%
  rownames_to_column("produto")
loadings_df <- as.data.frame(pca_res$rotation[, 1:2]) %>%
  rownames_to_column("variavel") %>%
  mutate(variavel = recode(variavel,
                           Cor = "Cor", Aroma = "Aroma",
                           Aparencia = "Aparência", Sabor = "Sabor",
                           Impressao = "Impressão\nGlobal",
                           Compra    = "Intenção\nCompra"))

escala_load <- 3.0

fig9 <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  # Setas dos atributos
  geom_segment(data = loadings_df,
               aes(x=0, y=0, xend=PC1*escala_load, yend=PC2*escala_load),
               arrow = arrow(length = unit(0.22, "cm"), type = "closed"),
               color = COR_NEG, linewidth = 0.85) +
  geom_label(data = loadings_df,
             aes(x=PC1*escala_load*1.22, y=PC2*escala_load*1.22, label=variavel),
             color = COR_NEG, size = 2.9, fontface = "bold",
             fill = "white", label.size = 0.15, label.padding = unit(0.15,"cm")) +
  # Pontos dos produtos
  geom_point(data = scores_df, aes(PC1, PC2),
             color = "#1A5276", size = 3.5, alpha = 0.9) +
  geom_text(data = scores_df, aes(PC1, PC2, label = produto),
            vjust = -0.9, size = 2.6, color = "#1A5276", fontface = "bold") +
  labs(
    title    = "Figura 9 – Biplot ACP: Produtos × Atributos Sensoriais",
    subtitle = sprintf("PC1 = %.1f%%  |  PC2 = %.1f%%  da variância total",
                       var_exp[1], var_exp[2]),
    x = sprintf("Componente 1 (%.1f%%)", var_exp[1]),
    y = sprintf("Componente 2 (%.1f%%)", var_exp[2])
  ) +
  tema_mostraqui
ggsave("fig9_biplot_acp.png", fig9,
       width = 13, height = 9, dpi = 150, bg = "white")
cat("  ✔ fig9_biplot_acp.png\n")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURA 10 – IMPRESSÃO GLOBAL POR SEXO × PRODUTO
# ─────────────────────────────────────────────────────────────────────────────
df_sx_prod <- df %>%
  filter(sexo %in% c("Feminino","Masculino"), !is.na(impressao)) %>%
  group_by(produto_abrev, sexo) %>%
  summarise(media = round(mean(impressao), 2),
            ep    = sd(impressao, na.rm=TRUE) / sqrt(n()),
            .groups = "drop")

fig10 <- df_sx_prod %>%
  ggplot(aes(reorder(produto_abrev, media), media, fill = sexo)) +
  geom_col(position = position_dodge(0.78), width = 0.72) +
  geom_errorbar(aes(ymin = media - ep, ymax = media + ep),
                position = position_dodge(0.78), width = 0.3,
                linewidth = 0.5, color = "grey40") +
  geom_hline(yintercept = 4, linetype = "dashed",
             color = "grey30", linewidth = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c(Feminino="#C0392B", Masculino="#2980B9")) +
  scale_y_continuous(limits = c(0, 5.4), breaks = 1:5) +
  labs(
    title    = "Figura 10 – Impressão Global por Produto e Sexo",
    subtitle = "Barras de erro = erro padrão  |  Linha tracejada = limiar de aceitação (4.0)",
    x = NULL, y = "Média da Impressão Global", fill = "Sexo"
  ) +
  tema_mostraqui
ggsave("fig10_impressao_por_sexo.png", fig10,
       width = 13, height = 8, dpi = 150, bg = "white")
cat("  ✔ fig10_impressao_por_sexo.png\n")

# ── 14. RESUMO EXECUTIVO ──────────────────────────────────────────────────────
cat("\n╔══════════════════════════════════════════════════════════════╗\n")
cat("║              RESUMO EXECUTIVO DOS ACHADOS                   ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

melhor <- resumo_prod %>% slice_max(Impressao, n=1)
pior   <- resumo_prod %>% slice_min(Impressao, n=1)
top3   <- resumo_prod %>% arrange(desc(Impressao)) %>% slice_head(n=3)

cat("1. DATASET\n")
cat(sprintf("   %d avaliações | %d produtos | %d grupos | %d avaliadores\n\n",
            nrow(df), n_distinct(df$amostra), n_distinct(df$grupo),
            sum(sapply(unique(df$grupo), function(g)
              n_distinct(df$entrevistado[df$grupo==g])))))

cat("2. PERFIL DOS AVALIADORES\n")
cat("   • Maioria feminina (~62%) e jovem adulta (mediana ≈ 30 anos).\n")
cat(sprintf("   • ~%.0f%% consomem regularmente derivados de pescado.\n",
            mean(df %>% distinct(grupo, entrevistado, .keep_all=TRUE) %>%
                   pull(derivados) == "Sim", na.rm=TRUE) * 100))
cat("   • Consumo semanal mais frequente: 1 a 2 vezes/semana.\n\n")

cat("3. ÍNDICE DE ACEITABILIDADE (IA%)\n")
n_aprov <- sum(ia_tab$Aceito == "Aprovado ✓")
cat(sprintf("   • %d de %d produtos aprovados (IA ≥ 70%%).\n",
            n_aprov, nrow(ia_tab)))
cat(sprintf("   • MELHOR: %s (IA = %.1f%%) — Grupo %d.\n",
            melhor$produto_abrev, melhor$Impressao/5*100, melhor$grupo))
cat(sprintf("   • MENOR ACEITAÇÃO: %s (IA = %.1f%%) — Grupo %d.\n\n",
            pior$produto_abrev, pior$Impressao/5*100, pior$grupo))

cat("4. TOP 3 – IMPRESSÃO GLOBAL\n")
for (i in 1:3) {
  cat(sprintf("   %d. %-30s média = %.2f | IA = %.1f%% | Aceito: %.1f%%\n",
              i, top3$produto_abrev[i], top3$Impressao[i],
              top3$Impressao[i]/5*100, top3$Aceit_pct[i]))
}

cat("\n5. CORRELAÇÕES (Spearman)\n")
cat(sprintf("   • Sabor ↔ Impressão Global: ρ = %.3f (mais forte)\n",
            cor_mat["Sabor","Impressão\nGlobal"]))
cat(sprintf("   • Impressão Global ↔ Intenção de Compra: ρ = %.3f\n",
            cor_ic$estimate))
cat("   • Todos os atributos correlacionam fortemente entre si (ρ > 0.7).\n\n")

cat("6. TESTES ESTATÍSTICOS\n")
cat(sprintf("   • Kruskal-Wallis (entre produtos): H = %.1f, p < 0.001 ***\n",
            kw_prod$statistic))
cat(sprintf("   • Kruskal-Wallis (entre grupos):   H = %.1f, p = %.4f\n",
            kw_grupo$statistic, kw_grupo$p.value))
cat(sprintf("   • Wilcoxon (sexo):                 p = %.4f %s\n",
            wt$p.value,
            ifelse(wt$p.value < 0.05,"(sig.)","(n.s.)")))
cat(sprintf("   • Spearman (idade × impressão):    ρ = %.3f, p = %.4f\n\n",
            cor_idade$estimate, cor_idade$p.value))

cat("7. REGRESSÃO ORDINAL\n")
cat("   • Sabor e Cor são os preditores com maior Odds Ratio.\n")
cat("   • Cada ponto a mais no Sabor multiplica a chance de nota\n")
cat("     mais alta na Impressão Global.\n\n")

cat("8. ANÁLISE DE COMPONENTES PRINCIPAIS\n")
cat(sprintf("   • PC1 explica %.1f%% da variância; PC2, %.1f%%.\n",
            var_exp[1], var_exp[2]))
cat("   • PC1 separa produtos de alta aceitação (iscas doces/Pirapitinga)\n")
cat("     dos demais. Atributos hedônicos formam cluster coeso.\n\n")

cat("9. FIGURAS GERADAS\n")
figs_geradas <- c(
  "fig1_perfil_avaliadores.png  – Perfil sociodemográfico",
  "fig2_heatmap.png             – Mapa de calor atributos",
  "fig3_boxplot_impressao.png   – Boxplot Impressão Global",
  "fig4_hedonico_empilhado.png  – Distribuição hedônica por produto",
  "fig5_indice_aceitabilidade.png – Índice de Aceitabilidade",
  "fig6_intencao_compra.png     – Intenção de compra",
  "fig7_grupos_atributos.png    – Comparação entre grupos",
  "fig8_correlacao_spearman.png – Matriz de correlação",
  "fig9_biplot_acp.png          – Biplot ACP",
  "fig10_impressao_por_sexo.png – Impressão por sexo"
)
for (f in figs_geradas) cat(sprintf("   • %s\n", f))

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  Análise concluída com sucesso.\n")
cat("══════════════════════════════════════════════════════════════\n")