# Carregando bibliotecas
library(ggplot2)
library(readxl)
library(dplyr)
library(kableExtra)
library(gt)
library(interactions)
library(scales)

# ================================
#        Leitura dos Dados
# ================================
dados <- read_excel("C:/Users/analu/OneDrive/Desktop/Prob/base.xlsx")

# ================================
# Função para resumo estatístico
# ================================
resumo_estatistico <- function(df, grupo, variavel) {
  df %>%
    group_by({{grupo}}) %>%
    summarise(
      Média = mean({{variavel}}, na.rm = TRUE),
      Mediana = median({{variavel}}, na.rm = TRUE),
      Desvio_Padrão = sd({{variavel}}, na.rm = TRUE),
      Min = min({{variavel}}, na.rm = TRUE),
      Max = max({{variavel}}, na.rm = TRUE),
      .groups = "drop"
    )
}

# ================================
# Questão 1 - Taxas de Mortalidade
# ================================
obitos_ano <- resumo_estatistico(dados, ANO, TX_OBITOS_100mil)
obitos_estado <- resumo_estatistico(dados, SG_UF, TX_OBITOS_100mil)

# Tabelas com kable
kable_styler <- function(tabela, caption) {
  tabela %>%
    kable(digits = 2, caption = caption) %>%
    kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
}

kable_styler(obitos_ano, "Resumo anual das taxas de mortalidade (por 100 mil mulheres)")
kable_styler(obitos_estado, "Resumo estadual das taxas de mortalidade (por 100 mil mulheres)")

# Tabelas salvas com GT
salvar_gt <- function(tabela, titulo, subtitulo, arquivo, colunas_fmt, col_label = NULL) {
  tabela %>%
    gt() %>%
    tab_header(title = titulo, subtitle = subtitulo) %>%
    fmt_number(columns = where(is.numeric), decimals = 2, dec_mark = ",", sep_mark = ".") %>%
    fmt_number(columns = ANO, decimals = 0, sep_mark = ".") %>%
    cols_label(.list = col_label) %>%
    gtsave(arquivo)
}

salvar_gt(obitos_ano,
          "Resumo anual das taxas de mortalidade por câncer de mama",
          "Região Sudeste (2005–2020)",
          "tabela_mortalidade_ano.png",
          c("Média", "Mediana", "Desvio_Padrão", "Min", "Max"),
          col_label = c(Desvio_Padrão = "Desvio Padrão", Min = "Mínimo", Max = "Máximo"))

salvar_gt(obitos_estado,
          "Resumo estadual das taxas de mortalidade por câncer de mama",
          "Região Sudeste (2005–2020)",
          "tabela_mortalidade_estado.png",
          c("Média", "Mediana", "Desvio_Padrão", "Min", "Max"),
          col_label = c(SG_UF = "Estado", Desvio_Padrão = "Desvio Padrão", Min = "Mínimo", Max = "Máximo"))

# Gráfico de linhas com barras de erro
ggplot(dados, aes(x = ANO, y = TX_OBITOS_100mil, color = SG_UF)) +
  geom_line(size = 1, alpha = 0.5) +
  geom_point(alpha = 0.6) +
  geom_errorbar(data = obitos_ano,
                aes(x = ANO, ymin = Média - Desvio_Padrão, ymax = Média + Desvio_Padrão),
                inherit.aes = FALSE, color = "black", width = 0.3) +
  geom_line(data = obitos_ano, aes(x = ANO, y = Média), color = "black", size = 1.2) +
  labs(title = "Evolução das Taxas de Mortalidade por Câncer de Mama (2005–2020)",
       subtitle = "Com médias e desvios padrão por ano",
       x = "Ano", y = "Taxa por 100 mil mulheres", color = "Estados") +
  theme_minimal()

# Boxplot por estado
ggplot(dados, aes(x = SG_UF, y = TX_OBITOS_100mil, fill = SG_UF)) +
  geom_boxplot() +
  labs(title = "Distribuição das Taxas de Mortalidade por Estado (2005–2020)",
       x = "Estado", y = "Taxa por 100 mil mulheres") +
  theme_minimal() +
  theme(legend.position = "none")

# ================================
# Questão 2 - Variação do SDI
# ================================
sdi_ano <- resumo_estatistico(dados, ANO, SDI)
sdi_estado <- resumo_estatistico(dados, SG_UF, SDI)

kable_styler(sdi_ano, "Resumo anual do índice de desenvolvimento sociodemográfico")
kable_styler(sdi_estado, "Resumo estadual do índice de desenvolvimento sociodemográfico")

salvar_gt(sdi_ano, "Resumo anual do índice de desenvolvimento sociodemográfico",
          "Região Sudeste (2005–2020)", "tabela_sdi_ano.png",
          c("Média", "Mediana", "Desvio_Padrão", "Min", "Max"))

salvar_gt(sdi_estado, "Resumo estadual do índice de desenvolvimento sociodemográfico",
          "Região Sudeste (2005–2020)", "tabela_sdi_estado.png",
          c("Média", "Mediana", "Desvio_Padrão", "Min", "Max"))

# Gráfico de SDI
ggplot(dados, aes(x = ANO, y = SDI, color = SG_UF)) +
  geom_line(size = 1, alpha = 0.5) +
  geom_point(alpha = 0.6) +
  labs(title = "Evolução do Índice de Desenvolvimento Sociodemográfico (2005–2020)",
       x = "Ano", y = "Taxa SDI", color = "Estados") +
  theme_minimal()

# ================================
# Questão 3 e 4 - Correlações
# ================================
cor_sdi <- cor.test(dados$SDI, dados$TX_OBITOS_100mil, method = "spearman")
cor_hosp <- cor.test(dados$QT_HOSPITAIS, dados$TX_OBITOS_100mil, method = "spearman")

# Correlação por ano (Hospitais)
correlacoes_anuais <- dados %>%
  group_by(ANO) %>%
  summarise(rho = cor(QT_HOSPITAIS, TX_OBITOS_100mil, method = "spearman"),
            .groups = "drop")

salvar_gt(correlacoes_anuais,
          "Resumo das Correlações entre Hospitais e Mortalidade",
          "Região Sudeste (2005–2020)", "tabela_correlacoes_estado.png",
          "rho", col_label = c(ANO = "Ano"))

# ================================
# Questão 5 - Mamografia, SDI, Mortalidade
# ================================
dados <- dados %>%
  mutate(
    SDI_quartil = cut(SDI, quantile(SDI, probs = seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE, labels = FALSE),
    Mamografia_quartil = cut(QT_MAMOGRAFIAS, quantile(QT_MAMOGRAFIAS, probs = seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE, labels = FALSE),
    grupo_combinado = paste0("SDI_", SDI_quartil, "_MAM_", Mamografia_quartil)
  )

# Testes estatísticos
cor_cob <- cor.test(dados$QT_MAMOGRAFIAS, dados$TX_OBITOS_100mil, method = "spearman")

# Mann-Whitney
grupo_baixo <- dados$TX_OBITOS_100mil[dados$Mamografia_quartil == 1]
grupo_alto  <- dados$TX_OBITOS_100mil[dados$Mamografia_quartil == 4]
teste_mw <- wilcox.test(grupo_baixo, grupo_alto)

# Resultados
cat(sprintf("Correlação SDI vs Mortalidade: rho = %.2f, p = %.4f\n", cor_sdi$estimate, cor_sdi$p.value))
cat(sprintf("Correlação Cobertura Mamografia vs Mortalidade: rho = %.2f, p = %.4f\n", cor_cob$estimate, cor_cob$p.value))
cat(sprintf("Teste Mann-Whitney (quartil 1 vs 4): p = %.4f\n", teste_mw$p.value))
if (teste_mw$p.value < 0.05) {
  cat("→ Diferença significativa nas taxas de mortalidade entre menor e maior cobertura.\n")
}

# Resumo por grupo combinado
resumo <- dados %>%
  group_by(grupo_combinado) %>%
  summarise(media_mortalidade = mean(TX_OBITOS_100mil, na.rm = TRUE), n = n()) %>%
  arrange(grupo_combinado)

# Visualização
ggplot(resumo, aes(x = reorder(grupo_combinado, media_mortalidade), y = media_mortalidade, fill = grupo_combinado)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Mortalidade Média por Grupo Combinado de SDI e Mamografia",
       x = "Grupo", y = "Taxa Média de Mortalidade") +
  theme_minimal()

# Modelo com interação
modelo <- lm(TX_OBITOS_100mil ~ SDI * QT_MAMOGRAFIAS, data = dados)
summary(modelo)

# Categorizando SDI
dados <- dados %>%
  mutate(
    SDI_cat = cut(SDI, breaks = quantile(SDI, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                  include.lowest = TRUE,
                  labels = c("Baixo SDI", "Médio SDI", "Alto SDI"))
  )

modelo2 <- lm(TX_OBITOS_100mil ~ SDI_cat * QT_MAMOGRAFIAS, data = dados)

interact_plot(modelo2,
              pred = QT_MAMOGRAFIAS,
              modx = SDI_cat,
              x.label = "Mamografias Realizadas",
              y.label = "Taxa de Mortalidade",
              main.title = "Efeito da Cobertura de Mamografia na Mortalidade por Nível de SDI",
              legend.main = "Nível de SDI",
              modx.labels = c("Baixo", "Intermediário", "Alto"),
              colors = c("#1b9e77", "#d95f02", "#7570b3")) +
  scale_x_continuous(labels = comma)
