
# Leitura dos Dados
library(readxl)
BaseDados <- read_excel("Desktop/Estatística/Base Dados IIE.xlsx")
BaseDados$PIB <- BaseDados$`PIB 2010`
BaseDados$GINI <- BaseDados$`Gini da Renda Domiciliar Per Capta 2010`
subconjunto <- BaseDados[BaseDados$`Municipios SP` != "São Paulo", ]

#### 1) TESTES DE NORMALIDADE ####

library(ggplot2)

## Histograma para o PIB:
ggplot(subconjunto, aes(x = PIB)) +
  geom_histogram(binwidth = 4000000, fill = "#FF69B4", color = "#2c3e50", alpha = 0.7, size = 0.2) +
  labs(title = "PIB dos municípios paulistas em 2010", x = "PIB 2010", y = "Frequência") +
  theme_minimal() + theme(axis.text = element_text(size = 10), axis.title = element_text(size = 12, 
  face = "bold"), plot.title = element_text(size = 12, face = "bold"), panel.grid.major = 
  element_line(color = "#ecf0f1"))

## Gráfico de Quantis (Q-Q plot) para o PIB:
ggplot(data.frame(QQ = subconjunto$PIB), aes(sample = QQ)) + stat_qq(color = "#FF69B4") +
  stat_qq_line(color = "#2c3e50") + labs(title = "Gráfico QQ para o PIB", x = "Quantis Teóricos",
  y = "Quantis Observados") + theme_minimal() + theme(plot.title = element_text(size = 14, face = "bold"),
  axis.title = element_text(size = 12, face = "bold"))

# Teste Normalidade de Shapiro-Wilk para o PIB:
shapiro_test_PIB <- shapiro.test(subconjunto$PIB)
# W = 0.30439, p-value < 2.2e-16
cat("Teste de Shapiro-Wilk:\n")
cat("H0: Os dados seguem uma distribuição normal\n")
cat("H1: Os dados não seguem uma distribuição normal\n")
cat("Resultado do teste de Shapiro-Wilk:\n")
cat("\nInterpretação:\n")
# Estabelecer 5% como nível de significância
if (shapiro_test_PIB$p.value < 0.05) 
{cat("Rejeita H0 no teste de Shapiro-Wilk. Conclusão: Os dados não seguem uma distribuição normal.\n")}
  else {cat("Não há evidências suficientes para rejeitar H0 no teste de Shapiro-Wilk. Conclusão: Os dados
  podem seguir uma distribuição normal.\n")}
# Resultado: Visto que o p-valor de 2.2e-16 é menor que 0.05, rejeita H0 no teste de Shapiro-Wilk. 
# Assim, conclui-se que o PIB dos municípios paulistas não seguem uma distribuição normal.


# Teste Normalidade de Shapiro-Wilk para o PIB:
shapiro_test_PIB <- shapiro.test(subconjunto$PIB)
# Estatística W = 0.30439, p-value = 1.003009e-43

# Definir as hipóteses
# H0: Os dados de PIB seguem uma distribuição normal"
# H1: Os dados de PIB não seguem uma distribuição normal"

# Conclusão com base no valor de p
nivel_de_significancia <- 0.05
if (shapiro_test_PIB$p.value < nivel_de_significancia) {
  cat("Rejeita H0 no teste de Shapiro-Wilk. Conclusão: Os dados não seguem uma distribuição normal.\n")} 
    else {cat("Não há evidências suficientes para rejeitar H0 no teste de Shapiro-Wilk. Conclusão: Os dados 
  podem seguir uma distribuição normal.\n")}
# Rejeita H0 no teste de Shapiro-Wilk. Conclusão: Os dados não seguem uma distribuição normal.





# Teste de Normalidade de Kolmogorov-Smirnov para o PIB:
ks_test_PIB <- ks.test(subconjunto$PIB, "pnorm")
# D = 1, p-value < 2.2e-16
# alternative hypothesis: two-sided
cat("\nTeste de Kolmogorov-Smirnov:\n")
cat("H0: Os dados seguem uma distribuição normal\n")
cat("H1: Os dados não seguem uma distribuição normal\n")
cat("Resultado do teste de Kolmogorov-Smirnov:\n")
cat("\nInterpretação:\n")
# Estabelecer 5% como nível de significância
if (ks_test_PIB$p.value < 0.05) {cat("Rejeita H0 no teste de Kolmogorov-Smirnov. Conclusão: Os dados não 
  seguem uma distribuição normal.\n")} else {cat("Não há evidências suficientes para rejeitar H0 no teste 
  de Kolmogorov-Smirnov. Conclusão: Os dados podem seguir uma distribuição normal.\n")}
# Resultado: Visto que o p-valor de 2.2e-16 é menor que 0.05, rejeita H0 no teste de Kolmogorov-Smirnov. 
# Assim, conclui-se que o PIB dos municípios paulistas não seguem uma distribuição normal.




## Histograma para o Índice de Gini:
ggplot(subconjunto, aes(x = GINI)) +
  geom_histogram(binwidth = 0.008, fill = "#2ecc71", color = "#2c3e50", alpha = 0.7, size = 0.2) +
  labs(title = "Índice de Gini per capita dos municípios paulistas em 2010", x = "Gini 2010",
  y = "Frequência") + theme_minimal() + theme(axis.text = element_text(size = 10), 
  axis.title = element_text(size = 12, face = "bold"), plot.title = element_text(size = 12, 
  face = "bold"), panel.grid.major = element_line(color = "#ecf0f1"))

## Gráfico de Quantis (Q-Q plot) para o Índice de Gini:
ggplot(data.frame(QQ = subconjunto$GINI), aes(sample = QQ)) + 
  stat_qq(color = "#2ecc71") + stat_qq_line(color = "#2c3e50") + labs(title = "Gráfico QQ para o 
  Índice de Gini", x = "Quantis Teóricos", y = "Quantis Observados") + theme_minimal() + 
  theme(plot.title = element_text(size = 14, face = "bold"), axis.title = element_text(size = 12, 
  face = "bold"))


## Teste Normalidade de Shapiro-Wilk para o Índice de Gini:
shapiro_test_Gini <- shapiro.test(subconjunto$GINI)
# W = 0.97778, p-value = 2.588e-08
cat("Teste de Shapiro-Wilk:\n")
cat("H0: Os dados seguem uma distribuição normal\n")
cat("H1: Os dados não seguem uma distribuição normal\n")
cat("Resultado do teste de Shapiro-Wilk:\n")
cat("\nInterpretação:\n")
# Estabelecer 5% como nível de significância
if (shapiro_test_Gini$p.value < 0.05) {cat("Rejeita H0 no teste de Shapiro-Wilk. Conclusão: Os dados não seguem 
  uma distribuição normal.\n")} else {cat("Não há evidências suficientes para rejeitar H0 no teste de Shapiro-Wilk. 
  Conclusão: Os dados podem seguir uma distribuição normal.\n")}
# Resultado: Visto que o p-valor de 1.43e-08 é menor que 0.05, rejeita H0 no teste de Shapiro-Wilk. 
# Assim, conclui-se que o Índice de Gini dos municípios paulistas não seguem uma distribuição normal.


## Teste Normalidade de Shapiro-Wilk para o Índice de Gini:
shapiro_test_Gini <- shapiro.test(subconjunto$GINI)
# Estatística W = 0.97778, p-value = 2.58769e-08

# Definir as hipóteses
# H0: Os dados de PIB seguem uma distribuição normal"
# H1: Os dados de PIB não seguem uma distribuição normal"

# Conclusão com base no valor de p
nivel_de_significancia <- 0.05
if (shapiro_test_Gini$p.value < nivel_de_significancia) {
  cat("Rejeita H0 no teste de Shapiro-Wilk. Conclusão: Os dados não seguem uma distribuição normal.\n")} 
else {cat("Não há evidências suficientes para rejeitar H0 no teste de Shapiro-Wilk. Conclusão: Os dados 
  podem seguir uma distribuição normal.\n")}
# Rejeita H0 no teste de Shapiro-Wilk. Conclusão: Os dados não seguem uma distribuição normal.


resultados_tabela_Gini <- data.frame(
  Estatistica = shapiro_test_Gini$statistic,
  P_valor = shapiro_test_Gini$p.value
)# Crie uma tabela bonita usando a função kable
tabela_formatada <- kable(resultados_tabela_Gini)
# Exiba a tabela
View(resultados_tabela_Gini)




# NORMALIZAR PIB
subconjunto$PIB_normalizado <- (subconjunto$PIB - min(subconjunto$PIB)) / (max(subconjunto$PIB) - min(subconjunto$PIB))



boxplot(subconjunto$PIB_normalizado, subconjunto$GINI, names = c("PIB", "Índice de Gini"),
        col = c("skyblue", "lightcoral"),  main = "Boxplot", ylab = "Valores", notch = TRUE)

## Teste de Normalidade de Kolmogorov-Smirnov para o Índice de Gini:
ks_test_Gini <- ks.test(subconjunto$GINI, "pnorm")
# D = 0.63077, p-value < 2.2e-16
# alternative hypothesis: two-sided
cat("\nTeste de Kolmogorov-Smirnov:\n")
cat("H0: Os dados seguem uma distribuição normal\n")
cat("H1: Os dados não seguem uma distribuição normal\n")
cat("Resultado do teste de Kolmogorov-Smirnov:\n")
cat("\nInterpretação:\n")
# Estabelecer 5% como nível de significância
if (ks_test_Gini$p.value < 0.05) {cat("Rejeita H0 no teste de Kolmogorov-Smirnov. Conclusão: Os dados não seguem 
  uma distribuição normal.\n")} else {cat("Não há evidências suficientes para rejeitar H0 no teste de Kolmogorov-
  Smirnov. Conclusão: Os dados podem seguir uma distribuição normal.\n")}
# Resultado: Visto que o p-valor de 1.43e-08 é menor que 0.05, rejeita H0 no teste de Kolmogorov-Smirnov. 
# Assim, conclui-se que o Índice de Gini dos municípios paulistas não seguem uma distribuição normal.


#### 2) TESTES NÃO-PARAMÉTRICOS ####

# Calcular o coeficiente de correlação de Spearman entre PIB e Gini
correlacao_spearman <- cor.test( subconjunto$GINI, 
                                 subconjunto$PIB_normalizado, method = "spearman", alternative = "two.sided",
                                 conf.level = 0.95)
# p-value = 8.561689e-35
# rho: 0.4585042 
hipotese_nula <- "Não há correlação significativa entre Gini e PIB nos municípios de SP."
hipotese_alternativa <- "Existe uma correlação significativa entre Gini e PIB nos municípios de SP."
# Conclusão com base no valor-p (usando um nível de significância de 0.05)
nivel_significancia <- 0.05
if (correlacao_spearman$p.value < nivel_significancia) {cat("Rejeita H0 no teste de correlação 
  de Spearman. Conclusão: ", hipotese_alternativa, "\n")} else {cat("Não há evidências suficientes 
  para rejeitar H0 no teste de correlação de Spearman. Conclusão: ", hipotese_nula, "\n")}
# Rejeita H0 no teste de correlação de Spearman. Conclusão:  Existe uma correlação positiva significativa 
# entre Gini e PIB nos municípios de SP.

library(ggplot2)

# Gráfico de Dispersão com reta de regressão
ggplot(subconjunto, aes(x = GINI, y = PIB_normalizado)) + geom_point(color = "#1f78b4") +
  geom_smooth(method = "lm", se = FALSE, color = "red") + labs(title = "Gráfico de Dispersão", 
  x = "GINI 2010", y = "PIB 2010") + theme_minimal() + theme(plot.title = element_text
  (size = 14, face = "bold"), axis.title = element_text(size = 12, face = "bold"))




ggplot(subconjunto, aes(x = GINI, y = PIB)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Adiciona uma linha de regressão
  labs(title = "Diagrama de Dispersão com Linha de Regressão",
       x = "Eixo X",
       y = "Eixo Y")




library(lattice)

xyplot(GINI ~ PIB_normalizado, data = subconjunto, main = "Diagrama de Dispersão", xlab = "Eixo X", ylab = "Eixo Y", pch = 16)

ggplot(data = subconjunto, aes(x = GINI, y = PIB_normalizado)) +
  geom_point(color = "#1f78b4") +
  geom_smooth(data = lm(formula = PIB ~ GINI, data = subconjunto), method = "lm", col = "red", se = FALSE) +
  theme_bw() +
  xlab("Renda per capita") +
  ylab("Consumo de água per capita (m³/ano)")


# REGRESSÃO LINEAR SIMPLES

modelo_regressao <- lm(formula = PIB_normalizado ~ GINI, data = subconjunto)
summary(modelo_regressao)

# Y=β0+β1X1
# Onde:
# Y = variável dependente (resposta/saída)
# X = variável independente (indicadora/explicativa/preditora)
# β0 = coeficiente do intercepto
# β1 = coeficiente da inclinação



modelo_PIB_Gini <- lm(formula = GINI ~ PIB, data = BaseDados)
summary(modelo_PIB_Gini)
# Resíduos:
# Mínimo = -0.123866
# Primeiro Quartil = -0.043154
# Mediana = -0.003692



plot(modelo1, which = 1)
plot(modelo1, which = 2)
plot(modelo1, which = 3)
plot(modelo1, which = 4)
plot(modelo1, which = 5)
plot(modelo1, which = 6)

ggplot(data = BaseDados, aes(x = `Gini da Renda Domiciliar Per Capta 2010`, y = `PIB Per Capta 2010`)) +
  geom_point() +
  geom_smooth(data = lm(formula = `Gini da Renda Domiciliar Per Capta 2010` ~ `PIB Per Capta 2010`, data = BaseDados), method = "lm", col = "red", se = FALSE) +
  theme_bw() +
  xlab("Gini 2010") +
  ylab("PIB 2010")



library(tidyverse)
install.packages("performance")
library(performance)
install.packages("broom")
library(broom)

modelo_simples_Gini_PIB <- lm(formula = PIB ~ Gini, data = BaseDados2)
summary(modelo_simples_Gini_PIB)

modelo_simples_IDH_PIB <- lm(formula = PIB ~ IDH, data = BaseDados2)
summary(modelo_simples_IDH_PIB)

modelo_multiplo <- lm(formula = PIB ~ IDH + Gini, data = BaseDados2)

tabela_modelos <- compare_performance(modelo_simples_Gini_PIB, modelo_simples_IDH_PIB, modelo_multiplo, rank = TRUE)







resultado_teste_kruskal <- kruskal.test(Gini~PIB, data = BaseDados2)


# kruskal wallis

# Realizar o teste de Kruskal-Wallis
resultado_teste_kruskal <- kruskal.test(PIB ~ Gini, data = BaseDados2)

# Exibir resultados
summary(teste_kruskal_wallis)

# Avaliar a hipótese
cat("\nResultado da Avaliação da Hipótese:\n")

# Conclusão com base no valor-p (usando um nível de significância de 0.05)
nivel_significancia <- 0.05

if (teste_kruskal_wallis$p.value < nivel_significancia) {
  cat("Rejeitamos a hipótese nula. Há evidências estatísticas de que o PIB per capita e o Índice de Gini têm distribuições diferentes entre os grupos de municípios.\n")
} else {
  cat("Não rejeitamos a hipótese nula. Não há evidências estatísticas suficientes para afirmar que o PIB per capita e o Índice de Gini têm distribuições diferentes entre os grupos de municípios.\n")
}








seu_dataframe <- subconjunto

# Definir a equação não linear (exemplo: a * exp(b * x) + c)
equacao_nao_linear <- function(par, x) {
  a <- par[1]
  b <- par[2]
  c <- par[3]
  a * exp(b * x) + c
}

# Função de erro para minimização
funcao_erro <- function(par, x, y) {
  predicoes <- equacao_nao_linear(par, x)
  erro <- predicoes - y
  sum(erro^2)
}

# Valores iniciais
valores_iniciais <- c(a = 1, b = 1, c = 1)

# Ajustar o modelo usando optim() com valores iniciais
resultado_otimizacao <- optim(par = valores_iniciais, 
                              fn = funcao_erro, 
                              x = seu_dataframe$GINI, 
                              y = seu_dataframe$PIB)

# Extrair coeficientes do modelo
coeficientes <- resultado_otimizacao$par

# Mostrar a equação ajustada
equacao_ajustada <- substitute(
  paste("PIB =", a, "* exp(", b, "* Gini) +", c),
  list(a = format(coeficientes[1], digits = 3),
       b = format(coeficientes[2], digits = 3),
       c = format(coeficientes[3], digits = 3))
)

cat("Equação ajustada:", as.character(equacao_ajustada), "\n")

ggplot(seu_dataframe, aes(x = GINI, y = PIB)) +
  geom_point() +
  labs(x = "Gini", y = "PIB", title = "Relação entre PIB e Gini") +
  
  # Adicionar a curva ajustada ao gráfico
  stat_function(fun = function(x) 47702 * exp(7.17 * x) + 2577, 
                color = "red") +
  
  # Adicionar uma legenda
  theme(legend.position="none")







resumo <- summary(equacao_ajustada)
r_squared <- resumo$r.squared
cat("R^2:", r_squared, "\n")







mean(subconjunto$PIB)
## 1305065
median(subconjunto$PIB)
## 192739.7
var(subconjunto$PIB)
## 1.751645e+13
sd(subconjunto$PIB)
## 4185266



