# Dados
dados <- data.frame(
  Concentracao = c(rep("R", 5), rep("D", 6), rep("M", 6)),  # Corrigido o número de repetições
  Dose = c(0.41, 0.58, 0.71, 0.89, 1.01, 
           0.71, 1.00, 1.18, 1.48, 1.61, 1.70, 
           0.40, 0.71, 1.00, 1.18, 1.31, 1.40),
  Expostos = c(50, 48, 46, 49, 50, 
               49, 48, 49, 47, 50, 48, 
               47, 46, 48, 47, 46, 50),
  Mortos = c(6, 16, 24, 42, 44, 
             16, 48, 43, 47, 47, 48, 
             7, 22, 27, 38, 43, 48)
)

# Variável resposta (número de mortos / expostos)
dados$Nao_mortos <- dados$Expostos - dados$Mortos
dados$Resposta <- cbind(dados$Mortos, dados$Nao_mortos)

# Ajuste do modelo com ligação logito
modelo_logit <- glm(Resposta ~ Dose * Concentracao, family = binomial(link = "logit"), data = dados)

# Ajuste do modelo com ligação probito
modelo_probit <- glm(Resposta ~ Dose * Concentracao, family = binomial(link = "probit"), data = dados)

# Ajuste do modelo com ligação cloglog
modelo_cloglog <- glm(Resposta ~ Dose * Concentracao, family = binomial(link = "cloglog"), data = dados)

# Resumo dos modelos
summary(modelo_logit)
summary(modelo_probit)
summary(modelo_cloglog)

# Instalar pacotes necessários
if (!require("hnp")) install.packages("hnp")

# Pacote para envelopes
library(hnp)

# Gerar envelopes para o modelo com ligação logit
hnp(modelo_logit, main = "Envelopes - Logit")

# Gerar envelopes para o modelo com ligação probit
hnp(modelo_probit, main = "Envelopes - Probit")

# Gerar envelopes para o modelo com ligação cloglog
hnp(modelo_cloglog, main = "Envelopes - Cloglog")

# Gráfico de resíduos
par(mfrow = c(1, 3))
plot(residuals(modelo_logit, type = "deviance"), main = "Resíduos - Logit")
plot(residuals(modelo_probit, type = "deviance"), main = "Resíduos - Probit")
plot(residuals(modelo_cloglog, type = "deviance"), main = "Resíduos - Cloglog")

library(MASS)

# Cálculo da dose letal 50 (DL50)
dl50 <- dose.p(modelo_logit, p = 0.5)
print(dl50)

# Extração dos valores estimados e erro padrão
dl50_estimativa <- as.numeric(dl50[1])  # Estimativa da DL50
dl50_se <- as.numeric(attr(dl50, "SE"))  # Erro padrão da DL50

# Cálculo do intervalo de confiança (95%)
alpha <- 0.05
z <- qnorm(1 - alpha / 2)  # Valor crítico da distribuição normal
ic_inf <- dl50_estimativa - z * dl50_se
ic_sup <- dl50_estimativa + z * dl50_se

# Exibir a estimativa da DL50 e o intervalo de confiança
cat("DL50 estimada:", dl50_estimativa, "\n")
cat("Intervalo de confiança 95%: [", ic_inf, ",", ic_sup, "]\n")

# Cálculo da dose letal 50 (DL50)
dl50 <- dose.p(modelo_probit, p = 0.5)
print(dl50)

# Extração dos valores estimados e erro padrão
dl50_estimativa <- as.numeric(dl50[1])  # Estimativa da DL50
dl50_se <- as.numeric(attr(dl50, "SE"))  # Erro padrão da DL50

# Cálculo do intervalo de confiança (95%)
alpha <- 0.05
z <- qnorm(1 - alpha / 2)  # Valor crítico da distribuição normal
ic_inf <- dl50_estimativa - z * dl50_se
ic_sup <- dl50_estimativa + z * dl50_se

# Exibir a estimativa da DL50 e o intervalo de confiança
cat("DL50 estimada:", dl50_estimativa, "\n")
cat("Intervalo de confiança 95%: [", ic_inf, ",", ic_sup, "]\n")

# Cálculo da dose letal 50 (DL50)
dl50 <- dose.p(modelo_cloglog, p = 0.5)
print(dl50)

# Extração dos valores estimados e erro padrão
dl50_estimativa <- as.numeric(dl50[1])  # Estimativa da DL50
dl50_se <- as.numeric(attr(dl50, "SE"))  # Erro padrão da DL50

# Cálculo do intervalo de confiança (95%)
alpha <- 0.05
z <- qnorm(1 - alpha / 2)  # Valor crítico da distribuição normal
ic_inf <- dl50_estimativa - z * dl50_se
ic_sup <- dl50_estimativa + z * dl50_se

# Exibir a estimativa da DL50 e o intervalo de confiança
cat("DL50 estimada:", dl50_estimativa, "\n")
cat("Intervalo de confiança 95%: [", ic_inf, ",", ic_sup, "]\n")
