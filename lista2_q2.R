# Dados da tabela
dados <- data.frame(
  Idade = rep(c("< 30", "30+"), each = 4),
  Cigarros = rep(c("< 5", "5+"), times = 4),
  Gestacao = rep(c("<= 260", "> 260"), times = 2, each = 2),
  Nao_sobreviveu = c(50, 24, 6, 4, 41, 14, 9, 1),
  Sobreviveu = c(315, 4012, 459, 124, 147, 1594, 40, 11)
)

# Variável resposta (sobrevivência)
dados$Total <- dados$Nao_sobreviveu + dados$Sobreviveu
dados$Sobrev <- cbind(dados$Sobreviveu, dados$Nao_sobreviveu)

# Ajuste do modelo logístico
modelo <- glm(Sobrev ~ Idade + Cigarros + Gestacao, family = binomial(link = "logit"), data = dados)

# Resumo do modelo
summary(modelo)

# Curva ROC
library(pROC)
probabilidades <- predict(modelo, type = "response")
roc_curve <- roc(dados$Sobreviveu / dados$Total, probabilidades)
plot(roc_curve)
auc(roc_curve) # AUC

# Resíduos deviance e Pearson
residuos_deviance <- residuals(modelo, type = "deviance")
residuos_pearson <- residuals(modelo, type = "pearson")
par(mfrow = c(1, 2))
plot(residuos_deviance, main = "Resíduos Deviance")
plot(residuos_pearson, main = "Resíduos Pearson")

# Razões de chances
exp(coef(modelo))

# Novos dados
nova_pessoa <- data.frame(Idade = "< 30", Cigarros = "5+", Gestacao = "> 260")

# Probabilidade estimada
prob_sobrev <- predict(modelo, newdata = nova_pessoa, type = "response")
prob_sobrev

# Gráfico com as curvas ajustadas e observadas
plot(dados$Sobreviveu / dados$Total, type = "b", col = "blue", pch = 19, ylab = "Probabilidade de Sobrevivência", xlab = "Grupo")
lines(probabilidades, type = "b", col = "red", pch = 18)
legend("topright", legend = c("Observado", "Ajustado"), col = c("blue", "red"), pch = c(19, 18))
