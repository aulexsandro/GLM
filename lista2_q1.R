# Valores dos coeficientes e seus erros padrão
coeficientes <- c(-6.949, 0.805, 0.161, 0.332, 0.116)
erros_padrao <- c(0.377, 0.0444, 0.113, 0.0393, 0.0204)

# Cálculo da estatística z
z_valores <- coeficientes / erros_padrao

# Cálculo do valor-p associado a cada estatística z (teste bilateral)
p_valores <- 2 * (1 - pnorm(abs(z_valores)))

# Exibindo resultados
resultado <- data.frame(Coefficient = coeficientes, SE = erros_padrao, Z = z_valores, P_value = p_valores)
print(resultado)

# Nível de confiança
z_alpha <- qnorm(1 - 0.05 / 2)

# Cálculo dos limites inferior e superior do intervalo de confiança
limite_inferior <- coeficientes - z_alpha * erros_padrao
limite_superior <- coeficientes + z_alpha * erros_padrao

# Exibindo intervalos de confiança
intervalos_conf <- data.frame(Coefficient = coeficientes, CI_lower = limite_inferior, CI_upper = limite_superior)
print(intervalos_conf)
