#Demand function Oligopol:
# Definition der Funktion qi
demand <- function(a_i, p_1, a, p_2, alpha, mu, beta, a_0) {
  
  # Berechnung des Exponenten für qi
  exp_numerator <- exp((a_i - p_1 / alpha) / mu)
  
  # Berechnung des Summen-Terms im Nenner (mit a_j - p_j / alpha)
  sum_exp <- (exp((a_i - p_2 / alpha) / mu)
  
  # Berechnung des zusätzlichen Exponential-Terms im Nenner
  exp_additional <- exp(a_0 / mu)
  
  # Berechnung von qi
  q_i <- beta * exp_numerator / (sum_exp + exp_additional)
  
  return(q_i)
}

# Beispielhafte Werte
a_i <- 2
p_1 <- 1
a <- c(1, 2, 3)
p_2 <- 1
alpha <- 3.2
mu <- 0.25
beta <- 100
a_0 <- 0

# Aufruf der Funktion mit den beispielhaften Werten
q1_value <- demand(a_i, p_1, a, p_2, alpha, mu, beta, a_0)
print(q1_value)
q2_value <- demand(a_i, p_2, a, p_1, alpha, mu, beta, a_0)
print(q2_value)

