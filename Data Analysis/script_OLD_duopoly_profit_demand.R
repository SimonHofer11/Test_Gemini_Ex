#Demand function Oligopol:
# Definition der Funktion qi
demand <- function(a_i, p_1, p_2, alpha, mu, beta, a_0){
  # Berechnung des Exponenten für qi
  exp_numerator <- exp((a_i - p_1 / alpha) / mu)
  # Berechnung des Summen-Terms im Nenner (mit a_i - p_j / alpha).Duopol 2 Firmen
  sum_exp <- (exp((a_i - p_2 / alpha) / mu)) + (exp((a_i - p_1 / alpha) / mu))
  
  # Berechnung des zusätzlichen Exponential-Terms im Nenner
  exp_additional <- exp(a_0 / mu)
  
  # Berechnung von qi
  q_i <- beta * exp_numerator / (sum_exp + exp_additional)
  
  return(q_i)
}

# Beispielhafte Werte
a_i <- 2
p_1 <- 1.5
p_2 <- 3.75
alpha <- 1
mu <- 0.25
beta <- 100
a_0 <- 0

# Aufruf der Funktion mit den beispielhaften Werten
q1_value <- demand(a_i, p_1, p_2, alpha, mu, beta, a_0)
print(q1_value)
q2_value <- demand(a_i, p_2, p_1, alpha, mu, beta, a_0)
print(q2_value)


# Funktion zur Berechnung des Gewinns
profit_function <- function(p_i, c_i, alpha, q_i) {
  # Berechnung des Gewinns
  pi_i <- (p_i - alpha * c_i) * q_i
  return(pi_i)
}


c_1 = 1
profit_1 <- profit_function(p_1, c_1, alpha, q1_value)
profit_1
profit_2 <- profit_function(p_2, c_1, alpha, q2_value)
profit_2
