# Nachfragefunktion: Berechnung des Preises
price_function <- function(Q) {
  return(max(100 - Q, 0))
}

#Kosten = q_i

# Gewinnberechnung (Profit Calculator)
profit_calculator <- function(q_i, q_others) {
  Q <- sum(q_others) + q_i  # Gesamte Menge inklusive der eigenen
  p <- price_function(Q)    # Preis basierend auf der Gesamtmenge
  revenue <- p * q_i         # Einnahmen (Preis * Menge)
  costs <- q_i  # Kosten
  profit <- revenue - costs  # Gewinn = Einnahmen - Kosten
  return(profit)
}

# Beispielhafte Simulation für eine Periode
simulate_period <- function(q_i, q_others) {
  profit <- profit_calculator(q_i, q_others)
  Q_total <- sum(q_others) + q_i
  return(list(profit = profit, Q_total = Q_total))
}

# Beispielhafte Daten: Mengen der anderen Firmen
q_others <- c(33,33)  # Beispielhafte Mengen der anderen Firmen

# Die eigene Menge q_i
q_i <- 33

# Simulation einer Periode
result <- simulate_period(q_i, q_others)
print(result)

# Ergebnis ausgeben
cat("Gewinn des Unternehmens:", result$profit, "\n")
cat("Gesamtmenge im Markt:", result$Q_total, "\n")