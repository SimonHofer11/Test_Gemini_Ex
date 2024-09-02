#Demand function
price_function <- function(Q) {
  return(max(100 - Q, 0))
}


# Profit Calculator
profit_calculator <- function(q_i, q_others) {
  Q <- sum(q_others) + q_i  # aggregated market quantity
  p <- price_function(Q)    
  revenue <- p * q_i         
  costs <- q_i 
  profit <- revenue - costs  
  return(profit)
}

# One Period calculation
simulate_period <- function(q_i, q_others) {
  profit <- profit_calculator(q_i, q_others)
  Q_total <- sum(q_others) + q_i
  price = price_function(Q_total)
  Q_other = sum(q_others)
  return(list(pi = profit, price = price, Q_other = Q_other, Q_total = Q_total))
}
