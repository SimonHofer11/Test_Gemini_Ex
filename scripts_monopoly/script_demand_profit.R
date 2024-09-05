#Demand function
price_function <- function(Q) {
  return(max(100 - Q, 0))
}


# Profit Calculator
profit_calculator <- function(q_i) {
  Q <- q_i  # aggregated market quantity
  p <- price_function(Q)    
  revenue <- p * Q        
  costs <- Q
  profit <- revenue - costs  
  return(profit)
}

# One Period calculation
simulate_period <- function(q_i) {
  profit <- profit_calculator(q_i)
  price = price_function(q_i)
  return(list(pi = profit, price = price))
}
