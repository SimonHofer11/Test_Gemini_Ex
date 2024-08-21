#Function to overwrite the txt files with the values provided by gemini/resulting experiment values
save_to_file <- function(round_number, agent_instance, price, quantity, total_quantity, profit, directory) {
  file_name <- paste0(directory,"/round_", round_number, "_", agent_instance, ".txt")
  
  
  #Information the agent reveives each round
  content <- paste0(
    "costs per unit = 1\n",
    "quantity you set in the last round = ", quantity, "\n",
    "total quantity in the markte= ", total_quantity, "\n",
    "resulting price in the market = ", price, "\n",
    "own profit in the last round= ", profit
  )
  
  #additional text for each round
  additional_text <- paste0(
    "As previously mentioned, we have provided you with a profit calculator, ",
    "which allows you to calculate your own profit based on your quantity and the total quantity ",
    "of your competitors in the market. The function (exemplarily written in the R programming language) ",
    "is as follows:\n\n",
    "profit_calculator <- function(q_i, q_others) {\n",
    "  Q <- sum(q_others) + q_i  # sum(q_others): sum of the quantity of other market members, q_i is the own quantity\n",
    "  p <- max(100 - Q, 0)  # price based on the total quantity\n",
    "  revenue <- p * q_i\n",
    "  costs <- q_i * 1\n",
    "  profit <- revenue - costs\n",
    "  return(profit)\n",
    "}\n\n",
    "Based on this information, I would like you to insert the quantity in the following section without justification:\n",
    "Quantity in the next round: ", quantity
  )
  
  write(content, file_name, append = TRUE)
  write(additional_text, file_name, append = TRUE)
}


# Für alle Runden und Instanzen speichern
for (i in 1:n_rounds) {
  for (j in 1:n_instances) {
    price <- sample(1:10, 1)  # Beispielhafte Preisgenerierung
    quantity <- sample(20:40, 1)  # Beispielhafte Nachfragegenerierung
    total_quantity <- sample(70:100, 1)  # Beispielhafte Gesamtnachfrage
    profit <- sample(30:50, 1)  # Beispielhafter Profit
    
    save_to_file(i, j, price, quantity, total_quantity, profit, c("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/prompts"))
  }
}