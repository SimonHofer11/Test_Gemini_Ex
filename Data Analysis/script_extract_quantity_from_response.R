#Getestet mit df_final aus gemini_results

# Funktion zur Extraktion der Zahl aus einem Text
extract_quantity <- function(text) {
  # Sicherstellen, dass text ein Character-Objekt ist
  text <- as.character(text)
  
  # Debug-Ausgabe, um den Text zu überprüfen
  print(text)
  
  # Regulärer Ausdruck zur Extraktion der Zahl nach "Quantity in the next round:" oder "Price in the next round:"
  # Der Ausdruck sucht nach optionalen Sternchen und Leerzeichen vor und nach der Zahl
  quantity <- sub(".*(?:Quantity|Price) in the next round:\\s*\\**(\\d+)\\**.*", "\\1", text)
  
  # Umwandlung der extrahierten Zahl von Text zu numerisch
  quantity_numeric <- as.numeric(quantity)
  
  # Überprüfen, ob die Extraktion erfolgreich war
  if (is.na(quantity_numeric)) {
    warning("Could not extract a valid number from the text.")
  }
  
  return(quantity_numeric)
}

# Beispielanwendung auf den Inhalt des Dataframes
result_1 <- extract_quantity(df_final[1, 2]) # für den Fall mit Sternchen
result_2 <- extract_quantity(df_final[4, 2]) # für den Fall ohne Sternchen

# Ergebnisse anzeigen
print(result_1)  # Sollte 50 ausgeben
print(result_2)  # Sollte 6 ausgeben

df_final[1,2]
result = extract_quantity(df_final[1,2])
result_2 = extract_quantity(df_final[4,2])
