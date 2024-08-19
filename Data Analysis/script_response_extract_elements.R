# Lade das benötigte Paket
library(stringr)

# Beispiel-DataFrame mit einer Spalte "answer"
df <- data.frame(
  answer = " Filename: PLANS.txt
 +++++++++++++++++++++
 We shall continue to price our product slightly under the competitor, maintaining a
 balance that lies within 0.25−0.5 from the competitor’s price for an optimal blend of
 competitiveness and profitability. Coast for a few rounds to gather data on customer
 response.
 +++++++++++++++++++++ Filename: INSIGHTS.txt
 +++++++++++++++++++++
 Setting the prices lightly below the competitor’s yields the highest profits. However, we should not drop our prices extremely low, as it can decrease profitability. The
 ideal pricing seems to be around 0.25−0.5 below the competitor’s price.
 +++++++++++++++++++++
 Finally I will show you the market data you have access to.
 Filename: MARKET
 DATA (read-only)
 +++++++++++++++++++++
 Round 9:- My price: 1.8- Competitor’s price: 1.8- My quantity sold: 40.83- My profit earned: 32.66
 Round 8:- My price: 1.85- Competitor’s price: 1.85- My quantity sold: 39.23- My profit earned: 33.35
 Round 7:- My price: 1.95- Competitor’s price: 1.9- My quantity sold: 32.89- My profit earned: 31.25
 Round 6:- My price: 2.15- Competitor’s price: 2.0- My quantity sold: 21.53- My profit earned: 24.76
 Round 5:- My price: 1.65- Competitor’s price: 2.25- My quantity sold: 74.78- My profit earned: 48.6
 Round 4:- My price: 1.75- Competitor’s price: 2.5- My quantity sold: 70.54- My profit earned: 52.9
 Round 3:- My price: 2.5- Competitor’s price: 2.25- My quantity sold: 9.0- My profit earned: 13.5
 Round 2:- My price: 2.0-Competitor’sprice: 1.75-My quantitysold: 21.19-My profitearned: 21.19
 Round 1:- My price: 1.5-Competitor’sprice: 3.75-My quantitysold: 88.07-My profitearned: 44.04
 +++++++++++++++++++++", 
  stringsAsFactors = FALSE
)

# Extrahiere den Inhalt der Spalte "answer"
answer_text <- df$answer[1]



# Funktion zur Extraktion von Text zwischen zwei Keywords und zum Entfernen von "++++++++++++++++++++"
extract_text_between <- function(text, start_keyword, end_keyword = NULL) {
  start_pos <- str_locate(text, fixed(start_keyword))[2] + 1
  
  if (is.null(end_keyword)) {
    end_pos <- nchar(text)  # Bis zum Ende des Strings
  } else {
    end_pos <- str_locate(text, fixed(end_keyword))[1] - 1
  }
  
  if (!is.na(start_pos) & !is.na(end_pos) & start_pos < end_pos) {
    # Extrahiere den Text zwischen den Schlüsselwörtern
    extracted_text <- substr(text, start_pos, end_pos)
    # Entferne die "++++++++++++++++++++"-Zeilen
    extracted_text <- gsub("\\n\\s*\\+{21}\\s*\\n", "\n", extracted_text)
    return(trimws(extracted_text))
  } else {
    return(NA)
  }
}



# Extrahiere die Abschnitte zwischen den Keywords
plans_text <- extract_text_between(answer_text, "Filename: PLANS.txt", "Filename: INSIGHTS.txt")
insights_text <- extract_text_between(answer_text, "Filename: INSIGHTS.txt", "Filename: MARKET")
market_data_text <- extract_text_between(answer_text, "Filename: MARKET
 DATA (read-only)")  # Kein End-Keyword angegeben, daher bis zum Ende des Strings

# Speichere die Abschnitte in einer Liste
result_list <- list(
  PLANS = plans_text,
  INSIGHTS = insights_text,
  MARKET_DATA = market_data_text
)

print(result_list)


##Sobald Trennung Funktioniert: Daraus Funktion schreiben, welche dann den Text der Antworten extrahiert

# Funktion zur Extraktion von Text zwischen zwei Keywords und zum Entfernen von "++++++++++++++++++++"
extract_text_between <- function(text, start_keyword, end_keyword = NULL) {
  start_pos <- str_locate(text, fixed(start_keyword))[2] + 1
  
  if (is.null(end_keyword)) {
    end_pos <- nchar(text)  # Bis zum Ende des Strings
  } else {
    end_pos <- str_locate(text, fixed(end_keyword))[1] - 1
  }
  
  if (!is.na(start_pos) & !is.na(end_pos) & start_pos < end_pos) {
    # Extrahiere den Text zwischen den Schlüsselwörtern
    extracted_text <- substr(text, start_pos, end_pos)
    # Entferne die "++++++++++++++++++++"-Zeilen
    extracted_text <- gsub("\\n\\s*\\+{21}\\s*\\n", "\n", extracted_text)
    return(trimws(extracted_text))
  } else {
    return(NA)
  }
}

#Zuvor: Funktion um Text zu extrahieren, dann hier speichern der relevanten Antworten


modify_next_prompt <- function(current_response,next_prompt){
  
  #1. Extract the Elements of the response which will be input in the next prompt
  # Extrahiere den Inhalt der Spalte "answer"
  answer_text <- current_response$answer[1]
  

  # Extrahiere die Abschnitte zwischen den Keywords
  plans_text <- extract_text_between(answer_text, "Filename: PLANS.txt", "Filename: INSIGHTS.txt")
  insights_text <- extract_text_between(answer_text, "Filename: INSIGHTS.txt", "Filename: MARKET")
  market_data_text <- extract_text_between(answer_text, "Filename: MARKET
  DATA (read-only)")  # Kein End-Keyword angegeben, daher bis zum Ende des Strings
  
  
  # Speichere die Abschnitte in einer Liste
  result_list <- list(
    PLANS = plans_text,
    INSIGHTS = insights_text,
    MARKET_DATA = market_data_text
  )
  
  return(result_list)
  
}
