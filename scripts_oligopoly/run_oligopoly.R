#Define the number of players (if you select several, the game will be repeated several times)
N_PLAYERS = c(2,3) #c(2,3)
N_ROUNDS = 2


is_local <- function(is_local = TRUE) {
  if (is_local) {
    
    message("Running local")
    if (!FALSE) {
      setwd("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex")
      source("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/scripts/gemini_analysis.R")
      perform_analysis()
    }
    
    if (FALSE) {
      API_KEY = Sys.getenv("API_KEY")
      
      #cat("API_KEY:", substr(API_KEY,1,4))
      
      #system("curl -h")

      cat("\n\nSTART TEST ANALYS\n\n")
      source("~/scripts/gemini_tools.R")
      
      
      res = run_gemini("Create a short JSON output about colors.", API_KEY, json_mode=TRUE)
      
      saveRDS(res, "/root/output/result.Rds")
      try(writeLines(toJSON(res), "/root/output/result.json"))
      
      
      cat("\n\nEND TEST ANALYS\n\n")
      
    }
    
  } else {
    if (!FALSE) {
      
      source("~/scripts_oligopoly/gemini_analysis_oligopoly.R")
      results <- list() 
      
      for (i in 1:length(N_PLAYERS)) {
        cat("\n i = ",i,"\n")
        #cat("\n Objekt results: ",results,".\n")
        n_players <- N_PLAYERS[i]  
        
        result <- perform_analysis(n_players = n_players, n_rounds = N_ROUNDS)
        cat("\nach Spiel Nummer: ",i,".\n")
        result_name <- paste0("game_", n_players, "_player")
        results[[result_name]] <- result
      }
    }
    
    if (FALSE) {
      API_KEY = Sys.getenv("API_KEY")
      
      #cat("API_KEY:", substr(API_KEY,1,4))
      
      #system("curl -h")
      
      cat("\n\nSTART TEST ANALYS\n\n")
      source("~/scripts/gemini_tools.R")
      
      
      res = run_gemini("Create a short JSON output about colors.", API_KEY, json_mode=TRUE)
      
      saveRDS(res, "/root/output/result.Rds")
      try(writeLines(toJSON(res), "/root/output/result.json"))
      
      
      cat("\n\nEND TEST ANALYS\n\n")
      
    }
    
  }
}

is_local(FALSE)

