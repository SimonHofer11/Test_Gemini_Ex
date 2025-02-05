#MONOPOLY GAME

#Define the number of rounds (if you select several, the game will be repeated several times with the amount of rounds you selected)
N_ROUNDS = c(25,25,25,25,25,25,25,25,25,25)


is_local <- function(is_local = TRUE) {
  if (is_local) {
    
    message("Running local")
    if (!FALSE) {
      setwd("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/scripts_monopoly")
      source("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/scripts_monopoly/gemini_analysis_monopoly.R")
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
      
      
      results <- list() 
      
      for (i in 1:length(N_ROUNDS)) {
        source("~/scripts_monopoly/gemini_analysis_monopoly.R")
        cat("\n i = ",i,"\n")
        #cat("\n Objekt results: ",results,".\n")
        n_rounds <- N_ROUNDS[i]  
        
        result <- perform_analysis(n_rounds = n_rounds)
        cat("\nach Spiel Nummer: ",i,".\n")
        result_name <- paste0("game_", n_rounds, "_rounds_run_",i)
        results[[result_name]] <- result
      }
      
      setwd("~")
      cur_time <- as.numeric(Sys.time())
      outdir = "/root/output"
      n_rounds_str <- paste(N_ROUNDS, collapse = "_")
      
      out_file_root_output = paste0(outdir, "/", "monopoly_experiment_", cur_time, "_", n_rounds_str, "_ROUNDS.Rds")
      
      
      saveRDS(results, out_file_root_output)
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

