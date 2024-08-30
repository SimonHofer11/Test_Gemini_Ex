#Idee: To-Do: bei is_local dann auch beispiel mit Gemini und den richtigen Prompts machen. Dafür Antworten in R übertragen und rumprobieren

is_local <- function(is_local = TRUE) {
  if (is_local) {
    
    message("Running local")
    if (!FALSE) {
      #Sys.setenv(HOME = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex")
      setwd("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex")
      source("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/scripts/gemini_analysis.R")
      perform_analysis()
    }
    
    if (FALSE) {
      print("FALSE")
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
    
      source("~/scripts/gemini_analysis_oligopoly.R")
      perform_analysis(n_players = 2, n_rounds = 25 )
    }
    
    if (FALSE) {
      print("FALSE")
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

#is_local()
is_local(FALSE)



# 
# 
# if (!FALSE) {
#   #Sys.setenv(HOME = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex")
#   #setwd("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex")
#   #print("!FALSE")
#   #print(getwd())
#   source("~/scripts/gemini_analysis.R")
#   #source("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/scripts/gemini_analysis.R")
#   perform_analysis()
# }
# 
# if (FALSE) {
#   print("FALSE")
#   API_KEY = Sys.getenv("API_KEY")
#   
#   #cat("API_KEY:", substr(API_KEY,1,4))
#   
#   #system("curl -h")
#   #setwd("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex")
#   
#   cat("\n\nSTART TEST ANALYS\n\n")
#   source("~/scripts/gemini_tools.R")
#   
#   
#   res = run_gemini("Create a short JSON output about colors.", API_KEY, json_mode=TRUE)
#   
#   saveRDS(res, "/root/output/result.Rds")
#   try(writeLines(toJSON(res), "/root/output/result.json"))
#   
#   
#   cat("\n\nEND TEST ANALYS\n\n")
#   
# }

