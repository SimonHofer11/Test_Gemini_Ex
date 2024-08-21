#Skript to read in the downloaded Gemini prompt results and save them in a dataframe format as a .rds-file
#Resulting .rds-file will be saved in the directory BEFORE "gemini_results"

#Currently only works for "one question, one answer" interaction with Gemini

#1.reading in prompts
#PATH OF YOUR PROJECT WHICH CONTAINS contains the prompts (ends with "/prompts")
prompt_directory = ("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/prompts")


df_final <- data.frame(prompt = character(), stringsAsFactors = FALSE)

#.txt files
txt_files <- list.files(prompt_directory, pattern = "\\.txt$", full.names = TRUE)

#reading in .txt files and save them in a df
for (file in txt_files) {
  content <- paste(readLines(file, warn = FALSE), collapse = "\n")
  df_final <- rbind(df_final, data.frame(prompt = content, stringsAsFactors = FALSE))
}



#2. reading in answers
#PATH TO DIRECTORY OF YOUR PROJECT WHICH CONTAINS the .zip file "gemini_results" (ends with "/gemini_results.zip")
gemini_results_path = ("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/gemini_results.zip")
#unzip
unzip(gemini_results_path, exdir = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/gemini_results")

gemini_results_path <- sub("\\.zip$", "", gemini_results_path)


#Create a list of all .rds files of "gemini_results
files <- list.files(path = gemini_results_path, pattern = "\\.Rds$", full.names = TRUE)

#Initialisation of an empty list to save the loaded data
rds_list <- list()

#Loop to load the .rds files and save them in the list with fitting names
for (i in seq_along(files)) {
  file_i <- files[i]
  name <- paste0("prompt_result", i)
  # Load file and save in the list
  rds_list[[name]] <- readRDS(file_i)
}

# Überprüfe, ob die Länge der Liste mit der Anzahl der Zeilen im DataFrame übereinstimmt
if (length(rds_list) != nrow(df_final)) {
  stop("Die Länge der Liste 'rds_list' stimmt nicht mit der Anzahl der Zeilen im DataFrame überein.")
}

# Initialisiere die neue Spalte "answer" im DataFrame
df_final$answer <- NA

# Füge die neue Spalte "answer" hinzu
for (i in seq_along(rds_list)) {
  current_element <- rds_list[[i]]
  
  # Speichere das aktuelle Listenelement in der Spalte "answer" der entsprechenden Zeile
  df_final$answer[i] <- current_element$candidates[[1]][[1]][[1]][["text"]]
}

#3.Save results in a .rds file
#Save the resulting df as .rds in the path directory BEFORE "gemini_results"
#Extract the path before "/gemini_results
target_path <- sub("/gemini_results$", "", gemini_results_path)
time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

filename <- paste0("gemini_results_", time, ".rds")

saveRDS(df_final, file = file.path(target_path, filename))







####TEST

#Function for splitting the list object into "Prompt" and "Answer" 
#split_and_clean <- function(text) {
#Splitting in Prompt und Answer
#parts <- strsplit(text, "\n\n")[[1]]

#remove \n
#prompt <- gsub("\n", "", parts[1])
#answer <- gsub("\n", "", parts[2])

#return(c(prompt, answer))
#}


#df_final <- data.frame()

#Loop through all elements in the rds_list and add promt/answer to df_final
#for (i in seq_along(rds_list)) {
#  
#  current_element <- rds_list[[i]]
#  
#  #Applying the function to the corresponding column and creating a new df
#  df_split <- as.data.frame(t(sapply(current_element$candidates[[1]][[1]][[1]][["text"]], split_and_clean)))
#  
#  colnames(df_split) <- c("prompt", "answer")
#  
#  #remove rownames
#  rownames(df_split) <- NULL
  
#  df_final <- rbind(df_final, df_split)
#}