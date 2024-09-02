#New Script read RDS
gemini_results_path = ("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/gemini_results.zip")
#unzip
unzip(gemini_results_path, exdir = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/gemini_results")

gemini_results_path <- sub("\\.zip$", "", gemini_results_path)


#Create a list of all .rds files of "gemini_results
files <- list.files(path = gemini_results_path, pattern = "\\.Rds$", full.names = TRUE)

df = readRDS(files)