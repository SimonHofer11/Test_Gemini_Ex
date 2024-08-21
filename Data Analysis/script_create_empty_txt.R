#empty .txt files for the rounds 1-25

#hier für Experiment auf 25 stellen
#bsp für 2 Agenten
n_instances <- 2
n_rounds <- 3
# create empty .txt files before the round starts.
create_files <- function(n_rounds, n_instances, directory) {
  # Überprüfen, ob das Verzeichnis existiert, falls nicht, erstellen
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  for (i in 1:n_rounds) {
    for (j in 1:n_instances) {
      file_name <- paste0(directory, "/round_", i, "_", j, ".txt")
      file.create(file_name)
    }
  }
}
create_files(n_rounds,n_instances,c("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/prompts"))
