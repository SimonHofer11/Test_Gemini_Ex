#Script to write df rows to .txt files to get the right format for using skranz/gemini-ex for writing prompts to Gemini.
#At the moment, only works for the "prompts" column of a dataframe. Code extracts the columns and writes a .txt file for each row into the /prompts directory of your project

#Just some random prompts, here you need to include your df consisting with the prompts in the column "prompts"
prompts <- c(
  "What is the capital of France?",
  "How does quantum computing work?",
  "Can you explain the theory of relativity?",
  "What are the benefits of meditation?",
  "How do you make a perfect cup of coffee?",
  "What are some interesting facts about the human brain?",
  "What is the meaning of life?",
  "Can you give me a brief overview of machine learning?",
  "How do I start investing in stocks?",
  "What are the most important programming languages to learn?",
  "Can you summarize the plot of 'Pride and Prejudice'?",
  "What are the health benefits of a vegan diet?",
  "How do I improve my public speaking skills?",
  "What is the history of the internet?",
  "Can you explain blockchain technology?",
  "What are the best practices for remote work?",
  "How do I learn to play the guitar?",
  "What is the difference between AI and machine learning?",
  "What are some tips for time management?",
  "How does photosynthesis work?"
)

set.seed(123) 
df <- data.frame(prompts = sample(prompts, 20, replace = TRUE), stringsAsFactors = FALSE)



#path directory to save the prompts in the "prompts" directory of your project
#If not otherwise forked from skranz/gemini-ex, directory should end with "gemini_ex-main/prompts"
target_path <- "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/prompts"

if (!dir.exists(target_path)) {
  dir.create(target_path, recursive = TRUE)
}

#save all the ROWS of the "prompts" column from the df as a .txt file to use those
for (i in 1:nrow(df)) {
  
  filename <- paste0("prompt", i, ".txt")
  
  #text from the ‘prompts’ column to the file
  writeLines(df$prompts[i], con = file.path(target_path, filename))
}
