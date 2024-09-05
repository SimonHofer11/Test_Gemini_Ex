MAX_RUNTIME_SEC = 5*60*60 # Max runtime on Github
MIN_SEC_PER_PROMPT = 5 # Minimum number of seconds between prompts


perform_analysis = function(n_players = 2, n_rounds = 25) {
  cat("Current working directory: ", getwd(), "\n")
  #Hier finden die ganzen Anpassungen statt, inklusive Einbindung ai_mod, errors, script_demand_profit,etc.
  library(dplyr)

  start_time = as.numeric(Sys.time())

  API_KEY = Sys.getenv("API_KEY")
  setwd("~")
  outdir = "/root/output"
  if (.Platform$OS.type == "windows") {
    setwd("C:/libraries/gpt/gemini/gemini_ex")
  }
  if (FALSE) {
    setwd("~/repbox/gemini/gemini_gha")
  }

  source("scripts_monopoly/ai_mod.R")
  source("scripts_monopoly/script_demand_profit.R")
  source("scripts_monopoly/errors.R")
  source("scripts_monopoly/gemini_tools.R")
  
  library(dplyr)
  library(jsonlite)
  
  config_df = load_prompt_configs()
  
  
  game = new_game(n_players= n_players, n_rounds = n_rounds)
  traceback()
  game = run_game(game, debug_mode = FALSE, start_time=start_time)
  game$ok
  game$err_msg
  
  options(warn=2)
  
  cat("\n Nach Game before save results\n")
  
  
  #cur_time = as.numeric(Sys.time())
  #save in gemini_results
  #results_dir <- "~/gemini_results"

  #cur_time <- as.numeric(Sys.time())
  #out_file <- paste0(results_dir, "/", "oligopoly_experiment", cur_time, "_", n_players, "_Firmen.Rds")
  
  
  #saveRDS(game, out_file)
  
  
  

  

  cat("\n\nFINISHED\n\n")
  game

}
  

analyse_prompt_file = function(file, config_df,  api_key,verbose=TRUE, add_prompt=TRUE){
  if (verbose) {
    cat(paste0("\n\nANALYSE ", file,"\n\n"))
  }
  prompt = paste0(readLines(file,warn = FALSE), collapse="\n")
  config = get_prompt_config(file, config_df)

  prompt_name = tools::file_path_sans_ext(basename(file))

  if (isTRUE(config$embedding)) {
    res = run_gemini_embedding(prompt, api_key,  model=config$model)
  } else {
    res = run_gemini(prompt, api_key, json_mode=config$json_mode, model=config$model, temperature = config$temperature)
  }

  res$prompt_name = prompt_name
  if (isTRUE(config$add_prompt)) {
    res$prompt = prompt
  }
  res
}


analyse_prompt_monopoly = function(prompt, config_df,  api_key,verbose=TRUE, add_prompt=TRUE){
  if (verbose) {
    cat(paste0("\n\nANALYSE ", file,"\n\n"))
  }
  prompt = paste0(readLines(file,warn = FALSE), collapse="\n")

  #prompt_name = tools::file_path_sans_ext(basename(file))
  

  res = run_gemini(prompt, api_key, json_mode=config$json_mode, model=config$model, temperature = 1)
  
  
  #res$prompt_name = prompt_name
  #if (isTRUE(config$add_prompt)) {
  #  res$prompt = prompt
  #}
  res
}












get_prompt_config = function(file, config_df) {
  def_config = config_df[config_df$prompt_type=="_default",]
  prompt_id = tools::file_path_sans_ext(basename(file))
  row = which(prompt_id==config_df$prompt_type)
  if (length(row)==0) {
    row = which(startsWith(prompt_id, config_df$prompt_type))
  }
  if (length(row)==0) return(def_config)
  config = config_df[row[1],]
  fields = names(config)
  fields = fields[!sapply(config,is.na)]
  def_config[1, fields] = config[1, fields]
  def_config[1,]
}

load_prompt_configs = function() {
  cat("\n in load prompt configs in gemini_analysis_monopoly.\n")
  library(stringi)
  library(dplyr)

  files = list.files("config",glob2rx("*.yml"),full.names = TRUE)
  config_df = bind_rows(lapply(files, function(config_file) {
    res = yaml::yaml.load_file(config_file)
    prompt_type = tools::file_path_sans_ext(basename(config_file))
    res$prompt_type = prompt_type
    res = as.data.frame(res)
  }))
  cat("\n nach load prompt configs in gemini_analysis_monopoly.\n")
  
  return(config_df)
  
}

