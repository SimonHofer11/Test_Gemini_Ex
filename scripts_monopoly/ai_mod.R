IS_ON_GHA = Sys.info()['sysname'] != "Windows"
#setwd("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/scripts_monopoly")

example = function() {
  source("errors.R")
  source("run_oligopoly.R")
  source("script_demand_profit.R")
  source("gemini_analysis_oligopoly.R")
  source("gemini_tools_oligopoly.R")

  library(dplyr)
  library(jsonlite)
  game = new_game(n_rounds = 10)

  # if error call
  traceback()
  #hier umstellen
  game = run_game(game, debug_mode = TRUE)
  game$ok
  game$err_msg

  options(warn=2)


}
#example


if (IS_ON_GHA) {
  # overwrite restore point
  restore.point = function(...) {}
} else {
  library(restorepoint)
  suppressWarnings(try(rm(restore.point),silent = TRUE))
  #restore.point.options(display.restore.point = TRUE)
}

run_game = function(game, debug_mode=FALSE, start_time) {
  if (IS_ON_GHA){
    API_KEY = Sys.getenv("API_KEY")
    setwd("~")
    outdir = "/root/output"
  }

    if (debug_mode) {
    while(game$cur_round < game$n_rounds) {
      start_time = as.numeric(Sys.Date())
      game = game_play_next_round(game, api_key = "", start_time = start_time)
    }

  } else {
    # Main loop
    res = try({
      while(game$cur_round < game$n_rounds) {
        game = game_play_next_round(game, api_key = API_KEY, start_time = start_time)
      }
    }, silent=TRUE)
    if (is(res, "try-error")) {
      game = .GlobalEnv$ERR_GAME
      cat("\nGame did not run without error:\n ", as.character(res))
    }
  }

  # Game finished without error
  return(game)
}

new_player_df = function(n_rounds) {
  tibble(
    i = 1,
    t = 1:n_rounds,
    p = rep(NA_real_, n_rounds),
    q = rep(0, n_rounds),
    pi = rep(NA_real_, n_rounds),
    strategy_response = rep("", n_rounds),
    hist_text = rep("", n_rounds),
    q_prompt = rep("", n_rounds),
    strategy_prompt = rep("", n_rounds)
  )
}

new_game = function(n_rounds, temperature =1) {
  restore.point("new_game")
  game = list(
    ok = TRUE,
    n_rounds = n_rounds,
    cur_round = 0,
    player_dfs = new_player_df(n_rounds)
  )
  
  
  game
}

game_play_next_round = function(game, api_key, start_time) {
  restore.point("game_play_next_round")

  game$cur_round = game$cur_round + 1
  
  game = player_make_history_text(game)
  game = player_make_strategy_prompt(game)
  game = player_run_strategy_prompt(game, api_key = api_key, start_time = start_time)
  game = player_make_q_prompt(game)
  game = player_run_q_prompt(game, api_key = api_key, start_time = start_time)
  

  # Computes prices and profits
  game = game_compute_round_results(game)

  game
}

player_run_strategy_prompt = function(game, attempt=1, max_attempts = 10, api_key, start_time) {
  restore.point("player_run_strategy_prompt")
  t = game$cur_round
  df = game$player_dfs
  prompt = df$strategy_prompt[t]
  
  if(IS_ON_GHA){
    res = run_gemini(prompt,api_key, model="gemini-1.5-flash", json_mode=FALSE, temperature= 1 , add_prompt=FALSE, verbose=TRUE)
    cur_time = as.numeric(Sys.time())
    
    if (cur_time - start_time > MAX_RUNTIME_SEC) {
      return()
    }
    Sys.sleep(5)
    
    
  } else{
    res = list(
      ok = TRUE,
      text = "--Here is the plan for the next round--"
    )

  }

  if (!res$ok) {
    if (attempt > max_attempts) {
      # Build that function which stops all
      stop_game(game, "Error in make plan")
    }
    game = player_run_strategy_prompt(game,i,attempt=attempt+1)
    return(game)
  }
  if (IS_ON_GHA) {
    df$strategy_response[[t]] = res$candidates[[1]][[1]][[1]][["text"]]
  } else {
    df$strategy_response[[t]] = res$text
  }
  
  

  game$player_dfs = df
  game

}


player_make_q_prompt = function(game){
  
  restore.point("player_make_q_prompt")
  df = game$player_dfs
  t = game$cur_round
  hist_text = df$hist_text[[t]]
  if(IS_ON_GHA){
    strategy_response_text = df$strategy_response[[t]]
  } else {
    strategy_response_text = ""
  }

  # Define the template with the history text included
  
  prompt <- paste0(
      "Let us do a small simulation experiment. I understand that you're a language model and not a decision-maker, but let's simulate what a participant in this experiment might do.\n",
      "In this experiment you repeatedly make decisions with the goal to maximize your profit.\n",
      "You will stay anonymous for us.\n",
      "In this experiment you represent a firm that produces and sells one product on a market. Your firm is the only firm in this market, so you have a monopoly in this market. Costs of production are 1$ per unit.\n",
      "In each round, you have to make one decision, namely what quantity you want to produce. The following important rule holds: The larger the quantity you choose, the smaller the price in the market. Moreover, the price will be zero from a certain amount of total output upwards.\n",
      "Your profit per unit of output will be the difference between the market price and the unit cost of 1$. Note that you will make a loss if the market price is below the unit costs.\n",
      "Your profit per round is, thus, equal to the profit per unit multiplied by the number of Units you sell.\n",
      "In each round the output decisions of you will be registered, the corresponding price will be determined and the profits will be computed.\n",
      "We play a total of 25 rounds. In each round you can write down a strategic plan and choose a quantity between 0 and 100 in 0.01 steps.\n\n",
      "To help you for the plan and simulate what quantity a participant might choose, we have provided you with a profit calculator, which allows you to calculate your own profit based on your quantity. The function (exemplarily written in the R programming language) is as follows:\n",
     
       "  profit_calculator <- function(q_i) {\n",
      "  #q_i is the chosen quantitiy of you in this round \n",
      "  p <- max(100 - q_i, 0)    # market price based on the quantity\n",
      "  revenue <- p * q_i\n",
      "  costs <- q_i\n",
      "  profit <- revenue - costs\n",
      "  return(profit)\n",
      "}\n\n",
      hist_text, "\n\n", 
      ###With Function
      "\nAs you have information about the quantity function: q = max(100-p,0) and profit pi = p*q - q, you might want to calculate the quantity to reach profit maximum (or you know it from your knowledge about profit maximum in monopoly situation).\n",
      
      "Additionally, I provide you with a plan which you wrote to figure out the quantity you want to provide in this round:", "\n\n", 
      strategy_response_text, "\n\n", 
      "Based on These information, I would like you to simulate what quantity a participant might choose in the next round to maximize profit, considering the provided rules.\n",
      "You don't need to actually participate in the experiment or make a decision— just simulate what could happen if a participant followed these rules.\n",
      "Just set the quantity according to your strategy and the information. It's okay if you're not 100% sure, as we play several rounds, you can gain information from each round for the next round.\n",
      "Please write down the quantity for the next round without any explanation/justification down below:\n",
      '<fill in quantity here, no explanation/justification>'
  )
  
  
  df$q_prompt[t] = prompt
  game$player_dfs = df
  game
}



player_run_q_prompt = function(game, i, attempt=1, max_attempts = 10, api_key, start_time) {
  library(jsonlite)
  restore.point("player_run_q_prompt")
  t = game$cur_round
  df = game$player_dfs

  prompt = df$q_prompt[[t]]

  if (IS_ON_GHA) {

  
    res = run_gemini(prompt,api_key, model="gemini-1.5-flash", json_mode=FALSE, temperature= 1 , add_prompt=FALSE, verbose=TRUE)
    cur_time = as.numeric(Sys.time())
    Sys.sleep(5)
    if (cur_time - start_time > MAX_RUNTIME_SEC) {
      return()
    }
    
  } else {
    res = list(
      ok = TRUE,
      text = df$q[t]
      
    )
  }
  q = NULL
  if(IS_ON_GHA){
    if (res$ok) {
      q = try({
        as.numeric(res$candidates[[1]][[1]][[1]][["text"]])
      })
    }
  } else {
    res$ok = TRUE
    if (res$ok) {
      q = try({
        res$text
      })
    }
  }

  
  # Possible errors
  is_err = !res$ok & is(q,"try-error")
  if (!is_err) {
    is_err = is.na(q)
  }
  
  if (is_err) {
    if (attempt > max_attempts) {
      # Build that function which stops all
      stop_game(game, "Error in make q")
    }
    cat("\n: i = ",i," attempt neu = ",attempt+1,"start time = ",start_time,"\n")
    game = player_run_q_prompt(game,i,attempt=attempt+1, api_key = api_key, start_time = start_time)
    return(game)
  }
  if (IS_ON_GHA){
    df$q[[t]] = as.numeric(res$candidates[[1]][[1]][[1]][["text"]])
  } else {
    df$q[[t]] = res$text 
    
  }
  game$player_dfs = df
  game

}


player_make_history_text = function(game) {
  restore.point("player_make_history_text")

  df = game$player_dfs
  t = game$cur_round

  # Build history text from past prices, q and profits for the player

  if(t==1){
    hist_text = "\nAs this is the first round, there is no historical market information.\n"
  } else {
    hist_text = "Furthermore, to help you, we have listed the quantities you have placed so far, the resulting market prices and your profits:\n\n"
    
    for (round in 1:(t-1)) {
      # Add the results of each round to the history text
      hist_text = paste0(hist_text,
                         "Round ", round, ":\n",
                         "Your quantity: ", df$q[round], "\n",
                         "Market price: ", round(df$p[round], 2), "\n",
                         "Your profit: ", round(df$pi[round], 2), "\n\n")
    }
  }
  
  df$hist_text[t] = hist_text

  game$player_dfs = df

  game

}

player_make_strategy_prompt = function(game){
  # Update game with the current round's history text
  game <- player_make_history_text(game)
  
  # Get the history text for the current round
  df <- game$player_dfs
  t <- game$cur_round
  hist_text <- df$hist_text[[t]]
  if (t > 1) {
    strategy_round_before = df$strategy_response[[t-1]]
  } else {
    strategy_round_before = "\nAs this is the first round, there is no previous strategy.\n"
  }

  # Define the template with the history text included
  prompt_text <- paste0(
    "Let us do a small simulation experiment. I understand that you're a language model and not a decision-maker, but let's simulate what a participant in this experiment might do.\n",
    "In this experiment you repeatedly make decisions with the goal to maximize your profit.\n",
    "You will stay anonymous for us.\n",
    "In this experiment you represent a firm that produces and sells one product on a market. Your firm is the only firm in this market, so you have a monopoly in this market. Costs of production are 1$ per unit.\n",
    "In each round, you have to make one decision, namely what quantity you want to produce. The following important rule holds: The larger the quantity you choose, the smaller the price in the market. Moreover, the price will be zero from a certain amount of total output upwards.\n",
    "Your profit per unit of output will be the difference between the market price and the unit cost of 1$. Note that you will make a loss if the market price is below the unit costs.\n",
    "Your profit per round is, thus, equal to the profit per unit multiplied by the number of Units you sell.\n",
    "In each round the output decisions of you will be registered, the corresponding price will be determined and the profits will be computed.\n",
    "We play a total of 25 rounds. In each round you can write down a strategic plan and choose a quantity between 0 and 100 in 0.01 steps.\n\n",
    "To help you for the plan and simulate what quantity a participant might choose, we have provided you with a profit calculator, which allows you to calculate your own profit based on your quantity. The function (exemplarily written in the R programming language) is as follows:\n",
    
    "profit_calculator <- function(q_i) {\n",
    "  #q_i is the chosen quantitiy of you in this round \n",
    "  p <- max(100 - q_i, 0)    # market price based on the quantity\n",
    "  revenue <- p * q_i\n",
    "  costs <- q_i\n",
    "  profit <- revenue - costs\n",
    "  return(profit)\n",
    "}\n\n",
 
    hist_text, "\n\n",  
    "In Addition, I provide you with the strategy you wrote down for the last round: \n",
    strategy_round_before,
    "\n",
    ###With Function
    "\nAs you have information about the quantity function : q = max(100-p,0) and profit pi = p*q - q, you might want to calculate the quantity to reach profit maximum (or you know it from your knowledge about profit maximum in monopoly situation).\n",
    
    "Based on these information, I would like you to write down your plans for what quantity strategy to run in the next round. Be detailed and precise but keep things succinct, while considering the provided rules.\n",
    "Simply write the plan for the next round in the field provided.\n",
    "Plan in the next round: <fill in plan here>\n"
  )
  
  df$strategy_prompt[t] = prompt_text
  
  game$player_dfs = df
  
  
  game
  
}


game_compute_round_results= function(game){
  df = game$player_dfs
  t = game$cur_round

  #Adding the figures of each round to the player
  
  period_results = simulate_period(df$q[[t]]) 
  df$pi[[t]] =  period_results$pi
  df$p[[t]] =  period_results$price

  

  
  game$player_dfs = df
  game
  
}
