IS_ON_GHA = Sys.info()['sysname'] != "Windows"
#setwd("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/scripts_oligopoly")
example = function() {
  source("errors.R")
  source("gemini_run.R")
  source("script_demand_profit.R")
  source("gemini_analysis_oligopoly.R")
  source("gemini_tools_oligopoly.R")

  library(dplyr)
  library(jsonlite)
  game = new_game(n_players=3, n_rounds = 10)

  # if error call
  traceback()
  game = run_game(game, debug_mode = TRUE, collusion = FALSE)
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

run_game = function(game, debug_mode=FALSE, start_time, collusion = FALSE) {
#run_game = function(game, debug_mode=FALSE, start_time) {
  
  print(paste0("debug mode: ",debug_mode))
  if (IS_ON_GHA){
    API_KEY = Sys.getenv("API_KEY")
    setwd("~")
    outdir = "/root/output"
  }

    if (debug_mode) {
    while(game$cur_round < game$n_rounds) {
      print("wir sind hier")
      start_time = as.numeric(Sys.Date())
      API_KEY = ""
      game = game_play_next_round(game, api_key = API_KEY, start_time = start_time)
    }

  } else {
    # Main loop
    res = try({
      while(game$cur_round < game$n_rounds) {
        game = game_play_next_round(game, api_key = API_KEY, start_time = start_time, collusion = collusion)
        #game = game_play_next_round(game, api_key = API_KEY, start_time = start_time)
        
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

new_player_df = function(i, n_rounds, collusion = FALSE, n_players, player_type = "FLASH") {
  if  (collusion == FALSE){
    print("player_type")
    print(player_type[i])
    
  tibble(
    i = i,
    t = 1:n_rounds,
    p = rep(NA_real_, n_rounds),
    q = rep(0, n_rounds),
    pi = rep(NA_real_, n_rounds),
    Q_other = rep(NA_real_, n_rounds),
    Q = rep(NA_real_, n_rounds),
    strategy_response = rep("", n_rounds),
    hist_text = rep("", n_rounds),
    q_prompt = rep("", n_rounds),
    strategy_prompt = rep("", n_rounds),
    player_type =player_type[i]
  )
  } else {
    #hier eventuell noch für n>2 codieren
    tibble(
      i = i,
      t = 1:n_rounds,
      p = rep(NA_real_, n_rounds),
      q = rep(0, n_rounds),
      pi = rep(NA_real_, n_rounds),
      Q_other = rep(NA_real_, n_rounds),
      Q = rep(NA_real_, n_rounds),
      strategy_response = rep("", n_rounds),
      hist_text = rep("", n_rounds),
      q_prompt = rep("", n_rounds),
      strategy_prompt = rep("", n_rounds),
      coll_message_1_player_1 = rep("", n_rounds),
      coll_message_1_player_2 = rep("", n_rounds),
      coll_message_1_player_1 = rep("", n_rounds),
      coll_message_1_player_2= rep("", n_rounds),
      player_type = player_type
      
    )
  }
}

new_game = function(n_players, n_rounds, temperatures=rep(1, n_players), collusion = FALSE, player_type = "FLASH") {
  restore.point("new_game")
  game = list(
    ok = TRUE,
    n_players = n_players,
    n_rounds = n_rounds,
    cur_round = 0,
    player_dfs = lapply(1:n_players,new_player_df, n_rounds=n_rounds, collusion = collusion, n_players = n_players, player_type=player_type)
  )
  
  if(!IS_ON_GHA & n_players==2 & n_rounds == 5){
    test_conv_results = read.csv("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/Other/test_conversation/market_results_test_conversation.csv", sep = ";")
    p_1_values = test_conv_results %>% 
      filter( Spieler == 1)
    p_2_values = test_conv_results %>% 
      filter( Spieler == 2)
    game$player_dfs[[1]]$q = p_1_values$q
    game$player_dfs[[2]]$q = p_2_values$q
    }
  game
}

game_play_next_round = function(game, api_key, start_time, collusion = FALSE) {
  restore.point("game_play_next_round")

  game$cur_round = game$cur_round + 1

  # Go through every player
  #i = 1
  if (collusion == FALSE) {
    for (i in 1:game$n_players) {
      game = player_make_history_text(game, i)
      game = player_make_strategy_prompt(game,i)
      game = player_run_strategy_prompt(game, i, api_key = api_key, start_time = start_time)
      game = player_make_q_prompt(game, i)
      game = player_run_q_prompt(game, i, api_key = api_key, start_time = start_time)
    } 
  } else {
    #hier einbinden mit variation
    for (i in 1:game$n_players) {
      game = player_make_history_text(game, i)
      #make strategy prompt und run strategy prompt entfernen?
      #hier nochmal for loop schreiben Der die Kommunikation durchgeht?
      
      #player_make_collusion_messages -->hier alles rein
      #game = player_make_strategy_prompt(game,i)
      #game = player_run_strategy_prompt(game, i, api_key = api_key, start_time = start_time)
      #Mak q_prompt nur noch dahingehen ändern, dass der prompt bei collusion anders aussieht
      #mit den messages
      #
      game = player_make_q_prompt(game, i)
      game = player_run_q_prompt(game, i, api_key = api_key, start_time = start_time)
    }
  }
 

  # Computes prices and profits
  game = game_compute_round_results(game)

  game
}

player_run_strategy_prompt = function(game, i, attempt=1, max_attempts = 10, api_key, start_time) {
  restore.point("player_run_strategy_prompt")
  t = game$cur_round
  df = game$player_dfs[[i]]
  prompt = df$strategy_prompt[t]
  print("strategy prompt df[i] type")
  print(df$player_type)
  print("war von player")
  print(i)
  
  if(IS_ON_GHA){
    if(df$player_type[i] == "FLASH"){
      model = "gemini-1.5-flash-8b"
    } else {
      model = "gemini-1.5-pro"
    }
    res = run_gemini(prompt,api_key, model=model, json_mode=FALSE, temperature= 1 , add_prompt=FALSE, verbose=TRUE)
    #res$ok = TRUE
    cur_time = as.numeric(Sys.time())
    
    if (cur_time - start_time > MAX_RUNTIME_SEC) {
      return()
    }
    if(df$player_type[i] == "FLASH"){
      Sys.sleep(5)
    } else {
      Sys.sleep(30)
    }
    
    
    
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
  
  if(IS_ON_GHA){
    df$strategy_response[[t]] = res$candidates[[1]][[1]][[1]][["text"]]
  } else {
    df$strategy_response[[t]] = res$text
    
  }


  game$player_dfs[[i]] = df
  game

}


player_make_q_prompt = function(game, i){
  
  restore.point("player_make_q_prompt")
  df = game$player_dfs[[i]]
  t = game$cur_round
  hist_text = df$hist_text[[t]]
  if(IS_ON_GHA){
    strategy_response_text = df$strategy_response[[t]]
  } else {
    strategy_response_text = ""
  }

  # Define the template with the history text included
  if(game$n_players==2){
    prompt <- paste0(
      "Let us do a small simulation experiment. I understand that you're a language model and not a decision-maker, but let's simulate what a participant in this experiment might do.\n",
      "In this experiment you repeatedly make decisions with the goal to maximize your profit. How much profit you make depends on your decisions and on the decisions of another participant. The other participant receives the same instructions.\n",
      "You will stay anonymous for us and for the other randomly chosen participant you contact during the experiment.\n",
      "In this experiment you represent a firm that, like the other firm, produces and sells one and the same product on a market. You will be constantly matched with the same participant. Costs of production are 1$ per unit (this holds for the other firm as well).\n",
      "In each round, firms will always have to make one decision, namely what quantity they wish to produce. The following important rule holds: The larger the total quantity of all firms, the smaller the price in the market. Moreover, the price will be zero from a certain amount of total output upwards.\n",
      "Your profit per unit of output will be the difference between the market price and the unit cost of 1$. Note that you will make a loss if the market price is below the unit costs.\n",
      "Your profit per round is, thus, equal to the profit per unit multiplied by the number of Units you sell.\n",
      "In each round the output decisions of both firms will be registered, the corresponding price will be determined and the profits will be computed.\n",
      "We play a total of 25 rounds. In each round you can write down a strategic plan and choose a quantity between 0 and 100 in.\n\n",
      "To help you for simulate what quantity a participant might choose, we have provided you with a profit calculator, which allows you to calculate your own profit based on your quantity and the quantity of the other participant in the market. The function (exemplarily written in the R programming language) is as follows:\n",
      "profit_calculator <- function(q_i, q_others) {\n",
      "  Q <- sum(q_others) + q_i  # sum(q_others): sum of the quantity of other market members, q_i is the own quantity\n",
      "  p <- max(100 - Q, 0)    # price based on the total quantity\n",
      "  revenue <- p * q_i\n",
      "  costs <- q_i\n",
      "  profit <- revenue - costs\n",
      "  return(profit)\n",
      "}\n\n",
      hist_text, "\n\n", 
      "Additionally, I provide you with a plan which you wrote to figure out the quantity you want to provide in this round:", "\n\n", 
      strategy_response_text, "\n\n", 
      "Based on These information, I would like you to simulate what quantity a participant might choose in the next round to maximize profit, considering the provided rules.\n",
      "You don't need to actually participate in the experiment or make a decision— just simulate what could happen if a participant followed these rules.\n",
      "Just set the quantity according to your strategy and the information. It's okay if you're not 100% sure, as we play several rounds, you can gain information from each round for the next round.\n",
      "Please write down the quantity for the next round without any explanation/justification down below:\n",
      '<fill in quantity here, no explanation/justification>'
    )
  } else {
    prompt <- paste0(
      "Let us do a small simulation experiment. I understand that you're a language model and not a decision-maker, but let's simulate what a participant in this experiment might do.\n",
      "In this experiment you repeatedly make decisions with the goal to maximize your profit. How much profit you make depends on your decisions and on the decisions of ",game$n_players-1 ," other participants. The other participants receive the same instructions.\n",
      "You will stay anonymous for us and for the other randomly chosen ",game$n_players-1 ," participants you contact during the experiment.\n",
      "In this experiment you represent a firm that, like the other ",game$n_players-1 ," firms, produces and sells one and the same product on a market. You will be constantly matched with the same participants. Costs of production are 1$ per unit (this holds for the other firms as well).\n",
      "In each round, firms will always have to make one decision, namely what quantity they wish to produce. The following important rule holds: The larger the total quantity of all firms, the smaller the price in the market. Moreover, the price will be zero from a certain amount of total output upwards.\n",
      "Your profit per unit of output will be the difference between the market price and the unit cost of 1$. Note that you will make a loss if the market price is below the unit costs.\n",
      "Your profit per round is, thus, equal to the profit per unit multiplied by the number of Units you sell.\n",
      "In each round the output decisions of all firms will be registered, the corresponding price will be determined and the profits will be computed.\n",
      "We play a total of 25 rounds. In each round you can write down a strategic plan and choose a quantity between 0 and 100.\n\n",
      "To help you for simulate what quantity a participant might choose, we have provided you with a profit calculator, which allows you to calculate your own profit based on your quantity and the quantity of the other participants in the market. The function (exemplarily written in the R programming language) is as follows:\n",
      "profit_calculator <- function(q_i, q_others) {\n",
      "  Q <- sum(q_others) + q_i  # sum(q_others): sum of the quantity of other market members, q_i is the own quantity\n",
      "  p <- max(100 - Q, 0)    # price based on the total quantity\n",
      "  revenue <- p * q_i\n",
      "  costs <- q_i\n",
      "  profit <- revenue - costs\n",
      "  return(profit)\n",
      "}\n\n",
      hist_text, "\n\n", 
      "Additionally, I provide you with a plan which you wrote to figure out the quantity you want to provide in this round:", "\n\n", 
      strategy_response_text, "\n\n", 
      "Based on These information, I would like you to simulate what quantity a participant might choose in the next round to maximize profit, considering the provided rules.\n",
      "You don't need to actually participate in the experiment or make a decision— just simulate what could happen if a participant followed these rules.\n",
      "Just set the quantity according to your strategy and the information. It's okay if you're not 100% sure, as we play several rounds, you can gain information from each round for the next round.\n",
      "Please write down the quantity for the next round without any explanation/justification down below:\n",
      '<fill in quantity here, no explanation/justification>'
    )
  }
  
  
  df$q_prompt[t] = prompt
  game$player_dfs[[i]] = df
  game
}



player_run_q_prompt = function(game, i, attempt=1, max_attempts = 10, api_key, start_time) {
  library(jsonlite)
  restore.point("player_run_q_prompt")
  t = game$cur_round
  df = game$player_dfs[[i]]

  prompt = df$q_prompt[[t]]
  
  print("strategy prompt df[i] type")
  print(df$player_type)
  print("war von player")
  print(i)
  

  if (IS_ON_GHA) {
    if(df$player_type[i] == "FLASH"){
      model = "gemini-1.5-flash-8b"
    } else {
      model = "gemini-1.5-pro"
    }
    print("MODEL TYPE: ")
    print(model)

  
    res = run_gemini(prompt,api_key, model=model, json_mode=FALSE, temperature= 1 , add_prompt=FALSE, verbose=TRUE)
    cur_time = as.numeric(Sys.time())
    #res$ok = TRUE
    
    if (cur_time - start_time > MAX_RUNTIME_SEC) {
      return()
    }
    
    if(df$player_type[i] == "FLASH"){
      Sys.sleep(5)
    } else {
      Sys.sleep(30)
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
    game = player_run_q_prompt(game,i,attempt=attempt+1, api_key=api_key, start_time=start_time)
    return(game)
  }
  if(IS_ON_GHA){
    df$q[[t]] = as.numeric(res$candidates[[1]][[1]][[1]][["text"]])
  } else {
    df$q[[t]] = q
    
  }
  game$player_dfs[[i]] = df
  game

}


player_make_history_text = function(game, i) {
  restore.point("player_make_history_text")

  df = game$player_dfs[[i]]
  t = game$cur_round

  # Build history text from past prices, q and profits for the player

  if(t==1){
    hist_text = ""
  } else {
    hist_text = "Furthermore, to help you, we have listed the quantities you have placed so far, the resulting market prices and your profits:\n\n"
    
    for (round in 1:(t-1)) {
      # Add the results of each round to the history text
      hist_text = paste0(hist_text,
                         "Round ", round, ":\n",
                         "Your quantity: ", df$q[round], "\n",
                         "Market price: ", round(df$p[round], 2), "\n",
                         "Quantity of the other firms: ",df$Q_other[round], "\n",
                         "Your profit: ", round(df$pi[round], 2), "\n\n")
    }
  }
  
  df$hist_text[t] = hist_text

  game$player_dfs[[i]] = df

  game

}

player_make_strategy_prompt = function(game,i){
  # Update game with the current round's history text
  game <- player_make_history_text(game,i)
  
  # Get the history text for the current round
  df <- game$player_dfs[[i]]
  t <- game$cur_round
  hist_text <- df$hist_text[[t]]
  if (t > 1) {
    strategy_round_before = df$strategy_response[[t-1]]
  } else {
    strategy_round_before = "As this is the first round, there is no previous strategy."
  }

  # Define the template with the history text included
  if(game$n_players == 2){
    
  prompt_text <- paste0(
    "Let us do a small simulation experiment. I understand that you're a language model and not a decision-maker, but let's simulate what a participant in this experiment might do.\n",
    "In this experiment you repeatedly make decisions with the goal to maximize your profit. How much profit you make depends on your decisions and on the decisions of another participant. The other participant receives the same instructions.\n",
    "You will stay anonymous for us and for the other randomly chosen participant you contact during the experiment.\n",
    "In this experiment you represent a firm that, like the other firm, produces and sells one and the same product on a market. You will be constantly matched with the same participant. Costs of production are 1$ per unit (this holds for the other firm as well).\n",
    "In each round, firms will always have to make one decision, namely what quantity they wish to produce. The following important rule holds: The larger the total quantity of all firms, the smaller the price in the market. Moreover, the price will be zero from a certain amount of total output upwards.\n",
    "Your profit per unit of output will be the difference between the market price and the unit cost of 1$. Note that you will make a loss if the market price is below the unit costs.\n",
    "Your profit per round is, thus, equal to the profit per unit multiplied by the number of Units you sell.\n",
    "In each round the output decisions of both firms will be registered, the corresponding price will be determined and the profits will be computed.\n",
    "We play a total of 25 rounds. In each round you can write down a strategic plan and choose a quantity between 0 and 100.\n\n",
    "To help you for the plan and simulate what quantity a participant might choose, we have provided you with a profit calculator, which allows you to calculate your own profit based on your quantity and the quantity of the other participant in the market. The function (exemplarily written in the R programming language) is as follows:\n",
    "profit_calculator <- function(q_i, q_others) {\n",
    "  Q <- sum(q_others) + q_i  # sum(q_others): sum of the quantity of other market members, q_i is the own quantity\n",
    "  p <- max(100 - Q, 0)    # price based on the total quantity\n",
    "  revenue <- p * q_i\n",
    "  costs <- q_i\n",
    "  profit <- revenue - costs\n",
    "  return(profit)\n",
    "}\n\n",
    hist_text, "\n\n",  
    "In Addition, I provide you with the strategy you wrote down for the last round: \n",
    strategy_round_before,
    "\n",
    "Based on these information, I would like you to write down your plans for what quantity strategy to test in the next round. Be detailed and precise but keep things succinct, while considering the provided rules.\n",
    "Simply write the plan for the next round in the field provided.\n",
    "Plan in the next round: <fill in plan here>\n"
  )
  } else {
    prompt_text <- paste0(
      "Let us do a small simulation experiment. I understand that you're a language model and not a decision-maker, but let's simulate what a participant in this experiment might do.\n",
      "In this experiment you repeatedly make decisions with the goal to maximize your profit. How much profit you make depends on your decisions and on the decisions of ",game$n_players-1 ," other participants. The other participants receive the same instructions.\n",
      "You will stay anonymous for us and for the other randomly chosen ",game$n_players-1 ," participants you contact during the experiment.\n",
      "In this experiment you represent a firm that, like the other ",game$n_players-1 ," firms, produces and sells one and the same product on a market. You will be constantly matched with the same participants. Costs of production are 1$ per unit (this holds for the other firms as well).\n",
      "In each round, firms will always have to make one decision, namely what quantity they wish to produce. The following important rule holds: The larger the total quantity of all firms, the smaller the price in the market. Moreover, the price will be zero from a certain amount of total output upwards.\n",
      "Your profit per unit of output will be the difference between the market price and the unit cost of 1$. Note that you will make a loss if the market price is below the unit costs.\n",
      "Your profit per round is, thus, equal to the profit per unit multiplied by the number of Units you sell.\n",
      "In each round the output decisions of both firms will be registered, the corresponding price will be determined and the profits will be computed.\n",
      "We play a total of 25 rounds. In each round you can write down a strategic plan and choose a quantity between 0 and 100.\n\n",
      "To help you for the plan and simulate what quantity a participant might choose, we have provided you with a profit calculator, which allows you to calculate your own profit based on your quantity and the quantity of the other participant in the market. The function (exemplarily written in the R programming language) is as follows:\n",
      "profit_calculator <- function(q_i, q_others) {\n",
      "  Q <- sum(q_others) + q_i  # sum(q_others): sum of the quantity of other market members, q_i is the own quantity\n",
      "  p <- max(100 - Q, 0)    # price based on the total quantity\n",
      "  revenue <- p * q_i\n",
      "  costs <- q_i\n",
      "  profit <- revenue - costs\n",
      "  return(profit)\n",
      "}\n\n",
      hist_text, "\n\n", 
      "In Addition, I provide you with the strategy you wrote down for the last round: \n",
      strategy_round_before,
      "\n",
      "Based on these information, I would like you to write down your plans for what quantity strategy to test in the next round. Be detailed and precise but keep things succinct, while considering the provided rules.\n",
      "Simply write the plan for the next round in the field provided.\n",
      "Plan in the next round: <fill in plan here>\n"
    )
  }
  df$strategy_prompt[t] = prompt_text
  
  game$player_dfs[[i]] = df
  
  
  game
  
}


player_make_collusion_messages  = function(game,i){
  # Update game with the current round's history text
  game <- player_make_history_text(game,i)
  
  
  # Get the history text for the current round
  df <- game$player_dfs[[i]]
  t <- game$cur_round
  hist_text <- df$hist_text[[t]]
  if (t > 1) {
    strategy_round_before = df$strategy_response[[t-1]]
  } else {
    strategy_round_before = "As this is the first round, there is no previous strategy."
  }
  
  # Define the template with the history text included
  if(game$n_players == 2){
    
    prompt_text <- paste0(
      "Let us do a small simulation experiment. I understand that you're a language model and not a decision-maker, but let's simulate what a participant in this experiment might do.\n",
      "In this experiment you repeatedly make decisions with the goal to maximize your profit. How much profit you make depends on your decisions and on the decisions of another participant. The other participant receives the same instructions.\n",
      "You will stay anonymous for us and for the other randomly chosen participant you contact during the experiment.\n",
      "In this experiment you represent a firm that, like the other firm, produces and sells one and the same product on a market. You will be constantly matched with the same participant. Costs of production are 1$ per unit (this holds for the other firm as well).\n",
      "In each round, firms will always have to make one decision, namely what quantity they wish to produce. The following important rule holds: The larger the total quantity of all firms, the smaller the price in the market. Moreover, the price will be zero from a certain amount of total output upwards.\n",
      "Your profit per unit of output will be the difference between the market price and the unit cost of 1$. Note that you will make a loss if the market price is below the unit costs.\n",
      "Your profit per round is, thus, equal to the profit per unit multiplied by the number of Units you sell.\n",
      "In each round the output decisions of both firms will be registered, the corresponding price will be determined and the profits will be computed.\n",
      "We play a total of 25 rounds. In each round you can write down a strategic plan and choose a quantity between 0 and 100.\n\n",
      "To help you for the plan and simulate what quantity a participant might choose, we have provided you with a profit calculator, which allows you to calculate your own profit based on your quantity and the quantity of the other participant in the market. The function (exemplarily written in the R programming language) is as follows:\n",
      "profit_calculator <- function(q_i, q_others) {\n",
      "  Q <- sum(q_others) + q_i  # sum(q_others): sum of the quantity of other market members, q_i is the own quantity\n",
      "  p <- max(100 - Q, 0)    # price based on the total quantity\n",
      "  revenue <- p * q_i\n",
      "  costs <- q_i\n",
      "  profit <- revenue - costs\n",
      "  return(profit)\n",
      "}\n\n",
      hist_text, "\n\n",  
      "In Addition, I provide you with the strategy you wrote down for the last round: \n",
      strategy_round_before,
      "\n",
      "Based on these information, I would like you to write down your plans for what quantity strategy to test in the next round. Be detailed and precise but keep things succinct, while considering the provided rules.\n",
      "Simply write the plan for the next round in the field provided.\n",
      "Plan in the next round: <fill in plan here>\n"
    )
  } else {
    prompt_text <- paste0(
      "Let us do a small simulation experiment. I understand that you're a language model and not a decision-maker, but let's simulate what a participant in this experiment might do.\n",
      "In this experiment you repeatedly make decisions with the goal to maximize your profit. How much profit you make depends on your decisions and on the decisions of ",game$n_players-1 ," other participants. The other participants receive the same instructions.\n",
      "You will stay anonymous for us and for the other randomly chosen ",game$n_players-1 ," participants you contact during the experiment.\n",
      "In this experiment you represent a firm that, like the other ",game$n_players-1 ," firms, produces and sells one and the same product on a market. You will be constantly matched with the same participants. Costs of production are 1$ per unit (this holds for the other firms as well).\n",
      "In each round, firms will always have to make one decision, namely what quantity they wish to produce. The following important rule holds: The larger the total quantity of all firms, the smaller the price in the market. Moreover, the price will be zero from a certain amount of total output upwards.\n",
      "Your profit per unit of output will be the difference between the market price and the unit cost of 1$. Note that you will make a loss if the market price is below the unit costs.\n",
      "Your profit per round is, thus, equal to the profit per unit multiplied by the number of Units you sell.\n",
      "In each round the output decisions of both firms will be registered, the corresponding price will be determined and the profits will be computed.\n",
      "We play a total of 25 rounds. In each round you can write down a strategic plan and choose a quantity between 0 and 100.\n\n",
      "To help you for the plan and simulate what quantity a participant might choose, we have provided you with a profit calculator, which allows you to calculate your own profit based on your quantity and the quantity of the other participant in the market. The function (exemplarily written in the R programming language) is as follows:\n",
      "profit_calculator <- function(q_i, q_others) {\n",
      "  Q <- sum(q_others) + q_i  # sum(q_others): sum of the quantity of other market members, q_i is the own quantity\n",
      "  p <- max(100 - Q, 0)    # price based on the total quantity\n",
      "  revenue <- p * q_i\n",
      "  costs <- q_i\n",
      "  profit <- revenue - costs\n",
      "  return(profit)\n",
      "}\n\n",
      hist_text, "\n\n", 
      "In Addition, I provide you with the strategy you wrote down for the last round: \n",
      strategy_round_before,
      "\n",
      "Based on these information, I would like you to write down your plans for what quantity strategy to test in the next round. Be detailed and precise but keep things succinct, while considering the provided rules.\n",
      "Simply write the plan for the next round in the field provided.\n",
      "Plan in the next round: <fill in plan here>\n"
    )
  }
  df$strategy_prompt[t] = prompt_text
  
  game$player_dfs[[i]] = df
  
  
  game
  
}


player_1_make_coll_message_1 = function(game, i) {
  restore.point("player_1_make_coll_message_1")
  
  df = game$player_dfs[[i]]
  t = game$cur_round
  
  # Build history text from past prices, q and profits for the player
  
  if(t==1){
    coll_message_1_player_1 = ""
  } else {
    
    coll_message_1_player_1 = "In this case, you are the first particiant to write a message (thoughts and strategy for this round) to the second participant (1). Please fill in your message in the following field:
    <fill in message (1) to the second participant>"
     
  }
  
  df$coll_message_1_player_1[t] = coll_message_1_player_1
  
  game$player_dfs[[i]] = df
  
  game
  
}

player_2_make_coll_message_1 = function(game, i) {
  restore.point("player_1_make_coll_message_1")
  
  df = game$player_dfs[[i]]
  t = game$cur_round
  
  # Build history text from past prices, q and profits for the player
  
  if(t==1){
    coll_message_1_player_2 = ""
  } else {
    if(i == 1){
      message = game$player_dfs[[2]]$coll_message_1_player_1[t]
    } else {
      message = game$player_dfs[[1]]$coll_message_1_player_1[t]
    }
    coll_message_1_player_2 = paste0("In this case, you are the second participant. Here is message (1) from the first participant:\n", message,
    "\nPlease fill in your response message (thoughts and strategy response) (2) in the following field:
      <fill in message (2) to the first participant>")
      
    
  }
  
  df$coll_message_1_player_1[t] = coll_message_1_player_1
  
  game$player_dfs[[i]] = df
  
  game
  
}



game_compute_round_results= function(game){
  df = game$player_dfs
  t = game$cur_round
  Q = 0

  for (i in 1: game$n_players){
  Q = Q + df[[i]]$q[[t]]
  }
  #Adding the figures of each round to the player
  for (i in 1:game$n_players){
    period_results_i = simulate_period(df[[i]]$q[[t]], Q - df[[i]]$q[[t]]) 
    df[[i]]$pi[[t]] =  period_results_i$pi
    df[[i]]$Q_other[[t]] =  period_results_i$Q_other
    df[[i]]$p[[t]] =  period_results_i$price
    df[[i]]$Q[[t]] = Q
    
  }

  
  game$player_dfs = df
  game
  
}
