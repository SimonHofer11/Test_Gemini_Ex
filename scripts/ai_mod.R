IS_ON_GHA = Sys.info()['sysname'] != "Windows"

example = function() {
  source("errors.R")
  source("gemini_run.R")

  library(dplyr)
  library(jsonlite)
  #In Funktion: new_game: Generierung eines neuen Spiels
  #In Funktion: game_play_next_round(game): Dann das Spiel als solches

  game = new_game(n_players=2, n_rounds = 10)
  # Runde einzeln durchlaufen lassen über diesen Command.
  #game = game_play_next_round(game)

  # if error call
  traceback()
  #Funktion: run_game: 
  #Ganzes Spiel durchlaufen lassen mit diesem Command
  game = run_game(game, debug_mode = FALSE)
  game$ok
  game$err_msg

  options(warn=2)


}
example

if (IS_ON_GHA) {
  # overwrite restore point
  restore.point = function(...) {}
} else {
  library(restorepoint)
  suppressWarnings(try(rm(restore.point),silent = TRUE))
  #restore.point.options(display.restore.point = TRUE)
}

run_game = function(game, debug_mode=TRUE) {
#Wenn noch Runden zu spielen sind: aktiviere Fkt: game_play_next_round:
  if (debug_mode) {
    while(game$cur_round < game$n_rounds) {
      game = game_play_next_round(game)
    }

  } else {
    # Later the main loop
    res = try({
      while(game$cur_round < game$n_rounds) {
        game = game_play_next_round(game)
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

new_player_df = function(i, n_rounds) {
  tibble(
    i = i,
    t = 1:n_rounds,
    #Hier Preis noch einfügen, welcher ermittelt wird
    p = rep(NA_real_, n_rounds),
    #Hier Menge des Players i aus jeder runde eintragen
    q = rep(NA_real_, n_rounds),
    #Hier Profit eines Spielers einfügen
    pi = rep(NA_real_, n_rounds),
    #Hier den ermittelten Plan der Gemini Antwort eintragen
    plan = rep("", n_rounds),
    #Hier die bisherigen Pläne einfügn
    hist_text = rep("", n_rounds),
    plan_prompt = rep("", n_rounds),
    q_prompt = rep("", n_rounds),
    strategy_prompt = rep("", n_rounds)
  )

}
#kreiert neues Spielobjekt, erster Call
new_game = function(n_players, n_rounds, temperatures=rep(1, n_players)) {
  restore.point("new_game")
  game = list(
    ok = TRUE,
    n_players = n_players,
    n_rounds = n_rounds,
    cur_round = 0,
    #Erklärung: n_rounds wird als Parameter an Funktion new_player_df weitergegeben
    player_dfs = lapply(1:n_players,new_player_df, n_rounds=n_rounds)
  )
  game
}

game_play_next_round = function(game) {
  restore.point("game_play_next_round")
  # Check if finished
  #Hier noch beenden des Spiels anfügen

  game$cur_round = game$cur_round + 1

  # Go through every player
  i = 1
  for (i in 1:game$n_players) {
    game = player_make_history_text(game, i)

    game = player_make_plan_prompt(game, i)
    game = player_make_strategy_prompt(game,i)
    game = player_run_plan_prompt(game, i)

    # TO DO
    game = player_make_q_prompt(game, i)
    game = player_run_q_prompt(game, i)
  }

  # Computes prices and profits
  # game = game_compute_round_results(game)

  game
}

player_run_plan_prompt = function(game, i, attempt=1, max_attempts = 10) {
  restore.point("player_run_plan_prompt")
  t = game$cur_round
  player_make_strategy_prompt(game,i)
  df = game$player_dfs[[i]]
  prompt = df$strategy_prompt[t]
  
  if(IS_ON_GHA){
    #TO-DO Hier noch überprüfen ob geht
    res = run_text_prompt(prompt, max_tries)
    #TO-DO: zudem format so machen dass in Variable nur text steht und in res$text gespeichert wird
  } else{
    res = list(
      ok = TRUE,
      #hier dann prompt übergeben
      text = "-- I am the plan --"
    )

  }

  #Wenn es fehlermeldung gibt/Antwort nicht passt -->nochmals probieren
  if (!res$ok) {
    if (attempt > max_attempts) {
      # Build that function which stops all
      stop_game(game, "Error in make plan")
    }
    game = player_run_plan_prompt(game,i,attempt=attempt+1)
    return(game)
  }
  #Her dann den Text aufnehmen von Gemini, ggf. hier Anpassungen treffen
  df$plan[[t]] = res$text

  game$player_dfs[[i]] = df
  game

}

###Kann man  deleten--< durch strategy_prompt ersetzt
player_make_plan_prompt = function(game, i) {
  restore.point("player_run_plan_prompt")
  df = game$player_dfs[[i]]
  t = game$cur_round
  
  #To Do:
  #Make Strategy prompt übergeben
  prompt = paste0("-- PLAN PROMPT FOR i = ",i," t = ", t)

  df$plan_prompt[t] = prompt
  game$player_dfs[[i]] = df
  game
}

#XXX
player_make_q_prompt = function(game, i){
  #muss template erhalten, zusätzlich: infos aus hist_text sowie strategy_prompt einbetten
  
  restore.point("player_make_q_prompt")
  df = game$player_dfs[[i]]
  t = game$cur_round
  #Doppelte Klammer?
  hist_text = df$hist_text[[t]]
  strategy_text = df$strategy_prompt[[t]]
  #To Do:
  #Make Strategy prompt übergeben
  
  #####
  # Define the template with the history text included
  prompt <- paste0(
    "Let us do a small simulation experiment. I understand that you're a language model and not a decision-maker, but let's simulate what a participant in this experiment might do.\n",
    "In this experiment you repeatedly make decisions with the goal to maximize your profit. How much profit you make depends on your decisions and on the decisions of another participant. The other participant receives the same instructions.\n",
    "You will stay anonymous for us and for the other randomly chosen participant you contact during the experiment.\n",
    "In this experiment you represent a firm that, like the other firm, produces and sells one and the same product on a market. You will be constantly matched with the same participant. Costs of production are 1$ per unit (this holds for the other firm as well).\n",
    "In each round, firms will always have to make one decision, namely what quantity they wish to produce. The following important rule holds: The larger the total quantity of all firms, the smaller the price in the market. Moreover, the price will be zero from a certain amount of total output upwards.\n",
    "Your profit per unit of output will be the difference between the market price and the unit cost of 1$. Note that you will make a loss if the market price is below the unit costs.\n",
    "Your profit per round is, thus, equal to the profit per unit multiplied by the number of Units you sell.\n",
    "In each round the output decisions of both firms will be registered, the corresponding price will be determined and the profits will be computed.\n",
    "We play a total of 25 rounds. In each round you can write down a strategic plan and choose a quantity between 0 and 100 in 0.01 steps.\n\n",
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
    strategy_text, "\n\n", 
    "Based on These information, I would like you to simulate what quantity a participant might choose in the next round to maximize profit, considering the provided rules.\n",
    "You don't need to actually participate in the experiment or make a decision— just simulate what could happen if a participant followed these rules.\n",
    "Simply write the quantity for the next round without explanation/justification in JSON-format:\n",
    "{'q': <fill in quantity here>}"

  )
  
  #prompt = paste0("-- PLAN PROMPT FOR i = ",i," t = ", t)
  
  df$q_prompt[t] = prompt
  game$player_dfs[[i]] = df
  game
}



player_run_q_prompt = function(game, i, attempt=1, max_attempts = 10) {
  restore.point("player_run_q_prompt")
  t = game$cur_round
  df = game$player_dfs[[i]]

  prompt = df$q_prompt[[t]]

  if (IS_ON_GHA) {
    #Hier dann Funktion schreiben mit run_gemini, welche den command übergibt.
    res = run_json_prompt(prompt, max_tries)
    #Muss dann die JSON übernehme
  } else {
    res = list(
      ok = TRUE,
      text = '{"q": 32.5}'
    )
  }
  #TO DO: hier dann die Funktion um q aus res zu extrahieren
  q = NULL
  if (res$ok) {
    q = try({
      obj = fromJSON(res$text)
      as.numeric(obj$q)
    })
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
    game = player_run_q_prompt(game,i,attempt=attempt+1)
    return(game)
  }

  df$q[[t]] = q
  game$player_dfs[[i]] = df
  game

}


player_make_history_text = function(game, i) {
  restore.point("player_make_history_text")

  df = game$player_dfs[[i]]
  t = game$cur_round

  # Build history text from past prices, q and profits for the player

  #hist_text = "-- I WILL BE THE HISTORY TEXT--" # TO DO
  hist_text = "Furthermore, to help you, we have listed the quantities you have placed so far, the resulting market prices and your profits:\n\n"
  
  for (round in 1:(t-1)) {
    # Add the results of each round to the history text
    hist_text = paste0(hist_text,
                       "Round ", round, ":\n",
                       "Your quantity: ", df$q[round], "\n",
                       "Market price: ", round(df$p[round], 2), "\n",
                       "Your profit: ", round(df$pi[round], 2), "\n\n")
  }
  
  df$hist_text[t] = hist_text

  game$player_dfs[[i]] = df

  game

}

#Noch nicht im Projekt integriert
player_make_strategy_prompt = function(game,i){
  # Update game with the current round's history text
  game <- player_make_history_text(game,i)
  
  # Get the history text for the current round
  df <- game$player_dfs[[i]]
  t <- game$cur_round
  hist_text <- df$hist_text[[t]]
  
  # Define the template with the history text included
  prompt_text <- paste0(
    "Let us do a small simulation experiment. I understand that you're a language model and not a decision-maker, but let's simulate what a participant in this experiment might do.\n",
    "In this experiment you repeatedly make decisions with the goal to maximize your profit. How much profit you make depends on your decisions and on the decisions of another participant. The other participant receives the same instructions.\n",
    "You will stay anonymous for us and for the other randomly chosen participant you contact during the experiment.\n",
    "In this experiment you represent a firm that, like the other firm, produces and sells one and the same product on a market. You will be constantly matched with the same participant. Costs of production are 1$ per unit (this holds for the other firm as well).\n",
    "In each round, firms will always have to make one decision, namely what quantity they wish to produce. The following important rule holds: The larger the total quantity of all firms, the smaller the price in the market. Moreover, the price will be zero from a certain amount of total output upwards.\n",
    "Your profit per unit of output will be the difference between the market price and the unit cost of 1$. Note that you will make a loss if the market price is below the unit costs.\n",
    "Your profit per round is, thus, equal to the profit per unit multiplied by the number of Units you sell.\n",
    "In each round the output decisions of both firms will be registered, the corresponding price will be determined and the profits will be computed.\n",
    "We play a total of 25 rounds. In each round you can write down a strategic plan and choose a quantity between 0 and 100 in 0.01 steps.\n\n",
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
    "Based on these information, I would like you to write down your plans for what quantity strategy to test in the next round. Be detailed and precise but keep things succinct, while considering the provided rules.\n",
    "Simply write the plan for the next round in the field provided.\n",
    "Plan in the next round: <fill in plan here>\n"
  )
  df$strategy_prompt[t] = prompt_text
  
  game$player_dfs[[i]] = df
  
  
  game
  
}

make_strategy_prompt = function(game,i) {
  
}

make_q_prompt = function(player, round, history_text, strategy) {
  # prompt to induce AI to return json with quantity
  # {q: quantity as numeric}

}

play_player_round = function(player, round,...) {
  prev_strategy = get_prev_strategy(player, round)
  hist_text = make_history_text(...)

  strat_prompt = make_strategy_prompt(player, round, history_text)

}
