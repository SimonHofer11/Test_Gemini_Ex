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
    plan_pompt = rep("", n_rounds),
    q_prompt = rep("", n_rounds)
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
    game = player_run_plan_prompt(game, i)

    # TO DO
    # game = player_make_q_prompt(game, i)
    game = player_run_q_prompt(game, i)
  }

  # Computes prices and profits
  # game = game_compute_round_results(game)

  game
}

player_run_plan_prompt = function(game, i, attempt=1, max_attempts = 10) {
  restore.point("player_run_plan_prompt")
  t = game$cur_round
  df = game$player_dfs[[i]]

  prompt = df$plan_pompt[[t]]

  if (IS_ON_GHA) {
    res = run_text_prompt(prompt, max_tries)
  } else {
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


player_make_plan_prompt = function(game, i) {
  restore.point("player_run_plan_prompt")
  df = game$player_dfs[[i]]
  t = game$cur_round
  
  #To Do:
  #Make Strategy prompt übergeben
  prompt = paste0("-- PLAN PROMPT FOR i = ",i," t = ", t)

  df$plan_pompt[t] = prompt
  game$player_dfs[[i]] = df
  game
}


player_run_q_prompt = function(game, i, attempt=1, max_attempts = 10) {
  restore.point("player_run_q_prompt")
  t = game$cur_round
  df = game$player_dfs[[i]]

  prompt = df$q_prompt[[t]]

  if (IS_ON_GHA) {
    res = run_json_prompt(prompt, max_tries)
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

  hist_text = "-- I WILL BE THE HISTORY TEXT--" # TO DO

  df$hist_text[t] = hist_text

  game$player_dfs[[i]] = df

  game

}

make_strategy_prompt = function(player, round, history_text, prev_strategy) {
  # returns prompt as text
  # strategy as text for player, in round,

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
