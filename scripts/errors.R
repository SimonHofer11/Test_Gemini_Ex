stop_game = function(game, msg) {
  game$ok = FALSE
  game$err_msg = msg
  .GlobalEnv$ERR_GAME = game
  stop(msg)
}
