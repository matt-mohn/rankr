library(tidyverse)
library(dplyr)


initialize <- function(name_vector, description_vector) {
  names <- data.frame(name_vector)
  names$score <- 1000
  colnames(names) <- c("name", "score")
  names$opp_rating <- 0
  names$wins <- 0
  names$losses <- 0
  names$draws <- 0
  names$games <- 0
  names$avg_opp <- 1000
  names$description <- description_vector
  frame_hold <- names
  return(frame_hold)
}

get_desc <- function(frame, name){
    id_row <- match(name, frame$name)
    tgt_full_row <- frame[id_row, ]
    tgt_full_row <- tgt_full_row$description
    return(tgt_full_row[1])
}


update <- function(frame, winner_name, loser_name) {
  win_row <- match(winner_name, frame$name)
  win_full <- frame[win_row, ]

  los_row <- match(loser_name, frame$name)
  los_full <- frame[los_row, ]

  win_init <- win_full$score
  los_init <- los_full$score

  win_full$opp_rating <- win_full$opp_rating + los_full$score
  win_full$wins <- win_full$wins + 1
  win_full$games <- win_full$games + 1
  win_full$avg_opp <- win_full$opp_rating / win_full$games

  los_full$opp_rating <- los_full$opp_rating + win_full$score
  los_full$losses <- los_full$losses + 1
  los_full$games <- los_full$games + 1
  los_full$avg_opp <- los_full$opp_rating / los_full$games

  win_spread <- win_full$wins - win_full$losses
  win_spread <- win_spread * 400
  los_spread <- los_full$wins - los_full$losses
  los_spread <- los_spread * 400
  win_spread <- win_spread + win_full$opp_rating
  los_spread <- los_spread + los_full$opp_rating
  win_full$score <- win_spread / win_full$games
  los_full$score <- los_spread / los_full$games

  frame[win_row, ] <- win_full
  frame[los_row, ] <- los_full
  return(frame)
}

update_tie <- function(frame, winner_name, loser_name) {
  win_row <- match(winner_name, frame$name)
  win_full <- frame[win_row, ]
  
  los_row <- match(loser_name, frame$name)
  los_full <- frame[los_row, ]
  
  win_init <- win_full$score
  los_init <- los_full$score
  
  win_full$opp_rating <- win_full$opp_rating + los_full$score
  win_full$wins <- win_full$wins + 0.5
  win_full$losses <- win_full$losses + 0.5
  win_full$draws <- win_full$draws + 1
  win_full$games <- win_full$games + 1
  win_full$avg_opp <- win_full$opp_rating / win_full$games
  
  los_full$opp_rating <- los_full$opp_rating + win_full$score
  los_full$wins <- los_full$wins + 0.5
  los_full$losses <- los_full$losses + 0.5
  los_full$draws <- los_full$draws + 1
  los_full$games <- los_full$games + 1
  los_full$avg_opp <- los_full$opp_rating / los_full$games
  
  win_spread <- win_full$wins - win_full$losses
  win_spread <- win_spread * 400
  los_spread <- los_full$wins - los_full$losses
  los_spread <- los_spread * 400
  win_spread <- win_spread + win_full$opp_rating
  los_spread <- los_spread + los_full$opp_rating
  win_full$score <- win_spread / win_full$games
  los_full$score <- los_spread / los_full$games
  
  frame[win_row, ] <- win_full
  frame[los_row, ] <- los_full
  return(frame)
}

frame_burn <- function(frame){
  frame <- frame |>
    arrange(-score) |>
    select(-opp_rating)
  
  frame$wins <- frame$wins - 0.5*frame$draws
  frame$losses <- frame$losses - 0.5*frame$draws
  frame$wins <- as.integer(frame$wins)
  frame$losses <- as.integer(frame$losses)
  frame$games <- as.integer(frame$games)
  frame$score <- as.integer(frame$score)
  frame$avg_opp <- as.integer(frame$avg_opp)
  frame$draws <- as.integer(frame$draws)
  
  frame <- frame |>
    select(
      score,
      name,
      description,
      wins,
      losses,
      draws,
      games,
      avg_opp
    )
  
  colnames(frame) <- c("Score",
                       "Name",
                       "Description",
                       "W",
                       "L",
                       "D",
                       "G",
                       "Avg. Opp")
  
  if(" "%in%frame$Description==T)
  {
    frame <- frame |>
      select(-Description)
  }
  return(frame)
}

go_elo <- function(frame_hold) {
  frame <- frame_hold[[1]]
  names <- frame$name
  merger <- combn(names, 2)
  merger <- t(merger)
  merger <- data.frame(merger)
  merger$sorter <- sample(nrow(merger),
    size = nrow(merger),
    replace = FALSE
  )
  merger <- merger |>
    arrange(sorter) |>
    select(-sorter)

  colnames(merger) <- c(
    "candidate_1",
    "candidate_2"
  )
  name_list <- merger
  # Automatically scramble

  no_quit <- F
  i <- 1
  runtime <- 1
  z <- nrow(merger)

  if (length(frame_hold) == 1) {
    aq_list <- c()
    aq_list[[1]] <- "Test_Test"
    frame_hold[[2]] <- aq_list
  }

  if (length(frame_hold) != 1) {
    aq_list <- frame_hold[[2]]
  }

  while (no_quit == F) {
    roster <- left_join(
      x = name_list, y = frame,
      by = c("candidate_1" = "name")
    )
    roster <- roster |>
      select(candidate_1, candidate_2, score, games, opp_rating, wins, losses) |>
      rename(score_1 = score, games_1 = games, opp_rating_1 = opp_rating, wins_1 = wins, losses_1 = losses)

    roster <- left_join(
      x = roster, y = frame,
      by = c("candidate_2" = "name")
    )
    roster <- roster |>
      select(candidate_1, candidate_2, score_1, games_1, opp_rating_1, wins_1, losses_1, score, games, opp_rating, wins, losses) |>
      rename(score_2 = score, games_2 = games, opp_rating_2 = opp_rating, wins_2 = wins, losses_2 = losses)

    # generate win_1 and win_2
    roster <- roster |>
      mutate(numerator = opp_rating_1 + score_2 + 400 * (wins_1 + 1 + losses_1)) |>
      mutate(denominator = wins_1 + losses_1 + 1) |>
      mutate(win_1 = numerator / denominator) |>
      mutate(win_1 = win_1 - score_1) |>
      mutate(numerator = opp_rating_2 + score_1 + 400 * (wins_2 + 1 + losses_2)) |>
      mutate(denominator = wins_2 + losses_2 + 1) |>
      mutate(win_2 = numerator / denominator) |>
      mutate(win_2 = win_2 - score_2) |>
      mutate(potential_gain = abs(win_1) + abs(win_2)) |>
      mutate(game_floor = pmax(games_1, games_2)) |>
      mutate(game_ceil = pmin(games_1, games_2))


    roster <- roster |> filter(game_ceil == min(game_ceil, na.rm = TRUE))


    roster$scrambler <- rnorm(nrow(roster), 0, 50)

    roster$potential_gain <- roster$potential_gain + roster$scrambler

    proceed <- F

    while (proceed == F) {
      if (runtime > 1 & runtime %% 4 == 0) {
        roster <- roster |>
          arrange(-potential_gain)
      }

      if (runtime > 1 & runtime %% 4 == 1)
        {{ roster <- roster |>
          arrange(potential_gain) }}

      if (runtime > 1 & (runtime %% 4 == 2 | runtime %% 4 == 3)) {
        roster <- roster[sample(1:nrow(roster)), ]
      }

      drawn_name <- paste0(roster[1, 1], "_", roster[1, 2])

      if (is.na(match(drawn_name, aq_list)) == T) {
        # Name works, proceed and add to new list
        proceed <- T
        aq_list[[length(aq_list) + 1]] <- drawn_name
      }
      if (proceed == F) {
        # If already drawn, delete
        roster <- roster[-1, ]
      }
    }

    var <- readline()
    var <- as.integer(var)

    if (var == 1) {
      frame <- update(frame, roster[1, 1], roster[1, 2])
    }
    if (var == 2) {
      frame <- update(frame, roster[1, 2], roster[1, 1])
    }
    if (var != 1 & var != 2) {
      no_quit <- T
    }

    runtime <- runtime + 1
  }

  frame_hold[[1]] <- frame
  frame_hold[[2]] <- aq_list
  return(frame_hold)
}

extract_recommended_names <- function(frame, aq_list, runtime, merger)
{
  no_quit <- F
  i <- 1
  z <- nrow(merger)
  
  if(runtime==(z-1))
  {runtime <- 1
  fakey_list <- c()
  fakey_list[[1]] <- "Test_Test"
  aq_list <- fakey_list}
  
    roster <- left_join(
      x = merger, y = frame,
      by = c("candidate_1" = "name")
    )
    roster <- roster |>
      select(candidate_1, candidate_2, score, games, opp_rating, wins, losses) |>
      rename(score_1 = score, games_1 = games, opp_rating_1 = opp_rating, wins_1 = wins, losses_1 = losses)
    
    roster <- left_join(
      x = roster, y = frame,
      by = c("candidate_2" = "name")
    )
    roster <- roster |>
      select(candidate_1, candidate_2, score_1, games_1, opp_rating_1, wins_1, losses_1, score, games, opp_rating, wins, losses) |>
      rename(score_2 = score, games_2 = games, opp_rating_2 = opp_rating, wins_2 = wins, losses_2 = losses)

    # generate win_1 and win_2
    roster <- roster |>
      mutate(numerator = opp_rating_1 + score_2 + 400 * (wins_1 + 1 + losses_1)) |>
      mutate(denominator = wins_1 + losses_1 + 1) |>
      mutate(win_1 = numerator / denominator) |>
      mutate(win_1 = win_1 - score_1) |>
      mutate(numerator = opp_rating_2 + score_1 + 400 * (wins_2 + 1 + losses_2)) |>
      mutate(denominator = wins_2 + losses_2 + 1) |>
      mutate(win_2 = numerator / denominator) |>
      mutate(win_2 = win_2 - score_2) |>
      mutate(potential_gain = abs(win_1) + abs(win_2)) |>
      mutate(game_floor = pmax(games_1, games_2)) |>
      mutate(game_ceil = pmin(games_1, games_2))
    
    
    roster <- roster |> filter(game_ceil == min(game_ceil, na.rm = TRUE))
    
    
    roster$scrambler <- rnorm(nrow(roster), 0, 50)
    
    roster$potential_gain <- roster$potential_gain + roster$scrambler
    
    proceed <- F
    
    while (proceed == F) {
      if (runtime > 1 & runtime %% 4 == 0) {
        roster <- roster |>
          arrange(-potential_gain)
      }
      
      if (runtime > 1 & runtime %% 4 == 1)
      {{ roster <- roster |>
        arrange(potential_gain) }}
      
      if (runtime > 1 & (runtime %% 4 == 2 | runtime %% 4 == 3)) {
        roster <- roster[sample(1:nrow(roster)), ]
      }
      
      drawn_name <- paste0(roster[1, 1], "_", roster[1, 2])
      
      if (is.na(match(drawn_name, aq_list)) == T) {
        # Name works, proceed and add to new list
        proceed <- T
        aq_list[[length(aq_list) + 1]] <- drawn_name
      }
      if (proceed == F) {
        # If already drawn, delete
        roster <- roster[-1, ]
      }
    }

  outlist <- c()
  outlist[[1]] <- roster[1, 1]
  outlist[[2]] <- roster[1, 2]
  outlist[[3]] <- aq_list
  return(outlist)
}

list_setup <- function(names)
{
  merger <- combn(names, 2)
  merger <- t(merger)
  merger <- data.frame(merger)
  merger$sorter <- sample(nrow(merger),
                          size = nrow(merger),
                          replace = FALSE
  )
  merger <- merger |>
    arrange(sorter) |>
    select(-sorter)
  
  colnames(merger) <- c(
    "candidate_1",
    "candidate_2"
  )
  return(merger)
}








