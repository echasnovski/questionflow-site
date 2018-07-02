library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(comperank)

theme_set(theme_bw())

# Doesn't needed. Add just in case.
set.seed(20180703)


# Data --------------------------------------------------------------------
matches_player_status <- snooker_matches %>%
  # Using actually happened matches
  filter(!walkover1, !walkover2) %>%
  select(id, player1Id, player2Id) %>%
  gather(playerNum, playerId, -id) %>%
  left_join(
    y = snooker_players %>% select(id, status), by = c(playerId = "id")
  ) %>%
  count(id, status) %>%
  spread(status, n, fill = 0) %>%
  count(ama, pro)

pro_players <- snooker_players %>%
  filter(status == "pro")

# Matches only between pro players.
pro_matches <- snooker_matches %>%
  # Using actually happened matches
  filter(!walkover1, !walkover2) %>%
  # Filter matches only between pro players
  semi_join(y = pro_players, by = c(player1Id = "id")) %>%
  semi_join(y = pro_players, by = c(player2Id = "id")) %>%
  # Add 'season' column
  left_join(
     y = snooker_events %>% select(id, season), by = c(eventId = "id")
  ) %>%
  # Ensure arranging by end date
  arrange(endDate) %>%
  # Convert to widecr format
  transmute(
    game = seq_len(n()),
    player1 = player1Id, score1, player2 = player2Id, score2,
    matchId = id, eventId, season, endDate,
    # Compute match type ("train", "validation", or "test")
    matchType = findInterval(row_number(), n() * c(0.5, 0.75)) + 1,
    matchType = c("train", "validation", "test")[matchType]
  ) %>%
  as_widecr()

pro_matches_off <- pro_matches %>%
  semi_join(
    y = snooker_events %>% filter(type != "Invitational"),
    by = c(eventId = "id")
  )

# Grid for K factor
k_grid <- 1:100

# Note that seven pro players began to play only in 2017/18 season.


# Functions ---------------------------------------------------------------
join_player_name <- function(tbl, player_data = snooker_players) {
  tbl %>%
    left_join(
      y = player_data %>% select(id, playerName = name), by = c(player = "id")
    )
}

trim_tails <- function(x, low = 0.01, high = 0.99) {
  case_when(
    x < low ~ low,
    x > high ~ high,
    TRUE ~ x
  )
}

# `matches` should be `widecr` data frame with character column `matchType`
  # containing type of match for the experiment: "train", "validation", or
  # "test". `test_type` is a character vector, indicating type of matches on
  # which goodness of fit is computed.
compute_goodness <- function(matches, test_type, k_vec, rate_fun_gen,
                             get_win_prob) {
  map_dfr(k_vec, function(cur_k) {
    cat(cur_k, " ")
    add_iterative_ratings(
      cr_data = matches, rate_fun = rate_fun_gen(cur_k), initial_ratings = 0
    ) %>%
      left_join(y = matches %>% select(game, matchType), by = "game") %>%
      filter(matchType %in% test_type) %>%
      mutate(
        winProb = get_win_prob(rating1Before, score1, rating2Before, score2),
        winProbTrim = trim_tails(winProb),
        result = as.integer(score1 > score2),
        binDev = -(result * log10(winProbTrim) +
                     (1 - result) * log10(1 - winProbTrim)),
        squareError = (result - winProb)^2
      ) %>%
      summarise(
        rmse = sqrt(mean(squareError)),
        binDeviance = mean(binDev)
      )
  }) %>%
    mutate(k = k_vec) %>%
    select(k, rmse, binDeviance)
}

compute_goodness_wrap <- function(data_name, test_type, rate_fun_gen_name,
                                  win_prob_fun_name) {
  matches_tbl <- get(data_name)
  rate_fun_gen <- get(rate_fun_gen_name)
  get_win_prob <- get(win_prob_fun_name)
  
  compute_goodness(
    matches_tbl, test_type, k_grid, rate_fun_gen, get_win_prob
  )
}

get_best_params <- . %>%
  group_by(group) %>%
  slice(which.min(rmse)) %>%
  ungroup() %>%
  arrange(group)

deframe_best_params <- . %>%
  get_best_params() %>%
  select(group, k) %>%
  tibble::deframe()
  

gather_to_longcr <- function(tbl) {
  bind_rows(
    tbl %>% select(-matches("2")) %>% rename_all(funs(gsub("1", "", .))),
    tbl %>% select(-matches("1")) %>% rename_all(funs(gsub("2", "", .)))
  ) %>%
    arrange(game)
}


# Elo ---------------------------------------------------------------------
# This doesn't actually depend on `score1` and `score2`. They are needed for
# functional interface in `compute_goodness`.
elo_win_prob <- function(rating1, score1, rating2, score2, ksi = 400) {
  norm_rating_diff <- (rating2 - rating1) / ksi
  
  1 / (1 + 10^norm_rating_diff)
}

elo_fun_gen <- function(K, ksi = 400) {
  function(rating1, score1, rating2, score2) {
    elo(rating1, score1, rating2, score2, K = K, ksi = ksi)[1, ]
  }
}


# EloBeta -----------------------------------------------------------------
elobeta_win_prob <- function(rating1, score1, rating2, score2, ksi = 400) {
  n <- pmax(score1, score2)
  prob_win_frame <- elo_win_prob(rating1, score1, rating2, score2, ksi)
  
  # Probability that first player wins n frames sooner than second player based
    # on probability of first player to win one frame `prob_win_frame`.
    # Frames are treated as independent games.
  pbeta(prob_win_frame, n, n)
}

elobeta_fun_gen <- function(K, ksi = 400) {
  function(rating1, score1, rating2, score2) {
    prob_win <- elobeta_win_prob(rating1, score1, rating2, score2, ksi)
    
    match_result <- as.integer(score1 > score2)
    delta <- K * (match_result - prob_win)
    
    c(rating1 + delta, rating2 - delta)
  }
}


# EloScale ----------------------------------------------------------------
eloscale_win_prob <- elobeta_win_prob

eloscale_fun_gen <- function(K, ksi = 400) {
  function(rating1, score1, rating2, score2) {
    n <- max(score1, score2)
    prob_win <- elobeta_win_prob(rating1, score1, rating2, score2, ksi)
    
    match_result <- as.integer(score1 > score2)
    delta <- n * K * (match_result - prob_win)
    
    c(rating1 + delta, rating2 - delta)
  }
}


# Validation goodness of fit ----------------------------------------------
pro_matches_valid_all <- pro_matches %>%
  filter(matchType != "test")
pro_matches_valid_off <- pro_matches_off %>%
  filter(matchType != "test")

validation_results <- crossing(
  ratingType = c("elo", "elobeta", "eloscale"),
  dataType = c("all", "off")
) %>%
  mutate(
    dataName = paste0("pro_matches_valid_", dataType),
    testType = "validation",
    rateFunGenName = paste0(ratingType, "_fun_gen"),
    winProbFunName = paste0(ratingType, "_win_prob"),
    goodness = pmap(
      list(dataName, testType, rateFunGenName, winProbFunName),
      compute_goodness_wrap
    )
  ) %>%
  mutate(group = paste0(ratingType, "_", dataType)) %>%
  unnest(goodness) %>%
  select(group, k, rmse, binDeviance)

best_validation_params_data <- get_best_params(validation_results)
best_validation_params <- deframe_best_params(validation_results)

validation_results %>%
  gather(key = goodnessType, value = goodness, rmse, binDeviance) %>%
  ggplot(aes(k, goodness, colour = group)) +
    geom_line() +
    facet_wrap(~ goodnessType) +
    labs("Validation")


# Test goodness of fit ----------------------------------------------------
pro_matches_test_all <- pro_matches
pro_matches_test_off <- pro_matches_off

test_results <- crossing(
  ratingType = c("elo", "elobeta", "eloscale"),
  dataType = c("all", "off")
) %>%
  mutate(
    dataName = paste0("pro_matches_test_", dataType),
    testType = "test",
    rateFunGenName = paste0(ratingType, "_fun_gen"),
    winProbFunName = paste0(ratingType, "_win_prob"),
    goodness = pmap(
      list(dataName, testType, rateFunGenName, winProbFunName),
      compute_goodness_wrap
    )
  ) %>%
  mutate(group = paste0(ratingType, "_", dataType)) %>%
  unnest(goodness) %>%
  select(group, k, rmse, binDeviance)

  # Goodness of fit for best validation parameters
test_results %>%
  semi_join(y = best_validation_params_data, by = c("group", "k"))

best_test_params_data <- get_best_params(test_results)
best_test_params <- deframe_best_params(test_results)

test_results %>%
  gather(key = goodnessType, value = goodness, rmse, binDeviance) %>%
  ggplot(aes(k, goodness, colour = group)) +
    geom_line() +
    facet_wrap(~ goodnessType) +
    labs("Test")


# Exploration of EloBeta with official matches  ---------------------------
elobeta_ratings_off <- pro_matches_off %>%
  rate_iterative(elobeta_fun_gen(5)) %>%
  rename(rating_elobeta = rating_iterative) %>%
  arrange(desc(rating_elobeta)) %>%
  join_player_name()

elobeta_history_off <- pro_matches_off %>%
  add_iterative_ratings(elobeta_fun_gen(5)) %>%
  left_join(
    y = pro_matches %>% select(game, matchType, endDate), by = "game"
  ) %>%
  filter(matchType == "test") %>%
  mutate(
    probWin1 = elobeta_win_prob(rating1Before, score1, rating2Before, score2),
    probWin2 = 1 - probWin1,
    result = as.integer(score1 > score2)
  )

elobeta_history_off %>%
  gather_to_longcr() %>%
  ggplot(aes(endDate, ratingAfter, group = player)) +
    geom_line()

elobeta_history_off %>%
  ggplot(aes(probWin1)) +
    geom_density() +
    facet_wrap(~ result)


# Exploration of EloScale with official matches ---------------------------
eloscale_ratings_off <- pro_matches_off %>%
  rate_iterative(eloscale_fun_gen(1)) %>%
  rename(rating_eloscale = rating_iterative) %>%
  arrange(desc(rating_eloscale)) %>%
  join_player_name()

eloscale_history_off <- pro_matches_off %>%
  add_iterative_ratings(eloscale_fun_gen(1)) %>%
  left_join(
    y = pro_matches %>% select(game, matchType, endDate), by = "game"
  ) %>%
  filter(matchType == "test") %>%
  mutate(
    probWin1 = eloscale_win_prob(rating1Before, score1, rating2Before, score2),
    probWin2 = 1 - probWin1,
    result = as.integer(score1 > score2)
  )

eloscale_history_off %>%
  gather_to_longcr() %>%
  ggplot(aes(endDate, ratingAfter, group = player)) +
    geom_line()

eloscale_history_off %>%
  ggplot(aes(probWin1)) +
    geom_density() +
    facet_wrap(~ result)
