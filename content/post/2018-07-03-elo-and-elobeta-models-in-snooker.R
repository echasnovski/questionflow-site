# Data wrangling packages
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(purrr)
# Visualization package
library(ggplot2)
# Package for ratings
suppressPackageStartupMessages(library(comperank))

theme_set(theme_bw())

# Shouldn't be needed. Added just in case.
set.seed(20180703)


# Elo model ---------------------------------------------------------------
#' @details This function is vectorized by all its arguments. Also usage of
#' `...` is crucial to allow supplying unrelated arguments in the future.
#' 
#' @return A probability of player 1 (rating `rating1`) wins in a match with
#'   player 2 (rating `rating2`). Here difference in ratings directly affects
#'   the outcome.
elo_win_prob <- function(rating1, rating2, ksi = 400, ...) {
  norm_rating_diff <- (rating2 - rating1) / ksi
  
  1 / (1 + 10^norm_rating_diff)
}

#' @return A rating function for Elo model that can be supplied to
#'   `comperank::add_iterative_ratings()`.
elo_fun_gen <- function(K, ksi = 400) {
  function(rating1, score1, rating2, score2) {
    comperank::elo(rating1, score1, rating2, score2, K = K, ksi = ksi)[1, ]
  }
}


# EloBeta model -----------------------------------------------------------
#' @details This function is vectorized by all its arguments.
#' 
#' @return A probability of player 1 (rating `rating1`) wins in a match with
#'   player 2 (rating `rating2`). Match is played until either player wins
#'   `frames_to_win` frames. Here difference in ratings directly affects
#'   the probability of winning one frame.
elobeta_win_prob <- function(rating1, rating2, frames_to_win, ksi = 400, ...) {
  prob_frame <- elo_win_prob(rating1 = rating1, rating2 = rating2, ksi = ksi)
  
  # Probability that first player wins `frames_to_win` frames sooner than second
    # player based on probability of first player to win one frame `prob_frame`.
    # Frames are treated as independent games.
  pbeta(prob_frame, frames_to_win, frames_to_win)
}

#' @return Match result in terms of player 1 win: 1 if he/she wins, 0.5 in case
#'   of a draw, and 0 if he/she loses.
get_match_result <- function(score1, score2) {
  # There are no ties in snooker but this handles general case
  near_score <- dplyr::near(score1, score2)
  
  dplyr::if_else(near_score, 0.5, as.numeric(score1 > score2))
}

#' @return A rating function for EloBeta model that can be supplied to
#'   `comperank::add_iterative_ratings()`.
elobeta_fun_gen <- function(K, ksi = 400) {
  function(rating1, score1, rating2, score2) {
    prob_win <- elobeta_win_prob(
      rating1 = rating1, rating2 = rating2,
      frames_to_win = pmax(score1, score2), ksi = ksi
    )
    
    match_result <- get_match_result(score1, score2)
    delta <- K * (match_result - prob_win)
    
    c(rating1 + delta, rating2 - delta)
  }
}


# Experiment data ---------------------------------------------------------
# Function to split cases between "train", "validation", and "test" types
split_cases <- function(n, props = c(0.5, 0.25, 0.25)) {
  breaks <- n * cumsum(head(props, -1)) / sum(props)
  id_vec <- findInterval(seq_len(n), breaks, left.open = TRUE) + 1
  
  c("train", "validation", "test")[id_vec]
}

pro_players <- snooker_players %>% filter(status == "pro")

# Matches only between pro players.
pro_matches_all <- snooker_matches %>%
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
  # Prepare for widecr format
  transmute(
    game = seq_len(n()),
    player1 = player1Id, score1, player2 = player2Id, score2,
    matchId = id, endDate, eventId, season,
    # Compute match type ("train", "validation", or "test") with 50/25/25 split
    matchType = split_cases(n())
  ) %>%
  # Convert to widecr format
  as_widecr()

# Matches only between pro players in not invitational events (which by
  # quantity is dominated by Championship League).
pro_matches_off <- pro_matches_all %>%
  anti_join(
    y = snooker_events %>% filter(type == "Invitational"),
    by = c(eventId = "id")
  )

# Split confirmation
get_split <- . %>% count(matchType) %>% mutate(share = n / sum(n))

# This should give 50/25/25 split (train/validation/test).
pro_matches_all %>% get_split()

# This gives different split because invitational events aren't spread evenly
  # during season. However, this way matches are split based on the same
  # __time__ breaks as in `pro_matches_all`. This ensures that matches with same
  # type represent identical __time periods__.
pro_matches_off %>% get_split()

# Grid for K factor
k_grid <- 1:100


# Experiment functions ----------------------------------------------------
#' @param matches A `longcr` or `widecr` object with column `matchType` (with
#'   type of match for the experiment: "train", "validation", or "test").
#' @param test_type A type of match to be used for computing goodness of fit.
#'   For experiment correctness, all matches with this type should happen later
#'   than all other ("warm-up") matches. This means having bigger values in
#'   `game` column.
#' @param k_vec Vector of "K factor" values to compute goodness of fit.
#' @param rate_fun_gen Function that, given "K factor" value, returns rating
#'   function that can be supplied to `comperank::add_iterative_ratings()`.
#' @param get_win_prob Function to compute rating probability based on
#'   ratings of players (`rating1`, `rating2`) and number of frames needed to
#'   win in a match (`frames_to_win`). __Note__ that it should be vectorized by
#'   all its arguments.
#' @param initial_ratings Initial ratings in format ready for
#'   `comperank::add_iterative_ratings()`.
#' 
#' @details This function computes:
#' - History of iterative ratings after arranging `matches` by `game` column.
#' - For matches with type equals to `test_type`:
#'     - Probability of player 1 winning.
#'     - Match result in terms of player 1 win: 1 if he/she wins, 0.5 in case of
#'     a draw, and 0 if he/she loses.
#' - Goodness of fit in the form of RMSE: square root of mean square error,
#' where "error" is difference between predicted probability and match result.
#' 
#' @return A tibble with columns 'k' for "K factor" and 'goodness' for RMSE
#'   goodness of fit.
compute_goodness <- function(matches, test_type, k_vec, rate_fun_gen,
                             get_win_prob, initial_ratings = 0) {
  cat("\n")
  map_dfr(k_vec, function(cur_k) {
    # Track execution
    cat(cur_k, " ")
    matches %>%
      arrange(game) %>%
      add_iterative_ratings(
        rate_fun = rate_fun_gen(cur_k), initial_ratings = initial_ratings
      ) %>%
      left_join(y = matches %>% select(game, matchType), by = "game") %>%
      filter(matchType %in% test_type) %>%
      mutate(
        # Number of frames needed to win in a match
        framesToWin = pmax(score1, score2),
        # Probability of player 1 winning a match with `frame_to_win` frames
          # needed to win.
        winProb = get_win_prob(
          rating1 = rating1Before, rating2 = rating2Before,
          frames_to_win = framesToWin
        ),
        result = get_match_result(score1, score2),
        squareError = (result - winProb)^2
      ) %>%
      summarise(goodness = sqrt(mean(squareError)))
  }) %>%
    mutate(k = k_vec) %>%
    select(k, goodness)
}

#' A wrapper for `compute_goodness()` to be used with design matrix data.
compute_goodness_wrap <- function(matches_name, test_type, k_vec,
                                  rate_fun_gen_name, win_prob_fun_name,
                                  initial_ratings = 0) {
  matches_tbl <- get(matches_name)
  rate_fun_gen <- get(rate_fun_gen_name)
  get_win_prob <- get(win_prob_fun_name)
  
  compute_goodness(
    matches_tbl, test_type, k_vec, rate_fun_gen, get_win_prob, initial_ratings
  )
}

#' Function to perform experiment.
#' 
#' @param test_type Vector of values for `test_type` for `compute_goodness()`.
#' @param rating_type Names of rating models.
#' @param data_type Suffixes of data types.
#' @param k_vec,initial_ratings Values for `compute_goodnes()`
#' 
#' @details This function generates design matrix and computes multiple values
#' of goodness of fit for different combinations of rating and data types. For
#' this to work, variables with the following combinations of names should be
#' created in the global environment:
#' - "pro_matches_" + `<test type>` + `<data type>` for matches data.
#' - `<rating type>` + "_fun_gen" for rating function generators.
#' - `<rating type>` + "_win_prob" for functions that compute win probability.
#' 
#' @return A tibble with columns:
#' - __testType__ <chr> : Test type identifier.
#' - __ratingType__ <chr> : Rating type identifier.
#' - __dataType__ <chr> : Data type identifier.
#' - __k__ <dbl/int> : Value of "K factor".
#' - __goodness__ <dbl> : Value of goodness of fit.
do_experiment <- function(test_type = c("validation", "test"),
                          rating_type = c("elo", "elobeta"),
                          data_type = c("all", "off"),
                          k_vec = k_grid,
                          initial_ratings = 0) {
  crossing(
    testType = test_type, ratingType = rating_type, dataType = data_type
  ) %>%
    mutate(
      dataName = paste0("pro_matches_", testType, "_", dataType),
      kVec = rep(list(k_vec), n()),
      rateFunGenName = paste0(ratingType, "_fun_gen"),
      winProbFunName = paste0(ratingType, "_win_prob"),
      initialRatings = rep(list(initial_ratings), n()),
      experimentData = pmap(
        list(dataName, testType, kVec,
             rateFunGenName, winProbFunName, initialRatings),
        compute_goodness_wrap
      )
    ) %>%
    unnest(experimentData) %>%
    select(testType, ratingType, dataType, k, goodness)
}


# Do experiment -----------------------------------------------------------
pro_matches_validation_all <- pro_matches_all %>% filter(matchType != "test")
pro_matches_validation_off <- pro_matches_off %>% filter(matchType != "test")
pro_matches_test_all <- pro_matches_all
pro_matches_test_off <- pro_matches_off

# Takes some time to run
experiment_tbl <- do_experiment()

saveRDS(
  experiment_tbl,
  file.path(
    "content", "post",
    "2018-07-03-elo-and-elobeta-models-in-snooker_results.rds"
  )
)


# Exploration of experiment results ---------------------------------------
cap_first <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

plot_data <- experiment_tbl %>%
  unite(group, ratingType, dataType) %>%
  mutate(
    testType = cap_first(testType),
    groupName = recode(
      group, elo_all = "Elo, all matches", elo_off = "Elo, official matches",
      elobeta_all = "EloBeta, all matches",
      elobeta_off = "EloBeta, official matches"
    ),
    # Ensure preferred order. This is needed because sorting of strings will
      # give "Elo, all matches", "EloBeta, all matches", "EloBeta, official
      # matches", and "Elo, official matches" as, apperently, non-letters are
      # ignored while sorting.
    groupName = factor(groupName, levels = unique(groupName))
  )

compute_optimal_k <- . %>% group_by(testType, groupName) %>%
  slice(which.min(goodness)) %>%
  ungroup()
compute_k_labels <- . %>% compute_optimal_k() %>%
  mutate(label = paste0("K = ", k)) %>%
  group_by(groupName) %>%
  # If optimal K within future facet is on the right, it needs a little
    # adjustment to the right. If on the left - full and a little adjustment to
    # the left.
  mutate(hjust = - (k == max(k)) * 1.1 + 1.05) %>%
  ungroup()

plot_experiment_results <- function(results_tbl) {
  ggplot(results_tbl) +
    geom_hline(
      yintercept = 0.5, colour = "#AA5555", size = 0.5, linetype = "dotted"
    ) +
    geom_line(aes(k, goodness, colour = testType)) +
    geom_vline(
      data = compute_optimal_k,
      mapping = aes(xintercept = k, colour = testType),
      linetype = "dashed", show.legend = FALSE
    ) +
    geom_text(
      data = compute_k_labels,
      mapping = aes(k, Inf, label = label, hjust = hjust),
      vjust = 1.2
    ) +
    facet_wrap(~ groupName) +
    scale_colour_manual(
      values = c(Validation = "#377EB8", Test = "#FF7F00"),
      guide = guide_legend(
        title = "Experiment", reverse = TRUE,
        override.aes = list(size = 4)
      )
    ) +
    labs(
      x = "K factor", y = "Goodness of fit (RMSE)",
      title = "Best goodness of fit of Elo and EloBeta models are almost equal",
      subtitle = paste0(
        'Using official matches (without invitational events) gives more ',
        'stable results.\n',
        'All optimal K values from test experiment (with longer "warm up") are',
        ' lower than from validation experiment.'
      )
    ) +
    theme(title = element_text(size = 14), strip.text = element_text(size = 12))
}

plot_experiment_results(plot_data)


# Exploration of EloBeta ratings on official matches ----------------------
# Helper function
gather_to_longcr <- function(tbl) {
  bind_rows(
    tbl %>% select(-matches("2")) %>% rename_all(funs(gsub("1", "", .))),
    tbl %>% select(-matches("1")) %>% rename_all(funs(gsub("2", "", .)))
  ) %>%
    arrange(game)
}

# Extract best "K factor" value
best_k <- experiment_tbl %>%
  filter(testType == "test", ratingType == "elobeta", dataType == "off") %>%
  slice(which.min(goodness)) %>%
  pull(k)

  #!!! Round to "pretty" number as it doesn't affect result that much!!!
best_k <- round(best_k / 5) * 5

# Compute ratings at the end of the data
elobeta_ratings <- rate_iterative(
  pro_matches_test_off, elobeta_fun_gen(best_k), initial_ratings = 0
) %>%
  rename(ratingEloBeta = rating_iterative) %>%
  arrange(desc(ratingEloBeta)) %>%
  left_join(
    y = snooker_players %>% select(id, playerName = name), by = c(player = "id")
  ) %>%
  mutate(rankEloBeta = order(ratingEloBeta, decreasing = TRUE)) %>%
  select(player, playerName, ratingEloBeta, rankEloBeta)

elobeta_top16 <- elobeta_ratings %>%
  filter(rankEloBeta <= 16) %>%
  mutate(
    rankChr = formatC(rankEloBeta, width = 2, format = "d", flag = "0"),
    ratingEloBeta = round(ratingEloBeta, 1)
  )
  
official_ratings <- tibble(
  player = c(
         5,      1,    237,      17,     12,     16,    224,     30,
        68,    154,     97,      39,     85,      2,    202,   1260
  ),
  rankOff = c(
         2,      3,      4,       1,      5,      7,      6,     13,
        16,     10,      8,       9,     26,     17,     12,     23
  ),
  ratingOff = c(
    905750, 878750, 751525, 1315275, 660250, 543225, 590525, 324587,
    303862, 356125, 453875,  416250, 180862, 291025, 332450, 215125
  )
)


# Evolution of EloBeta ratings --------------------------------------------
# Helper data
seasons_break <- ISOdatetime(2017, 5, 2, 0, 0, 0, tz = "UTC")

  # Compute evolution of ratings
elobeta_history <- pro_matches_test_off %>%
  add_iterative_ratings(elobeta_fun_gen(best_k), initial_ratings = 0) %>%
  gather_to_longcr() %>%
  left_join(y = pro_matches_test_off %>% select(game, endDate), by = "game")

  # Generate plot
plot_all_elobeta_history <- function(history_tbl) {
  history_tbl %>%
    mutate(isTop16 = player %in% elobeta_top16$player) %>%
    ggplot(aes(endDate, ratingAfter, group = player)) +
    geom_step(data = . %>% filter(!isTop16), colour = "#C2DF9A") +
    geom_step(data = . %>% filter(isTop16), colour = "#22A01C") +
    geom_hline(yintercept = 0, colour = "#AAAAAA") +
    geom_vline(
      xintercept = seasons_break, linetype = "dotted",
      colour = "#E41A1C", size = 1
    ) +
    geom_text(
      x = seasons_break, y = Inf, label = "End of 2016/17",
      colour = "#E41A1C", hjust = 1.05, vjust = 1.2
    ) +
    scale_x_datetime(date_labels = "%Y-%m") +
    labs(
      x = NULL, y = "EloBeta rating",
      title = paste0(
        "Most of current top 16 established at the end of 2016/17 season"
      ),
      subtitle = paste0(
        "Winning of event is well noticable as rapid increase without ",
        "descrease at the end."
      )
    ) +
    theme(title = element_text(size = 14))
}

plot_all_elobeta_history(elobeta_history)


# Evolution of top 16 EloBeta ratings -------------------------------------
  # Compute plot data
top16_rating_evolution <- elobeta_history %>%
  # Using `inner_join` to leave only players from `elobeta_top16`
  inner_join(y = elobeta_top16 %>% select(-ratingEloBeta), by = "player") %>%
  # Leave games only from 2017/18 season
  semi_join(
    y = pro_matches_test_off %>% filter(season == 2017), by = "game"
  ) %>%
  mutate(playerLabel = paste(rankChr, playerName))

  # Generate plot
plot_top16_elobeta_history <- function(elobeta_history) {
  ggplot(elobeta_history) +
    geom_step(aes(endDate, ratingAfter, group = player), colour = "#22A01C") +
    geom_hline(yintercept = 0, colour = "#AAAAAA") +
    geom_rug(
      data = elobeta_top16,
      mapping = aes(y = ratingEloBeta), sides = "r"
    ) +
    facet_wrap(~ playerLabel, nrow = 4, ncol = 4) +
    scale_x_datetime(date_labels = "%Y-%m") +
    labs(
      x = NULL, y = "EloBeta rating",
      title = "Rating evolution for EloBeta top 16 (as of 2017/18 end)",
      subtitle = paste0(
        "Ronnie O'Sullivan and Mark J Williams did very well in 2017/18 ",
        "season.\n",
        "As did Jack Lisowski: rise from negative rating to place 13."
      )
    ) +
    theme(title = element_text(size = 14), strip.text = element_text(size = 12))
}

plot_top16_elobeta_history(top16_rating_evolution)