# Code for time consuming computations of "Mythical generic overhead" post
# Should be run with working directory being root of site repository

suppressMessages(library(tidyverse))
suppressMessages(library(rlang))
suppressMessages(library(microbenchmark))

set.seed(1105)

ggplot2::theme_set(theme_bw())


# Function generators -----------------------------------------------------
# Wrapper for creating function with one argument `x` in environment `env`
new_f <- function(name, body, env) {
  fun <- new_function(alist(x = ), parse_expr(body), env)
  
  assign(x = name, value = fun, envir = env)
}

new_if_get_true <- function(n_class = 1, env = caller_env()) {
  body <- paste0(
    'if (class(x) == "class', seq_len(n_class), '") { return(TRUE) }',
    collapse = " else "
  )
  
  new_f("if_get_true", body, env)
}

new_switch_get_true <- function(n_class = 1, env = caller_env()) {
  body <- paste0(
    "switch(\nclass(x),\n",
    paste0("class", seq_len(n_class), " = return(TRUE)",
           collapse = ",\n"),
    "\n)"
  )
  
  new_f("switch_get_true", body, env)
}

new_gen_get_true <- function(n_class = 1, env = caller_env()) {
  # Create generic
  new_f("gen_get_true", 'UseMethod("gen_get_true")', env)
  
  # Create methods
  method_names <- paste0("gen_get_true.class", seq_len(n_class))
  
  walk(method_names, new_f, body = "return(TRUE)", env = env)
}

new_get_true_all <- function(n_class = 1, env = caller_env()) {
  new_if_get_true(n_class = n_class, env = env)
  new_switch_get_true(n_class = n_class, env = env)
  new_gen_get_true(n_class = n_class, env = env)
  
  env
}


# Benchmark function ------------------------------------------------------
bench_funs <- function(n_class = 1, env = caller_env(), times = 1000000) {
  bench <- map(seq_len(n_class), function(class_id) {
    assign("x", structure(1, class = paste0("class", class_id)), envir = env)
    assign("times", times, envir = env)
    
    eval(
      quote(microbenchmark(
        'if' = if_get_true(x),
        'switch' = switch_get_true(x),
        gen = gen_get_true(x),
        times = times
      )),
      envir = env
    ) %>%
      as_tibble() %>%
      group_by(expr) %>%
      # Median computation time in microseconds
      summarise(time = median(time) / 1000) %>%
      mutate(class_id = class_id)
  }) %>%
    bind_rows() %>%
    rename(method = expr)
  
  rm(list = c("x", "times"), envir = env)
  
  bench
}


# Computing benchmarks ---------------------------------------------------
# Takes considerable amount of time to run
overhead_bench <- tibble(n_class = 1:20) %>%
  mutate(
    env = rerun(n(), child_env(.GlobalEnv)),
    env = map2(n_class, env, new_get_true_all),
    bench = map2(n_class, env, bench_funs, times = 1000000)
  ) %>%
  select(-env) %>%
  unnest(bench) %>%
  mutate(method = as.character(method)) %>%
  select(n_class, class_id, method, time)


# Saving data -------------------------------------------------------------
save(
  new_f,
  new_if_get_true, new_switch_get_true, new_gen_get_true,
  new_get_true_all,
  overhead_bench,
  file = file.path(
    "content", "post",
    "2017-11-05-mythical-generic-overhead_data.RData"
  )
)
