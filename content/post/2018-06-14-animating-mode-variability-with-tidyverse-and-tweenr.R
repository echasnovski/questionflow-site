# Used tidyverse packages
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(purrr)
library(ggplot2)

# Using current CRAN version 0.1.5
library(tweenr)

# Using current github 'dgrtwo/gganimate' version 0.1.0.9000 (commit bf82002)
library(gganimate)

theme_set(theme_bw())

animation::ani.options(interval = 1/24)

# Very important step for reproducibility
set.seed(20180614)

# Data about distributions of interest and sample size
r_funs <- list(
  # Using `purrr::partial()` to create functions that expect only `n` argument
  "runif" = partial(runif),
  "rnorm" = partial(rnorm, mean = 0.5, sd = 0.25),
  "rbeta" = partial(rbeta, shape1 = 5, shape2 = 1)
)
n_sample <- 1000


# Data generation ---------------------------------------------------------
# Generate data by sampling from different distributions
# Note that all elements of `r_fun_list` should have names, which will be used
  # as distribution names.
# Elements of `r_fun_list` should be functions which take only one argument `n`
  # for a sample size to be generated. Use `purrr::partial` to create those.
generate_data <- function(r_fun_list, n_sample) {
  pmap_dfr(
    list(names(r_fun_list), r_fun_list, n_sample),
    # Interestingly, due to implementation of anonymous formula functions, you
      # can't use `..i` placeholders if they represent functions to be called.
    ~ tibble(distr = .x, value = .y(n = ..3))
  )
}

sample_data <- rerun(10, generate_data(r_funs, n_sample))

# One set of samples for examples
distr_data <- sample_data[[1]] %>% mutate(value = round(value, 2))
distr_data


# Static plot functions ---------------------------------------------------
# Compute data for plot. Basically, it:
  # - Counts values in each group.
  # - Normalizes by maximum count within each group (producing 'mode shares').
  # - Computes whether certain value is mode according to rule "`mode share` is
    # note less then `mode_thres`".
  # - Produces plotting data (including colour and size of segments).
# Here `tbl` should have 'distr' and 'value' columns.
# `mode_thres` represents minimum 'mode share' for value to be considered mode.
get_plot_data <- function(tbl, mode_thres = 1) {
  tbl %>%
    # Compute mode share distribution by counting value occurence in groups and
      # normalize by maximum count within groups.
    group_by(distr, value) %>%
    summarise(n = n()) %>%
    mutate(
      modeShare = n / max(n),
      isMode = modeShare >= mode_thres
    ) %>%
    ungroup() %>%
    # Prepare plot data
    transmute(
      distr, x = value,
      # Distributions are plotted on integer levels of groups.
      # Using factor 'distr' column is a way to control vertical order of
        # distributions.
      y = as.integer(as.factor(distr)),
      # Here using 0.9 ensures that segments won't overlap
      yend = y + modeShare * 0.9,
      isMode,
      colour = if_else(isMode, "red", "black"),
      size = if_else(isMode, 2, 1)
    )
}

get_plot_data(distr_data)
get_plot_data(distr_data, mode_thres = 0.01)
distr_data %>%
  mutate(distr = factor(distr, levels = c("runif", "rnorm", "rbeta"))) %>%
  get_plot_data()

# Adds segment layers. May be used in both static and animated plots.
  # In static plots should be preceded by `ggplot(data)` in which `data` should
    # have column `.frame` with all 1.
  # In animated plots should be preceded by
    # `ggplot(data_tweened, aes(frame = .frame))`.
# Returns a list with results of `ggplot2` constructor functions. Elements will
  # be used sequentially to build plot. This list should be added to `ggplot()`
  # call with `+` function (like other `ggplot2` functionality).
add_layers <- function() {
  common_aes <- aes(x = x, xend = x, y = y, yend = yend,
                    colour = colour, size = size)
  list(
    # Plotting segments in two different layers to highlight mode segments.
    geom_segment(
      # Value of `data` argument in layer function can be a function! It will be
        # applied to present data initialized in `ggplot()` call.
      # Notation `. %>% ...` creates a functional sequence.
      data = . %>% filter(!isMode), mapping = common_aes
    ),
    geom_segment(data = . %>% filter(isMode), mapping = common_aes),
    # Explicitly label distributions.
    geom_label(
      data = . %>%
        # Compute all possible labels in case of factor `distr`
        distinct(.frame, distr) %>%
        complete(.frame, distr) %>%
        # Position label on the left side and in the middle of the group plot
        mutate(x = -Inf, y = as.integer(as.factor(distr)) + 0.5),
      mapping = aes(x = x, y = y, label = distr),
      size = 5, hjust = -0.1
    ),
    # Tweak axes labels
    scale_x_continuous(name = NULL),
    scale_y_continuous(
      name = NULL, labels = NULL, breaks = NULL, minor_breaks = NULL
    ),
    scale_colour_identity(),
    scale_size_identity(),
    # Manually fix plot range for x axis to (0, 1).
    coord_cartesian(xlim = c(0, 1)),
    # Add common subtitle to describe plot. Wrap `labs()` in `list()` as
      # `labs()` itself returns a list which will be appended in a wrong way.
    list(labs(
      subtitle = paste0(
        c("Rows represent counts of samples from different distributions.",
          "Actual counts are normalized by maximum count to plot mode share.",
          "Modes (in red) are values with mode share above some threshold."),
        collapse = "\n"
      )
    ))
  )
}

# Wrapper for constructing static plot
make_static_plot <- function(plot_data, title = NULL) {
  plot_data %>%
    mutate(.frame = 1) %>%
    ggplot() +
      add_layers() +
      labs(title = title)
}


# Create static plots -----------------------------------------------------
distr_data %>%
  get_plot_data() %>%
  make_static_plot(
    title = "Mode share distributions for samples rounded to 2 digits"
  )

distr_data %>%
  get_plot_data(mode_thres = 0.8) %>%
  make_static_plot(
    title = paste0(
      c("Mode share distributions for samples rounded to 2 digits.",
        "Relaxed definition of mode"),
      collapse = " "
    )
  )

distr_data %>%
  mutate(distr = factor(distr, levels = c("rnorm", "runif", "rbeta"))) %>%
  get_plot_data() %>%
  make_static_plot(
    title = "Control order of distributions with factor `distr` column"
  )


# Animation functions -----------------------------------------------------
# Analogous to `findInterval()` but:
  # - Returns not the index of "left" `vec` element of interval but index of
    # nearest element between two.
  # - `vec` can be unordered.
find_nearest_ind <- function(x, vec) {
  if (length(vec) == 1) {
    return(rep(1, length(x)))
  }
  
  # Produce sorted vector and use `findInterval` on it
  vec_order <- order(vec)
  vec_sorted <- vec[vec_order]
  x_ind <- findInterval(x, vec_sorted, all.inside = TRUE)
  
  # Modify interval index by possibly adding 1 if right interval end is nearer.
  x_nearest <- x_ind + (vec_sorted[x_ind + 1] - x < x - vec_sorted[x_ind])
  
  vec_order[x_nearest]
}

# Completely match x to y
# It computes:
  # - Index of the nearest `y` value to `x` value for every index of `x`.
  # - Similar data, but index of nearest `x` value to `y` values that were not
    # chosen as nearest ones in the previous step.
# Output is a tibble with 'from' and 'to' columns. In column 'from' all indices
  # of `x` are present and in 'to' - all indices of `y` are present.
match_nearest <- function(x, y) {
  matching_x <- find_nearest_ind(x, y)
  rest_y <- setdiff(seq_along(y), matching_x)
  
  if (length(rest_y) > 0) {
    matching_rest_y <- find_nearest_ind(y[rest_y], x)
    
    tibble(
      from = c(seq_along(x), matching_rest_y),
      to   = c(matching_x,   rest_y)
    )
  } else {
    tibble(from = seq_along(x), to = matching_x)
  }
}

# Return modified second argument with zero height segments in case first
  # argument is `NULL`.
# This function is essential for aesthetically pleasing animation in case of
  # different distribution sets. Should be used in conjunction with
  # `dplyr::full_join()`.
coalesce_segments <- function(subject, reference) {
  if (is.null(subject)) {
    reference %>% mutate(yend = y)
  } else {
    subject
  }
}

# Prepare data for `tweenr::tween_states()`
# `plot_data_from` and `plot_data_to` represent `get_plot_data()` outputs of
  # two tables which plots should be animated one into another.
# The idea is to ensure that every segment in both "from" and "to" data
  # actively takes part in transition and not just pops up in the end.
  # This is achieved by doing complete match within every distribution of
  # segments in "from" and "to" plot data sets.
transition_nearest_value <- function(plot_data_from, plot_data_to) {
  nested_from <- plot_data_from %>% nest(-distr, .key = "dataFrom")
  nested_to <- plot_data_to %>% nest(-distr, .key = "dataTo")
  
  nested_transitions <- nested_from %>%
    # Join two tables with plot data by distribution.
    # Note the use of `full_join()` which enables animating data with different
      # sets of distributions.
    full_join(y = nested_to, by = "distr") %>%
    # Modify possibly missing distributions to appear "from the ground".
    mutate(
      dataFrom = map2(dataFrom, dataTo, coalesce_segments),
      dataTo = map2(dataTo, dataFrom, coalesce_segments)
    ) %>%
    # Compute matching by nearest x value
    mutate(
      matching = map2(dataFrom, dataTo, ~ match_nearest(.x[["x"]], .y[["x"]])),
      transitionFrom = map2(dataFrom, matching, ~ .x[.y[["from"]], ]),
      transitionTo = map2(dataTo, matching, ~ .x[.y[["to"]], ])
    )
  
  # Return a list with pair of tibbles where corresponding pairs of rows
    # represent segements to be transfromed one into another.
  list(
    from = nested_transitions %>% unnest(transitionFrom),
    to = nested_transitions %>% unnest(transitionTo)
  )
}

# Tween consecutive plot data sets to be transformed one into another
# The output will be data for cycled animation: last plot will be transformed
  # into the first one.
tween_cycled <- function(plot_data_list, tweenlength = 2, statelength = 2,
                         ease = "linear", nframes = 24) {
  states <- c(plot_data_list, plot_data_list[1])
  
  # As in result of every `tweenr::tween_states()` call column `.frame` starts
    # from 1, it is needed to offset frames for every pair.
  frame_offset <- (seq_along(plot_data_list) - 1) * nframes
  
  map2(states[-length(states)], states[-1], transition_nearest_value) %>%
    map2_dfr(
      frame_offset,
      function(pair_tbls, f_offset) {
        pair_mod <- pair_tbls %>%
          # Converting column `distr` to factor is needed to avoid
            # 'Error in col2rgb(d)' with not correct colour name.
          # This is due to not optimal treatment of character columns in current
            # CRAN version of `tweenr`.
            # This seems to be solved in dev. version at 'thomasp85/tweenr'.
            # However, for now it is rather unstable.
          # Fortunately, converting to factor should give expected results as in
            # `transition_nearest_value()` it is ensured common value set and
            # order of their appearence by using `full_join()`.
          map(. %>% mutate(distr = as.factor(distr)))
        
        # CRAN version of `tweenr` also changes levels of factors during
          # `tween_states()`. This also should be solved in development version.
        # For now, use workaround with manual patching of levels.
        distr_levels <- union(
          levels(pair_mod[[1]]$distr), levels(pair_mod[[2]]$distr)
        )
        
        pair_mod %>%
          tweenr::tween_states(
            tweenlength = tweenlength, statelength = statelength,
            ease = ease, nframes = nframes
          ) %>%
          mutate(
            # Offset frames
            .frame = .frame + f_offset,
            # Repare columns after applying `tween_states()`
            distr = factor(as.character(distr), levels = distr_levels),
            isMode = as.logical(isMode),
            colour = as.character(colour)
          )
      }
    )
}

# Wrapper for constructing animation
make_animation <- function(plot_data_list, title = NULL,
                           tweenlength = 2, statelength = 2,
                           ease = "linear", nframes = 24,
                           filename = NULL) {
  p <- plot_data_list %>%
    tween_cycled(tweenlength = tweenlength, statelength = statelength,
                 ease = ease, nframes = nframes) %>%
    # Construct plot
    ggplot(aes(frame = .frame)) +
      add_layers() +
      labs(title = title)
  
  gganimate(
    p, filename = filename, title_frame = FALSE,
    # Change resolution by supplying function for graphic device.
    # Thanks to https://stackoverflow.com/a/46878163/7360839
    ani.dev = function(...) {png(res = 100, ...)},
    ani.width = 675, ani.height = 450
  )
}


# Create animations -------------------------------------------------------
# Here commented code represents the one used in post

# Common name for animations. This assumes that all needed folders are created
  # and this code is run in fresh R session (working directory being project
  # root).
ani_name <- file.path(
  "static", "post",
  "2018-06-14-animating-mode-variability-with-tidyverse-and-tweenr_files",
  "2018-06-14-animating-mode-variability-with-tidyverse-and-tweenr_animation_"
)

# # Mode variability of different samples with values rounded to 2 digits
# sample_data %>%
#   map(. %>% mutate(value = round(value, 2))) %>%
#   map(get_plot_data) %>%
#   make_animation(title = "Mode variability for independent samples")
sample_data %>%
  map(. %>% mutate(value = round(value, 2))) %>%
  map(get_plot_data) %>%
  make_animation(
    filename = paste0(ani_name, "1.gif"),
    title = "Mode variability for independent samples"
)

# # Animation for tables with different distribution sets
# sample_data[1:3] %>%
#   # Convert `distr` to factor with common levels to ensure animation stability
#   map(~ mutate(., distr = factor(distr, levels = sort(names(r_funs))))) %>%
#   # Remove distributions in tables
#   map2(names(r_funs), ~ filter(.x, as.character(distr) != .y)) %>%
#   # The same animation process
#   map(. %>% mutate(value = round(value, 2))) %>%
#   map(get_plot_data) %>%
#   make_animation(
#     title = "Automated animation for different distribution sets"
#   )
sample_data[1:3] %>%
  # Convert `distr` to factor with common levels to ensure animation stability
  map(~ mutate(., distr = factor(distr, levels = sort(names(r_funs))))) %>%
  # Remove distributions in tables
  map2(names(r_funs), ~ filter(.x, as.character(distr) != .y)) %>%
  # The same animation process
  map(. %>% mutate(value = round(value, 2))) %>%
  map(get_plot_data) %>%
  make_animation(
    filename = paste0(ani_name, "2.gif"),
    title = "Automated animation for different distribution sets"
  )

# # Evolution of modes for different mode thresholds
# map(
#   seq(from = 1, to = 0, by = -0.05),
#   ~ get_plot_data(sample_data[[1]] %>% mutate(value = round(value, 2)), .x)
# ) %>%
#   make_animation(
#     title = "Evolution of modes for different mode thresholds (from 1 to 0)",
#     nframes = 6
#   )
map(
  seq(from = 1, to = 0, by = -0.05),
  ~ get_plot_data(sample_data[[1]] %>% mutate(value = round(value, 2)), .x)
) %>%
  make_animation(
    filename = paste0(ani_name, "3.gif"),
    title = "Evolution of modes for different mode thresholds (from 1 to 0)",
    nframes = 6
  )

# # Mode variability of one sample with different roundings
# map2(sample_data[1], 3:1, ~ mutate(.x, value = round(value, .y))) %>%
#   map(get_plot_data) %>%
#   make_animation(
#     title = "Mode variability for different roundings of one sample",
#     tweenlength = 3, statelength = 1
#   )
map2(sample_data[1], 3:1, ~ mutate(.x, value = round(value, .y))) %>%
  map(get_plot_data) %>%
  make_animation(
    filename = paste0(ani_name, "4.gif"),
    title = "Mode variability for different roundings of one sample",
    tweenlength = 3, statelength = 1
  )


# Accidental art ----------------------------------------------------------
map2(sample_data[1], 3:1, ~ mutate(.x, value = round(value, .y))) %>%
  map(get_plot_data) %>%
  tween_cycled() %>%
  make_static_plot(title = "Accidental art")
