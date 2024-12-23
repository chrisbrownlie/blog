library(stringr)
library(dplyr)
library(tibble)
library(purrr)
library(readr)
library(tidyr)

test <- read_fwf("posts/advent_of_code_2024/day_12/test.txt",
                 col_positions = fwf_widths(rep(1,10), str_c("col_", 1:10)),
                 col_types = "c")


update_regions <- function(cell, df, regions_df) {

  plot_type <- df[[cell[2]]][[cell[1]]]

  already_in <- regions_df |>
    filter(row == cell[1],
           col == cell[2])

  r_already_in <- regions_df |>
    filter(row == cell[1],
           col == cell[2]+1)

  # Is cell part of an existing region?
  region <- NA
  if (NROW(already_in)) {
    region <- already_in$region
  }
  if (NROW(r_already_in) && r_already_in$type[[1]] == plot_type) {
    region <- r_already_in$region
  }
  if (is.na(region)) {
    region <- str_c(plot_type, sum(startsWith(unique(regions_df$region), plot_type))+1)
    regions_df <- regions_df |>
      bind_rows(list("type" = plot_type, "region" = region, "row" = cell[1], "col" = cell[2]))
  }

  # Check right and down
  if ((cell[2]+1 <= NCOL(df)) && !NROW(r_already_in)) {
    right <- df[[cell[2]+1]][[cell[1]]]

    if (right == plot_type) {
      regions_df <- regions_df |>
        bind_rows(list("type" = plot_type, "region" = region, "row" = cell[1], "col" = cell[2]+1))
    }
  }

  d_already_in <- regions_df |>
    filter(row == cell[1]+1,
           col == cell[2])
  if ((cell[1]+1 <= NROW(df)) && !NROW(d_already_in)) {
    down <- df[[cell[2]]][[cell[1]+1]]
    if (down == plot_type) {
      regions_df <- regions_df |>
        bind_rows(list("type" = plot_type, "region" = region, "row" = cell[1]+1, "col" = cell[2]))
    }
  }

  return(regions_df)
}

empty_regions <- tibble::tibble(type = character(), region = character(), row = integer(), col = integer())

coords <- expand_grid(r = seq_len(NROW(test)), c = seq_len(NCOL(test)))
all_regions <- purrr::reduce2(
  .x = coords$r,
  .y = coords$c,
  \(r_df, r, c) {
    update_regions(c(r, c), df = test, regions_df = r_df)
  },
  .init = empty_regions
)


## Start again, different tactic
get_perimeter <- function(cell, df) {
  perim <- 4
  plot_t <- df[[cell[2]]][[cell[1]]]
  if (cell[2]+1 <= NCOL(df)) {
    right <- df[[cell[2]+1]][[cell[1]]]
    if (right == plot_t) perim <- perim-1
  }
  if (cell[2]-1 >= 1) {
    left <- df[[cell[2]-1]][[cell[1]]]
    if (left == plot_t) perim <- perim-1
  }
  if (cell[1]+1 <= NROW(df)) {
    down <- df[[cell[2]]][[cell[1]+1]]
    if (down == plot_t) perim <- perim-1
  }
  if (cell[1]-1 >= 1) {
    up <- df[[cell[2]]][[cell[1]-1]]
    if (up == plot_t) perim <- perim-1
  }
  perim
}

right_valid <- function(cell, df, type, rdf) {
  if (cell[2]+1 > NCOL(df)) return(FALSE)
  right <- df[[cell[2]+1]][[cell[1]]]
  r_in <- NROW(filter(rdf, row == cell[1], col == cell[2]+1))>0
  df[[cell[2]]][[cell[1]]] == right && !r_in
}
left_valid <- function(cell, df, type, rdf) {
  if (cell[2]-1 < 1) return(FALSE)
  left <- df[[cell[2]-1]][[cell[1]]]
  l_in <- NROW(filter(rdf, row == cell[1], col == cell[2]-1))>0
  df[[cell[2]]][[cell[1]]] == left && !l_in
}
up_valid <- function(cell, df, type, rdf) {
  if (cell[1]-1 < 1) return(FALSE)
  up <- df[[cell[2]]][[cell[1]-1]]
  u_in <- NROW(filter(rdf, row == cell[1]-1, col == cell[2]))>0
  df[[cell[2]]][[cell[1]]] == up && !u_in
}
down_valid <- function(cell, df, type, rdf) {
  if (cell[1]+1 > NROW(df)) return(FALSE)
  down <- df[[cell[2]]][[cell[1]+1]]
  d_in <- NROW(filter(rdf, row == cell[1]+1, col == cell[2]))>0
  (df[[cell[2]]][[cell[1]]] == down) && !d_in
}

find_region <- function(cell, df, region_df) {

  plot_type <- df[[cell[2]]][[cell[1]]]

  if (NROW(filter(region_df, row == cell[1], col == cell[2])) == 0) {
    p <- get_perimeter(cell, df)
    region_df <- region_df |>
      bind_rows(
        list("type" = plot_type,
             "row" = cell[1],
             "col" = cell[2],
             "p_up" = p[["up"]],
             "p_down" = p[["down"]],
             "p_left" = p[["left"]],
             "p_right" = p[["right"]])
      )
  }
  if (right_valid(cell, df, plot_type, region_df)) {
    region_df <- find_region(c(cell[1], cell[2]+1), df, region_df)
  }
  if (down_valid(cell, df, plot_type, region_df)) {
    region_df <- find_region(c(cell[1]+1, cell[2]), df, region_df)
  }
  if (left_valid(cell, df, plot_type, region_df)) {
    region_df <- find_region(c(cell[1], cell[2]-1), df, region_df)
  }
  if (up_valid(cell, df, plot_type, region_df)) {
    region_df <- find_region(c(cell[1]-1, cell[2]), df, region_df)
  }
  return(region_df)
}

empty_region_table <- tibble::tibble(type = character(), row = integer(), col = integer())
attempt <- find_region(c(1,1), test, empty_region_table)
test_coords <- expand_grid(r = seq_len(NROW(test)), c = seq_len(NCOL(test)))

all_regions <- purrr::reduce2(
  test_coords$r,
  test_coords$c,
  .init = bind_cols(empty_region_table, list(rid = character())),
  \(all_regions, r, c, df = test) {
    if (NROW(filter(all_regions, col == c, row == r))>0) return(all_regions)
    all_regions <- all_regions |>
      bind_rows(
        find_region(c(r, c), df, empty_region_table) |>
          mutate(rid = str_c(df[[c]][[r]], sum(startsWith(unique(all_regions$rid), df[[c]][[r]]))+1))
      )
  }
)

# Real data

input <- read_fwf("posts/advent_of_code_2024/day_12/input.txt",
                 col_positions = fwf_widths(rep(1,140), str_c("col_", 1:140)),
                 col_types = "c")
coords <- expand_grid(r = seq_len(NROW(input)), c = seq_len(NCOL(input)))
all_regions_real <- purrr::reduce2(
  coords$r,
  coords$c,
  .init = bind_cols(empty_region_table, list(rid = character())),
  \(all_regions, r, c, df = input) {
    if (NROW(filter(all_regions, col == c, row == r))>0) return(all_regions)
    print(paste0("finding region for coord: ", r, ",", c))
    all_regions |>
      bind_rows(
        find_region(c(r, c), df, empty_region_table) |>
          mutate(rid = str_c(df[[c]][[r]], sum(startsWith(unique(all_regions$rid), df[[c]][[r]]))+1))
      )
  }
)



# part 2
get_perimeter <- function(cell, df) {
  perim <- c(up = 1, down = 1, right = 1, left = 1)
  plot_t <- df[[cell[2]]][[cell[1]]]
  if (cell[2]+1 <= NCOL(df)) {
    right <- df[[cell[2]+1]][[cell[1]]]
    if (right == plot_t) perim["right"] <- 0
  }
  if (cell[2]-1 >= 1) {
    left <- df[[cell[2]-1]][[cell[1]]]
    if (left == plot_t) perim["left"] <- 0
  }
  if (cell[1]+1 <= NROW(df)) {
    down <- df[[cell[2]]][[cell[1]+1]]
    if (down == plot_t) perim["down"] <- 0
  }
  if (cell[1]-1 >= 1) {
    up <- df[[cell[2]]][[cell[1]-1]]
    if (up == plot_t) perim["up"] <- 0
  }
  perim
}

find_horiz_edges <- function(check_row, region_df) {
  above_edges <- region_df |>
    filter(row == check_row, p_up == 1) |>
    arrange(col) |>
    mutate(
      group = NA,
      prev_col = coalesce(lag(col), col),
      adj_col = if_else(prev_col == col, col, col-1),
      same_above_side = prev_col==adj_col
    )
  for (i in seq_len(NROW(above_edges))) {
    if (i == 1) {
      group <- 1
    } else if (above_edges$same_above_side[i]) {
      group <- above_edges$group[i-1]
    } else {
      group <- above_edges$group[i-1]+1
    }
    above_edges$group[i] <- group
  }
  below_edges <- region_df |>
    filter(row == check_row, p_down == 1) |>
    arrange(col) |>
    mutate(
      group = NA,
      prev_col = coalesce(lag(col), col),
      adj_col = if_else(prev_col == col, col, col-1),
      same_below_side = prev_col==adj_col
    )
  for (i in seq_len(NROW(below_edges))) {
    if (i == 1) {
      group <- 1
    } else if (below_edges$same_below_side[i]) {
      group <- below_edges$group[i-1]
    } else {
      group <- below_edges$group[i-1]+1
    }
    below_edges$group[i] <- group
  }
  length(unique(above_edges$group))+length(unique(below_edges$group))
}

find_vert_edges <- function(check_col, region_df) {
  right_edges <- region_df |>
    filter(col == check_col, p_right == 1) |>
    arrange(row) |>
    mutate(
      group = NA,
      prev_row = coalesce(lag(row), row),
      adj_row = if_else(prev_row == row, row, row-1),
      same_right_side = prev_row==adj_row
    )
  for (i in seq_len(NROW(right_edges))) {
    if (i == 1) {
      group <- 1
    } else if (right_edges$same_right_side[i]) {
      group <- right_edges$group[i-1]
    } else {
      group <- right_edges$group[i-1]+1
    }
    right_edges$group[i] <- group
  }
  left_edges <- region_df |>
    filter(col == check_col, p_left == 1) |>
    arrange(row) |>
    mutate(
      group = NA,
      prev_row = coalesce(lag(row), row),
      adj_row = if_else(prev_row == row, row, row-1),
      same_left_side = prev_row==adj_row
    )
  for (i in seq_len(NROW(left_edges))) {
    if (i == 1) {
      group <- 1
    } else if (left_edges$same_left_side[i]) {
      group <- left_edges$group[i-1]
    } else {
      group <- left_edges$group[i-1]+1
    }
    left_edges$group[i] <- group
  }
  length(unique(right_edges$group))+length(unique(left_edges$group))
}

find_region_sides <- function(rdf) {
  rows_to_check <- unique(rdf$row)
  cols_to_check <- unique(rdf$col)
  row_edges <- map_dbl(rows_to_check, \(r) find_horiz_edges(r, rdf))
  col_edges <- map_dbl(cols_to_check, \(c) find_vert_edges(c, rdf))
  sum(row_edges) + sum(col_edges)
}

# test
test_region_new_cost <- all_regions |>
  group_by(rid) |>
  summarise(edges = find_region_sides(pick(everything())), area = n()) |>
  mutate(cost = edges*area)
sum(test_region_new_cost$cost)

real_new_cost <- all_regions_real |>
  group_by(rid) |>
  summarise(edges = find_region_sides(pick(everything())), area = n()) |>
  mutate(cost = edges*area)
