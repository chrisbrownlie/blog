library(stringr)
library(dplyr)
library(tibble)
library(purrr)
library(readr)
library(tidyr)

test <- read_table("posts/advent_of_code_2024/day_10/test.txt", col_types = list("incol" = col_character()), col_names = "incol") |>
  separate(incol, into = str_c("col_", 1:9), sep = "") |>
  select(-col_1) |>
  mutate(across(everything(), as.character))

get_locs <- function(val, input_df = input) {
  pmap(input_df, \(...) which(str_detect(list(...), str_c("^", val)))) |>
    imap_dfr(
      \(x, i) {
        list("val" = val, "row_n" = i, "col_n" = x)
      }
    )
}

ends <- get_locs(9, test)
label_test <- test
for (i in seq_len(NROW(ends))) {
  label_test[ends$row_n[i], ends$col_n[i]] <- str_c(ends$val[i], "_", str_c(LETTERS[ends$row_n[i]], LETTERS[ends$col_n[i]]))
}

look_around <- function(row, col, in_map, search) {
  vals <- c()
  if (row != NROW(in_map)) vals <- c(vals, in_map[row+1, col])
  if (row != 1) vals <- c(vals, in_map[row-1, col])
  if (col != NCOL(in_map)) vals <- c(vals, in_map[row, col+1])
  if (col != 1) vals <- c(vals, in_map[row, col-1])
  continued_routes <- vals |>
    as.list() |>
    keep(\(x) str_detect(x, search)) |>
    str_extract_all("[[:upper:]]+") |>
    unlist() |>
    unique()
  str_c(continued_routes, collapse = "_")
}

calc_scores <- function(in_map) {
  reduce(
    8:0,
    .init = in_map,
    \(m, n) {
      locs <- get_locs(
        n, m
      ) |>
        rowwise() |>
        mutate(
          routes = pmap_chr(
            list(row_n, col_n),
            \(...) {
              look_around(row_n, col_n, m, search = str_c("^", n+1, "_"))
            }
          )
        )
      if (n == 0) return(locs)
      new_map <- m
      for (i in seq_len(NROW(locs))) {
        new_map[locs$row_n[i], locs$col_n[i]] <- str_c(locs$val[i], "_", locs$routes[i])
      }
      new_map
    }
  )
}
z <- calc_scores(label_test) |>
  mutate(score = length(unlist(str_extract_all(routes, "[[:upper:]]{2}"))))

real <- read_table("posts/advent_of_code_2024/day_10/input.txt", col_types = list("incol" = col_character()), col_names = "incol") |>
  separate(incol, into = str_c("col_", 0:45), sep = "") |>
  select(-col_0) |>
  mutate(across(everything(), as.character))

real_ends <- get_locs(9, real)
id_list <- expand_grid(LETTERS, LETTERS, LETTERS) |>
  unite("id", sep = "") |>
  slice(1:(45*45)) |>
  bind_cols(expand_grid(row = 1:45, col = 1:45))
label_real <- real
for (i in seq_len(NROW(real_ends))) {
  id <- id_list$id[id_list$row == real_ends$row_n[i] & id_list$col == real_ends$col_n[i]]
  label_real[real_ends$row_n[i], real_ends$col_n[i]] <- str_c(real_ends$val[i], "_", id)
}

z <- calc_scores(label_real) |>
  mutate(score = length(unlist(str_extract_all(routes, "[[:upper:]]{3}"))))




# Part 2
look_around_p2 <- function(row, col, in_map, search) {
  vals <- c()
  if (row != NROW(in_map)) vals <- c(vals, in_map[row+1, col])
  if (row != 1) vals <- c(vals, in_map[row-1, col])
  if (col != NCOL(in_map)) vals <- c(vals, in_map[row, col+1])
  if (col != 1) vals <- c(vals, in_map[row, col-1])
  continued_routes <- vals |>
    as.list() |>
    keep(\(x) str_detect(x, search)) |>
    str_extract_all("[[:upper:]]+") |>
    unlist()
  str_c(continued_routes, collapse = "_")
}

calc_scores <- function(in_map) {
  reduce(
    8:0,
    .init = in_map,
    \(m, n) {
      locs <- get_locs(
        n, m
      ) |>
        rowwise() |>
        mutate(
          routes = pmap_chr(
            list(row_n, col_n),
            \(...) {
              look_around_p2(row_n, col_n, m, search = str_c("^", n+1, "_"))
            }
          )
        )
      if (n == 0) return(locs)
      new_map <- m
      for (i in seq_len(NROW(locs))) {
        new_map[locs$row_n[i], locs$col_n[i]] <- str_c(locs$val[i], "_", locs$routes[i])
      }
      new_map
    }
  )
}

z <- calc_scores(label_test) |>
  mutate(score = length(unlist(str_extract_all(routes, "[[:upper:]]{2}"))))

zr <- calc_scores(label_real) |>
  mutate(score = length(unlist(str_extract_all(routes, "[[:upper:]]{3}"))))
