library(stringr)
library(dplyr)
library(tibble)

# Get locations
get_antennae_locs <- function(antenna, input_df = input) {
  pmap(input_df, \(...) which(list(...) == antenna)) |>
    imap_dfr(
      \(x, i) {
        list("antenna" = antenna, "row" = i, "col" = x)
      }
    )
}

# Test
test <- readr::read_lines("posts/advent_of_code_2024/day_8/test.txt") |>
  map_dfr(\(l) {
    l |>
      str_split("") |>
      pluck(1) |>
      set_names(str_c("col_", 1:str_length(l))) |>
      as_tibble_row()
  })

t_antennae <- unique(unlist(test)) |> discard(\(a) a == ".")

ta_locs <- t_antennae |>
  map_dfr(get_antennae_locs)

antinode_locs <- ta_locs |>
  inner_join(ta_locs, by = "antenna", relationship = "many-to-many",
             suffix = c("_first", "_second")) |>
  filter(row_first != row_second | col_first != col_second) |>
  mutate(new_row = row_second + (row_second - row_first),
         new_col = col_second + (col_second - col_first)) |>
  filter(new_row <= NROW(input) & new_col <= NCOL(test) & new_row > 0 & new_col > 0) |>
  select(new_row, new_col) |>
  unique() |>
  tally()


# Real
input <- readr::read_lines("posts/advent_of_code_2024/day_8/input.txt") |>
  map_dfr(\(l) {
    l |>
      str_split("") |>
      pluck(1) |>
      set_names(str_c("col_", 1:str_length(l))) |>
      as_tibble_row()
  })

antennae <- unique(unlist(input)) |> discard(\(a) a == ".")

a_locs <- antennae |>
  map_dfr(get_antennae_locs)

antinode_locs <- a_locs |>
  inner_join(a_locs, by = "antenna", relationship = "many-to-many",
             suffix = c("_first", "_second")) |>
  filter(row_first != row_second | col_first != col_second) |>
  mutate(new_row = row_second + (row_second - row_first),
         new_col = col_second + (col_second - col_first)) |>
  filter(new_row <= NROW(input) & new_col <= NCOL(input) & new_row > 0 & new_col > 0) |>
  select(new_row, new_col) |>
  unique() |>
  tally()

# Part 2
# Test
tantinode_locs_p2 <- ta_locs

ta_locs_id <- ta_locs |>
  mutate(id = row_number())
coord_locs <- ta_locs_id |>
  group_by(antenna) |>
  summarise(y = list(as.data.frame(t(combn(id, 2)))), .groups = 'drop') |>
  tidyr::unnest(y) |> ungroup() |>
  left_join(ta_locs_id, by = c("V1" = "id"), suffix = c("_first", "_second")) |>
  left_join(ta_locs_id, by = c("V2" = "id"), suffix = c("_first", "_second")) |>
  select(antenna, row_first, col_first, row_second, col_second) |>
  mutate(x_dist = row_second-row_first,
         y_dist = col_second-col_first) |>
  pmap(
    \(...) {
      row <- list(...)
      map(
        -12:12,
        \(n, r = row$row_second, c = row$col_second, dr = row$x_dist, dc = row$y_dist) {
          coord <- c(r, c) + (n * c(dr, dc))
          if (any(coord < 1) | any(coord > 12)) return(NA)
          coord
        }
      ) |>
        discard(\(x)length(x)==1)
    }
  )

#real

a_locs_id <- a_locs |>
  mutate(id = row_number())
real_coord_locs <- a_locs_id |>
  group_by(antenna) |>
  summarise(y = list(as.data.frame(t(combn(id, 2)))), .groups = 'drop') |>
  tidyr::unnest(y) |> ungroup() |>
  left_join(a_locs_id, by = c("V1" = "id"), suffix = c("_first", "_second")) |>
  left_join(a_locs_id, by = c("V2" = "id"), suffix = c("_first", "_second")) |>
  select(antenna, row_first, col_first, row_second, col_second) |>
  mutate(x_dist = row_second-row_first,
         y_dist = col_second-col_first) |>
  pmap(
    \(...) {
      row <- list(...)
      map(
        -50:50,
        \(n, r = row$row_second, c = row$col_second, dr = row$x_dist, dc = row$y_dist) {
          coord <- c(r, c) + (n * c(dr, dc))
          if (any(coord < 1) | any(coord > 50)) return(NA)
          coord
        }
      ) |>
        discard(\(x)length(x)==1)
    }
  )
actual_test <- real_coord_locs |>
  list_flatten() |>
  unique()


# visualising


mark_on_map <- function(map, location) {
  map[location[1], location[2]] <- "#"
  map
}
new <- reduce(coord_locs_t, \(m,l) mark_on_map(m, l), .init = test)
