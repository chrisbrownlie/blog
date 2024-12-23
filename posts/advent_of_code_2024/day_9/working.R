library(stringr)
library(dplyr)
library(tibble)
library(purrr)

to_blocks <- function(fragment) {
  fragment |>
    str_split("") |>
    pluck(1) |>
    imap(
      \(f, n) {
        if (n%%2==0) return(rep(".", f))
        rep(floor(n/2), f)
      }
    )
}

rearrange_one <- function(blocks) {
  repl <- blocks[blocks != "."] |>
    last()
  blocks[first(which(blocks == "."))] <- repl
  blocks[-max(which(blocks == repl))]
}
rearrange_blocks <- function(blocks) {
  while(min(which(blocks == ".")) < max(which(blocks != "."))) blocks <- rearrange_one(blocks)
  blocks |>
    discard(\(x) x==".")
}

calculate_checksum <- function(arr_blocks) {
  arr_blocks |>
    map2_dbl((1:length(arr_blocks))-1, \(.x, .y) {as.numeric(.x)*.y}) |>
    sum()
}

# test
test <- readr::read_lines("posts/advent_of_code_2024/day_9/test.txt")

test_blocks <- test |>
  to_blocks()
test_arranged <- test_blocks |>
  rearrange_blocks()
test_checksum <- test_arranged |>
  calculate_checksum()

# Real
input <- readr::read_lines("posts/advent_of_code_2024/day_9/input.txt")
in_blocks <- input |>
  to_blocks()
rearranged_input <- in_blocks |>
  rearrange_blocks()
calculate_checksum(rearranged_input)



## Part two
find_space <- function(blocks, i) {
  repl <- blocks[i]
  if (!length(repl[[1]])) return(blocks)
  if (any(str_detect(repl[[1]], "\\."))) return(blocks)
  space <- blocks |>
    map_lgl(
      \(x) {
        any(x == ".") &&
          sum(x == ".") >= length(repl[[1]])
      }
    )
  if (all(!space)) return(blocks)
  if (min(which(space)) >= i) return(blocks)
  bl <- blocks |>
    modify_at(min(which(space)), \(x) {
      sp <- which(x == ".")
      x[min(sp):(min(sp)+length(repl[[1]])-1)] <- repl[[1]]
      x
    }) |>
    modify_at(i, \(x) rep(".", length(repl[[1]])))
}
rearrange_blocks_p2 <- function(blocks) {
  length(blocks):1 |>
    reduce(
      .init = blocks,
      \(b, n) {
        find_space(blocks = b, i = n)
      }
    )
}
calculate_checksum_p2 <- function(arr_blocks) {
  arr_blocks |>
    map2_dbl((1:length(arr_blocks))-1, \(.x, .y) {
      if (.x == ".") return(0)
      as.numeric(.x)*.y
    }) |>
    sum()
}

test_2 <- rearrange_blocks_p2(test_blocks)
test_2 |>
  unlist() |>
  str_c(collapse="")
test_2 |> unlist() |> calculate_checksum_p2()



# Real
input <- readr::read_lines("posts/advent_of_code_2024/day_9/input.txt")
in_blocks <- input |>
  to_blocks()
rearranged_input <- in_blocks |>
  rearrange_blocks_p2()
rearranged_input |> unlist() -> zz
calculate_checksum(zz)

