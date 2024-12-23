library(stringr)
library(dplyr)
library(tibble)
library(purrr)
library(readr)
library(tidyr)

test <- read_lines("posts/advent_of_code_2024/day_11/test.txt") |>
  str_split(" ") |>
  pluck(1) |>
  as.list()

blink_l <- function(l) {
  l |>
    map(
      \(x) {
        if (x == 0) return(1)
        if (str_length(x)%%2==0) {
          new <- x |>
            str_extract_all(str_c("\\d{", str_length(x)/2, "}")) |>
            unlist() |>
            as.numeric() |>
            as.character()
          return(new)
        }
        return(as.character((as.numeric(x)*2024)))
      }
    ) |>
    unlist() |>
    as.list()
}

blink_stone <- function(s) {
  if (s == 0) return(1)
  if (nchar(s)%%2==0) return(
    c(
      as.numeric(substr(s, 1, nchar(s)/2)),
      as.numeric(substr(s, (nchar(s)/2)+1, nchar(s)))
    )
  )
  return(s*2024)
}
blink <- function(v) {
  unlist(sapply(v, blink_stone))
}


microbenchmark::microbenchmark(
  orig = blink_l(test),
  new = blink(as.numeric(unlist(test)))
)

result <- as.numeric(unlist(test))
for (i in 1:25) {
  result <- blink(result)
}

input <- read_lines("posts/advent_of_code_2024/day_11/input.txt") |>
  str_split(" ") |>
  pluck(1) |>
  as.list()


zeros_results_after <- list()
zeros_zeros_after <- list()
real_result <- 0
for (i in 1:40) {
  print(paste("iteration", i, "-", length(real_result), "elements"))
  real_result <- blink(real_result)
  zeros_results_after[i] <- length(real_result)
  zeros_zeros_after[i] <- sum(real_result == 0)
}



after_n_blinks <- function(stone, remaining_blinks) {
  a_cache <- new.env()

}















# hrbrmstrs solution v

transform_stone <- function(stone) {
  if (stone == 0) return(1L)
  stone_str <- as.character(stone)
  if (nchar(stone_str)%%2==0) {
    half_length <- nchar(stone_str) / 2
    c(
      as.integer(substr(stone_str, 1, half_length)),
      as.integer(substr(stone_str, half_length+1, nchar(stone_str)))
    )
  } else {
    stone * 2024
  }
}

count_stones_after_blinks <- function(initial_stones, total_blinks) {

  transformation_cache <- new.env()

  count_stones <- function(stone, remaining_blinks) {
    cache_key <- paste(stone, remaining_blinks, sep = "_")

    if (exists(cache_key, transformation_cache)) {
      return(get(cache_key, transformation_cache))
    }

    transformed_stones <- transform_stone(stone)

    if (remaining_blinks == 1) {
      result <- length(transformed_stones)
    } else {
      result <- sum(sapply(transformed_stones, count_stones, remaining_blinks-1))
    }

    assign(cache_key, result, transformation_cache)
    result
  }

  sum(sapply(initial_stones, count_stones, total_blinks))
}

test_v <- test |> unlist() |> as.numeric()
input_v <- input |> unlist() |> as.numeric()
count_stones_after_blinks(input_v, 75)
