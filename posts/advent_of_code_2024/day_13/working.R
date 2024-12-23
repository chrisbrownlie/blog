library(stringr)
library(dplyr)
library(tibble)
library(purrr)
library(readr)
library(tidyr)

test <- read_file("posts/advent_of_code_2024/day_13/test.txt") |>
  str_split("\nButton A") |>
  pluck(1)

solutions <- test |>
  map(
    \(str) {
      n <- str_extract_all(str, "\\d+") |> pluck(1) |> as.numeric()
      a <- matrix(c(n[1], n[3], n[2], n[4]), nrow = 2, byrow = TRUE)
      b <- c(n[5], n[6])
      solve(a,b)
    }
  )
cost <- solutions |>
  map_dbl(
    \(sol) {
      if (any(sol>100) || !is.integer(type.convert(sol, as.is = TRUE))) return(NA)
      (sol[1]*3)+sol[2]
    }
  )


input <- read_file("posts/advent_of_code_2024/day_13/input.txt") |>
  str_split("\nButton A") |>
  pluck(1)
solutions_real <- input |>
  map(
    \(str) {
      n <- str_extract_all(str, "\\d+") |> pluck(1) |> as.numeric()
      a <- matrix(c(n[1], n[3], n[2], n[4]), nrow = 2, byrow = TRUE)
      b <- c(n[5], n[6])
      solve(a,b)
    }
  )
cost_real <- solutions_real |>
  map_dbl(
    \(sol) {
      if (any(sol>100) || !(all(near(sol%%1, 1)|near(sol%%1, 0)))) {
        print(paste0("invalid: ", paste(sol, collapse = ",")))
        return(NA)
      }
      (sol[1]*3)+sol[2]
    }
  )


# Part 2
solutions_new <- input |>
  map(
    \(str) {
      n <- str_extract_all(str, "\\d+") |> pluck(1) |> as.numeric()
      a <- matrix(c(n[1], n[3], n[2], n[4]), nrow = 2, byrow = TRUE)
      b <- c(n[5]+10000000000000, n[6]+10000000000000)
      solve(a,b)
    }
  )
cost_new <- solutions_new |>
  map_dbl(
    \(sol) {
      if (!(all(near(sol%%1, 1, tol =0.001)|near(sol%%1, 0, tol=0.001)))) {
        print(paste0("invalid: ", paste(sol, collapse = ",")))
        return(NA)
      }
      (sol[1]*3)+sol[2]
    }
  )
