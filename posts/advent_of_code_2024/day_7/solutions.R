library(purrr)
library(stringr)

test_input <- readr::read_lines("posts/advent_of_code_2024/day_7/test.txt") |>
  map(
    \(str) {
      elements <- as.numeric(str_extract_all(str, "\\d+")[[1]])
      list(target = elements[1],
           nums = elements[-1])
    }
  )

input <- readr::read_lines("posts/advent_of_code_2024/day_7/input.txt") |>
  map(
    \(str) {
      elements <- as.numeric(str_extract_all(str, "\\d+")[[1]])
      list(target = elements[1],
           nums = elements[-1])
    }
  )

calc_possible <- function(target, nums, ops_vec = c(`*`, `+`), pr) {
  if(length(pr)) pr$tick()
  ops <- map(1:(length(nums)-1), \(i) ops_vec)
  op_combos <- tidyr::expand_grid(!!!ops)

  res <- purrr::pmap_dbl(
    op_combos,
    \(..., nv = nums) {
      nv |>
        purrr::reduce2(
          list(...),
          \(total, num, op, t = target) {
            r <- op(total, num)
            if (r == t) done(r)
            r
          }
        )
    }
  )
  target * any(res == target)
}

res <- input |>
  purrr::map_dbl(\(v) calc_possible(v$target, v$nums, pr = NULL))

# Don't need to run if greater the first time? elephant will always increase value
res_targets <- purrr::map_dbl(input, \(x) x$target)
subs_input <- input[res==0]

elephant_str <- function(x, y) as.numeric(str_c(x, y))
elephant_sprintf <- function(x, y) as.numeric(sprintf("%s%s", x, y))


pb <- progress_bar$new(total = length(subs_input))

res2 <- subs_input |>
  purrr::map_dbl(\(v) calc_possible(v$target, v$nums, ops_vec = c(`*`, `+`, elephant_sprintf), pr = pb))
