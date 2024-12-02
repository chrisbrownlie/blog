library(httr2)
library(rvest)

convert_linkedin_to_qmd <- function(article_url) {
  raw_req <- article_url |>
    request() |>
    req_method("GET") |>
    req_perform()

  resp <- raw_req |>
    resp_body_html()

  # Get title
  title <- resp |>
    html_element(css = "h1") |>
    html_text2()

  # Get description
  desc <- resp |>
    html_element("meta[property='og:description']") |>
    html_attr("content")

  # Get date
  pub_date <- resp |>
    html_element(".base-main-card__metadata") |>
    html_text2() |>
    stringr::str_remove("^Published ") |>
    lubridate::mdy()

  # Get content of article
  body <- resp |>
    html_element("div.article-main__content")

  front_matter <- c(
    "---",
    glue::glue('title: "{title}"'),
    glue::glue('description: "{desc}"'),
    'author: "Chris Brownlie"',
    glue::glue('date: {format(pub_date, "%Y-%m-%d")}'),
    "from: markdown+emoji",
    "categories:",
    "- rstats",
    '- "this week in r"',
    "---"
  )

  # Create folder in posts folder
  folder_name <- title |>
    stringr::str_to_lower() |>
    stringr::str_remove_all("[\"\\-\\(\\)]") |>
    stringr::str_replace_all(" ", "_")

  folder_path <- fs::path("posts",
                          "this_week_in_r",
                          folder_name)

  if (fs::dir_exists(folder_path)) {
    cli::cli_alert_info("Looks like article {.val {title}} has already been converted, aborting")
    return(FALSE)
  }

  folder_path |>
    fs::dir_create()

  # Write post to html
  body |>
    xml2::write_html(file = "temp.html")

  # Convert to qmd
  rmarkdown::pandoc_convert(
    input = "temp.html",
    to = "markdown",
    output = "temp.md"
  )

  # Read in md
  converted_post_md <- "temp.md" |>
    readr::read_lines() |>
    purrr::discard(\(x) stringr::str_starts(x, ":::"))

  # Add frontmatter and write
  c(
    front_matter,
    "",
    '![](/assets/this_week_in_r.png){height=90% width=90% fig-align=center fig-alt="Logo for This week in R. Numbers and graphs on a blue background with the text \'This week in R\'"}',
    "",
    converted_post_md,
    "",
    glue::glue("**This was originally posted on Linkedin [here]({article_url}) and automatically converted to qmd, so may contain some formatting issues**")
  ) |>
    readr::write_lines(
      fs::path(folder_path, "index.qmd")
    )

  cli::cli_alert_success("Article {.val {title}} converted to qmd")
  return(TRUE)
}
