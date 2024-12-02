source("scripts/convert_linkedin.R")

# Add new weeks to the top of the vector for now
articles <- c(
  "https://www.linkedin.com/pulse/week-r-2024-11-29-chris-brownlie-ox3ee",
  "https://www.linkedin.com/pulse/week-r-2024-11-22-chris-brownlie-lvouc",
  "https://www.linkedin.com/pulse/week-r-2024-11-15-chris-brownlie-xwgte",
  "https://www.linkedin.com/pulse/week-r-2024-11-08-chris-brownlie-ln6ae",
  "https://www.linkedin.com/pulse/week-r-2024-11-01-chris-brownlie-kshpe",
  "https://www.linkedin.com/pulse/week-r-2024-10-25-chris-brownlie-zqsie",
  "https://www.linkedin.com/pulse/week-r-2024-10-18-chris-brownlie-lgpme",
  "https://www.linkedin.com/pulse/week-r-2024-10-11-chris-brownlie-9lv3e",
  "https://www.linkedin.com/pulse/week-r-2024-10-04-chris-brownlie-jo3ve",
  "https://www.linkedin.com/pulse/week-r-2024-09-27-chris-brownlie-l8zbe",
  "https://www.linkedin.com/pulse/week-r-2024-09-20-chris-brownlie-mw2ye",
  "https://www.linkedin.com/pulse/week-r-2024-04-26-chris-brownlie-kbgce",
  "https://www.linkedin.com/pulse/week-r-2024-05-03-chris-brownlie-m1vsc",
  "https://www.linkedin.com/pulse/week-r-2024-04-19-chris-brownlie-lhfle",
  "https://www.linkedin.com/pulse/week-r-2024-04-12-chris-brownlie-rofhe",
  "https://www.linkedin.com/pulse/week-r-2024-04-05-chris-brownlie-vf5ge",
  "https://www.linkedin.com/pulse/week-r-2024-03-22-chris-brownlie-jolge",
  "https://www.linkedin.com/pulse/week-r-chris-brownlie-rnjve",
  "https://www.linkedin.com/pulse/week-r-2024-03-15-chris-brownlie-ae5ue"
)

purrr::walk(
  articles,
  convert_linkedin_to_qmd
)
