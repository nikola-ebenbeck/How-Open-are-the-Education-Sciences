# This function cleans and pulls the links into the desired format. This is not a part of the analysis and should be considered as a producer.

library(RSQLite)
library(tidyverse)
library(readr)
library(stringr)
library(scales)
library(jsonlite)
library(httr)
library(progress)

get_authors <- function(openalex_id) {
  if (is.na(openalex_id) || !nzchar(openalex_id)) {
    return(NA_character_)
  }
  url <- paste0("https://api.openalex.org/works/", openalex_id)
  tryCatch(
    {
      response <- GET(url, timeout(10)) # 10 second timeout
      if (
        status_code(response) == 200 &&
          http_type(response) == "application/json"
      ) {
        data <- suppressWarnings(fromJSON(content(
          response,
          "text",
          encoding = "UTF-8"
        )))
        if (!is.null(data$authorships) && length(data$authorships) > 0) {
          authors <- data$authorships$author$display_name
          if (length(authors) > 0 && all(is.character(authors))) {
            return(paste(authors, collapse = "; "))
          }
        }
      }
      return(NA_character_)
    },
    error = function(e) {
      return(NA_character_)
    }
  )
}

get_title <- function(openalex_id) {
  if (is.na(openalex_id) || !nzchar(openalex_id)) {
    return(NA_character_)
  }
  url <- paste0("https://api.openalex.org/works/", openalex_id)
  tryCatch(
    {
      response <- GET(url, timeout(10))
      if (
        status_code(response) == 200 &&
          http_type(response) == "application/json"
      ) {
        data <- suppressWarnings(fromJSON(content(
          response,
          "text",
          encoding = "UTF-8"
        )))
        if (!is.null(data$title) && is.character(data$title)) {
          return(data$title)
        }
      }
      return(NA_character_)
    },
    error = function(e) {
      return(NA_character_)
    }
  )
}


get_country <- function(openalex_id) {
  if (is.na(openalex_id) || !nzchar(openalex_id)) {
    return(NA_character_)
  }
  url <- paste0("https://api.openalex.org/works/", openalex_id)
  tryCatch(
    {
      response <- GET(url, timeout(10))
      if (
        status_code(response) == 200 &&
          http_type(response) == "application/json"
      ) {
        data <- suppressWarnings(fromJSON(content(
          response,
          "text",
          encoding = "UTF-8"
        )))

        if (!is.null(data$authorships) && length(data$authorships) > 0) {
          codes <- c()
          for (a in data$authorships) {
            if (!is.null(a$institutions) && length(a$institutions) > 0) {
              for (inst in a$institutions) {
                if (!is.null(inst$country_code) && nzchar(as.character(inst$country_code))) {
                  codes <- c(codes, toupper(as.character(inst$country_code)))
                } else if (!is.null(inst$id) && nzchar(as.character(inst$id))) {
                  try({
                    inst_resp <- GET(as.character(inst$id), timeout(5))
                    if (status_code(inst_resp) == 200 && http_type(inst_resp) == "application/json") {
                      inst_data <- suppressWarnings(fromJSON(content(inst_resp, "text", encoding = "UTF-8")))
                      if (!is.null(inst_data$country_code) && nzchar(as.character(inst_data$country_code))) {
                        codes <- c(codes, toupper(as.character(inst_data$country_code)))
                      }
                    }
                  }, silent = TRUE)
                }
              }
            }
          }

          codes <- unique(codes[!is.na(codes) & nzchar(codes)])
          if (length(codes) > 0) {
            return(paste(codes, collapse = "; "))
          }
        }
      }
      return(NA_character_)
    },
    error = function(e) {
      return(NA_character_)
    }
  )
}

format_osf_links <- function(link_text) {
  if (is.na(link_text) || !nzchar(link_text)) {
    return(NA_character_)
  }
  links <- strsplit(link_text, "\\; ")[[1]]

  formatted_links <- sapply(links, function(link) {
    link <- str_trim(link)

    link <- str_replace_all(link, " only", "only")
    link <- str_replace_all(link, "view  ", "view")
    link <- str_replace_all(link, " ", "")

    link <- str_to_lower(link)

    if (!str_starts(link, "https://") && !str_starts(link, "http://")) {
      link <- paste0("https://", link)
    }

    return(link)
  })

  return(paste(formatted_links, collapse = "; "))
}

conn <- dbConnect(RSQLite::SQLite(), "example_path.db") # ../../db/index.merged.db
works <- dbGetQuery(conn, "SELECT * FROM works")
paper_links <- dbGetQuery(
  conn,
  "SELECT openalex_id, osf_links, git_links FROM paper_links"
)
dbDisconnect(conn)


journal_map <- tribble(
  ~short , ~id           , ~full_name                                                            ,
  "mdpi" , "S2738008561" , "Education Sciences"                                                  ,
  "fe"   , "S2596526815" , "Frontiers in Education"                                              ,
  "cog"  , "S2764918247" , "COGENT EDUCATION"                                                    ,
  "flr"  , "S4210191100" , "Frontline Learning Research"                                         ,
  "aero" , "S2738252563" , "AERA Open"                                                           ,
  "sage" , "S148277943"  , "SAGE Open"                                                           ,
  "ijet" , "S4210201537" , "International Journal of Educational Technology in Higher Education" ,
)


reg <- journal_map |>
  select(id, journal_short = short, journal_long = full_name)

parse_link_column <- function(link_text) {
  if (is.na(link_text) || !nzchar(link_text)) {
    return(character(0))
  }
  strsplit(link_text, "\\|\\|")[[1]]
}

index_with_links <- works |>
  inner_join(reg, by = c("journal_id" = "id")) |>
  left_join(paper_links, by = "openalex_id") |>
  mutate(
    osf_links_raw = map(osf_links, parse_link_column),
    git_links_raw = map(git_links, parse_link_column),
    has_osf = map_lgl(osf_links_raw, ~ length(.x) > 0),
    has_git = map_lgl(git_links_raw, ~ length(.x) > 0)
  ) |>
  mutate(
    all_osf_links = map_chr(osf_links_raw, ~ paste(.x, collapse = "; ")),
    all_git_links = map_chr(git_links_raw, ~ paste(.x, collapse = "; ")),
    has_any_link = has_osf | has_git
  ) |>
  select(-osf_links_raw, -git_links_raw, -osf_links, -git_links)

print(index_with_links %>% count(journal_short, publication_year, has_any_link))

clean_links <- function(link_text) {
  if (is.na(link_text) || !nzchar(link_text)) {
    return(NA_character_)
  }
  link_text |>
    str_trim() |>
    str_to_lower() |>
    str_remove("^https?://") |>
    str_remove_all("/+$") |>
    unique() |>
    paste(collapse = "; ")
}

index_with_links <- index_with_links |>
  mutate(
    all_osf_links_clean = map_chr(all_osf_links, clean_links),
    all_git_links_clean = map_chr(all_git_links, clean_links)
  ) 


write.csv(index_with_links, "results/all_papers_data_tmp.csv")

subset_df <- index_with_links |>
  filter(has_any_link) |>
  select(
    openalex_id,
    journal_name,
    journal_id,
    has_git,
    has_osf,
    has_any_link
  )

p <- progress_bar$new(
  total = 2 * nrow(subset_df),
  format = "Fetching authors & titles [:bar] :percent :eta"
)

subset_df <- subset_df |>
  mutate(
    author = map_chr(
      openalex_id,
      ~ {
        p$tick()
        get_authors(.x)
      }
    ),
    title = map_chr(
      openalex_id,
      ~ {
        p$tick()
        get_title(.x)
      }
    ),
  )

subset_df <- subset_df |>
  mutate(osf_links = map_chr(osf_links, format_osf_links))

write.csv(subset_df, "results/papers_subset.csv")